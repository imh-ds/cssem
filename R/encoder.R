.safe_scale <- function(x) { s <- stats::sd(x, na.rm = TRUE); if (!is.finite(s) || s == 0) 1 else s }

.prepare_item <- function(x, scale, key, levels = NULL) {
  if (scale == "continuous") {
    y <- suppressWarnings(as.numeric(x)); if (key < 0) y <- -y
    return(list(y = y, levels = NULL))
  }
  raw <- if (is.factor(x) || is.character(x)) as.integer(factor(x, ordered = TRUE)) else as.integer(x)
  lev <- if (is.null(levels)) sort(unique(raw[!is.na(raw)])) else levels
  if (length(lev) < 2L) stop("Ordinal indicators need at least two observed categories.", call. = FALSE)
  y <- match(raw, lev); if (key < 0) y <- ifelse(is.na(y), NA_integer_, length(lev) + 1L - y)
  list(y = y, levels = lev)
}

.prepare_for_encoder <- function(x, scale, key, levels) {
  if (scale == "continuous") {
    y <- suppressWarnings(as.numeric(x)); if (key < 0) y <- -y
    return(y)
  }
  raw <- if (is.factor(x) || is.character(x)) as.integer(factor(x, ordered = TRUE)) else as.integer(x)
  y <- match(raw, levels)
  if (any(!is.na(raw) & is.na(y))) stop("Scoring data contain an unseen ordinal category.", call. = FALSE)
  if (key < 0) y <- ifelse(is.na(y), NA_integer_, length(levels) + 1L - y)
  y
}

.ordinal_nll <- function(par, z, y, ridge = .02) {
  a <- exp(par[1]); tau <- cumsum(c(par[2], exp(par[-c(1, 2)])))
  eta <- outer(z, tau, function(zz, tt) stats::plogis(tt - a * zz))
  p <- cbind(eta[, 1], eta[, -1, drop = FALSE] - eta[, -ncol(eta), drop = FALSE], 1 - eta[, ncol(eta)])
  -sum(log(p[cbind(seq_along(y), y)] + 1e-12)) + ridge * par[1]^2
}

.ordinal_probability <- function(a, tau, nodes, k) {
  q <- sapply(tau, function(t) stats::plogis(t - a * nodes))
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)
  cbind(q[, 1L], q[, -1L, drop = FALSE] - q[, -ncol(q), drop = FALSE], 1 - q[, ncol(q)])
}

.threshold_parameters <- function(tau) {
  if (length(tau) == 1L) return(tau)
  c(tau[1L], log(pmax(diff(tau), .05)))
}

.ordinal_em_nll <- function(par, nodes, y, posterior, k, ridge = .02) {
  a <- exp(par[1L]); tau <- cumsum(c(par[2L], exp(par[-c(1L, 2L)])))
  p <- .ordinal_probability(a, tau, nodes, k)
  keep <- !is.na(y)
  likelihood <- p[, y[keep], drop = FALSE]
  # likelihood is node x respondent; posterior is respondent x node.
  -sum(t(posterior[keep, , drop = FALSE]) * log(likelihood + 1e-12)) + ridge * par[1L]^2
}

.eap_posterior <- function(encoders, Y, nodes, prior_weights) {
  n <- nrow(Y); log_posterior <- matrix(log(prior_weights), n, length(nodes), byrow = TRUE)
  for (j in seq_along(encoders)) {
    y <- Y[, j]; observed <- !is.na(y); if (!any(observed)) next
    e <- encoders[[j]]
    p <- .ordinal_probability(e$a, e$tau, nodes, e$k)
    log_posterior[observed, ] <- log_posterior[observed, , drop = FALSE] + t(log(p[, y[observed], drop = FALSE] + 1e-12))
  }
  max_log <- apply(log_posterior, 1L, max)
  unnorm <- exp(log_posterior - max_log)
  unnorm / rowSums(unnorm)
}

.fit_ordinal_mml <- function(Y, k, iterations = 30L, nodes = seq(-4, 4, length.out = 31L)) {
  prior_weights <- stats::dnorm(nodes); prior_weights <- prior_weights / sum(prior_weights)
  starter <- apply(Y, 2L, function(y) (y - mean(y, na.rm = TRUE)) / .safe_scale(y))
  z <- rowMeans(starter, na.rm = TRUE); z[!is.finite(z)] <- 0; z <- as.numeric(scale(z))
  encoders <- lapply(seq_len(ncol(Y)), function(j) .fit_ordinal(z, Y[, j], k[j]))
  converged <- FALSE
  for (step in seq_len(iterations)) {
    posterior <- .eap_posterior(encoders, Y, nodes, prior_weights)
    next_encoders <- lapply(seq_along(encoders), function(j) {
      old <- encoders[[j]]; y <- Y[, j]
      start <- c(log(old$a), .threshold_parameters(old$tau))
      opt <- stats::optim(start, .ordinal_em_nll, nodes = nodes, y = y, posterior = posterior, k = old$k,
        method = "BFGS", control = list(maxit = 100L))
      list(type = "ordinal", a = exp(opt$par[1L]),
        tau = cumsum(c(opt$par[2L], exp(opt$par[-c(1L, 2L)]))), k = old$k,
        convergence = opt$convergence)
    })
    delta <- max(vapply(seq_along(encoders), function(j) {
      max(abs(c(encoders[[j]]$a, encoders[[j]]$tau) - c(next_encoders[[j]]$a, next_encoders[[j]]$tau)))
    }, numeric(1)))
    encoders <- next_encoders
    if (delta < 1e-3) { converged <- TRUE; break }
  }
  posterior <- .eap_posterior(encoders, Y, nodes, prior_weights)
  scores <- drop(posterior %*% nodes)
  list(encoders = encoders, nodes = nodes, prior_weights = prior_weights,
    training_scores = (scores - mean(scores)) / .safe_scale(scores),
    converged = converged, iterations = step, estimator = "marginal_graded_response")
}

.fit_ordinal <- function(z, y, k = NULL) {
  keep <- !is.na(y); yy <- y[keep]; zz <- z[keep]; k <- if (is.null(k)) max(yy) else k
  counts <- tabulate(yy, k) + .5
  probs <- pmin(pmax(cumsum(counts) / sum(counts), .02), .98)
  tau <- stats::qlogis(probs[-k])
  delta <- c(tau[1], log(pmax(diff(tau), .05)))
  opt <- stats::optim(c(log(1), delta), .ordinal_nll, z = zz, y = yy, method = "BFGS", control = list(maxit = 100))
  list(type = "ordinal", a = exp(opt$par[1]), tau = cumsum(c(opt$par[2], exp(opt$par[-c(1, 2)]))), k = k)
}

.fit_continuous <- function(z, y) {
  keep <- !is.na(y); yy <- y[keep]; zz <- z[keep]
  X <- cbind(1, zz); w <- rep(1, length(yy)); beta <- c(mean(yy), 0)
  for (i in seq_len(8L)) {
    beta <- stats::lm.wfit(X, yy, w)$coefficients
    r <- yy - drop(X %*% beta); s <- stats::mad(r, constant = 1, na.rm = TRUE) + 1e-6
    w <- pmin(1, 1.345 * s / pmax(abs(r), 1e-8))
  }
  list(type = "continuous", intercept = beta[1], slope = beta[2], sigma = sqrt(weighted.mean((yy - drop(X %*% beta))^2, w)) + 1e-6)
}

.row_nll <- function(z, enc, Y) {
  ans <- z^2 / 2
  for (j in seq_along(enc)) {
    y <- Y[[j]]; if (is.na(y)) next
    e <- enc[[j]]
    if (e$type == "continuous") ans <- ans + .5 * ((y - e$intercept - e$slope * z) / e$sigma)^2 + log(e$sigma)
    else {
      q <- stats::plogis(e$tau - e$a * z)
      p <- if (y == 1L) q[1] else if (y == e$k) 1 - q[e$k - 1L] else q[y] - q[y - 1L]
      ans <- ans - log(p + 1e-12)
    }
  }
  ans
}

.score_rows <- function(encoders, Y) {
  vapply(seq_len(nrow(Y)), function(i) stats::optimize(.row_nll, c(-5, 5), enc = encoders, Y = as.list(Y[i, ]))$minimum, numeric(1))
}

.ordinal_expected <- function(encoder, z) {
  q <- sapply(encoder$tau, function(t) stats::plogis(t - encoder$a * z))
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)
  p <- cbind(q[, 1L], q[, -1L, drop = FALSE] - q[, -ncol(q), drop = FALSE], 1 - q[, ncol(q)])
  drop(p %*% seq_len(encoder$k))
}

.fit_encoder <- function(data, spec, iterations = 6L, category_levels = NULL) {
  if (is.null(category_levels)) category_levels <- vector("list", length(spec$indicators))
  items <- Map(.prepare_item, data[spec$indicators], spec$scales, spec$keys, category_levels)
  Y <- do.call(cbind, lapply(items, `[[`, "y")); colnames(Y) <- spec$indicators
  if (all(spec$scales == "ordinal")) {
    fitted <- .fit_ordinal_mml(Y, k = vapply(items, function(x) length(x$levels), integer(1)), iterations = max(8L, iterations * 2L))
    return(c(fitted, list(indicators = spec$indicators, scales = spec$scales, keys = spec$keys,
      levels = lapply(items, `[[`, "levels"))))
  }
  starter <- apply(Y, 2, function(y) (y - mean(y, na.rm = TRUE)) / .safe_scale(y))
  z <- rowMeans(starter, na.rm = TRUE); z[!is.finite(z)] <- 0; z <- as.numeric(scale(z))
  enc <- NULL
  for (step in seq_len(iterations)) {
    enc <- Map(function(y, sc, lev) if (sc == "ordinal") .fit_ordinal(z, y, length(lev)) else .fit_continuous(z, y), as.data.frame(Y), spec$scales, lapply(items, `[[`, "levels"))
    z <- .score_rows(enc, Y); z <- (z - mean(z)) / .safe_scale(z)
  }
  list(encoders = enc, indicators = spec$indicators, scales = spec$scales, keys = spec$keys,
       levels = lapply(items, `[[`, "levels"), training_scores = z,
       converged = NA, iterations = iterations, estimator = "alternating_mixed_scale")
}

.predict_encoder <- function(encoder, data) {
  if (!identical(names(data), encoder$indicators)) stop("Scoring data columns must exactly match the declared indicator order.", call. = FALSE)
  Y <- do.call(cbind, Map(.prepare_for_encoder, data, encoder$scales, encoder$keys, encoder$levels))
  if (!is.null(encoder$nodes)) {
    posterior <- .eap_posterior(encoder$encoders, Y, encoder$nodes, encoder$prior_weights)
    return(drop(posterior %*% encoder$nodes))
  }
  .score_rows(encoder$encoders, Y)
}

.item_metrics <- function(encoder, data) {
  z <- .predict_encoder(encoder, data[, encoder$indicators, drop = FALSE])
  out <- vector("list", length(encoder$encoders))
  for (j in seq_along(out)) {
    y <- .prepare_item(data[[encoder$indicators[j]]], encoder$scales[j], encoder$keys[j], encoder$levels[[j]])$y; e <- encoder$encoders[[j]]
    if (e$type == "continuous") out[[j]] <- data.frame(item = encoder$indicators[j], metric = "rmse", value = sqrt(mean((y - e$intercept - e$slope * z)^2, na.rm = TRUE)))
    else {
      # row-wise category probability is calculated directly to avoid recycling ambiguity
      lp <- vapply(seq_along(y), function(i) if (is.na(y[i])) NA_real_ else { qq <- stats::plogis(e$tau - e$a * z[i]); pp <- if (y[i] == 1) qq[1] else if (y[i] == e$k) 1 - qq[e$k - 1] else qq[y[i]] - qq[y[i] - 1]; -log(pp + 1e-12) }, numeric(1))
      out[[j]] <- data.frame(item = encoder$indicators[j], metric = "log_loss", value = mean(lp, na.rm = TRUE))
    }
  }
  do.call(rbind, out)
}
