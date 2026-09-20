.safe_scale <- function(x) { s <- stats::sd(x, na.rm = TRUE); if (!is.finite(s) || s == 0) 1 else s }

# Text category labels carry no order. Coercing them with factor() would sort
# them alphabetically -- "always" < "never" < "often" < "rarely" < "sometimes"
# for an ordinary frequency scale -- which scrambles the response scale and
# destroys the construct without any error. Numeric strings ("1", "2") are
# unambiguous and are converted; anything else must arrive as an ordered factor
# (whose level order the user declared) or as integer codes.
.as_ordinal_codes <- function(x) {
  if (!is.character(x)) return(x)
  numeric_x <- suppressWarnings(as.numeric(x))
  unparsed <- is.na(numeric_x) & !is.na(x)
  if (any(unparsed))
    stop("Ordinal indicators supplied as text have no inferable category order (found \"",
      x[which(unparsed)[1L]], "\"); alphabetical order would scramble the scale. ",
      "Supply the item as an ordered factor whose levels are in scale order, or as integer category codes.",
      call. = FALSE)
  numeric_x
}

.prepare_item <- function(x, scale, key, levels = NULL) {
  if (scale %in% c("continuous", "manifest")) {
    y <- suppressWarnings(as.numeric(x)); if (key < 0) y <- -y
    return(list(y = y, levels = NULL))
  }
  x <- .as_ordinal_codes(x)
  # Ordinal category codes must be whole numbers. Silently truncating a
  # non-integer (e.g. an averaged sub-scale accidentally declared ordinal)
  # would quietly discard information instead of surfacing the mistake.
  if (!is.factor(x)) {
    numeric_x <- suppressWarnings(as.numeric(x))
    non_integer <- !is.na(numeric_x) & abs(numeric_x - round(numeric_x)) > 1e-8
    if (any(non_integer))
      stop("Ordinal indicators must have whole-number category codes (e.g. 1, 2, 3); found a non-integer value (",
        signif(numeric_x[which(non_integer)[1L]], 6),
        "). Declare this item continuous(), or round/bin it to categories before declaring it ordinal().",
        call. = FALSE)
  }
  # A factor's own level order is the declared scale order, so its codes are
  # level positions rather than a re-sort.
  raw <- as.integer(x)
  lev <- if (is.null(levels)) sort(unique(raw[!is.na(raw)])) else levels
  if (length(lev) < 2L) stop("Ordinal indicators need at least two observed categories.", call. = FALSE)
  y <- match(raw, lev); if (key < 0) y <- ifelse(is.na(y), NA_integer_, length(lev) + 1L - y)
  list(y = y, levels = lev)
}

.prepare_for_encoder <- function(x, scale, key, levels) {
  if (scale %in% c("continuous", "manifest")) {
    y <- suppressWarnings(as.numeric(x)); if (key < 0) y <- -y
    return(y)
  }
  raw <- as.integer(.as_ordinal_codes(x))
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

# Closed-form weighted-least-squares M-step for one continuous item in the
# mixture regression y_j = intercept + slope * z + N(0, sigma^2). Expands
# each observed respondent's contribution across every quadrature node,
# weighted by that respondent's posterior probability at the node -- the
# standard EM M-step for a linear-Gaussian mixture, so no numerical
# optimizer is needed (unlike the ordinal item's BFGS step).
.continuous_em_update <- function(nodes, y, posterior) {
  observed <- !is.na(y)
  w <- as.vector(posterior[observed, , drop = FALSE])
  design <- cbind(1, rep(nodes, each = sum(observed)))
  response <- rep(y[observed], times = length(nodes))
  fit <- stats::lm.wfit(design, response, w)
  resid <- response - drop(design %*% fit$coefficients)
  sigma <- sqrt(sum(w * resid^2) / sum(w))
  list(type = "continuous", intercept = unname(fit$coefficients[1L]), slope = unname(fit$coefficients[2L]),
    sigma = max(sigma, 1e-6))
}

.encoder_params <- function(e) if (identical(e$type, "ordinal")) c(e$a, e$tau) else c(e$intercept, e$slope, e$sigma)

# Respondent-by-node posterior for a marginal mixture measurement model.
# Ordinal items contribute a graded-response category log-probability;
# continuous items contribute a Gaussian log-density around intercept +
# slope * node. Both are summed in log-space onto the same quadrature grid,
# so ordinal-only, continuous-only, and mixed constructs share one posterior
# computation.
.eap_posterior <- function(encoders, Y, nodes, prior_weights) {
  n <- nrow(Y); log_posterior <- matrix(log(prior_weights), n, length(nodes), byrow = TRUE)
  for (j in seq_along(encoders)) {
    y <- Y[, j]; observed <- !is.na(y); if (!any(observed)) next
    e <- encoders[[j]]
    if (identical(e$type, "continuous")) {
      mu <- e$intercept + e$slope * nodes
      dens <- outer(y[observed], mu, function(yy, mm) stats::dnorm(yy, mm, e$sigma, log = TRUE))
      log_posterior[observed, ] <- log_posterior[observed, , drop = FALSE] + dens
    } else {
      p <- .ordinal_probability(e$a, e$tau, nodes, e$k)
      log_posterior[observed, ] <- log_posterior[observed, , drop = FALSE] + t(log(p[, y[observed], drop = FALSE] + 1e-12))
    }
  }
  max_log <- apply(log_posterior, 1L, max)
  unnorm <- exp(log_posterior - max_log)
  unnorm / rowSums(unnorm)
}

# Marginal-ML/EM measurement model for one construct, on a shared quadrature
# grid. Item types (ordinal graded-response, continuous linear-Gaussian) are
# dispatched per item within the same E-step/M-step loop, so an all-ordinal
# construct runs the identical sequence of operations as before generalizing
# to continuous/mixed items -- this function is a strict superset of the
# prior ordinal-only estimator, not a rewrite of it.
.fit_construct_mml <- function(Y, scales, k, iterations = 30L, nodes = seq(-4, 4, length.out = 31L)) {
  prior_weights <- stats::dnorm(nodes); prior_weights <- prior_weights / sum(prior_weights)
  starter <- apply(Y, 2L, function(y) (y - mean(y, na.rm = TRUE)) / .safe_scale(y))
  z <- rowMeans(starter, na.rm = TRUE); z[!is.finite(z)] <- 0; z <- as.numeric(scale(z))
  encoders <- lapply(seq_len(ncol(Y)), function(j)
    if (scales[j] == "ordinal") .fit_ordinal(z, Y[, j], k[j]) else .fit_continuous(z, Y[, j]))
  converged <- FALSE
  for (step in seq_len(iterations)) {
    posterior <- .eap_posterior(encoders, Y, nodes, prior_weights)
    next_encoders <- lapply(seq_along(encoders), function(j) {
      old <- encoders[[j]]; y <- Y[, j]
      if (identical(old$type, "ordinal")) {
        start <- c(log(old$a), .threshold_parameters(old$tau))
        opt <- stats::optim(start, .ordinal_em_nll, nodes = nodes, y = y, posterior = posterior, k = old$k,
          method = "BFGS", control = list(maxit = 100L))
        list(type = "ordinal", a = exp(opt$par[1L]),
          tau = cumsum(c(opt$par[2L], exp(opt$par[-c(1L, 2L)]))), k = old$k)
      } else {
        .continuous_em_update(nodes, y, posterior)
      }
    })
    delta <- max(vapply(seq_along(encoders), function(j) {
      max(abs(.encoder_params(encoders[[j]]) - .encoder_params(next_encoders[[j]])))
    }, numeric(1)))
    encoders <- next_encoders
    if (delta < 1e-3) { converged <- TRUE; break }
  }
  posterior <- .eap_posterior(encoders, Y, nodes, prior_weights)
  scores <- drop(posterior %*% nodes)
  list(encoders = encoders, nodes = nodes, prior_weights = prior_weights,
    training_scores = (scores - mean(scores)) / .safe_scale(scores),
    converged = converged, iterations = step)
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

.fit_encoder <- function(data, spec, iterations = 6L, category_levels = NULL) {
  if (identical(spec$scales[[1L]], "manifest")) {
    y <- suppressWarnings(as.numeric(data[[spec$indicators]])); if (spec$keys[[1L]] < 0) y <- -y
    standardize <- isTRUE(spec$standardize)
    center <- if (standardize) mean(y, na.rm = TRUE) else 0
    sc <- if (standardize) .safe_scale(y) else 1
    return(list(type = "manifest", indicators = spec$indicators, key = spec$keys[[1L]],
      standardize = standardize, center = center, scale = sc,
      estimator = "manifest", converged = NA, iterations = 0L))
  }
  if (is.null(category_levels)) category_levels <- vector("list", length(spec$indicators))
  items <- Map(.prepare_item, data[spec$indicators], spec$scales, spec$keys, category_levels)
  Y <- do.call(cbind, lapply(items, `[[`, "y")); colnames(Y) <- spec$indicators
  k <- vapply(items, function(x) if (is.null(x$levels)) NA_integer_ else length(x$levels), integer(1))
  fitted <- .fit_construct_mml(Y, spec$scales, k = k, iterations = max(8L, iterations * 2L))
  estimator <- if (all(spec$scales == "ordinal")) "marginal_graded_response"
    else if (all(spec$scales == "continuous")) "marginal_linear_factor"
    else "marginal_mixed"
  c(fitted, list(estimator = estimator, indicators = spec$indicators, scales = spec$scales, keys = spec$keys,
    levels = lapply(items, `[[`, "levels")))
}

.predict_encoder <- function(encoder, data) {
  if (!identical(names(data), encoder$indicators)) stop("Scoring data columns must exactly match the declared indicator order.", call. = FALSE)
  if (identical(encoder$type, "manifest")) {
    y <- suppressWarnings(as.numeric(data[[encoder$indicators]])); if (encoder$key < 0) y <- -y
    return((y - encoder$center) / encoder$scale)
  }
  Y <- do.call(cbind, Map(.prepare_for_encoder, data, encoder$scales, encoder$keys, encoder$levels))
  posterior <- .eap_posterior(encoder$encoders, Y, encoder$nodes, encoder$prior_weights)
  drop(posterior %*% encoder$nodes)
}

# Return the full respondent-by-node posterior for a marginal mixture
# encoder, or NULL for a manifest (passthrough, no latent grid) encoder.
# This is the principled measurement-uncertainty object: the posterior mean
# is the locked score, the posterior variance is the respondent's
# measurement information, and posterior draws are plausible latent values.
.encoder_posterior <- function(encoder, data) {
  if (identical(encoder$type, "manifest")) return(NULL)
  if (!identical(names(data), encoder$indicators)) stop("Scoring data columns must exactly match the declared indicator order.", call. = FALSE)
  Y <- do.call(cbind, Map(.prepare_for_encoder, data, encoder$scales, encoder$keys, encoder$levels))
  .eap_posterior(encoder$encoders, Y, encoder$nodes, encoder$prior_weights)
}

# Posterior mean and variance on the raw latent-node scale (one row per
# respondent). Variance is the EAP posterior variance used for reliability and
# for the heteroskedastic respondent-information diagnostic.
.posterior_moments <- function(posterior, nodes) {
  mean <- drop(posterior %*% nodes)
  list(mean = mean, variance = pmax(drop(posterior %*% nodes^2) - mean^2, 0))
}

# Draw plausible latent values from a respondent-by-node posterior. Each draw
# samples one node per respondent with probability equal to that respondent's
# posterior row, returning a length-n numeric vector on the raw node scale.
.draw_posterior_values <- function(posterior, nodes) {
  cumulative <- t(apply(posterior, 1L, cumsum))
  u <- stats::runif(nrow(posterior))
  index <- max.col(u <= cumulative, ties.method = "first")
  nodes[index]
}

.ordinal_expected <- function(encoder, z) {
  q <- sapply(encoder$tau, function(t) stats::plogis(t - encoder$a * z))
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)
  p <- cbind(q[, 1L], q[, -1L, drop = FALSE] - q[, -ncol(q), drop = FALSE], 1 - q[, ncol(q)])
  drop(p %*% seq_len(encoder$k))
}

.item_metrics <- function(encoder, data) {
  if (identical(encoder$type, "manifest"))
    return(data.frame(item = character(), metric = character(), value = numeric()))
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
