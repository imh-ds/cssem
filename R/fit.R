#' Fit cross-fitted CS-SEM construct states
#'
#' Fits a scale-aware measurement encoder for every theory-declared construct.
#' Ordinal-only blocks use a marginal graded-response model with EAP scoring.
#' Each returned locked score is predicted by an encoder trained without that
#' respondent's fold.
#'
#' @param model A `cssem_model` object.
#' @param data A data frame containing all declared indicator columns.
#' @param seed Integer random seed for fold assignment and experimental
#'   uncertainty draws.
#' @param draws Number of exploratory latent-state uncertainty draws. Set to
#'   zero to omit them. These draws are not release-validated inferential
#'   outputs.
#' @param iterations Maximum optimization iterations for each measurement fit.
#' @param diagnostics Whether to calculate exploratory residual diagnostics and
#'   item warnings. Disable for high-throughput simulation benchmarks.
#' @param preset Runtime preset. Use `"exploratory"` for lighter-weight fitting
#'   defaults while iterating on a live project.
#' @return An object of class `cssem_fit` containing locked scores, full-data
#'   encoders for future scoring, diagnostics, and measurement metadata.
#' @examples
#' data <- simulate_cssem_data(n = 80, seed = 1)
#' model <- cssem_model(list(
#'   A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
#'   B = list(indicators = paste0("b", 1:4), scales = "ordinal")
#' ), folds = 3)
#' fit <- cssem_fit(model, data, seed = 1, diagnostics = FALSE)
#' @family measurement fitting functions
#' @export
cssem_fit <- function(model, data, seed = 1L, draws = 0L, iterations = 6L,
                      diagnostics = TRUE, preset = c("default", "exploratory")) {
  if (!inherits(model, "cssem_model")) stop("model must be a cssem_model.", call. = FALSE)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  preset <- match.arg(preset)
  if (preset == "exploratory") {
    if (missing(iterations)) iterations <- 4L
    if (missing(draws)) draws <- 0L
  }
  needed <- unlist(lapply(model$constructs, `[[`, "indicators"), use.names = FALSE)
  if (!all(needed %in% names(data))) stop("data is missing declared indicators.", call. = FALSE)
  n <- nrow(data); if (n < model$folds * 4L) warning("Small folds may make construct states unstable.", call. = FALSE)
  set.seed(seed); fold <- sample(rep(seq_len(model$folds), length.out = n))
  construct_names <- names(model$constructs)
  locked <- matrix(NA_real_, n, length(model$constructs), dimnames = list(NULL, construct_names))
  # Raw-scale out-of-fold posterior variance; NA for constructs scored without a
  # latent grid (the experimental mixed-scale fallback).
  oof_variance <- matrix(NA_real_, n, length(model$constructs), dimnames = list(NULL, construct_names))
  oof_posterior <- vector("list", length(model$constructs)); names(oof_posterior) <- construct_names
  posterior_nodes <- NULL
  full <- vector("list", length(model$constructs)); names(full) <- construct_names
  metric_list <- list(); stability <- numeric(length(full)); names(stability) <- names(full)
  for (nm in names(model$constructs)) {
    spec <- model$constructs[[nm]]; fold_scores <- matrix(NA_real_, n, model$folds)
    category_levels <- Map(function(x, scale, key) .prepare_item(x, scale, key)$levels,
      data[spec$indicators], spec$scales, spec$keys)
    for (k in seq_len(model$folds)) {
      train <- data[fold != k, spec$indicators, drop = FALSE]; test <- data[fold == k, spec$indicators, drop = FALSE]
      enc <- .fit_encoder(train, spec, iterations, category_levels)
      posterior <- .encoder_posterior(enc, test)
      if (!is.null(posterior)) {
        if (is.null(posterior_nodes)) {
          posterior_nodes <- enc$nodes
          oof_posterior <- lapply(construct_names, function(x) matrix(NA_real_, n, length(posterior_nodes)))
          names(oof_posterior) <- construct_names
        }
        moments <- .posterior_moments(posterior, enc$nodes)
        locked[fold == k, nm] <- moments$mean
        oof_variance[fold == k, nm] <- moments$variance
        oof_posterior[[nm]][fold == k, ] <- posterior
      } else {
        locked[fold == k, nm] <- .predict_encoder(enc, test)
      }
      fold_scores[fold == k, k] <- locked[fold == k, nm]
      metric_list[[paste(nm, k)]] <- cbind(construct = nm, fold = k, .item_metrics(enc, test))
    }
    full[[nm]] <- .fit_encoder(data[, spec$indicators, drop = FALSE], spec, iterations)
    full_score <- .predict_encoder(full[[nm]], data[, spec$indicators, drop = FALSE])
    stability[nm] <- abs(stats::cor(locked[, nm], full_score, use = "complete.obs"))
  }
  # Marginal EAP reliability on the raw latent scale: signal variance over signal
  # plus mean posterior (measurement) variance. This is the disattenuation factor
  # used by the errors-in-variables structural correction.
  centers <- apply(locked, 2L, mean, na.rm = TRUE)
  scales_raw <- apply(locked, 2L, .safe_scale)
  reliability <- vapply(construct_names, function(nm) {
    signal <- stats::var(locked[, nm], na.rm = TRUE)
    error <- mean(oof_variance[, nm], na.rm = TRUE)
    if (!is.finite(signal) || !is.finite(error) || signal <= 0) NA_real_ else signal / (signal + error)
  }, numeric(1))
  # Per-respondent posterior SD on the standardized score scale (heteroskedastic
  # respondent information; NA where no latent grid was used).
  score_posterior_sd <- sweep(sqrt(oof_variance), 2L, scales_raw, "/")
  locked <- sweep(locked, 2L, centers, "-"); locked <- sweep(locked, 2L, scales_raw, "/")
  if (is.null(dim(locked))) locked <- matrix(locked, ncol = 1)
  colnames(locked) <- construct_names
  redundancy <- stats::cor(locked, use = "pairwise.complete.obs")
  residual_dependence <- if (isTRUE(diagnostics)) .residual_dependence(model, data, iterations) else
    data.frame(construct=character(), item_a=character(), item_b=character(), residual_correlation=numeric())
  warnings <- if (isTRUE(diagnostics)) .diagnose(model, data, locked, redundancy) else
    data.frame(type=character(), target=character(), detail=character())
  bags <- NULL
  if (draws > 0L) {
    bags <- .plausible_values(oof_posterior, posterior_nodes, locked, centers, scales_raw, draws, seed)
  }
  structure(list(model = model, locked_scores = as.data.frame(locked), full_encoders = full, folds = fold,
    item_metrics = do.call(rbind, metric_list), stability = stability, redundancy = redundancy,
    reliability = reliability, score_posterior_sd = as.data.frame(score_posterior_sd),
    warnings = warnings, residual_dependence = residual_dependence,
    uncertainty_draws = bags, seed = seed,
    measurement_engine = lapply(full, function(x) list(estimator = x$estimator, converged = x$converged, iterations = x$iterations))), class = "cssem_fit")
}

# Build the latent-state bag from real posterior draws. Each draw samples one
# plausible value per respondent from the construct's out-of-fold posterior,
# standardized with the same centering and scaling used for the locked scores so
# the draw cloud carries the construct's true (unshrunk) spread. Constructs
# without a latent grid reuse their locked score unchanged.
.plausible_values <- function(oof_posterior, nodes, locked, centers, scales_raw, draws, seed) {
  n <- nrow(locked); construct_names <- colnames(locked)
  bags <- array(NA_real_, c(n, ncol(locked), draws), dimnames = list(NULL, construct_names, NULL))
  set.seed(seed + 99L)
  for (b in seq_len(draws)) {
    for (j in seq_along(construct_names)) {
      nm <- construct_names[j]; posterior <- if (is.null(nodes)) NULL else oof_posterior[[nm]]
      if (is.null(posterior) || anyNA(posterior[, 1L])) {
        bags[, j, b] <- locked[, nm]
      } else {
        raw <- .draw_posterior_values(posterior, nodes)
        bags[, j, b] <- (raw - centers[[nm]]) / scales_raw[[nm]]
      }
    }
  }
  bags
}

.loo_item_residuals <- function(data, spec, iterations) {
  # Each residual is based on a score that excludes its own item.  This avoids
  # the circular, spuriously correlated residuals produced by a score using all
  # items, and respects the ordinal response model.
  if (length(spec$indicators) < 3L) return(NULL)
  residuals <- vector("list", length(spec$indicators)); names(residuals) <- spec$indicators
  for (j in seq_along(spec$indicators)) {
    keep <- setdiff(seq_along(spec$indicators), j)
    reduced <- list(indicators = spec$indicators[keep], scales = spec$scales[keep], keys = spec$keys[keep])
    reduced_encoder <- .fit_encoder(data[, reduced$indicators, drop = FALSE], reduced, iterations)
    z <- .predict_encoder(reduced_encoder, data[, reduced$indicators, drop = FALSE])
    y <- .prepare_item(data[[spec$indicators[j]]], spec$scales[j], spec$keys[j])$y
    if (spec$scales[j] == "ordinal") {
      item_encoder <- .fit_ordinal(z, y)
      expected <- .ordinal_expected(item_encoder, z)
      residuals[[j]] <- (y - expected) / sqrt(stats::var(y - expected, na.rm = TRUE) + 1e-8)
    } else {
      item_encoder <- .fit_continuous(z, y)
      residuals[[j]] <- (y - item_encoder$intercept - item_encoder$slope * z) / item_encoder$sigma
    }
  }
  as.data.frame(residuals)
}

.residual_dependence <- function(model, data, iterations) {
  rows <- list(); n <- 0L
  for (nm in names(model$constructs)) {
    spec <- model$constructs[[nm]]
    values <- .loo_item_residuals(data, spec, iterations)
    if (is.null(values) || ncol(values) < 2L) next
    rc <- stats::cor(values, use = "pairwise.complete.obs")
    for (i in seq_len(ncol(rc) - 1L)) for (j in (i + 1L):ncol(rc)) {
      n <- n + 1L
      rows[[n]] <- data.frame(construct = nm, item_a = names(values)[i], item_b = names(values)[j],
        residual_correlation = rc[i, j], stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) data.frame(construct=character(), item_a=character(), item_b=character(), residual_correlation=numeric()) else do.call(rbind, rows)
}

.diagnose <- function(model, data, scores, redundancy) {
  out <- list(); n <- 0L
  for (nm in names(model$constructs)) {
    s <- model$constructs[[nm]]
    for (j in seq_along(s$indicators)) {
      x <- data[[s$indicators[j]]]
      if (s$scales[j] == "ordinal") { prop <- min(table(x, useNA = "no") / sum(!is.na(x))); if (is.finite(prop) && prop < .05) { n <- n + 1L; out[[n]] <- data.frame(type="sparse_category", target=s$indicators[j], detail="An observed category has under 5% support.") } }
      if (s$keys[j] < 0) { n <- n + 1L; out[[n]] <- data.frame(type="reverse_keyed", target=s$indicators[j], detail="Declared reverse key was applied.") }
    }
  }
  if (ncol(scores) > 1L) for (i in seq_len(ncol(scores)-1L)) for (j in (i+1L):ncol(scores)) if (redundancy[i,j]^2 > .64) { n <- n + 1L; out[[n]] <- data.frame(type="redundancy", target=paste(colnames(scores)[i], colnames(scores)[j], sep=" / "), detail="Squared construct correlation exceeds 0.64.") }
  if (!length(out)) data.frame(type=character(), target=character(), detail=character()) else do.call(rbind, out)
}

#' Score new compatible data
#'
#' Applies the full-data encoders retained in a [cssem_fit()] object. This is
#' intended for new observations; use `fit$locked_scores` for analyses of the
#' estimation sample.
#'
#' @param fit A `cssem_fit` object.
#' @param new_data A data frame whose columns exactly match all model indicators
#'   in their declared order.
#' @return A data frame of construct scores.
#' @examples
#' # cssem_score(fit, new_survey_rows)
#' @export
cssem_score <- function(fit, new_data) {
  if (!inherits(fit, "cssem_fit")) stop("fit must be a cssem_fit.", call. = FALSE)
  expected <- unlist(lapply(fit$model$constructs, `[[`, "indicators"), use.names = FALSE)
  if (!identical(names(new_data), expected))
    stop("Scoring data columns must exactly match all declared indicators in their declared order.", call. = FALSE)
  ans <- lapply(fit$full_encoders, function(e) .predict_encoder(e, new_data[, e$indicators, drop = FALSE]))
  as.data.frame(ans)
}

#' Create a construct evidence card
#'
#' @param fit A `cssem_fit` object.
#' @param construct Name of a declared construct.
#' @return A list containing measurement-engine metadata, held-out item metrics,
#'   stability, relevant warnings, and exploratory residual dependence.
#' @examples
#' # cssem_construct_card(fit, "Trust")
#' @export
cssem_construct_card <- function(fit, construct) {
  if (!inherits(fit, "cssem_fit") || !construct %in% names(fit$full_encoders)) stop("Unknown construct.", call. = FALSE)
  indicators <- fit$model$constructs[[construct]]$indicators
  relevant <- fit$warnings$target == construct
  for (item in indicators) relevant <- relevant | grepl(item, fit$warnings$target, fixed = TRUE)
  list(construct = construct, measurement_engine = fit$measurement_engine[[construct]], held_out_metrics = fit$item_metrics[fit$item_metrics$construct == construct, , drop = FALSE], stability = fit$stability[[construct]], warnings = fit$warnings[relevant, , drop = FALSE], residual_dependence = fit$residual_dependence[fit$residual_dependence$construct == construct, , drop = FALSE])
}

#' Return exploratory leave-one-item-out residual correlations
#'
#' These values are reported for inspection and are not automatically treated
#' as warnings until the simulation calibration study establishes a threshold.
#'
#' @param fit A `cssem_fit` object.
#' @param construct Optional construct name. If `NULL`, returns diagnostics for
#'   all constructs.
#' @return A data frame of pairwise leave-one-item-out residual correlations.
#' @examples
#' # cssem_residual_diagnostics(fit, "Trust")
#' @export
cssem_residual_diagnostics <- function(fit, construct = NULL) {
  if (!inherits(fit, "cssem_fit")) stop("fit must be a cssem_fit.", call. = FALSE)
  if (is.null(construct)) return(fit$residual_dependence)
  fit$residual_dependence[fit$residual_dependence$construct == construct, , drop = FALSE]
}

#' Create the construct evidence ledger
#'
#' @param fit A `cssem_fit` object.
#' @return A data frame of construct stability, mean held-out loss, maximum
#'   redundancy, and warning count.
#' @examples
#' # cssem_evidence_ledger(fit)
#' @export
cssem_evidence_ledger <- function(fit) {
  average_loss <- vapply(names(fit$stability), function(nm) mean(fit$item_metrics$value[fit$item_metrics$construct == nm], na.rm = TRUE), numeric(1))
  warning_count <- vapply(names(fit$stability), function(nm) nrow(cssem_construct_card(fit, nm)$warnings), integer(1))
  data.frame(construct = names(fit$stability), stability = unname(fit$stability), held_out_loss = average_loss,
    redundancy_max = vapply(seq_along(fit$stability), function(i) if (length(fit$stability) == 1L) 0 else max(abs(fit$redundancy[i, -i]), na.rm = TRUE), numeric(1)),
    warnings = warning_count,
    stringsAsFactors = FALSE)
}

#' Plot CS-SEM measurement diagnostics
#'
#' @param x A `cssem_fit` object.
#' @param type Diagnostic to plot: locked construct `"scores"`, construct
#'   `"redundancy"`, or held-out `"item_loss"`.
#' @param ... Additional arguments passed to the underlying base graphics call.
#' @return `x`, invisibly.
#' @examples
#' # plot(fit, type = "redundancy")
#' @export
plot.cssem_fit <- function(x, type = c("scores", "redundancy", "item_loss"), ...) {
  type <- match.arg(type)
  if (type == "scores") {
    graphics::pairs(x$locked_scores, main = "CS-SEM locked construct states", ...)
  } else if (type == "redundancy") {
    graphics::image(seq_len(ncol(x$redundancy)), seq_len(nrow(x$redundancy)), x$redundancy, axes = FALSE, col = grDevices::hcl.colors(20, "Blue-Red 3"), ...)
    graphics::axis(1, at = seq_len(ncol(x$redundancy)), labels = colnames(x$redundancy)); graphics::axis(2, at = seq_len(nrow(x$redundancy)), labels = rownames(x$redundancy))
    graphics::title("Construct correlations")
  } else {
    graphics::stripchart(value ~ item, data = x$item_metrics, method = "jitter", vertical = TRUE, ylab = "Held-out loss", xlab = "Indicator", ...)
  }
  invisible(x)
}

#' Print a CS-SEM fit
#'
#' @param x A `cssem_fit` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_fit <- function(x, ...) { cat("CS-SEM fit: ", ncol(x$locked_scores), " locked construct state(s), ", length(unique(x$folds)), " folds\n", sep=""); invisible(x) }
