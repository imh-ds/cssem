#' Compute a probability-scale marginal contrast for a structural outcome
#'
#' The contrast changes one declared locked-score predictor from `values[1]`
#' to `values[2]` for every supplied row and averages the fitted response.
#' For categorical outcomes the returned table includes one contrast per
#' declared category; `estimate` is the expected-category contrast.
#'
#' @param association A `cssem_association` object.
#' @param outcome A declared structural outcome.
#' @param predictor A main-effect predictor of `outcome`.
#' @param values A finite numeric vector of length two: low and high settings.
#' @param newdata Optional locked-score data frame. Defaults to the fitted
#'   complete structural score rows.
#' @param reps Number of row-bootstrap replicates for percentile intervals.
#'   Zero (the default) returns point contrasts without intervals.
#' @param level Confidence level for the percentile intervals.
#' @param seed Random seed for the bootstrap; the caller's random stream is
#'   restored after fitting.
#' @return A `cssem_marginal_contrast` object with expected and probability
#'   contrasts.
#' @export
marginal_contrast <- function(association, outcome, predictor, values,
                              newdata = NULL, reps = 0L, level = .95,
                              seed = 1L) {
  .preserve_seed()
  reps <- .bootstrap_scalar_integer(reps, "reps", minimum = 0L)
  level <- .bootstrap_validate_level(level)
  seed <- .bootstrap_scalar_integer(seed, "seed", minimum = 0L)
  if (!inherits(association, "cssem_association"))
    stop("association must be a cssem_association.", call. = FALSE)
  if (length(outcome) != 1L || !outcome %in% names(association$full_models))
    stop("outcome must name one declared structural outcome.", call. = FALSE)
  model <- association$full_models[[outcome]]
  if (length(predictor) != 1L || !predictor %in% names(model$shapes) || .is_interaction(predictor))
    stop("predictor must name a declared main-effect structural predictor.", call. = FALSE)
  if (!is.numeric(values) || length(values) != 2L || any(!is.finite(values)))
    stop("values must be a finite numeric vector of length two (low, high).", call. = FALSE)
  data <- if (is.null(newdata)) association$scores else newdata
  if (!is.data.frame(data) || !nrow(data)) stop("newdata must be a non-empty locked-score data frame.", call. = FALSE)
  required <- .prediction_constructs(model)
  if (!all(required %in% names(data)))
    stop(sprintf("newdata must contain locked predictors: %s.", paste(setdiff(required, names(data)), collapse = ", ")), call. = FALSE)
  if (!all(vapply(data[, required, drop = FALSE], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(data[, required, drop = FALSE]))))
    stop("newdata must contain finite numeric locked scores for every structural predictor.", call. = FALSE)
  family <- .structural_family(model$family)
  evaluate <- function(fitted_model, target_data) {
    low_data <- target_data; high_data <- target_data
    low_data[[predictor]] <- values[[1L]]; high_data[[predictor]] <- values[[2L]]
    expected <- mean(.predict_shape_model(fitted_model, high_data) -
      .predict_shape_model(fitted_model, low_data))
    probabilities <- if (family$family == "gaussian") NULL else colMeans(
      .predict_shape_model(fitted_model, high_data, "probability") -
        .predict_shape_model(fitted_model, low_data, "probability"))
    c(expected = expected, probabilities)
  }
  point <- evaluate(model, data)
  expected <- unname(point[["expected"]])
  probabilities <- if (family$family == "gaussian") NULL else point[-1L]
  draws <- matrix(NA_real_, nrow = reps,
    ncol = if (is.null(probabilities)) 1L else length(probabilities) + 1L,
    dimnames = list(NULL, c("expected", names(probabilities))))
  if (reps > 0L) {
    set.seed(seed)
    training <- association$scores
    for (b in seq_len(reps)) {
      indices <- sample.int(nrow(training), nrow(training), replace = TRUE)
      bootstrap <- tryCatch({
        sample_model <- .fit_shape_model(training[indices, , drop = FALSE], outcome,
          model$shapes, family)
        if (any(!is.finite(sample_model$coefficient))) NULL else {
          target_data <- if (is.null(newdata)) training[indices, , drop = FALSE] else data
          statistic <- evaluate(sample_model, target_data)
          if (any(!is.finite(statistic))) NULL else statistic
        }
      }, error = function(e) NULL)
      if (!is.null(bootstrap)) draws[b, ] <- bootstrap
    }
  }
  valid_draw <- if (reps) apply(draws, 1L, function(x) all(is.finite(x))) else logical()
  successful <- sum(valid_draw)
  minimum_success <- max(5L, ceiling(reps / 2))
  interval_status <- if (!reps) "not_requested" else if (successful < minimum_success)
    "insufficient_successful_replicates" else "available"
  if (interval_status == "available") {
    limits <- t(apply(draws[valid_draw, , drop = FALSE], 2L, stats::quantile,
      probs = c((1 - level) / 2, (1 + level) / 2), names = FALSE))
    rownames(limits) <- colnames(draws)
    colnames(limits) <- c("lower", "upper")
  } else {
    limits <- matrix(NA_real_, nrow = ncol(draws), ncol = 2L,
      dimnames = list(colnames(draws), c("lower", "upper")))
  }
  interval_method <- if (interval_status == "available") "percentile pairs bootstrap" else
    if (interval_status == "not_requested") "not requested" else "unavailable (too few successful bootstrap replicates)"
  if (family$family != "gaussian") {
    result <- data.frame(outcome = outcome, predictor = predictor, category = names(probabilities),
      estimate = rep(expected, length(probabilities)), probability_contrast = unname(probabilities),
      estimate_ci_low = rep(limits["expected", "lower"], length(probabilities)),
      estimate_ci_high = rep(limits["expected", "upper"], length(probabilities)),
      probability_contrast_ci_low = unname(limits[names(probabilities), "lower"]),
      probability_contrast_ci_high = unname(limits[names(probabilities), "upper"]),
      interval_method = interval_method, interval_status = interval_status,
      interval_reps = reps, successful_replicates = successful, level = level,
      low = values[[1L]], high = values[[2L]], n = nrow(data),
      stringsAsFactors = FALSE)
  } else {
    result <- data.frame(outcome = outcome, predictor = predictor, category = NA_character_,
      estimate = expected, probability_contrast = NA_real_, low = values[[1L]], high = values[[2L]],
      estimate_ci_low = limits["expected", "lower"], estimate_ci_high = limits["expected", "upper"],
      probability_contrast_ci_low = NA_real_, probability_contrast_ci_high = NA_real_,
      interval_method = interval_method, interval_status = interval_status,
      interval_reps = reps, successful_replicates = successful, level = level,
      n = nrow(data), stringsAsFactors = FALSE)
  }
  structure(list(contrast = result, family = family, outcome = outcome,
    predictor = predictor, values = values, draws = draws,
    successful_replicates = successful, failure_count = reps - successful,
    level = level, seed = seed, interval_status = interval_status),
    class = c("cssem_marginal_contrast", "list"))
}

#' @export
as.data.frame.cssem_marginal_contrast <- function(x, row.names = NULL, optional = FALSE, ...) x$contrast

#' @export
print.cssem_marginal_contrast <- function(x, ...) {
  cat("CS-SEM marginal contrast: ", x$outcome, " by ", x$predictor, "\n", sep = "")
  print(x$contrast, row.names = FALSE)
  invisible(x)
}
