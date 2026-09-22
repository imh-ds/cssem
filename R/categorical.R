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
#' @return A `cssem_marginal_contrast` object with expected and probability
#'   contrasts.
#' @export
marginal_contrast <- function(association, outcome, predictor, values,
                              newdata = NULL) {
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
  low <- data; high <- data
  low[[predictor]] <- values[[1L]]; high[[predictor]] <- values[[2L]]
  family <- .structural_family(model$family)
  expected_low <- .predict_shape_model(model, low)
  expected_high <- .predict_shape_model(model, high)
  expected <- mean(expected_high - expected_low, na.rm = TRUE)
  probabilities <- NULL
  if (family$family != "gaussian") {
    p_low <- .predict_shape_model(model, low, "probability")
    p_high <- .predict_shape_model(model, high, "probability")
    probabilities <- colMeans(p_high - p_low)
    result <- data.frame(outcome = outcome, predictor = predictor, category = names(probabilities),
      estimate = rep(expected, length(probabilities)), probability_contrast = unname(probabilities),
      low = values[[1L]], high = values[[2L]], n = nrow(data),
      stringsAsFactors = FALSE)
  } else {
    result <- data.frame(outcome = outcome, predictor = predictor, category = NA_character_,
      estimate = expected, probability_contrast = NA_real_, low = values[[1L]], high = values[[2L]],
      n = nrow(data), stringsAsFactors = FALSE)
  }
  structure(list(contrast = result, family = family, outcome = outcome,
    predictor = predictor, values = values), class = c("cssem_marginal_contrast", "list"))
}

#' @export
as.data.frame.cssem_marginal_contrast <- function(x, row.names = NULL, optional = FALSE, ...) x$contrast

#' @export
print.cssem_marginal_contrast <- function(x, ...) {
  cat("CS-SEM marginal contrast: ", x$outcome, " by ", x$predictor, "\n", sep = "")
  print(x$contrast, row.names = FALSE)
  invisible(x)
}
