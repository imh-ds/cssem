# Structural prediction for new observations.

.prediction_outcomes <- function(association, outcomes = NULL) {
  if (!inherits(association, "cssem_association"))
    stop("association must be a cssem_association.", call. = FALSE)
  available <- names(association$full_models)
  if (is.null(outcomes)) return(available)
  if (!is.character(outcomes) || !length(outcomes) || anyNA(outcomes) || any(!nzchar(outcomes)))
    stop("outcomes must be a non-empty character vector of structural outcomes.", call. = FALSE)
  unknown <- setdiff(outcomes, available)
  if (length(unknown))
    stop(sprintf("Unknown structural outcome(s): %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  unique(outcomes)
}

.prediction_constructs <- function(shape_model) {
  unique(unlist(lapply(names(shape_model$shapes), .predictor_constructs), use.names = FALSE))
}

.prediction_indicator_columns <- function(fit, constructs) {
  unique(unlist(lapply(constructs, function(construct) fit$model$constructs[[construct]]$indicators),
    use.names = FALSE))
}

.prediction_support <- function(association, constructs) {
  scores <- association$scores
  rows <- lapply(constructs, function(construct) {
    values <- scores[[construct]]
    values <- values[is.finite(values)]
    data.frame(construct = construct,
      minimum = if (length(values)) min(values) else NA_real_,
      maximum = if (length(values)) max(values) else NA_real_,
      stringsAsFactors = FALSE)
  })
  if (!length(rows)) data.frame(construct = character(), minimum = numeric(), maximum = numeric(),
    stringsAsFactors = FALSE) else do.call(rbind, rows)
}

.prediction_standardize <- function(fit, construct, values) {
  if (is.null(fit$score_center) || is.null(fit$score_scale)) {
    warning("This fit predates stored score standardization; refit with fit_states() before prediction.",
      call. = FALSE)
    return(values)
  }
  center <- unname(fit$score_center[[construct]])
  scale <- unname(fit$score_scale[[construct]])
  if (!is.finite(center) || !is.finite(scale) || scale <= 0)
    stop(sprintf("Stored score standardization for '%s' is unavailable.", construct), call. = FALSE)
  (values - center) / scale
}

.prediction_score_constructs <- function(fit, newdata, constructs) {
  n <- nrow(newdata)
  states <- data.frame(row_id = seq_len(n), stringsAsFactors = FALSE)
  status <- vector("list", length(constructs)); names(status) <- constructs
  source <- vector("list", length(constructs)); names(source) <- constructs
  missing_columns <- vector("list", length(constructs)); names(missing_columns) <- constructs
  for (construct in constructs) {
    indicators <- fit$model$constructs[[construct]]$indicators
    missing <- setdiff(indicators, names(newdata))
    missing_columns[[construct]] <- missing
    if (length(missing)) {
      states[[construct]] <- rep(NA_real_, n)
      status[[construct]] <- rep("missing_input", n)
      source[[construct]] <- rep("unavailable", n)
      next
    }
    block <- newdata[, indicators, drop = FALSE]
    values <- .predict_encoder(fit$full_encoders[[construct]], block)
    values <- .prediction_standardize(fit, construct, values)
    observed <- rowSums(!is.na(block))
    one_status <- ifelse(observed == 0L, "prior_only",
      ifelse(observed < length(indicators), "partial", "complete"))
    one_status[!is.finite(values)] <- "unavailable"
    states[[construct]] <- as.numeric(values)
    status[[construct]] <- as.character(one_status)
    source[[construct]] <- rep("observed", n)
  }
  list(states = states[, c("row_id", constructs), drop = FALSE],
    status = status, source = source, missing_columns = missing_columns)
}

.prediction_support_flags <- function(states, constructs, support) {
  flags <- rep(FALSE, nrow(states))
  if (!length(constructs) || !nrow(support)) return(flags)
  for (construct in constructs) {
    row <- support[support$construct == construct, , drop = FALSE]
    values <- states[[construct]]
    if (!nrow(row) || !any(is.finite(values))) next
    flags <- flags | (is.finite(values) &
      ((is.finite(row$minimum[[1L]]) & values < row$minimum[[1L]]) |
       (is.finite(row$maximum[[1L]]) & values > row$maximum[[1L]])))
  }
  flags
}

.prediction_status <- function(status, constructs) {
  if (!length(constructs)) return(rep("ok", length(status[[1L]])))
  n <- length(status[[constructs[[1L]]]])
  out <- rep("ok", n)
  for (construct in constructs) {
    one <- status[[construct]]
    out[one == "partial" & out == "ok"] <- "partial"
    out[one == "prior_only" & out %in% c("ok", "partial")] <- "prior_only"
    out[one == "unavailable" & out != "missing_input"] <- "unavailable"
    out[one == "missing_input"] <- "missing_input"
  }
  out
}

.prediction_required_table <- function(outcome, constructs, fit, missing_columns) {
  indicators <- .prediction_indicator_columns(fit, constructs)
  missing <- unique(unlist(missing_columns, use.names = FALSE))
  data.frame(outcome = outcome,
    required_constructs = paste(constructs, collapse = ", "),
    required_columns = paste(indicators, collapse = ", "),
    missing_columns = paste(missing, collapse = ", "),
    stringsAsFactors = FALSE)
}

.prediction_required_constructs <- function(association, outcome) {
  models <- association$full_models
  required <- character()
  visit <- function(construct) {
    if (construct %in% required) return(invisible(NULL))
    required <<- c(required, construct)
    model <- models[[construct]]
    if (!is.null(model)) for (parent in .prediction_constructs(model)) visit(parent)
    invisible(NULL)
  }
  for (parent in .prediction_constructs(models[[outcome]])) visit(parent)
  required
}

.prediction_check_cycles <- function(association, outcome) {
  models <- association$full_models
  visiting <- character(); visited <- character()
  visit <- function(construct) {
    if (construct %in% visited) return(invisible(NULL))
    if (construct %in% visiting) {
      path <- paste(c(visiting[match(construct, visiting):length(visiting)], construct), collapse = " -> ")
      stop(sprintf("Cannot recursively predict '%s': structural cycle detected (%s).", outcome, path), call. = FALSE)
    }
    visiting <<- c(visiting, construct)
    model <- models[[construct]]
    if (!is.null(model)) for (parent in .prediction_constructs(model)) visit(parent)
    visiting <<- setdiff(visiting, construct)
    visited <<- c(visited, construct)
    invisible(NULL)
  }
  visit(outcome)
  invisible(NULL)
}

.prediction_recursive_states <- function(association, fit, newdata, outcome, missing_policy) {
  .prediction_check_cycles(association, outcome)
  constructs <- .prediction_required_constructs(association, outcome)
  direct <- .prediction_score_constructs(fit, newdata, constructs)
  models <- association$full_models
  exogenous_missing <- vapply(constructs, function(construct) {
    is.null(models[[construct]]) && length(direct$missing_columns[[construct]]) > 0L
  }, logical(1))
  if (any(exogenous_missing) && identical(missing_policy, "error")) {
    missing <- unique(unlist(direct$missing_columns[names(exogenous_missing)[exogenous_missing]], use.names = FALSE))
    stop(sprintf("Prediction has unavailable exogenous construct(s): %s; required indicator column(s): %s.",
      paste(names(exogenous_missing)[exogenous_missing], collapse = ", "), paste(missing, collapse = ", ")), call. = FALSE)
  }
  n <- nrow(newdata); memo <- list(); resolving <- character()
  resolve <- function(construct) {
    if (!is.null(memo[[construct]])) return(memo[[construct]])
    if (construct %in% resolving) {
      path <- paste(c(resolving[match(construct, resolving):length(resolving)], construct), collapse = " -> ")
      stop(sprintf("Cannot recursively predict '%s': structural cycle detected (%s).", outcome, path), call. = FALSE)
    }
    resolving <<- c(resolving, construct)
    state <- direct$states[[construct]]
    direct_status <- direct$status[[construct]]
    observed <- is.finite(state) & direct_status %in% c("complete", "partial")
    status <- ifelse(observed, direct_status, ifelse(direct_status == "missing_input", "missing_input", "unavailable"))
    source <- ifelse(observed, "observed", "unavailable")
    model <- models[[construct]]
    if (!is.null(model) && any(!observed)) {
      parents <- .prediction_constructs(model)
      parent_results <- lapply(parents, resolve)
      parent_states <- as.data.frame(lapply(parent_results, `[[`, "state"),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(parent_states) <- parents
      parent_status <- as.data.frame(lapply(parent_results, `[[`, "status"),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(parent_status) <- parents
      parent_source <- as.data.frame(lapply(parent_results, `[[`, "source"),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(parent_source) <- parents
      parent_valid <- apply(as.matrix(parent_states), 1L, function(row) all(is.finite(row)))
      predicted <- rep(NA_real_, n)
      if (any(parent_valid)) predicted[parent_valid] <- .predict_shape_model(model,
        parent_states[parent_valid, , drop = FALSE])
      use_recursive <- !observed & parent_valid & is.finite(predicted)
      state[use_recursive] <- predicted[use_recursive]
      inherited_partial <- apply(as.matrix(parent_status), 1L, function(row)
        any(row %in% c("partial", "prior_only")))
      status[use_recursive] <- ifelse(inherited_partial[use_recursive], "partial", "ok")
      source[use_recursive] <- "recursive"
    }
    resolving <<- setdiff(resolving, construct)
    result <- list(state = state, status = status, source = source,
      missing_columns = direct$missing_columns[[construct]])
    memo[[construct]] <<- result
    result
  }
  resolved <- lapply(constructs, resolve); names(resolved) <- constructs
  list(states = data.frame(row_id = seq_len(n),
      as.data.frame(lapply(resolved, `[[`, "state"), check.names = FALSE,
        stringsAsFactors = FALSE), check.names = FALSE),
    status = stats::setNames(lapply(resolved, `[[`, "status"), constructs),
    source = stats::setNames(lapply(resolved, `[[`, "source"), constructs),
    missing_columns = direct$missing_columns, constructs = constructs)
}

.prediction_source <- function(source, status, constructs) {
  n <- length(status[[constructs[[1L]]]])
  out <- rep("observed", n)
  for (construct in constructs) {
    one_source <- source[[construct]]; one_status <- status[[construct]]
    out[one_source == "recursive"] <- "recursive"
    out[one_source == "unavailable" | one_status %in% c("missing_input", "unavailable")] <- "unavailable"
  }
  out
}

.prediction_validate_data <- function(newdata) {
  if (!is.data.frame(newdata)) stop("newdata must be a data frame.", call. = FALSE)
  if (!nrow(newdata)) stop("newdata must contain at least one row.", call. = FALSE)
  if (is.null(names(newdata)) || any(!nzchar(names(newdata))) || anyDuplicated(names(newdata)))
    stop("newdata must have unique, non-empty column names.", call. = FALSE)
}

#' Predict selected structural outcomes for new observations
#'
#' The selected associational structural models are evaluated on construct
#' states scored with the fitted measurement encoders. Requested outcome
#' indicators are not required; only direct parent indicators are scored.
#'
#' @param object A cssem_association object from associate().
#' @param newdata A data frame containing indicators for the required parent
#'   constructs. Extra columns are ignored.
#' @param outcomes Optional character vector of structural outcomes. Defaults
#'   to every declared outcome.
#' @param mode Prediction mode: "observed" uses direct observed parents;
#'   "recursive" is reserved for recursive upstream prediction.
#' @param missing_policy Whether missing required columns stop with an error or
#'   return unavailable rows with NA predictions.
#' @param ... Unused.
#' @return A cssem_prediction object with prediction and availability tables.
#' @export
predict.cssem_association <- function(object, newdata, outcomes = NULL,
                                      mode = c("observed", "recursive"),
                                      missing_policy = c("error", "na"), ...) {
  mode <- match.arg(mode)
  missing_policy <- match.arg(missing_policy)
  .prediction_validate_data(newdata)
  outcomes <- .prediction_outcomes(object, outcomes)
  fit <- object$fit
  if (!inherits(fit, "fit_states"))
    stop("association does not retain a fit_states object.", call. = FALSE)
  support_constructs <- unique(unlist(lapply(outcomes, function(outcome) {
    if (identical(mode, "recursive")) .prediction_required_constructs(object, outcome) else
      .prediction_constructs(object$full_models[[outcome]])
  }), use.names = FALSE))
  support <- .prediction_support(object, support_constructs)
  prediction_rows <- list(); availability_rows <- list()
  for (outcome in outcomes) {
    model <- object$full_models[[outcome]]
    constructs <- .prediction_constructs(model)
    scored <- if (identical(mode, "recursive"))
      .prediction_recursive_states(object, fit, newdata, outcome, missing_policy) else
      .prediction_score_constructs(fit, newdata, constructs)
    missing <- unique(unlist(scored$missing_columns, use.names = FALSE))
    if (length(missing) && identical(missing_policy, "error") && identical(mode, "observed"))
      stop(sprintf("Prediction requires indicator column(s) for the direct parent constructs: %s.",
        paste(missing, collapse = ", ")), call. = FALSE)
    states <- scored$states[, constructs, drop = FALSE]
    row_status <- .prediction_status(scored$status, constructs)
    valid <- row_status %in% c("ok", "partial") &
      apply(as.matrix(states), 1L, function(x) all(is.finite(x)))
    values <- rep(NA_real_, nrow(newdata))
    if (any(valid)) values[valid] <- .predict_shape_model(model, states[valid, , drop = FALSE])
    resolved_constructs <- if (identical(mode, "recursive")) scored$constructs else constructs
    extrapolated <- .prediction_support_flags(scored$states[, resolved_constructs, drop = FALSE],
      resolved_constructs, support)
    source <- if (identical(mode, "recursive")) .prediction_source(scored$source,
      scored$status, constructs) else rep("observed", nrow(newdata))
    source[row_status %in% c("unavailable", "missing_input")] <- "unavailable"
    prediction_rows[[outcome]] <- data.frame(row_id = seq_len(nrow(newdata)),
      outcome = outcome, prediction = values, mode = mode, status = row_status,
      source = source, extrapolated = extrapolated, stringsAsFactors = FALSE)
    availability_rows[[outcome]] <- .prediction_required_table(outcome, constructs, fit,
      scored$missing_columns)
  }
  predictions <- do.call(rbind, prediction_rows)
  row.names(predictions) <- NULL
  availability <- do.call(rbind, availability_rows)
  row.names(availability) <- NULL
  structure(list(predictions = predictions, availability = availability,
    settings = list(outcomes = outcomes, mode = mode, missing_policy = missing_policy,
      support = support)), class = c("cssem_prediction", "list"))
}

#' @export
as.data.frame.cssem_prediction <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$predictions
}

#' @export
print.cssem_prediction <- function(x, ...) {
  cat("CS-SEM structural prediction: ", length(unique(x$predictions$outcome)),
    " outcome(s), ", length(unique(x$predictions$row_id)), " row(s)
", sep = "")
  counts <- table(x$predictions$status)
  if (length(counts)) print(counts)
  invisible(x)
}

.prediction_assessment_metrics <- function(observed, predicted, baseline_value = NA_real_) {
  keep <- is.finite(observed) & is.finite(predicted)
  n <- sum(keep)
  result <- list(n = as.integer(n), rmse = NA_real_, mae = NA_real_, r_squared = NA_real_,
    calibration_intercept = NA_real_, calibration_slope = NA_real_,
    baseline_rmse = NA_real_, baseline_mae = NA_real_, baseline_r_squared = NA_real_)
  if (!n) return(result)
  y <- observed[keep]; p <- predicted[keep]
  residual <- y - p; sst <- sum((y - mean(y))^2)
  result$rmse <- sqrt(mean(residual^2)); result$mae <- mean(abs(residual))
  result$r_squared <- if (sst > 0) 1 - sum(residual^2) / sst else NA_real_
  if (n >= 2L && stats::var(p) > 0) {
    slope <- stats::cov(y, p) / stats::var(p)
    result$calibration_slope <- slope
    result$calibration_intercept <- mean(y) - slope * mean(p)
  }
  if (is.finite(baseline_value)) {
    baseline_residual <- y - baseline_value
    result$baseline_rmse <- sqrt(mean(baseline_residual^2))
    result$baseline_mae <- mean(abs(baseline_residual))
    result$baseline_r_squared <- if (sst > 0) 1 - sum(baseline_residual^2) / sst else NA_real_
  }
  result
}

#' Assess structural predictions against observed target indicators
#'
#' Scores the requested structural outcomes on new observations and compares
#' finite predictions with target construct states scored by the same fitted
#' measurement encoders. Target indicators are used only for assessment; they
#' are never used as predictors.
#'
#' @param association A cssem_association object from associate().
#' @param newdata A data frame containing predictor and, when available, target
#'   indicators.
#' @param outcomes Optional character vector of structural outcomes. Defaults
#'   to every declared outcome.
#' @param mode Prediction mode passed to predict.cssem_association().
#' @param missing_policy Missing-input policy passed to predict.cssem_association().
#' @param baseline Whether to report a training target-mean baseline or omit it.
#' @return An object of class cssem_prediction_assessment with metrics,
#'   row-level predictions, and settings.
#' @export
prediction_assessment <- function(association, newdata, outcomes = NULL,
                                  mode = c("observed", "recursive"),
                                  missing_policy = c("error", "na"),
                                  baseline = c("mean", "none")) {
  if (!inherits(association, "cssem_association"))
    stop("association must be a cssem_association.", call. = FALSE)
  mode <- match.arg(mode); missing_policy <- match.arg(missing_policy); baseline <- match.arg(baseline)
  .prediction_validate_data(newdata)
  outcomes <- .prediction_outcomes(association, outcomes)
  prediction <- predict(association, newdata, outcomes = outcomes, mode = mode,
    missing_policy = missing_policy)
  fit <- association$fit
  metric_rows <- list(); prediction_rows <- list()
  for (outcome in outcomes) {
    target_indicators <- fit$model$constructs[[outcome]]$indicators
    target_available <- all(target_indicators %in% names(newdata))
    target <- if (target_available) .prediction_score_constructs(fit, newdata, outcome) else NULL
    target_values <- if (target_available) target$states[[outcome]] else rep(NA_real_, nrow(newdata))
    target_status <- if (target_available) target$status[[outcome]] else rep("target_unavailable", nrow(newdata))
    rows <- prediction$predictions[prediction$predictions$outcome == outcome, , drop = FALSE]
    rows <- rows[order(rows$row_id), , drop = FALSE]
    rows$observed <- target_values[rows$row_id]
    rows$target_status <- target_status[rows$row_id]
    prediction_rows[[outcome]] <- rows
    keep <- target_status[rows$row_id] %in% c("complete", "partial") &
      is.finite(rows$observed) & is.finite(rows$prediction)
    baseline_value <- if (identical(baseline, "mean")) mean(association$scores[[outcome]], na.rm = TRUE) else NA_real_
    measures <- .prediction_assessment_metrics(rows$observed[keep], rows$prediction[keep], baseline_value)
    status <- if (!target_available) "target_unavailable" else if (!measures$n) "no_complete_rows" else "ok"
    metric_rows[[outcome]] <- data.frame(
      outcome = outcome, status = status, target_available = target_available,
      n = measures$n, rmse = measures$rmse, mae = measures$mae,
      r_squared = measures$r_squared, calibration_intercept = measures$calibration_intercept,
      calibration_slope = measures$calibration_slope, baseline = baseline,
      baseline_mean = baseline_value, baseline_rmse = measures$baseline_rmse,
      baseline_mae = measures$baseline_mae, baseline_r_squared = measures$baseline_r_squared,
      stringsAsFactors = FALSE)
  }
  metrics <- do.call(rbind, metric_rows); row.names(metrics) <- NULL
  predictions <- do.call(rbind, prediction_rows); row.names(predictions) <- NULL
  structure(list(metrics = metrics, predictions = predictions,
    settings = list(outcomes = outcomes, mode = mode, missing_policy = missing_policy,
      baseline = baseline)), class = c("cssem_prediction_assessment", "list"))
}

#' @export
as.data.frame.cssem_prediction_assessment <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$metrics
}

#' @export
print.cssem_prediction_assessment <- function(x, ...) {
  cat("CS-SEM prediction assessment: ", nrow(x$metrics), " outcome(s)\n", sep = "")
  print.data.frame(x$metrics[, intersect(c("outcome", "status", "n", "rmse", "mae",
    "r_squared", "calibration_intercept", "calibration_slope", "baseline_rmse"),
    names(x$metrics)), drop = FALSE], row.names = FALSE)
  invisible(x)
}
