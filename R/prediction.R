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
  if (identical(mode, "recursive"))
    stop('mode = "recursive" is not implemented yet.', call. = FALSE)
  fit <- object$fit
  if (!inherits(fit, "fit_states"))
    stop("association does not retain a fit_states object.", call. = FALSE)
  support <- .prediction_support(object, unique(unlist(lapply(outcomes,
    function(outcome) .prediction_constructs(object$full_models[[outcome]])),
    use.names = FALSE)))
  prediction_rows <- list(); availability_rows <- list()
  for (outcome in outcomes) {
    model <- object$full_models[[outcome]]
    constructs <- .prediction_constructs(model)
    scored <- .prediction_score_constructs(fit, newdata, constructs)
    missing <- unique(unlist(scored$missing_columns, use.names = FALSE))
    if (length(missing) && identical(missing_policy, "error"))
      stop(sprintf("Prediction requires indicator column(s) for the direct parent constructs: %s.",
        paste(missing, collapse = ", ")), call. = FALSE)
    states <- scored$states[, constructs, drop = FALSE]
    row_status <- .prediction_status(scored$status, constructs)
    valid <- row_status %in% c("ok", "partial") &
      apply(as.matrix(states), 1L, function(x) all(is.finite(x)))
    values <- rep(NA_real_, nrow(newdata))
    if (any(valid)) values[valid] <- .predict_shape_model(model, states[valid, , drop = FALSE])
    extrapolated <- .prediction_support_flags(states, constructs, support)
    prediction_rows[[outcome]] <- data.frame(row_id = seq_len(nrow(newdata)),
      outcome = outcome, prediction = values, mode = mode, status = row_status,
      extrapolated = extrapolated, stringsAsFactors = FALSE)
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
