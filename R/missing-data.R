# Missing-data policy and effective-sample accounting.

.sample_row_names <- function(data) {
  if (!is.data.frame(data)) return(character())
  as.character(row.names(data))
}

.measurement_sample_ledger <- function(model, input_data, retained_ids,
                                       scores = NULL, policy = "partial") {
  if (!is.data.frame(input_data)) stop("input_data must be a data frame.", call. = FALSE)
  input_n <- nrow(input_data)
  retained <- seq_len(input_n) %in% as.integer(retained_ids)
  row_ids <- seq_len(input_n)
  row_names <- .sample_row_names(input_data)
  score_matrix <- matrix(NA_real_, input_n, length(model$constructs),
    dimnames = list(NULL, names(model$constructs)))
  if (!is.null(scores) && nrow(as.data.frame(scores)) == length(retained_ids))
    score_matrix[retained, ] <- as.matrix(scores)
  rows <- list(); summaries <- list()
  for (nm in names(model$constructs)) {
    spec <- model$constructs[[nm]]
    indicators <- spec$indicators
    observed_n <- rowSums(!is.na(input_data[, indicators, drop = FALSE]))
    total_n <- length(indicators)
    status <- ifelse(!retained, "excluded",
      ifelse(observed_n == 0L, "prior_only",
        ifelse(observed_n < total_n, "partial", "complete")))
    finite_score <- is.finite(score_matrix[, nm])
    reason <- ifelse(!retained, "missing_indicator",
      ifelse(status == "prior_only", "missing_indicator",
        ifelse(status == "partial", "partial_indicator_observation", "")))
    reason[retained & !finite_score] <- ifelse(nzchar(reason[retained & !finite_score]),
      paste0(reason[retained & !finite_score], ";nonfinite_score"), "nonfinite_score")
    rows[[nm]] <- data.frame(stage = "measurement", target = nm, row_id = row_ids,
      row_name = row_names, indicators_observed = as.integer(observed_n),
      indicators_total = as.integer(total_n), observed_fraction = observed_n / total_n,
      status = status, retained = retained, score_finite = finite_score,
      reason = reason, stringsAsFactors = FALSE)
    summaries[[nm]] <- data.frame(stage = "measurement", target = nm,
      n_total = input_n, n_retained = sum(retained),
      n_effective = sum(retained & status %in% c("complete", "partial")),
      n_score_finite = sum(retained & finite_score),
      n_complete = sum(retained & status == "complete"),
      n_partial = sum(retained & status == "partial"),
      n_prior_only = sum(retained & status == "prior_only"),
      n_excluded = sum(!retained), stringsAsFactors = FALSE)
  }
  list(summary = do.call(rbind, summaries), rows = do.call(rbind, rows),
    policy = policy, input_n = input_n, retained_n = sum(retained),
    retained_ids = as.integer(retained_ids))
}

.association_sample_ledger <- function(association) {
  scores <- as.data.frame(association$scores)
  fit <- association$fit
  input_n <- if (!is.null(fit$sample_ledger$input_n)) fit$sample_ledger$input_n else nrow(scores)
  input_names <- if (!is.null(fit$input_data)) .sample_row_names(fit$input_data) else as.character(seq_len(input_n))
  fit_ids <- if (!is.null(fit$row_ids)) as.integer(fit$row_ids) else seq_len(nrow(fit$locked_scores))
  association_ids <- if (!is.null(association$row_ids)) as.integer(association$row_ids) else fit_ids
  available <- seq_len(input_n) %in% association_ids
  rows <- list(); summaries <- list()
  for (outcome in names(association$structure$effects)) {
    predictors <- names(association$structure$effects[[outcome]])
    required <- unique(c(outcome, unlist(lapply(predictors, .predictor_constructs), use.names = FALSE)))
    observed <- rep(FALSE, input_n)
    complete <- rep(FALSE, input_n)
    observed_n <- rep(0L, input_n)
    if (all(required %in% names(scores))) {
      values <- scores[, required, drop = FALSE]
      observed_available <- rowSums(is.finite(as.matrix(values)))
      complete_available <- stats::complete.cases(values) &
        apply(as.matrix(values), 1L, function(x) all(is.finite(x)))
      complete[available] <- complete_available
      observed[available] <- TRUE
      observed_n[available] <- observed_available
    }
    status <- ifelse(!available, "excluded", ifelse(complete, "included", "excluded"))
    reason <- ifelse(!available, "measurement_excluded",
      ifelse(complete, "", "missing_score"))
    rows[[outcome]] <- data.frame(stage = "structural", target = outcome,
      row_id = seq_len(input_n), row_name = input_names,
      indicators_observed = as.integer(observed_n), indicators_total = length(required),
      observed_fraction = observed_n / length(required), status = status,
      retained = available, score_finite = complete, reason = reason,
      stringsAsFactors = FALSE)
    summaries[[outcome]] <- data.frame(stage = "structural", target = outcome,
      n_total = input_n, n_retained = sum(available), n_effective = sum(complete),
      n_score_finite = sum(complete), n_complete = sum(complete), n_partial = 0L,
      n_prior_only = 0L, n_excluded = sum(!complete), stringsAsFactors = FALSE)
  }
  list(summary = if (length(summaries)) do.call(rbind, summaries) else data.frame(),
    rows = if (length(rows)) do.call(rbind, rows) else data.frame(),
    policy = association$missing_policy, input_n = input_n,
    retained_n = sum(available), retained_ids = association_ids)
}

.effect_sample_ledger <- function(effect, stage = "causal", target = NULL,
                                  required = character()) {
  association <- effect$association
  if (!inherits(association, "cssem_association"))
    stop("The effect object does not retain its source association; refit the effect to obtain sample accounting.", call. = FALSE)
  fit <- association$fit
  scores <- as.data.frame(association$scores)
  input_n <- if (!is.null(fit$sample_ledger$input_n)) fit$sample_ledger$input_n else nrow(scores)
  input_names <- if (!is.null(fit$input_data)) .sample_row_names(fit$input_data) else as.character(seq_len(input_n))
  association_ids <- if (!is.null(association$row_ids)) as.integer(association$row_ids) else seq_len(nrow(scores))
  available <- seq_len(input_n) %in% association_ids
  required <- unique(required[required %in% names(scores)])
  observed_n <- rep(0L, input_n)
  complete <- rep(FALSE, input_n)
  if (length(required)) {
    values <- scores[, required, drop = FALSE]
    observed_available <- rowSums(is.finite(as.matrix(values)))
    complete_available <- stats::complete.cases(values) &
      apply(as.matrix(values), 1L, function(x) all(is.finite(x)))
    observed_n[available] <- observed_available
    complete[available] <- complete_available
  }
  target <- if (is.null(target)) "effect" else as.character(target)
  status <- ifelse(!available, "excluded", ifelse(complete, "included", "excluded"))
  reason <- ifelse(!available, "measurement_excluded",
    ifelse(complete, "", "missing_score"))
  rows <- data.frame(stage = stage, target = target, row_id = seq_len(input_n),
    row_name = input_names, indicators_observed = as.integer(observed_n),
    indicators_total = length(required), observed_fraction = if (length(required))
      observed_n / length(required) else 1,
    status = status, retained = available, score_finite = complete,
    reason = reason, stringsAsFactors = FALSE)
  summary <- data.frame(stage = stage, target = target, n_total = input_n,
    n_retained = sum(available), n_effective = sum(complete),
    n_score_finite = sum(complete), n_complete = sum(complete), n_partial = 0L,
    n_prior_only = 0L, n_excluded = sum(!complete), stringsAsFactors = FALSE)
  list(summary = summary, rows = rows, retained_n = sum(available),
    retained_ids = association_ids)
}

#' Report missing-data coverage and effective sample sizes
#'
#' Returns a stable sample ledger for measurement and structural stages. The
#' row table distinguishes complete, partial, prior-only, and excluded records;
#' the summary table reports per-target effective sample sizes. Partial item
#' responses remain valid in the default `partial` measurement policy, but a
#' prior-only state is identified explicitly rather than counted as observed
#' measurement evidence.
#'
#' @param object A `fit_states`, `cssem_association`, or derived effect object.
#' @param ... Unused.
#' @return An object of class `cssem_sample_accounting` with `summary`, `rows`,
#'   `policy`, `input_n`, and `retained_n` fields.
#' @export
sample_accounting <- function(object, ...) UseMethod("sample_accounting")

#' @export
sample_accounting.fit_states <- function(object, ...) {
  if (!inherits(object, "fit_states")) stop("object must be a fit_states object.", call. = FALSE)
  input_data <- if (!is.null(object$input_data)) object$input_data else object$data
  retained_ids <- if (!is.null(object$row_ids)) object$row_ids else seq_len(nrow(object$data))
  policy <- if (is.null(object$missing_policy)) "partial" else object$missing_policy
  ledger <- if (!is.null(object$sample_ledger)) object$sample_ledger else
    .measurement_sample_ledger(object$model, input_data, retained_ids, object$locked_scores, policy)
  class(ledger) <- c("cssem_sample_accounting", "list")
  ledger
}

#' @export
sample_accounting.cssem_association <- function(object, ...) {
  if (!inherits(object, "cssem_association")) stop("object must be a cssem_association object.", call. = FALSE)
  measurement <- sample_accounting(object$fit)
  structural <- .association_sample_ledger(object)
  out <- list(summary = rbind(measurement$summary, structural$summary),
    rows = rbind(measurement$rows, structural$rows),
    policy = object$missing_policy, input_n = measurement$input_n,
    retained_n = structural$retained_n,
    retained_ids = structural$retained_ids)
  class(out) <- c("cssem_sample_accounting", "list")
  out
}

#' @export
sample_accounting.causal_effect <- function(object, ...) {
  if (!inherits(object, "causal_effect")) stop("object must be a causal_effect object.", call. = FALSE)
  measurement <- sample_accounting(object$association)
  effect <- .effect_sample_ledger(object, stage = "causal",
    target = paste(object$treatment, object$outcome, sep = " -> "),
    required = c(object$treatment, object$outcome, object$adjust))
  out <- list(summary = rbind(measurement$summary, effect$summary),
    rows = rbind(measurement$rows, effect$rows),
    policy = object$association$missing_policy, input_n = measurement$input_n,
    retained_n = effect$retained_n, retained_ids = effect$retained_ids)
  class(out) <- c("cssem_sample_accounting", "list")
  out
}

#' @export
sample_accounting.indirect_effect <- function(object, ...) {
  if (!inherits(object, "indirect_effect")) stop("object must be an indirect_effect object.", call. = FALSE)
  measurement <- sample_accounting(object$association)
  effect <- .effect_sample_ledger(object, stage = "causal",
    target = paste(object$x, object$y, sep = " -> "),
    required = c(object$x, object$y, object$mediators))
  out <- list(summary = rbind(measurement$summary, effect$summary),
    rows = rbind(measurement$rows, effect$rows), policy = object$association$missing_policy,
    input_n = measurement$input_n, retained_n = effect$retained_n,
    retained_ids = effect$retained_ids)
  class(out) <- c("cssem_sample_accounting", "list")
  out
}

#' @export
sample_accounting.conditional_slopes <- function(object, ...) {
  if (!inherits(object, "conditional_slopes")) stop("object must be a conditional_slopes object.", call. = FALSE)
  measurement <- sample_accounting(object$association)
  effect <- .effect_sample_ledger(object, stage = "causal",
    target = paste(object$predictor, object$outcome, sep = " -> "),
    required = c(object$predictor, object$outcome, object$moderator))
  out <- list(summary = rbind(measurement$summary, effect$summary),
    rows = rbind(measurement$rows, effect$rows), policy = object$association$missing_policy,
    input_n = measurement$input_n, retained_n = effect$retained_n,
    retained_ids = effect$retained_ids)
  class(out) <- c("cssem_sample_accounting", "list")
  out
}

#' @export
sample_accounting.conditional_indirect_effect <- function(object, ...) {
  if (!inherits(object, "conditional_indirect_effect"))
    stop("object must be a conditional_indirect_effect object.", call. = FALSE)
  measurement <- sample_accounting(object$association)
  effect <- .effect_sample_ledger(object, stage = "causal",
    target = paste(object$x, object$y, sep = " -> "),
    required = c(object$x, object$y, object$moderator))
  out <- list(summary = rbind(measurement$summary, effect$summary),
    rows = rbind(measurement$rows, effect$rows), policy = object$association$missing_policy,
    input_n = measurement$input_n, retained_n = effect$retained_n,
    retained_ids = effect$retained_ids)
  class(out) <- c("cssem_sample_accounting", "list")
  out
}

#' @export
print.cssem_sample_accounting <- function(x, ...) {
  cat("CS-SEM sample accounting: ", x$retained_n, "/", x$input_n,
    " rows retained (policy: ", x$policy, ")\n", sep = "")
  if (nrow(x$summary)) print(x$summary, row.names = FALSE)
  invisible(x)
}
