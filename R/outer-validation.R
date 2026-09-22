# Whole-pipeline outer validation with explicit train/test provenance.

.outer_empty_predictions <- function() data.frame(
  outer_id = integer(), row_id = integer(), outcome = character(),
  observed = numeric(), predicted = numeric(), metric_scope = character(),
  stringsAsFactors = FALSE
)

.outer_empty_test_metrics <- function() data.frame(
  outer_id = integer(), outcome = character(), n = integer(), rmse = numeric(),
  mae = numeric(), r_squared = numeric(), metric_scope = character(),
  stringsAsFactors = FALSE
)

.outer_empty_selection_metrics <- function() data.frame(
  outer_id = integer(), metric_scope = character(), stringsAsFactors = FALSE
)

.outer_empty_failures <- function() data.frame(
  outer_id = integer(), stage = character(), message = character(),
  stringsAsFactors = FALSE
)

.outer_prediction_metrics <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  n <- sum(keep)
  if (!n) return(c(n = 0, rmse = NA_real_, mae = NA_real_, r_squared = NA_real_))
  residual <- observed[keep] - predicted[keep]
  sst <- sum((observed[keep] - mean(observed[keep]))^2)
  c(n = n, rmse = sqrt(mean(residual^2)), mae = mean(abs(residual)),
    r_squared = if (sst > 0) 1 - sum(residual^2) / sst else NA_real_)
}

.outer_training_split <- function(splits, assignment, train_ids, cluster_ids = NULL) {
  source_assignment <- as.integer(assignment[train_ids])
  levels <- sort(unique(source_assignment))
  if (length(levels) < 2L)
    stop("The outer training partition has fewer than two measurement folds.", call. = FALSE)
  local_assignment <- match(source_assignment, levels)
  provenance <- splits$provenance
  provenance$folds <- length(levels)
  structure(list(method = splits$method, folds = length(levels), seed = splits$seed,
    assignment = as.integer(local_assignment), row_ids = seq_along(train_ids),
    provenance = provenance, cluster_values = if (is.null(cluster_ids)) NULL else cluster_ids[train_ids],
    unit_n = if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids[train_ids])),
    unit_summary = if (is.null(cluster_ids)) data.frame() else .cluster_summary(cluster_ids[train_ids])),
    class = c("cssem_splits", "list"))
}

.outer_measurement_missing <- function(data, model) {
  indicators <- unlist(lapply(model$constructs, `[[`, "indicators"), use.names = FALSE)
  if (!all(indicators %in% names(data))) {
    missing <- setdiff(indicators, names(data))
    stop(paste0("G7 limitation: predictor-only prospective scoring is unsupported; held-out data are missing declared indicator column(s): ",
      paste(missing, collapse = ", "), "."), call. = FALSE)
  }
  !stats::complete.cases(data[, indicators, drop = FALSE])
}

.outer_prepare_test_data <- function(data, model, ids, policy) {
  test <- data[ids, , drop = FALSE]
  missing <- .outer_measurement_missing(test, model)
  if (policy == "error" && any(missing))
    stop(sprintf("measurement_missing_policy = \"error\" found %d held-out row(s) with missing declared indicators.",
      sum(missing)), call. = FALSE)
  if (policy == "listwise") {
    keep <- !missing
    test <- test[keep, , drop = FALSE]
    ids <- ids[keep]
    if (!nrow(test)) stop("measurement_missing_policy = \"listwise\" removed every held-out row.", call. = FALSE)
  }
  list(data = test, ids = as.integer(ids))
}

.outer_provenance_row <- function(outer_id, splits, train_ids, test_ids, seed, status,
                                  train_unit_n = NA_integer_, test_unit_n = NA_integer_,
                                  selected_shapes = NA_character_, status_detail = "") {
  row <- data.frame(outer_id = as.integer(outer_id), method = as.character(splits$method),
    seed = as.numeric(seed), train_n = length(train_ids), test_n = length(test_ids),
    train_unit_n = as.integer(train_unit_n), test_unit_n = as.integer(test_unit_n),
    status = status, status_detail = status_detail, selected_shapes = selected_shapes,
    stringsAsFactors = FALSE)
  row$train_ids <- list(as.integer(train_ids)); row$test_ids <- list(as.integer(test_ids))
  row
}

.outer_validate_ids <- function(value, label, n) {
  if (!is.numeric(value) || !length(value) || any(!is.finite(value)) ||
      any(abs(value) > .Machine$integer.max) || any(value != as.integer(value)))
    stop(sprintf("Outer %s IDs must be a non-empty integer vector.", label), call. = FALSE)
  value <- as.integer(value)
  if (any(value < 1L | value > n) || anyDuplicated(value))
    stop(sprintf("Outer %s IDs must be unique and within the supplied data.", label), call. = FALSE)
  value
}

.outer_validate_partitions <- function(splits, data, cluster_ids = NULL) {
  if (!nrow(splits$outer)) stop("splits must contain at least one outer partition.", call. = FALSE)
  n <- nrow(data); partitions <- vector("list", nrow(splits$outer))
  for (i in seq_len(nrow(splits$outer))) {
    row <- splits$outer[i, , drop = FALSE]
    train <- .outer_validate_ids(row$train_ids[[1L]], "training", n)
    test <- .outer_validate_ids(row$test_ids[[1L]], "test", n)
    if (any(train %in% test)) stop(sprintf("Outer partition %s has overlapping training and test IDs.", row$outer_id[[1L]]), call. = FALSE)
    if (splits$method %in% c("random", "group") && !identical(sort(c(train, test)), seq_len(n)))
      stop(sprintf("Outer partition %s must cover every row exactly once.", row$outer_id[[1L]]), call. = FALSE)
    partitions[[i]] <- list(train_ids = train, test_ids = test,
      train_unit_n = if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids[train])),
      test_unit_n = if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids[test])))
  }
  if (!is.null(cluster_ids)) {
    .validate_cluster_labels(cluster_ids, n, "cluster")
    for (i in seq_along(partitions)) {
      train <- partitions[[i]]$train_ids; test <- partitions[[i]]$test_ids
      if (any(cluster_ids[train] %in% cluster_ids[test]))
        stop(sprintf("Outer partition %s places one cluster in both training and test data.",
          splits$outer$outer_id[[i]]), call. = FALSE)
    }
  }
  provenance <- splits$provenance
  source_group <- if (nrow(provenance) && "group" %in% names(provenance)) as.character(provenance$group[[1L]]) else NA_character_
  groups <- if (splits$method == "group" && !is.na(source_group) && source_group %in% names(data)) data[[source_group]] else splits$group_values
  if (splits$method == "group" && (is.null(groups) || length(groups) != n))
    stop("Group outer partitions cannot be verified because their group labels are unavailable.", call. = FALSE)
  if (splits$method == "group") {
    for (i in seq_along(partitions)) if (any(groups[partitions[[i]]$test_ids] %in% groups[partitions[[i]]$train_ids]))
      stop(sprintf("Outer group partition %s places one group in both training and test data.", splits$outer$outer_id[[i]]), call. = FALSE)
  }
  source_time <- if (nrow(provenance) && "time" %in% names(provenance)) as.character(provenance$time[[1L]]) else NA_character_
  if (splits$method == "time") {
    if (!is.na(source_time) && source_time != "<vector>" && !(source_time %in% names(data)))
      stop(sprintf("Time source column '%s' is unavailable for outer partition validation.", source_time), call. = FALSE)
    times <- if (!is.na(source_time) && source_time != "<vector>") data[[source_time]] else splits$time_values
    if (is.null(times) || length(times) != n || any(!is.finite(times)))
      stop("Time outer partitions cannot be verified because their source values are unavailable or non-finite.", call. = FALSE)
    for (i in seq_along(partitions)) if (max(times[partitions[[i]]$train_ids]) >= min(times[partitions[[i]]$test_ids]))
      stop(sprintf("Outer time partition %s uses a test row at or before its training maximum.", splits$outer$outer_id[[i]]), call. = FALSE)
  }
  partitions
}

#' Validate measurement, structural selection, and held-out prediction together
#'
#' Fits each outer partition using training rows only. Measurement assignments
#' are resolved from [make_splits()], structural shape selection is performed by
#' [associate()], and predictions for untouched rows are reported separately
#' from internal selection metrics. Held-out predictions require the declared
#' outcome indicators; predictor-only prospective scoring remains outside this
#' interface (see the G7 gap in `docs/gaps.md`).
#'
#' @param model A `cssem_model` object.
#' @param structure A `cssem_structure` object.
#' @param data A data frame containing all declared measurement indicators.
#' @param splits A `cssem_splits` object from [make_splits()].
#' @param seed Base seed for partition fitting and structural selection.
#' @param iterations Measurement fitting iteration budget.
#' @param tolerance Measurement convergence tolerance.
#' @param quadrature Latent quadrature nodes passed to [fit_states()].
#' @param diagnostics Whether measurement diagnostics are calculated.
#' @param structural_args Named arguments forwarded to [associate()]. `fit`,
#'   `structure`, `folds`, and `missing_policy` are controlled by this helper.
#' @param measurement_missing_policy Missing-indicator policy for training and
#'   held-out measurement data: `"partial"`, `"listwise"`, or `"error"`.
#' @param structural_missing_policy Missing locked-score policy for structural
#'   selection: `"complete"` or `"error"`.
#' @return An object of class `cssem_outer_validation` containing predictions,
#'   internal selection metrics, held-out metrics, provenance, and failures.
#'   The provenance table includes `status_detail` when a partition has no
#'   observed held-out outcomes; split provenance retains the source group/time
#'   vectors needed to recheck grouped and time-ordered constraints.
#' @export
validate_outer <- function(model, structure, data, splits, seed = 1L,
                           iterations = 15L, tolerance = 1e-3,
                           quadrature = seq(-4, 4, length.out = 31L),
                           diagnostics = FALSE, structural_args = list(),
                           measurement_missing_policy = c("partial", "listwise", "error"),
                           structural_missing_policy = c("complete", "error"),
                           cluster = NULL, design = NULL) {
  .preserve_seed()
  if (!inherits(model, "cssem_model")) stop("model must be a cssem_model.", call. = FALSE)
  if (!inherits(structure, "cssem_structure")) stop("structure must be a cssem_structure.", call. = FALSE)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (!inherits(splits, "cssem_splits")) stop("splits must be a cssem_splits object from make_splits().", call. = FALSE)
  if (!is.list(structural_args) || (length(structural_args) &&
      (is.null(names(structural_args)) || any(!nzchar(names(structural_args))))))
    stop("structural_args must be a named list.", call. = FALSE)
  forbidden <- intersect(names(structural_args), c("fit", "structure", "folds", "missing_policy"))
  if (length(forbidden)) stop(sprintf("structural_args cannot override: %s.", paste(forbidden, collapse = ", ")), call. = FALSE)
  measurement_missing_policy <- match.arg(measurement_missing_policy)
  structural_missing_policy <- match.arg(structural_missing_policy)
  .reject_unsupported_design_fields(design)
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed))
    stop("seed must be a finite numeric scalar.", call. = FALSE)
  resolved <- .resolve_split_assignment(splits, data, model$folds)
  cluster_ids <- .resolve_cluster_ids(cluster, data)
  if (is.null(cluster_ids) && identical(splits$method, "group")) cluster_ids <- splits$group_values
  if (!is.null(cluster_ids)) .validate_cluster_labels(cluster_ids, nrow(data), "cluster")
  if (is.null(splits$outer) || !all(c("outer_id", "train_ids", "test_ids") %in% names(splits$outer)))
    stop("splits must include outer train/test partitions.", call. = FALSE)
  partition_ids <- .outer_validate_partitions(splits, data, cluster_ids = cluster_ids)
  indicator_order <- unlist(lapply(model$constructs, `[[`, "indicators"), use.names = FALSE)
  structure_nodes <- unique(c(names(structure$effects), unlist(lapply(structure$effects,
    function(edges) unlist(lapply(names(edges), .predictor_constructs), use.names = FALSE)), use.names = FALSE)))
  missing_nodes <- setdiff(structure_nodes, names(model$constructs))
  if (length(missing_nodes))
    stop(sprintf("Structural declarations use undeclared model construct(s): %s.", paste(missing_nodes, collapse = ", ")), call. = FALSE)
  outcome_indicators <- unique(unlist(lapply(names(structure$effects), function(outcome)
    model$constructs[[outcome]]$indicators), use.names = FALSE))
  missing_outcome_columns <- setdiff(outcome_indicators, names(data))
  if (length(missing_outcome_columns))
    stop(paste0("G7 limitation: predictor-only prospective scoring is unsupported; held-out data are missing declared outcome indicator column(s): ",
      paste(missing_outcome_columns, collapse = ", "), "."), call. = FALSE)
  missing_model_columns <- setdiff(indicator_order, names(data))
  if (length(missing_model_columns))
    stop(sprintf("Data is missing declared model indicator column(s) before outer fitting: %s.",
      paste(missing_model_columns, collapse = ", ")), call. = FALSE)
  prediction_rows <- list(); test_metric_rows <- list(); selection_rows <- list()
  provenance_rows <- list(); failure_rows <- list()
  for (i in seq_len(nrow(splits$outer))) {
    partition <- splits$outer[i, , drop = FALSE]
    outer_id <- as.integer(partition$outer_id[[1L]])
    train_ids <- partition_ids[[i]]$train_ids
    test_ids <- partition_ids[[i]]$test_ids
    partition_seed <- as.numeric(seed) + outer_id - 1
    stage <- "measurement"
    selected_shapes <- NA_character_
    status_detail <- ""
    status <- "failed"
    tryCatch({
      train_split <- .outer_training_split(splits, resolved$assignment, train_ids, cluster_ids = cluster_ids)
      training_model <- model
      training_model$folds <- train_split$folds
      train_data <- data[train_ids, , drop = FALSE]
      fit <- fit_states(training_model, train_data, seed = partition_seed,
        iterations = iterations, tolerance = tolerance, quadrature = quadrature,
        diagnostics = diagnostics, missing_policy = measurement_missing_policy,
        split = train_split,
        cluster = if (is.null(cluster_ids)) NULL else cluster_ids[train_ids],
        design = design)

      stage <- "structural_selection"
      association_args <- structural_args
      association_args$fit <- fit
      association_args$structure <- structure
      association_args$folds <- fit$folds
      association_args$missing_policy <- structural_missing_policy
      if (is.null(association_args$seed)) association_args$seed <- partition_seed
      association <- do.call(associate, association_args)
      selection <- effect_ledger(association)
      if (nrow(selection)) {
        selection$outer_id <- outer_id
        selection$metric_scope <- "internal_selection"
        selection_rows[[length(selection_rows) + 1L]] <- selection
        selected_shapes <- paste(paste0(selection$outcome, "~", selection$predictor, ":", selection$shape), collapse = ";")
      }

      stage <- "outer_test_scoring"
      held_out <- .outer_prepare_test_data(data, model, test_ids, measurement_missing_policy)
      if (!all(indicator_order %in% names(held_out$data)))
        stop("G7 limitation: predictor-only prospective scoring is unsupported; held-out data must include all declared outcome indicators.", call. = FALSE)
      test_scores <- score_states(fit, held_out$data[, indicator_order, drop = FALSE])
      prediction_for_partition <- list(); metrics_for_partition <- list()
      for (outcome in names(association$full_models)) {
        shape_model <- association$full_models[[outcome]]
        predicted <- .predict_shape_model(shape_model, test_scores)
        observed <- test_scores[[outcome]]
        required_constructs <- unique(c(outcome, unlist(lapply(names(shape_model$shapes), .predictor_constructs), use.names = FALSE)))
        observed_construct <- vapply(required_constructs, function(construct) {
          indicators <- association$fit$model$constructs[[construct]]$indicators
          rowSums(!is.na(held_out$data[, indicators, drop = FALSE])) > 0
        }, logical(nrow(held_out$data)))
        valid_rows <- if (is.matrix(observed_construct)) apply(observed_construct, 1L, all) else observed_construct
        metric <- .outer_prediction_metrics(observed[valid_rows], predicted[valid_rows])
        metrics_for_partition[[length(metrics_for_partition) + 1L]] <- data.frame(
          outer_id = outer_id, outcome = outcome, n = as.integer(metric[["n"]]),
          rmse = unname(metric[["rmse"]]), mae = unname(metric[["mae"]]),
          r_squared = unname(metric[["r_squared"]]), metric_scope = "outer_test",
          stringsAsFactors = FALSE)
        if (any(valid_rows)) prediction_for_partition[[length(prediction_for_partition) + 1L]] <- data.frame(
          outer_id = outer_id, row_id = held_out$ids[valid_rows], outcome = outcome,
          observed = as.numeric(observed[valid_rows]), predicted = as.numeric(predicted[valid_rows]),
          metric_scope = "outer_test", stringsAsFactors = FALSE)
      }
      if (length(metrics_for_partition)) test_metric_rows[[length(test_metric_rows) + 1L]] <- do.call(rbind, metrics_for_partition)
      if (length(prediction_for_partition)) prediction_rows[[length(prediction_rows) + 1L]] <- do.call(rbind, prediction_for_partition)
      has_observed_test <- length(metrics_for_partition) && any(vapply(metrics_for_partition,
        function(row) isTRUE(row$n[[1L]] > 0L), logical(1)))
      status <- if (has_observed_test) "success" else "partial"
      if (!has_observed_test) status_detail <- "no_observed_outcome"
    }, error = function(condition) {
      failure_rows[[length(failure_rows) + 1L]] <<- data.frame(outer_id = outer_id,
        stage = stage, message = conditionMessage(condition), stringsAsFactors = FALSE)
    })
    provenance_rows[[length(provenance_rows) + 1L]] <- .outer_provenance_row(
      outer_id, splits, train_ids, test_ids, partition_seed, status,
      train_unit_n = partition_ids[[i]]$train_unit_n,
      test_unit_n = partition_ids[[i]]$test_unit_n,
      selected_shapes = selected_shapes, status_detail = status_detail)
  }
  failures <- if (length(failure_rows)) do.call(rbind, failure_rows) else .outer_empty_failures()
  predictions <- if (length(prediction_rows)) do.call(rbind, prediction_rows) else .outer_empty_predictions()
  test_metrics <- if (length(test_metric_rows)) do.call(rbind, test_metric_rows) else .outer_empty_test_metrics()
  selection_metrics <- if (length(selection_rows)) do.call(rbind, selection_rows) else .outer_empty_selection_metrics()
  provenance <- if (length(provenance_rows)) do.call(rbind, provenance_rows) else data.frame()
  successful <- sum(provenance$status == "success")
  status <- if (!successful) "failed" else if (successful < nrow(provenance)) "partial" else "complete"
  structure(list(predictions = predictions, selection_metrics = selection_metrics,
    test_metrics = test_metrics, provenance = provenance, failures = failures,
    settings = list(seed = seed, iterations = iterations, tolerance = tolerance,
      quadrature = quadrature, diagnostics = diagnostics, structural_args = structural_args,
      measurement_missing_policy = measurement_missing_policy,
      structural_missing_policy = structural_missing_policy, splits = splits,
      cluster = cluster, design = design),
    status = status), class = c("cssem_outer_validation", "list"))
}

#' @export
print.cssem_outer_validation <- function(x, ...) {
  successes <- if (nrow(x$provenance)) sum(x$provenance$status == "success") else 0L
  total <- nrow(x$provenance)
  cat("CS-SEM outer validation: ", successes, "/", total,
    " partitions successful; status = ", x$status, "\n", sep = "")
  cat("  held-out metrics: outer_test; selection metrics: internal_selection\n")
  if (nrow(x$failures)) cat("  failures retained: ", nrow(x$failures), "\n", sep = "")
  invisible(x)
}
