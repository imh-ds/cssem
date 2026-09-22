.comparison_scalar_integer <- function(value, name, minimum = 1L) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
      value != as.integer(value) || value < minimum)
    stop(sprintf("%s must be a whole number of at least %d.", name, minimum), call. = FALSE)
  as.integer(value)
}

.comparison_validate_metrics <- function(metrics) {
  metrics <- unique(as.character(metrics))
  allowed <- c("rmse", "mae", "r_squared")
  if (!length(metrics) || any(!metrics %in% allowed))
    stop("metrics must contain only rmse, mae, and r_squared.", call. = FALSE)
  metrics
}

.comparison_validate_identity <- function(first, second, require_targets = TRUE) {
  if (!inherits(first, "cssem_outer_validation") || !inherits(second, "cssem_outer_validation"))
    stop("first and second must be cssem_outer_validation objects.", call. = FALSE)
  if (!identical(first$settings$split_fingerprint, second$settings$split_fingerprint))
    stop("Models were evaluated on different outer partitions; split fingerprints do not match.", call. = FALSE)
  if (!identical(first$settings$observation_fingerprint, second$settings$observation_fingerprint))
    stop("Models were evaluated on different observation rows; observation fingerprints do not match.", call. = FALSE)
  if (require_targets && !identical(first$settings$target_fingerprint, second$settings$target_fingerprint))
    stop("Models use different target indicators; target fingerprints do not match.", call. = FALSE)
  if (!identical(first$settings$score_basis_fingerprint, second$settings$score_basis_fingerprint))
    stop("Models use different score bases; score-basis fingerprints do not match.", call. = FALSE)
  if (!identical(first$provenance$outer_id, second$provenance$outer_id))
    stop("Outer partition IDs do not match.", call. = FALSE)
  for (i in seq_len(nrow(first$provenance))) {
    if (!identical(as.integer(first$provenance$train_ids[[i]]), as.integer(second$provenance$train_ids[[i]])) ||
        !identical(as.integer(first$provenance$test_ids[[i]]), as.integer(second$provenance$test_ids[[i]])))
      stop("Models have different partition row IDs.", call. = FALSE)
  }
}

.comparison_aligned_metrics <- function(first, second, alignment, metrics) {
  required <- c("outer_id", "outcome", "first_row_id", "second_row_id")
  if (!is.data.frame(alignment) || !all(required %in% names(alignment)))
    stop("alignment must be a data frame with outer_id, outcome, first_row_id, and second_row_id columns.", call. = FALSE)
  rows <- lapply(seq_len(nrow(alignment)), function(i) {
    row <- alignment[i, , drop = FALSE]; outer_id <- row$outer_id[[1L]]; outcome <- as.character(row$outcome[[1L]])
    first_rows <- first$predictions[first$predictions$outer_id == outer_id & first$predictions$outcome == outcome, , drop = FALSE]
    second_rows <- second$predictions[second$predictions$outer_id == outer_id & second$predictions$outcome == outcome, , drop = FALSE]
    a <- first_rows[first_rows$row_id == row$first_row_id[[1L]], , drop = FALSE]
    b <- second_rows[second_rows$row_id == row$second_row_id[[1L]], , drop = FALSE]
    if (nrow(a) != 1L || nrow(b) != 1L) stop("alignment refers to a missing held-out prediction row.", call. = FALSE)
    cbind(a, observed_b = b$observed, predicted_b = b$predicted)
  })
  if (!length(rows)) stop("alignment must contain at least one paired observed-target row.", call. = FALSE)
  aligned <- do.call(rbind, rows)
  groups <- split(seq_len(nrow(aligned)), interaction(aligned$outer_id, aligned$outcome, drop = TRUE, lex.order = TRUE))
  do.call(rbind, lapply(groups, function(index) {
    row <- aligned[index[[1L]], , drop = FALSE]
    first_metric <- .outer_prediction_metrics(aligned$observed[index], aligned$predicted[index])
    second_metric <- .outer_prediction_metrics(aligned$observed_b[index], aligned$predicted_b[index])
    data.frame(outer_id = row$outer_id[[1L]], outcome = row$outcome[[1L]],
      n = min(first_metric[["n"]], second_metric[["n"]]),
      rmse = second_metric[["rmse"]] - first_metric[["rmse"]],
      mae = second_metric[["mae"]] - first_metric[["mae"]],
      r_squared = second_metric[["r_squared"]] - first_metric[["r_squared"]],
      stringsAsFactors = FALSE)
  }))
}

.comparison_metric_table <- function(result, metrics) {
  table <- result$test_metrics
  if (!is.data.frame(table) || !nrow(table)) stop("Outer validation contains no held-out metrics.", call. = FALSE)
  required <- c("outer_id", "outcome", "n", metrics)
  if (!all(required %in% names(table))) stop("Outer validation is missing required held-out metric columns.", call. = FALSE)
  if ("metric_scope" %in% names(table) && any(table$metric_scope != "outer_test"))
    stop("Outer comparison requires held-out metrics with metric_scope = \"outer_test\".", call. = FALSE)
  table[, c("outer_id", "outcome", "n", metrics), drop = FALSE]
}

.comparison_remap_constructs <- function(result, alignment, target_fingerprint = NULL, score_basis_fingerprint = NULL) {
  if (!is.character(alignment) || is.null(names(alignment)) || any(!nzchar(names(alignment))) ||
      any(!nzchar(alignment)) || anyDuplicated(names(alignment)) || anyDuplicated(unname(alignment)))
    stop("alignment must be a named character map from model-B construct names to model-A names.", call. = FALSE)
  out <- result; map <- alignment
  remap <- function(values) {
    values <- as.character(values); hit <- values %in% names(map); values[hit] <- unname(map[values[hit]]); values
  }
  if (is.data.frame(out$test_metrics) && "outcome" %in% names(out$test_metrics)) out$test_metrics$outcome <- remap(out$test_metrics$outcome)
  if (is.data.frame(out$predictions) && "outcome" %in% names(out$predictions)) out$predictions$outcome <- remap(out$predictions$outcome)
  if (is.data.frame(out$selection_metrics) && "outcome" %in% names(out$selection_metrics)) out$selection_metrics$outcome <- remap(out$selection_metrics$outcome)
  if (!is.null(target_fingerprint)) out$settings$target_fingerprint <- target_fingerprint
  if (!is.null(score_basis_fingerprint)) out$settings$score_basis_fingerprint <- score_basis_fingerprint
  out
}

.comparison_validate_model_alignment <- function(model_a, model_b, alignment) {
  names_a <- names(model_a$constructs); names_b <- names(model_b$constructs)
  map <- if (is.null(alignment)) character() else alignment
  if (is.null(alignment) && !setequal(names_a, names_b))
    stop("Different measurement construct names require an explicit alignment map.", call. = FALSE)
  if (!is.null(alignment) && !is.character(alignment)) return(invisible(TRUE))
  for (construct_b in names_b) {
    construct_a <- if (construct_b %in% names(map)) unname(map[[construct_b]]) else construct_b
    if (!construct_a %in% names_a)
      stop(sprintf("Alignment does not map model-B construct '%s' to model A.", construct_b), call. = FALSE)
    indicators_a <- model_a$constructs[[construct_a]]$indicators
    indicators_b <- model_b$constructs[[construct_b]]$indicators
    if (!identical(as.character(indicators_a), as.character(indicators_b)))
      stop(sprintf("Aligned constructs '%s' and '%s' do not share the same observed indicators and score basis.", construct_b, construct_a), call. = FALSE)
  }
  invisible(TRUE)
}

.comparison_bootstrap <- function(differences, metrics, reps, seed) {
  .preserve_seed(); set.seed(seed)
  partitions <- sort(unique(differences$outer_id)); draws <- vector("list", reps)
  for (b in seq_len(reps)) {
    sampled <- sample(partitions, length(partitions), replace = TRUE)
    rows <- do.call(rbind, lapply(sampled, function(partition) {
      differences[differences$outer_id == partition, , drop = FALSE]
    }))
    draws[[b]] <- vapply(metrics, function(metric) mean(rows[[metric]], na.rm = TRUE), numeric(1))
  }
  matrix(unlist(draws), nrow = reps, byrow = TRUE, dimnames = list(NULL, metrics))
}

#' Compare two outer-validation results on identical held-out partitions
#'
#' The comparison is predictive: it reports paired held-out metric deltas and
#' a partition bootstrap interval. It does not compare likelihood, AIC/BIC,
#' global fit, or latent-scale quantities.
#' @param first,second `cssem_outer_validation` results.
#' @param metrics Held-out metrics among `rmse`, `mae`, and `r_squared`.
#' @param reps Number of paired partition-bootstrap replicates.
#' @param seed Seed for the partition bootstrap.
#' @return A `cssem_model_comparison` object.
#' @export
compare_outer <- function(first, second, metrics = c("rmse", "mae", "r_squared"),
                          reps = 999L, seed = 1L, alignment = NULL) {
  comparison_call <- match.call()
  if (is.character(alignment)) {
    second <- .comparison_remap_constructs(second, alignment)
    result <- compare_outer(first, second, metrics = metrics, reps = reps, seed = seed)
    result$alignment <- alignment
    result$provenance_record <- .comparison_provenance_record("compare_outer", comparison_call,
      first, second, list(metrics = metrics, reps = reps, seed = seed,
        alignment = .comparison_provenance_alignment(alignment)))
    return(result)
  }
  .comparison_validate_identity(first, second, require_targets = is.null(alignment))
  metrics <- .comparison_validate_metrics(metrics)
  reps <- .comparison_scalar_integer(reps, "reps")
  seed <- .comparison_scalar_integer(seed, "seed", minimum = 0L)
  if (!is.null(alignment)) {
    aligned <- .comparison_aligned_metrics(first, second, alignment, metrics)
    differences <- aligned[, c("outer_id", "outcome", "n", metrics), drop = FALSE]
    draws <- .comparison_bootstrap(differences, metrics, reps, seed)
    intervals <- do.call(rbind, lapply(metrics, function(metric) {
      values <- draws[, metric]
      data.frame(metric = metric, estimate = mean(differences[[metric]], na.rm = TRUE),
        ci_low = unname(stats::quantile(values, .025, names = FALSE)),
        ci_high = unname(stats::quantile(values, .975, names = FALSE)),
        direction = if (metric %in% c("rmse", "mae")) ifelse(mean(differences[[metric]]) < 0, "second_better", ifelse(mean(differences[[metric]]) > 0, "first_better", "tie")) else
          ifelse(mean(differences[[metric]]) > 0, "second_better", ifelse(mean(differences[[metric]]) < 0, "first_better", "tie")),
        level = .95, stringsAsFactors = FALSE)
    }))
    result <- structure(list(first = first, second = second, differences = differences,
      intervals = intervals, draws = draws, metrics = metrics, reps = reps, seed = seed,
      alignment = alignment, status = "complete",
      limitation = "paired predictive held-out comparison only; no likelihood or global-fit comparison"),
      class = c("cssem_model_comparison", "list"))
    result$provenance_record <- .comparison_provenance_record("compare_outer", comparison_call,
      first, second, list(metrics = metrics, reps = reps, seed = seed,
        alignment = .comparison_provenance_alignment(alignment)))
    return(result)
  }
  first_metrics <- .comparison_metric_table(first, metrics)
  second_metrics <- .comparison_metric_table(second, metrics)
  first_key <- paste(first_metrics$outer_id, first_metrics$outcome, sep = "::")
  second_key <- paste(second_metrics$outer_id, second_metrics$outcome, sep = "::")
  if (!identical(sort(first_key), sort(second_key)))
    stop("Models have different target availability across partitions.", call. = FALSE)
  first_metrics <- first_metrics[match(sort(first_key), first_key), , drop = FALSE]
  second_metrics <- second_metrics[match(sort(second_key), second_key), , drop = FALSE]
  if (any(first_metrics$n != second_metrics$n))
    stop("Models have different comparable-row counts; supply an explicit observed-target alignment before comparison.", call. = FALSE)
  differences <- first_metrics[, c("outer_id", "outcome", "n"), drop = FALSE]
  for (metric in metrics) differences[[metric]] <- second_metrics[[metric]] - first_metrics[[metric]]
  draws <- .comparison_bootstrap(differences, metrics, reps, seed)
  intervals <- do.call(rbind, lapply(metrics, function(metric) {
    values <- draws[, metric]; probs <- c(.025, .975)
    data.frame(metric = metric, estimate = mean(differences[[metric]], na.rm = TRUE),
      ci_low = unname(stats::quantile(values, probs[[1L]], names = FALSE)),
      ci_high = unname(stats::quantile(values, probs[[2L]], names = FALSE)),
      direction = if (metric %in% c("rmse", "mae")) ifelse(mean(differences[[metric]]) < 0, "second_better", ifelse(mean(differences[[metric]]) > 0, "first_better", "tie")) else
        ifelse(mean(differences[[metric]]) > 0, "second_better", ifelse(mean(differences[[metric]]) < 0, "first_better", "tie")),
      level = .95, stringsAsFactors = FALSE)
  }))
  result <- structure(list(first = first, second = second, differences = differences,
    intervals = intervals, draws = draws, metrics = metrics, reps = reps, seed = seed,
    status = "complete", limitation = "paired predictive held-out comparison only; no likelihood or global-fit comparison"),
    class = c("cssem_model_comparison", "list"))
  result$provenance_record <- .comparison_provenance_record("compare_outer", comparison_call,
    first, second, list(metrics = metrics, reps = reps, seed = seed, alignment = alignment))
  result
}

.comparison_provenance_record <- function(operation, call, first, second, settings) {
  parents <- list()
  first_record <- first$provenance_record
  second_record <- second$provenance_record
  if (inherits(first_record, "cssem_provenance")) parents$first <- first_record
  if (inherits(second_record, "cssem_provenance")) parents$second <- second_record
  input <- if (!is.null(first_record$input)) first_record$input else
    if (!is.null(second_record$input)) second_record$input else .cssem_provenance_input_summary()
  .cssem_provenance_record(operation, call, settings = settings, input = input,
    parent = parents, packages = "MASS")
}

.comparison_provenance_alignment <- function(alignment) {
  if (is.null(alignment)) return(NULL)
  if (is.character(alignment)) return(alignment)
  if (is.data.frame(alignment))
    return(list(type = "data.frame", rows = as.integer(nrow(alignment)),
      columns = as.character(names(alignment))))
  list(type = class(alignment))
}

#' Compare two model specifications on one supplied outer split design
#'
#' @param model_a,model_b `cssem_model` objects.
#' @param structure_a,structure_b `cssem_structure` objects.
#' @param data Data frame containing both model specifications' indicators.
#' @param splits A single `cssem_splits` object reused for both validations.
#' @param seed Base validation seed.
#' @param args_a,args_b Named arguments forwarded to [validate_outer()].
#' @param alignment Reserved explicit observed-target alignment metadata.
#' @param ... Additional arguments forwarded to both validations.
#' @return A `cssem_model_comparison` object retaining both validations.
#' @export
compare_models <- function(model_a, structure_a, model_b, structure_b, data, splits,
                           seed = 1L, args_a = list(), args_b = list(), alignment = NULL, ...) {
  compare_models_call <- match.call()
  if (!inherits(model_a, "cssem_model") || !inherits(model_b, "cssem_model"))
    stop("model_a and model_b must be cssem_model objects.", call. = FALSE)
  if (!inherits(structure_a, "cssem_structure") || !inherits(structure_b, "cssem_structure"))
    stop("structure_a and structure_b must be cssem_structure objects.", call. = FALSE)
  if (!is.list(args_a) || !is.list(args_b)) stop("args_a and args_b must be named lists.", call. = FALSE)
  if (!is.null(alignment) && !is.character(alignment) && !is.data.frame(alignment))
    stop("alignment must be NULL, a named construct map, or an observed-row alignment data frame.", call. = FALSE)
  .comparison_validate_model_alignment(model_a, model_b, alignment)
  common <- list(data = data, splits = splits, seed = seed)
  extra <- list(...)
  first <- do.call(validate_outer, c(list(model = model_a, structure = structure_a), common, extra, args_a))
  second <- do.call(validate_outer, c(list(model = model_b, structure = structure_b), common, extra, args_b))
  result <- compare_outer(first, second, alignment = alignment)
  result$model_a <- model_a; result$structure_a <- structure_a
  result$model_b <- model_b; result$structure_b <- structure_b
  result$alignment <- alignment
  parents <- list()
  if (inherits(first$provenance_record, "cssem_provenance")) parents$first <- first$provenance_record
  if (inherits(second$provenance_record, "cssem_provenance")) parents$second <- second$provenance_record
  input <- if (!is.null(first$provenance_record$input)) first$provenance_record$input else
    .cssem_provenance_input_summary(data, split_ids = list(outer_split = seq_len(nrow(data))))
  result$provenance_record <- .cssem_provenance_record("compare_models", compare_models_call,
    settings = list(model_a = .cssem_provenance_model_specification(model_a),
      structure_a = .cssem_provenance_structure_specification(structure_a),
      model_b = .cssem_provenance_model_specification(model_b),
      structure_b = .cssem_provenance_structure_specification(structure_b),
      seed = seed, comparison = list(metrics = result$metrics, reps = result$reps,
        seed = result$seed),
      alignment = .comparison_provenance_alignment(alignment),
      split = list(method = splits$method, folds = splits$folds,
        fingerprint = .outer_split_fingerprint(splits)),
      forwarded_argument_names = list(common = names(extra), args_a = names(args_a),
        args_b = names(args_b))),
    input = input, parent = parents, packages = c("MASS", "rpart", "splines"))
  result
}

#' @export
print.cssem_model_comparison <- function(x, ...) {
  cat("CS-SEM paired model comparison: ", length(x$metrics), " metric(s); status = ", x$status, "\n", sep = "")
  print(x$intervals, row.names = FALSE)
  invisible(x)
}
