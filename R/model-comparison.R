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
  table[, c("outer_id", "outcome", "n", metrics), drop = FALSE]
}

.comparison_bootstrap <- function(differences, metrics, reps, seed) {
  .preserve_seed(); set.seed(seed)
  partitions <- sort(unique(differences$outer_id)); draws <- vector("list", reps)
  for (b in seq_len(reps)) {
    sampled <- sample(partitions, length(partitions), replace = TRUE)
    rows <- differences[match(sampled, differences$outer_id), , drop = FALSE]
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
    return(structure(list(first = first, second = second, differences = differences,
      intervals = intervals, draws = draws, metrics = metrics, reps = reps, seed = seed,
      alignment = alignment, status = "complete",
      limitation = "paired predictive held-out comparison only; no likelihood or global-fit comparison"),
      class = c("cssem_model_comparison", "list")))
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
  structure(list(first = first, second = second, differences = differences,
    intervals = intervals, draws = draws, metrics = metrics, reps = reps, seed = seed,
    status = "complete", limitation = "paired predictive held-out comparison only; no likelihood or global-fit comparison"),
    class = c("cssem_model_comparison", "list"))
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
  if (!is.list(args_a) || !is.list(args_b)) stop("args_a and args_b must be named lists.", call. = FALSE)
  common <- list(data = data, splits = splits, seed = seed)
  extra <- list(...)
  first <- do.call(validate_outer, c(list(model = model_a, structure = structure_a), common, extra, args_a))
  second <- do.call(validate_outer, c(list(model = model_b, structure = structure_b), common, extra, args_b))
  result <- compare_outer(first, second, alignment = alignment)
  result$model_a <- model_a; result$structure_a <- structure_a
  result$model_b <- model_b; result$structure_b <- structure_b
  result$alignment <- alignment
  result
}

#' @export
print.cssem_model_comparison <- function(x, ...) {
  cat("CS-SEM paired model comparison: ", length(x$metrics), " metric(s); status = ", x$status, "\n", sep = "")
  print(x$intervals, row.names = FALSE)
  invisible(x)
}
