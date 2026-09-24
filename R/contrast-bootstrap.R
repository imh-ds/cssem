# The fit that contrast bootstraps resample: the association's own retained
# rows. Resampling every fitted row would reintroduce rows the point
# association excluded (prior-only or incomplete scores), so the draws would
# describe a different sample from the point estimate.
.association_bootstrap_fit <- function(association) {
  fit <- association$fit
  if (is.null(fit$data)) fit$data <- as.data.frame(fit$locked_scores)
  n <- nrow(fit$locked_scores)
  fit_ids <- if (!is.null(fit$row_ids) && length(fit$row_ids) == n) as.integer(fit$row_ids) else seq_len(n)
  ids <- if (!is.null(association$row_ids)) as.integer(association$row_ids) else fit_ids
  keep <- match(ids, fit_ids)
  if (anyNA(keep)) stop("Association rows cannot be aligned to the fitted rows for bootstrap resampling.", call. = FALSE)
  subset_rows <- function(value) {
    if (is.null(value)) return(NULL)
    if (is.data.frame(value) || is.matrix(value)) {
      if (nrow(value) == n) value[keep, , drop = FALSE] else value
    } else if (length(value) == n) value[keep] else value
  }
  fit$data <- subset_rows(fit$data)
  fit$locked_scores <- subset_rows(fit$locked_scores)
  fit$folds <- subset_rows(fit$folds)
  fit$score_posterior_sd <- subset_rows(fit$score_posterior_sd)
  fit$cluster_ids <- subset_rows(fit$cluster_ids)
  # Input-row IDs, so cluster columns and input-length cluster vectors
  # still resolve against the original data.
  fit$row_ids <- ids
  fit$sample_ledger <- NULL
  fit
}

# `context$fit` is the association-row fit from .association_bootstrap_fit(),
# so fold labels, posterior SDs, and indices all refer to the same rows.
.bootstrap_association <- function(context, association, selection = c("fixed", "repeat")) {
  selection <- match.arg(selection)
  base <- context$fit
  refit <- base
  refit$locked_scores <- context$scores
  refit$folds <- if (!is.null(base$folds) && length(base$folds) == nrow(base$locked_scores) &&
    identical(context$refit, "locked_scores")) base$folds[context$indices] else base$folds
  refit$row_ids <- seq_len(nrow(context$scores))
  refit$data <- context$data
  refit$input_data <- context$data
  refit$sample_ledger <- NULL
  refit$cluster_ids <- context$cluster_ids
  # Source rows of each draw, so repeated structural folds keep a duplicated
  # row's copies together (see .structural_fold_sets()).
  refit$resample_source_ids <- context$indices
  if (!is.null(base$score_posterior_sd) && nrow(as.data.frame(base$score_posterior_sd)) == nrow(base$locked_scores) &&
      identical(context$refit, "locked_scores"))
    refit$score_posterior_sd <- base$score_posterior_sd[context$indices, , drop = FALSE]
  settings <- association$association_settings
  settings$folds <- refit$folds
  settings$seed <- context$seed
  settings$eiv_bootstrap <- 0L
  settings$reliability <- association$association_settings$reliability
  settings$missing_policy <- association$missing_policy
  settings$fixed_shapes <- if (selection == "fixed")
    lapply(association$full_models, `[[`, "shapes") else NULL
  do.call(associate, c(list(fit = refit, structure = association$structure), settings))
}
