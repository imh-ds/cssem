.bootstrap_association <- function(context, association, selection = c("fixed", "repeat")) {
  selection <- match.arg(selection)
  refit <- association$fit
  refit$locked_scores <- context$scores
  if (!is.null(association$fit$folds) && length(association$fit$folds) == nrow(association$fit$locked_scores))
    refit$folds <- association$fit$folds[context$indices] else refit$folds <- context$fit$folds
  refit$row_ids <- seq_len(nrow(context$scores))
  refit$data <- context$data
  refit$input_data <- context$data
  refit$sample_ledger <- NULL
  refit$cluster_ids <- context$cluster_ids
  if (!is.null(refit$score_posterior_sd) && nrow(as.data.frame(refit$score_posterior_sd)) == nrow(association$fit$locked_scores))
    refit$score_posterior_sd <- refit$score_posterior_sd[context$indices, , drop = FALSE]
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
