.bootstrap_association <- function(context, association, selection = c("fixed", "repeat")) {
  selection <- match.arg(selection)
  refit <- association$fit
  refit$locked_scores <- context$scores
  refit$folds <- context$fit$folds
  refit$row_ids <- seq_len(nrow(context$scores))
  refit$data <- context$data
  settings <- association$association_settings
  settings$folds <- context$fit$folds
  settings$seed <- context$seed
  settings$eiv_bootstrap <- 0L
  settings$reliability <- association$association_settings$reliability
  settings$missing_policy <- association$missing_policy
  settings$fixed_shapes <- if (selection == "fixed")
    lapply(association$full_models, `[[`, "shapes") else NULL
  do.call(associate, c(list(fit = refit, structure = association$structure), settings))
}
