#' Declare an associational CS-SEM structural model
#'
#' Declares theory-specified associational effects among locked construct states.
#' This object carries no causal interpretation: it does not accept adjustment
#' sets, estimands, mediation, or causal labels.
#'
#' @param effects A named list. Each name is an endogenous construct and each
#'   value is a non-empty character vector of its declared predictors.
#' @param order Optional character vector giving the user-declared temporal
#'   order of all constructs. Earlier constructs are eligible predictors in a
#'   temporally admissible shadow model. If omitted, CS-SEM derives an order
#'   from an acyclic declared graph when possible.
#' @return An object of class `cssem_structure`.
#' @examples
#' structure <- cssem_structure(list(
#'   Loyalty = c("Trust", "Satisfaction"),
#'   Satisfaction = "Trust"
#' ))
#' @export
cssem_structure <- function(effects, order = NULL) {
  if (!is.list(effects) || is.null(names(effects)) || any(names(effects) == ""))
    stop("effects must be a named list of outcome-predictor declarations.", call. = FALSE)
  parsed <- lapply(effects, function(predictors) {
    predictors <- as.character(predictors)
    if (!length(predictors) || any(!nzchar(predictors)) || anyDuplicated(predictors))
      stop("Each outcome needs one or more unique predictor names.", call. = FALSE)
    predictors
  })
  for (outcome in names(parsed)) {
    if (outcome %in% parsed[[outcome]])
      stop("An outcome cannot be its own predictor.", call. = FALSE)
  }
  if (!is.null(order) && (anyDuplicated(order) || any(!nzchar(order))))
    stop("order must contain unique, non-empty construct names.", call. = FALSE)
  structure(list(effects = parsed, order = if (is.null(order)) NULL else as.character(order),
    status = "associational"), class = "cssem_structure")
}

.derived_temporal_order <- function(effects, all_names) {
  graph_names <- unique(c(names(effects), unlist(effects, use.names = FALSE)))
  if (!setequal(graph_names, all_names)) return(NULL)
  remaining <- all_names; resolved <- character()
  while (length(remaining)) {
    available <- remaining[vapply(remaining, function(node) {
      parents <- if (node %in% names(effects)) effects[[node]] else character()
      all(parents %in% resolved)
    }, logical(1))]
    if (!length(available)) return(NULL)
    resolved <- c(resolved, available)
    remaining <- setdiff(remaining, available)
  }
  resolved
}

.resolve_temporal_order <- function(structure, all_names) {
  order <- structure$order
  if (is.null(order)) order <- .derived_temporal_order(structure$effects, all_names)
  if (is.null(order) || !setequal(order, all_names))
    stop("Temporal shadows require an explicit order containing every locked construct when the declared graph is cyclic or incomplete.", call. = FALSE)
  for (outcome in names(structure$effects)) {
    if (any(match(structure$effects[[outcome]], order) >= match(outcome, order)))
      stop("Every declared predictor must occur before its outcome in order.", call. = FALSE)
  }
  order
}

.structural_formula <- function(outcome, predictors, shape, spline_df) {
  terms <- if (shape == "linear") predictors else
    sprintf("splines::ns(%s, df = %d)", predictors, spline_df)
  stats::as.formula(paste(outcome, "~", paste(terms, collapse = " + ")))
}

.cv_predictions <- function(scores, outcome, predictors, folds, shape, spline_df) {
  formula <- .structural_formula(outcome, predictors, shape, spline_df)
  prediction <- rep(NA_real_, nrow(scores))
  for (fold in sort(unique(folds))) {
    train <- scores[folds != fold, , drop = FALSE]
    test <- scores[folds == fold, , drop = FALSE]
    model <- stats::lm(formula, data = train)
    prediction[folds == fold] <- stats::predict(model, newdata = test)
  }
  prediction
}

.structural_fold_sets <- function(folds, repeats, seed) {
  repeats <- as.integer(repeats)
  if (is.na(repeats) || repeats < 1L) stop("structural_repeats must be at least 1.", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed))
    stop("seed must be a finite numeric scalar.", call. = FALSE)
  sets <- vector("list", repeats)
  sets[[1L]] <- folds
  if (repeats > 1L) {
    k <- length(unique(folds))
    for (repeat_index in 2:repeats) {
      set.seed(seed + repeat_index)
      sets[[repeat_index]] <- sample(rep(seq_len(k), length.out = length(folds)))
    }
  }
  sets
}

.cv_candidate <- function(scores, outcome, predictors, fold_sets, shape, spline_df) {
  predictions <- vector("list", length(fold_sets))
  losses <- numeric()
  for (repeat_index in seq_along(fold_sets)) {
    predictions[[repeat_index]] <- .cv_predictions(scores, outcome, predictors, fold_sets[[repeat_index]], shape, spline_df)
    losses <- c(losses, .foldwise_mse(scores[[outcome]], predictions[[repeat_index]], fold_sets[[repeat_index]]))
  }
  list(prediction = predictions[[1L]], fold_mse = losses,
    # Keep reported candidate metrics on the primary fold assignment so they
    # remain directly comparable with the single-pass shadow diagnostics.
    metrics = .prediction_metrics(scores[[outcome]], predictions[[1L]]))
}

.shadow_predictions <- function(scores, outcome, predictors, folds) {
  prediction <- rep(NA_real_, nrow(scores))
  if (!length(predictors)) return(prediction)
  formula <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  for (fold in sort(unique(folds))) {
    train <- scores[folds != fold, , drop = FALSE]
    test <- scores[folds == fold, , drop = FALSE]
    control <- rpart::rpart.control(
      minsplit = max(20L, floor(nrow(train) * .10)), maxdepth = 3L,
      cp = .01, xval = 0L
    )
    model <- rpart::rpart(formula, data = train, method = "anova", control = control)
    prediction[folds == fold] <- stats::predict(model, newdata = test)
  }
  prediction
}

.prediction_metrics <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  sse <- sum((observed[keep] - predicted[keep])^2)
  sst <- sum((observed[keep] - mean(observed[keep]))^2)
  c(rmse = sqrt(mean((observed[keep] - predicted[keep])^2)), r_squared = 1 - sse / sst)
}

.foldwise_mse <- function(observed, predicted, folds) {
  vapply(sort(unique(folds)), function(fold) {
    index <- folds == fold
    mean((observed[index] - predicted[index])^2)
  }, numeric(1))
}

.effect_summary <- function(model, outcome, predictors, shape, scores) {
  if (shape == "linear") {
    coefficient <- stats::coef(model)[predictors]
    return(data.frame(outcome = outcome, predictor = predictors, shape = shape,
      estimate = unname(coefficient), x = NA_real_, fitted = NA_real_, stringsAsFactors = FALSE))
  }
  base <- as.data.frame(lapply(scores[predictors], mean)); names(base) <- predictors
  rows <- lapply(predictors, function(predictor) {
    grid <- seq(stats::quantile(scores[[predictor]], .05), stats::quantile(scores[[predictor]], .95), length.out = 50L)
    new_data <- base[rep(1L, length(grid)), , drop = FALSE]
    new_data[[predictor]] <- grid
    curve <- stats::predict(model, newdata = new_data)
    data.frame(outcome = outcome, predictor = predictor, shape = shape,
      estimate = NA_real_, x = grid, fitted = curve, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Fit associational structural effects on locked construct states
#'
#' Compares a theory-declared linear model with a low-complexity additive spline
#' model by paired cross-validation. A smooth model is retained only if its
#' average foldwise loss improvement exceeds `smooth_uncertainty` times its
#' cross-fold standard error. Shadow models are shallow
#' regression trees used as adequacy diagnostics, not discovered theory models.
#' A temporal shadow uses only constructs preceding the outcome in the declared
#' order; an unrestricted shadow uses all other same-wave constructs.
#'
#' @param fit A `cssem_fit` object. Only `fit$locked_scores` are used.
#' @param structure A `cssem_structure` object.
#' @param folds Optional structural validation folds. Defaults to the measurement
#'   cross-fitting folds stored in `fit`.
#' @param spline_df Predeclared degrees of freedom for additive natural-spline
#'   candidates. The default compares df 3 and df 4.
#' @param smooth_uncertainty Multiplier for the paired foldwise-loss standard
#'   error. The default of one is a one-standard-error selection rule.
#' @param structural_repeats Number of deterministic structural CV fold
#'   assignments used for shape selection.
#' @param seed Seed used only to generate repeated structural fold assignments.
#' @param shadow_scope Shadow benchmark scope: `"both"` (default),
#'   `"temporal"`, or `"unrestricted"`.
#' @return An object of class `cssem_association` with candidate metrics,
#'   selected effect summaries, out-of-fold predictions, and specification gaps.
#' @examples
#' # association <- cssem_associate(
#' #   fit, cssem_structure(list(Loyalty = c("Trust", "Satisfaction")))
#' # )
#' @export
cssem_associate <- function(fit, structure, folds = NULL, spline_df = c(3L, 4L),
                             smooth_uncertainty = 1,
                             structural_repeats = 3L, seed = 1L,
                             shadow_scope = c("both", "temporal", "unrestricted")) {
  if (!inherits(fit, "cssem_fit")) stop("fit must be a cssem_fit.", call. = FALSE)
  if (!inherits(structure, "cssem_structure")) stop("structure must be a cssem_structure.", call. = FALSE)
  scores <- fit$locked_scores
  all_names <- names(scores)
  declared <- unique(c(names(structure$effects), unlist(structure$effects, use.names = FALSE)))
  if (!all(declared %in% all_names)) stop("Structural declarations must use locked construct names.", call. = FALSE)
  shadow_scope <- match.arg(shadow_scope)
  scopes <- switch(shadow_scope, both = c("temporal", "unrestricted"), shadow_scope)
  temporal_order <- if ("temporal" %in% scopes) .resolve_temporal_order(structure, all_names) else NULL
  folds <- if (is.null(folds)) fit$folds else as.integer(folds)
  if (length(folds) != nrow(scores) || length(unique(folds)) < 2L)
    stop("folds must assign every row to at least two validation folds.", call. = FALSE)
  spline_df <- unique(as.integer(spline_df))
  if (!length(spline_df) || any(is.na(spline_df)) || any(spline_df < 2L)) stop("spline_df must contain values of at least 2.", call. = FALSE)
  if (!is.numeric(smooth_uncertainty) || length(smooth_uncertainty) != 1L || !is.finite(smooth_uncertainty) || smooth_uncertainty < 0)
    stop("smooth_uncertainty must be a non-negative numeric scalar.", call. = FALSE)

  fold_sets <- .structural_fold_sets(folds, structural_repeats, seed)
  candidates <- list(); effects <- list(); predictions <- list(); gaps <- list(); models <- list()
  for (outcome in names(structure$effects)) {
    predictors <- structure$effects[[outcome]]
    linear <- .cv_candidate(scores, outcome, predictors, fold_sets, "linear", spline_df[1L])
    smooth <- lapply(spline_df, function(df) .cv_candidate(scores, outcome, predictors, fold_sets, "smooth", df))
    improvement_mean <- vapply(smooth, function(candidate) mean(linear$fold_mse - candidate$fold_mse), numeric(1))
    improvement_se <- vapply(smooth, function(candidate) {
      difference <- linear$fold_mse - candidate$fold_mse
      stats::sd(difference) / sqrt(length(difference))
    }, numeric(1))
    best_index <- which.max(improvement_mean)
    best_smooth <- smooth[[best_index]]
    selected_shape <- if (improvement_mean[best_index] > smooth_uncertainty * improvement_se[best_index]) "smooth" else "linear"
    selected_df <- if (selected_shape == "smooth") spline_df[best_index] else NA_integer_
    selected_prediction <- if (selected_shape == "smooth") best_smooth$prediction else linear$prediction
    selected_metrics <- if (selected_shape == "smooth") best_smooth$metrics else linear$metrics
    formula <- .structural_formula(outcome, predictors, selected_shape, if (is.na(selected_df)) spline_df[1L] else selected_df)
    model <- stats::lm(formula, data = scores)
    shadow_predictions <- list()
    shadow_rows <- list()
    for (scope in scopes) {
      shadow_predictors <- if (scope == "temporal") {
        temporal_order[match(temporal_order, temporal_order) < match(outcome, temporal_order)]
      } else setdiff(all_names, outcome)
      shadow_prediction <- .shadow_predictions(scores, outcome, shadow_predictors, folds)
      shadow_metrics <- .prediction_metrics(scores[[outcome]], shadow_prediction)
      shadow_predictions[[scope]] <- shadow_prediction
      shadow_rows[[scope]] <- data.frame(outcome = outcome, shadow_scope = scope,
        eligible_predictors = paste(shadow_predictors, collapse = " + "),
        theory_r_squared = selected_metrics[["r_squared"]],
        shadow_r_squared = shadow_metrics[["r_squared"]],
        specification_gap = selected_metrics[["r_squared"]] - shadow_metrics[["r_squared"]],
        selected_shape = selected_shape, selected_spline_df = selected_df,
        structural_repeats = structural_repeats, stringsAsFactors = FALSE)
    }
    candidates[[outcome]] <- rbind(
      data.frame(outcome = outcome, candidate = "linear", spline_df = NA_integer_, rmse = linear$metrics[["rmse"]], r_squared = linear$metrics[["r_squared"]], mean_mse_improvement = NA_real_, mse_improvement_se = NA_real_, selected = selected_shape == "linear"),
      data.frame(outcome = outcome, candidate = paste0("smooth_df", spline_df), spline_df = spline_df,
        rmse = vapply(smooth, function(candidate) candidate$metrics[["rmse"]], numeric(1)),
        r_squared = vapply(smooth, function(candidate) candidate$metrics[["r_squared"]], numeric(1)),
        mean_mse_improvement = improvement_mean, mse_improvement_se = improvement_se,
        selected = selected_shape == "smooth" & spline_df == selected_df)
    )
    effect_data <- .effect_summary(model, outcome, predictors, selected_shape, scores)
    effect_data$spline_df <- selected_df
    effects[[outcome]] <- effect_data
    predictions[[outcome]] <- as.data.frame(c(list(observed = scores[[outcome]], theory = selected_prediction), shadow_predictions))
    gaps[[outcome]] <- do.call(rbind, shadow_rows)
    models[[outcome]] <- model
  }
  structure(list(structure = structure, candidate_metrics = do.call(rbind, candidates),
    effects = do.call(rbind, effects), predictions = predictions,
    specification_gap = do.call(rbind, gaps), full_models = models,
    folds = folds, structural_repeats = structural_repeats, temporal_order = temporal_order, shadow_scope = scopes,
    status = "associational"), class = "cssem_association")
}

#' Return a declared effect's associational evidence card
#'
#' @param association A `cssem_association` object.
#' @param outcome Declared endogenous construct name.
#' @return A list containing selected effects, candidate metrics, and temporal
#'   and/or unrestricted shadow specification gaps for the outcome.
#' @examples
#' # cssem_effect_card(association, "Loyalty")
#' @export
cssem_effect_card <- function(association, outcome) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  if (!outcome %in% names(association$full_models)) stop("Unknown structural outcome.", call. = FALSE)
  list(outcome = outcome,
    effects = association$effects[association$effects$outcome == outcome, , drop = FALSE],
    candidates = association$candidate_metrics[association$candidate_metrics$outcome == outcome, , drop = FALSE],
    specification_gap = association$specification_gap[association$specification_gap$outcome == outcome, , drop = FALSE],
    status = "associational")
}

#' Return structural shadow-model specification gaps
#'
#' @param association A `cssem_association` object.
#' @param scope Optional shadow scope (`"temporal"` or `"unrestricted"`).
#' @return A data frame with theory and shadow cross-validated R-squared values,
#'   eligible shadow predictors, and `theory - shadow` specification gaps.
#' @examples
#' # cssem_specification_gap(association)
#' @export
cssem_specification_gap <- function(association, scope = NULL) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  if (is.null(scope)) return(association$specification_gap)
  scope <- match.arg(scope, c("temporal", "unrestricted"))
  association$specification_gap[association$specification_gap$shadow_scope == scope, , drop = FALSE]
}

#' Print an associational CS-SEM structural fit
#'
#' @param x A `cssem_association` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_association <- function(x, ...) {
  cat("CS-SEM associational fit:", length(x$full_models), "declared outcome(s); causal interpretation disabled\n")
  invisible(x)
}
