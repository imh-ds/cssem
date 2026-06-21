#' Declare an edge-level CS-SEM associational effect policy
#'
#' @param shape A declared shape policy: `"auto"`, `"linear"`,
#'   `"auto_monotone"`, `"monotone_increasing"`,
#'   `"monotone_decreasing"`, or `"smooth"`.
#' @return An object of class `cssem_effect`.
#' @export
cssem_effect <- function(shape = c("auto", "linear", "auto_monotone",
                                  "monotone_increasing", "monotone_decreasing", "smooth")) {
  shape <- match.arg(shape)
  structure(list(shape = shape), class = "cssem_effect")
}

.effect_policy <- function(value) {
  if (inherits(value, "cssem_effect")) return(value)
  if (is.character(value) && length(value) == 1L) return(cssem_effect(value))
  stop("Each named edge must be a cssem_effect() declaration or a supported shape string.", call. = FALSE)
}

.parse_effects <- function(value) {
  if (is.character(value)) {
    if (!length(value) || any(!nzchar(value)) || anyDuplicated(value))
      stop("Each outcome needs one or more unique predictor names.", call. = FALSE)
    result <- lapply(value, function(x) cssem_effect("auto")); names(result) <- value
    return(result)
  }
  if (!is.list(value) || is.null(names(value)) || any(names(value) == "") || anyDuplicated(names(value)))
    stop("Edge declarations must be a non-empty character vector or a named list.", call. = FALSE)
  lapply(value, .effect_policy)
}

#' Declare an associational CS-SEM structural model
#'
#' Character-vector declarations remain supported. Named edge declarations use
#' [cssem_effect()] to state each predictor's shape policy.
#'
#' @param effects A named list of outcomes. Each value is either a non-empty
#'   character vector of predictors or a named list of [cssem_effect()] objects.
#' @param order Optional character vector giving the user-declared temporal
#'   order of all constructs.
#' @return An object of class `cssem_structure`.
#' @export
cssem_structure <- function(effects, order = NULL) {
  if (!is.list(effects) || is.null(names(effects)) || any(names(effects) == ""))
    stop("effects must be a named list of outcome-predictor declarations.", call. = FALSE)
  parsed <- lapply(effects, .parse_effects)
  for (outcome in names(parsed)) if (outcome %in% names(parsed[[outcome]]))
    stop("An outcome cannot be its own predictor.", call. = FALSE)
  if (!is.null(order) && (anyDuplicated(order) || any(!nzchar(order))))
    stop("order must contain unique, non-empty construct names.", call. = FALSE)
  structure(list(effects = parsed, order = if (is.null(order)) NULL else as.character(order),
    status = "associational"), class = "cssem_structure")
}

.effect_predictors <- function(effect_specs) names(effect_specs)

.derived_temporal_order <- function(effects, all_names) {
  graph_names <- unique(c(names(effects), unlist(lapply(effects, names), use.names = FALSE)))
  if (!setequal(graph_names, all_names)) return(NULL)
  remaining <- all_names; resolved <- character()
  while (length(remaining)) {
    available <- remaining[vapply(remaining, function(node) {
      parents <- if (node %in% names(effects)) names(effects[[node]]) else character()
      all(parents %in% resolved)
    }, logical(1))]
    if (!length(available)) return(NULL)
    resolved <- c(resolved, available); remaining <- setdiff(remaining, available)
  }
  resolved
}

.resolve_temporal_order <- function(structure, all_names) {
  order <- structure$order
  if (is.null(order)) order <- .derived_temporal_order(structure$effects, all_names)
  if (is.null(order) || !setequal(order, all_names))
    stop("Temporal shadows require an explicit order containing every locked construct when the declared graph is cyclic or incomplete.", call. = FALSE)
  for (outcome in names(structure$effects)) {
    if (any(match(names(structure$effects[[outcome]]), order) >= match(outcome, order)))
      stop("Every declared predictor must occur before its outcome in order.", call. = FALSE)
  }
  order
}

.structural_fold_sets <- function(folds, repeats, seed) {
  repeats <- as.integer(repeats)
  if (is.na(repeats) || repeats < 1L) stop("structural_repeats must be at least 1.", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) stop("seed must be a finite numeric scalar.", call. = FALSE)
  sets <- vector("list", repeats); sets[[1L]] <- folds
  if (repeats > 1L) {
    k <- length(unique(folds))
    for (repeat_index in 2:repeats) {
      set.seed(seed + repeat_index)
      sets[[repeat_index]] <- sample(rep(seq_len(k), length.out = length(folds)))
    }
  }
  sets
}

.shape_candidates <- function(policy, spline_df = c(3L, 4L)) {
  smooth <- paste0("smooth_df", spline_df)
  switch(policy,
    linear = "linear",
    auto_monotone = c("linear", "monotone_increasing", "monotone_decreasing"),
    monotone_increasing = c("linear", "monotone_increasing"),
    monotone_decreasing = c("linear", "monotone_decreasing"),
    smooth = c("linear", smooth),
    auto = c("linear", "monotone_increasing", "monotone_decreasing", smooth))
}

.shape_kind <- function(shape) if (grepl("^smooth", shape)) "smooth" else shape
.shape_df <- function(shape) if (grepl("^smooth_df", shape)) as.integer(sub("smooth_df", "", shape)) else NA_integer_
.shape_complexity <- function(shape) {
  if (shape == "linear") return(1L)
  if (shape %in% c("monotone_increasing", "monotone_decreasing")) return(2L)
  2L + .shape_df(shape)
}

.hinge_basis <- function(x, knots) {
  if (!length(knots)) return(matrix(x, ncol = 1L))
  cbind(x, vapply(knots, function(knot) pmax(0, x - knot), numeric(length(x))))
}

.train_basis <- function(x, shape) {
  if (shape == "linear") return(list(values = matrix(x, ncol = 1L), info = list(shape = shape)))
  if (grepl("^smooth", shape)) {
    basis <- splines::ns(x, df = .shape_df(shape))
    return(list(values = basis, info = list(shape = shape, knots = attr(basis, "knots"), boundary = attr(basis, "Boundary.knots"))))
  }
  knots <- unique(as.numeric(stats::quantile(x, c(.25, .50, .75), na.rm = TRUE, names = FALSE)))
  knots <- knots[knots > min(x, na.rm = TRUE) & knots < max(x, na.rm = TRUE)]
  list(values = .hinge_basis(x, knots), info = list(shape = shape, knots = knots))
}

.predict_basis <- function(x, info) {
  if (info$shape == "linear") return(matrix(x, ncol = 1L))
  if (grepl("^smooth", info$shape))
    return(splines::ns(x, knots = info$knots, Boundary.knots = info$boundary))
  .hinge_basis(x, info$knots)
}

.projected_least_squares <- function(x, y, constrained, directions) {
  p <- ncol(x); coefficient <- rep(0, p)
  eigenvalue <- max(eigen(crossprod(x) / nrow(x), symmetric = TRUE, only.values = TRUE)$values)
  step <- 1 / max(eigenvalue, 1e-6)
  for (iteration in seq_len(1500L)) {
    updated <- coefficient - step * drop(crossprod(x, x %*% coefficient - y)) / nrow(x)
    increasing <- constrained[directions > 0]
    decreasing <- constrained[directions < 0]
    if (length(increasing)) updated[increasing] <- pmax(updated[increasing], 0)
    if (length(decreasing)) updated[decreasing] <- pmin(updated[decreasing], 0)
    if (max(abs(updated - coefficient)) < 1e-8) break
    coefficient <- updated
  }
  coefficient
}

.fit_shape_model <- function(data, outcome, shapes) {
  predictors <- names(shapes); blocks <- list(); infos <- list(); constrained <- integer(); directions <- integer()
  for (predictor in predictors) {
    built <- .train_basis(data[[predictor]], shapes[[predictor]])
    blocks[[predictor]] <- built$values; infos[[predictor]] <- built$info
    index <- seq_len(ncol(built$values)) + sum(vapply(blocks[-length(blocks)], ncol, integer(1))) + 1L
    if (built$info$shape %in% c("monotone_increasing", "monotone_decreasing")) {
      constrained <- c(constrained, index)
      directions <- c(directions, rep(if (built$info$shape == "monotone_increasing") 1L else -1L, length(index)))
    }
  }
  design <- if (length(blocks)) cbind(`(Intercept)` = 1, do.call(cbind, blocks)) else matrix(1, nrow(data), 1L)
  coefficient <- if (length(constrained)) .projected_least_squares(design, data[[outcome]], constrained, directions) else
    drop(solve(crossprod(design) + diag(1e-8, ncol(design)), crossprod(design, data[[outcome]])))
  maps <- list(); start <- 2L
  for (predictor in predictors) {
    width <- ncol(blocks[[predictor]]); maps[[predictor]] <- start:(start + width - 1L); start <- start + width
  }
  list(outcome = outcome, shapes = shapes, infos = infos, coefficient = coefficient, maps = maps)
}

.predict_shape_model <- function(model, data) {
  blocks <- lapply(names(model$shapes), function(predictor) .predict_basis(data[[predictor]], model$infos[[predictor]]))
  design <- if (length(blocks)) cbind(`(Intercept)` = 1, do.call(cbind, blocks)) else matrix(1, nrow(data), 1L)
  drop(design %*% model$coefficient)
}

.foldwise_mse <- function(observed, predicted, folds) vapply(sort(unique(folds)), function(fold) {
  index <- folds == fold; mean((observed[index] - predicted[index])^2)
}, numeric(1))

.prediction_metrics <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  sse <- sum((observed[keep] - predicted[keep])^2); sst <- sum((observed[keep] - mean(observed[keep]))^2)
  c(rmse = sqrt(mean((observed[keep] - predicted[keep])^2)), r_squared = 1 - sse / sst)
}

.cv_shape_candidate <- function(scores, outcome, shapes, fold_sets) {
  predictions <- vector("list", length(fold_sets)); losses <- numeric()
  for (repeat_index in seq_along(fold_sets)) {
    folds <- fold_sets[[repeat_index]]; prediction <- rep(NA_real_, nrow(scores))
    for (fold in sort(unique(folds))) {
      train <- scores[folds != fold, , drop = FALSE]; test <- scores[folds == fold, , drop = FALSE]
      model <- .fit_shape_model(train, outcome, shapes)
      prediction[folds == fold] <- .predict_shape_model(model, test)
    }
    predictions[[repeat_index]] <- prediction
    losses <- c(losses, .foldwise_mse(scores[[outcome]], prediction, folds))
  }
  list(prediction = predictions[[1L]], fold_mse = losses,
    metrics = .prediction_metrics(scores[[outcome]], predictions[[1L]]))
}

.selection_frequency <- function(base, candidates, multiplier, folds_per_repeat) {
  repeats <- as.integer(length(base$fold_mse) / folds_per_repeat)
  # Stability means that a candidate repeatedly clears the same paired
  # baseline-improvement rule; it is not a winner-take-all contest between
  # nearly equivalent nonlinear bases.
  vapply(candidates, function(candidate) {
    supported <- vapply(seq_len(repeats), function(repeat_index) {
      index <- ((repeat_index - 1L) * folds_per_repeat + 1L):(repeat_index * folds_per_repeat)
      difference <- base$fold_mse[index] - candidate$fold_mse[index]
      mean(difference) > multiplier * stats::sd(difference) / sqrt(length(difference))
    }, logical(1))
    mean(supported)
  }, numeric(1))
}

.shadow_predictions <- function(scores, outcome, predictors, folds) {
  prediction <- rep(NA_real_, nrow(scores)); if (!length(predictors)) return(prediction)
  formula <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  for (fold in sort(unique(folds))) {
    train <- scores[folds != fold, , drop = FALSE]; test <- scores[folds == fold, , drop = FALSE]
    control <- rpart::rpart.control(minsplit = max(20L, floor(nrow(train) * .10)), maxdepth = 3L, cp = .01, xval = 0L)
    prediction[folds == fold] <- stats::predict(rpart::rpart(formula, data = train, method = "anova", control = control), newdata = test)
  }
  prediction
}

.effect_rows <- function(model, scores, outcome) {
  rows <- lapply(names(model$shapes), function(predictor) {
    shape <- model$shapes[[predictor]]
    if (shape == "linear") return(data.frame(outcome = outcome, predictor = predictor, shape = shape,
      estimate = model$coefficient[model$maps[[predictor]]][1L], x = NA_real_, fitted = NA_real_, strongest_region = NA_character_))
    grid <- seq(stats::quantile(scores[[predictor]], .05), stats::quantile(scores[[predictor]], .95), length.out = 50L)
    new_data <- as.data.frame(lapply(names(model$shapes), function(name) rep(mean(scores[[name]]), length(grid))))
    names(new_data) <- names(model$shapes); new_data[[predictor]] <- grid
    curve <- .predict_shape_model(model, new_data); slope <- abs(diff(curve) / diff(grid)); active <- which(slope >= .75 * max(slope))
    region <- if (length(active)) sprintf("%.2f to %.2f", grid[min(active)], grid[max(active) + 1L]) else NA_character_
    data.frame(outcome = outcome, predictor = predictor, shape = shape, estimate = NA_real_, x = grid, fitted = curve, strongest_region = region)
  })
  do.call(rbind, rows)
}

#' Fit associational structural effects on locked construct states
#'
#' Selects at most one nonlinear declared edge per outcome. Candidate shape
#' selection is repeated cross-validation only; it does not make causal claims.
#'
#' @param fit A `cssem_fit` object.
#' @param structure A `cssem_structure` object.
#' @param folds Optional structural validation folds.
#' @param spline_df Degrees of freedom for low-complexity unconstrained spline
#'   candidates. Defaults to 3 and 4.
#' @param smooth_uncertainty Paired foldwise-loss standard-error multiplier.
#' @param shape_stability_min Minimum repeated-CV selection frequency for a
#'   nonlinear candidate.
#' @param structural_repeats Number of deterministic structural CV assignments.
#' @param seed Seed used only for repeated structural folds.
#' @param shadow_scope Shadow benchmark scope.
#' @return An object of class `cssem_association`.
#' @export
cssem_associate <- function(fit, structure, folds = NULL, spline_df = c(3L, 4L), smooth_uncertainty = 1,
                             shape_stability_min = .70, structural_repeats = 5L, seed = 1L,
                             shadow_scope = c("both", "temporal", "unrestricted")) {
  if (!inherits(fit, "cssem_fit")) stop("fit must be a cssem_fit.", call. = FALSE)
  if (!inherits(structure, "cssem_structure")) stop("structure must be a cssem_structure.", call. = FALSE)
  if (!is.numeric(smooth_uncertainty) || length(smooth_uncertainty) != 1L || !is.finite(smooth_uncertainty) || smooth_uncertainty < 0)
    stop("smooth_uncertainty must be a non-negative numeric scalar.", call. = FALSE)
  if (!is.numeric(shape_stability_min) || length(shape_stability_min) != 1L || shape_stability_min < 0 || shape_stability_min > 1)
    stop("shape_stability_min must be between zero and one.", call. = FALSE)
  spline_df <- unique(as.integer(spline_df))
  if (!length(spline_df) || any(is.na(spline_df)) || any(spline_df < 2L)) stop("spline_df must contain values of at least 2.", call. = FALSE)
  scores <- fit$locked_scores; all_names <- names(scores)
  declared <- unique(c(names(structure$effects), unlist(lapply(structure$effects, names), use.names = FALSE)))
  if (!all(declared %in% all_names)) stop("Structural declarations must use locked construct names.", call. = FALSE)
  shadow_scope <- match.arg(shadow_scope); scopes <- if (shadow_scope == "both") c("temporal", "unrestricted") else shadow_scope
  temporal_order <- if ("temporal" %in% scopes) .resolve_temporal_order(structure, all_names) else NULL
  folds <- if (is.null(folds)) fit$folds else as.integer(folds)
  if (length(folds) != nrow(scores) || length(unique(folds)) < 2L) stop("folds must assign every row to at least two validation folds.", call. = FALSE)
  fold_sets <- .structural_fold_sets(folds, structural_repeats, seed)
  candidates <- list(); effects <- list(); predictions <- list(); gaps <- list(); models <- list(); contributions <- list()
  for (outcome in names(structure$effects)) {
    policies <- structure$effects[[outcome]]; predictors <- names(policies); baseline_shapes <- stats::setNames(rep("linear", length(predictors)), predictors)
    baseline <- .cv_shape_candidate(scores, outcome, baseline_shapes, fold_sets)
    nonlinear <- list(); candidate_meta <- list()
    for (predictor in predictors) for (shape in setdiff(.shape_candidates(policies[[predictor]]$shape, spline_df), "linear")) {
      shapes <- baseline_shapes; shapes[[predictor]] <- shape; key <- paste(predictor, shape, sep = "::")
      nonlinear[[key]] <- .cv_shape_candidate(scores, outcome, shapes, fold_sets)
      candidate_meta[[key]] <- list(predictor = predictor, shape = shape, shapes = shapes)
    }
    candidate_keys <- names(nonlinear)
    frequency <- if (length(nonlinear)) setNames(.selection_frequency(baseline, nonlinear, smooth_uncertainty, length(unique(fold_sets[[1L]]))), candidate_keys) else numeric()
    improvement <- if (length(nonlinear)) setNames(vapply(nonlinear, function(x) mean(baseline$fold_mse - x$fold_mse), numeric(1)), candidate_keys) else numeric()
    improvement_se <- if (length(nonlinear)) setNames(vapply(nonlinear, function(x) stats::sd(baseline$fold_mse - x$fold_mse) / sqrt(length(x$fold_mse)), numeric(1)), candidate_keys) else numeric()
    winner <- NA_character_
    if (length(nonlinear)) {
      valid <- which(is.finite(improvement) & is.finite(improvement_se))
      if (length(valid)) {
        best <- valid[which.max(improvement[valid])]
        indistinguishable <- valid[improvement[valid] >= improvement[[best]] - smooth_uncertainty *
          sqrt(improvement_se[[best]]^2 + improvement_se[valid]^2)]
        complexity <- vapply(candidate_keys[indistinguishable], function(key) .shape_complexity(candidate_meta[[key]]$shape), integer(1))
        simplest <- indistinguishable[complexity == min(complexity)]
        winner <- candidate_keys[simplest[which.max(improvement[simplest])]]
      }
    }
    select_nonlinear <- length(nonlinear) && length(winner) == 1L && !is.na(winner) && improvement[[winner]] > smooth_uncertainty * improvement_se[[winner]] && frequency[[winner]] >= shape_stability_min
    selected_shapes <- if (select_nonlinear) candidate_meta[[winner]]$shapes else baseline_shapes
    selected <- if (select_nonlinear) nonlinear[[winner]] else baseline
    full_model <- .fit_shape_model(scores, outcome, selected_shapes)
    candidate_rows <- lapply(predictors, function(predictor) data.frame(outcome = outcome, predictor = predictor,
      candidate = "linear", shape = "linear", rmse = baseline$metrics[["rmse"]], r_squared = baseline$metrics[["r_squared"]],
      mean_mse_improvement = 0, mse_improvement_se = NA_real_, selection_frequency = if (select_nonlinear && candidate_meta[[winner]]$predictor == predictor) 0 else 1,
      selected = !select_nonlinear || candidate_meta[[winner]]$predictor != predictor, stringsAsFactors = FALSE))
    if (length(nonlinear)) for (key in names(nonlinear)) {
      meta <- candidate_meta[[key]]; candidate_rows[[length(candidate_rows) + 1L]] <- data.frame(outcome = outcome, predictor = meta$predictor,
        candidate = meta$shape, shape = meta$shape, rmse = nonlinear[[key]]$metrics[["rmse"]], r_squared = nonlinear[[key]]$metrics[["r_squared"]],
        mean_mse_improvement = improvement[[key]], mse_improvement_se = improvement_se[[key]], selection_frequency = frequency[[key]],
        selected = isTRUE(select_nonlinear) && identical(key, winner), stringsAsFactors = FALSE)
    }
    candidates[[outcome]] <- do.call(rbind, candidate_rows)
    for (predictor in predictors) {
      dropped_shapes <- selected_shapes[setdiff(names(selected_shapes), predictor)]
      dropped <- .cv_shape_candidate(scores, outcome, dropped_shapes, fold_sets)
      difference <- dropped$fold_mse - selected$fold_mse
      contributions[[paste(outcome, predictor, sep = "::")]] <- data.frame(outcome = outcome, predictor = predictor,
        edge_drop_mse_increase = mean(difference), edge_drop_mse_se = stats::sd(difference) / sqrt(length(difference)), stringsAsFactors = FALSE)
    }
    shadow_predictions <- list(); shadow_rows <- list()
    for (scope in scopes) {
      shadow_predictors <- if (scope == "temporal") temporal_order[match(temporal_order, temporal_order) < match(outcome, temporal_order)] else setdiff(all_names, outcome)
      shadow_prediction <- .shadow_predictions(scores, outcome, shadow_predictors, folds); shadow_metrics <- .prediction_metrics(scores[[outcome]], shadow_prediction)
      shadow_predictions[[scope]] <- shadow_prediction
      shadow_rows[[scope]] <- data.frame(outcome = outcome, shadow_scope = scope, eligible_predictors = paste(shadow_predictors, collapse = " + "),
        theory_r_squared = selected$metrics[["r_squared"]], shadow_r_squared = shadow_metrics[["r_squared"]],
        specification_gap = selected$metrics[["r_squared"]] - shadow_metrics[["r_squared"]], stringsAsFactors = FALSE)
    }
    effect_data <- .effect_rows(full_model, scores, outcome)
    effect_data$selection_stability <- vapply(effect_data$predictor, function(predictor) {
      row <- candidates[[outcome]][candidates[[outcome]]$predictor == predictor & candidates[[outcome]]$selected, , drop = FALSE]
      if (nrow(row) && row$shape[[1L]] != "linear") row$selection_frequency[[1L]] else 1
    }, numeric(1))
    effects[[outcome]] <- effect_data; predictions[[outcome]] <- as.data.frame(c(list(observed = scores[[outcome]], theory = selected$prediction), shadow_predictions))
    gaps[[outcome]] <- do.call(rbind, shadow_rows); models[[outcome]] <- full_model
  }
  structure(list(structure = structure, candidate_metrics = do.call(rbind, candidates), effects = do.call(rbind, effects),
    contributions = do.call(rbind, contributions), predictions = predictions, specification_gap = do.call(rbind, gaps), full_models = models,
    folds = folds, structural_repeats = structural_repeats, temporal_order = temporal_order, shadow_scope = scopes,
    status = "associational"), class = "cssem_association")
}

#' Return a declared effect's associational evidence card
#' @param association A `cssem_association` object.
#' @param outcome Declared endogenous construct name.
#' @return A list of selected effects, candidates, contributions, and shadow gaps.
#' @export
cssem_effect_card <- function(association, outcome) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  if (!outcome %in% names(association$full_models)) stop("Unknown structural outcome.", call. = FALSE)
  list(outcome = outcome,
    effects = association$effects[association$effects$outcome == outcome, , drop = FALSE],
    candidates = association$candidate_metrics[association$candidate_metrics$outcome == outcome, , drop = FALSE],
    contributions = association$contributions[association$contributions$outcome == outcome, , drop = FALSE],
    specification_gap = association$specification_gap[association$specification_gap$outcome == outcome, , drop = FALSE],
    status = "associational")
}

#' Return effect-level CS-SEM evidence profiles
#' @param association A `cssem_association` object.
#' @return A data frame with shape, predictive contribution, stability, shadow
#'   gaps, and associational status for each declared edge.
#' @export
cssem_effect_ledger <- function(association) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  selected <- association$candidate_metrics[association$candidate_metrics$selected, c("outcome", "predictor", "shape", "r_squared", "mean_mse_improvement", "mse_improvement_se", "selection_frequency"), drop = FALSE]
  names(selected)[names(selected) == "r_squared"] <- "theory_r_squared"
  ledger <- merge(selected, association$contributions, by = c("outcome", "predictor"), all.x = TRUE, sort = FALSE)
  temporal <- association$specification_gap[association$specification_gap$shadow_scope == "temporal", c("outcome", "specification_gap"), drop = FALSE]
  unrestricted <- association$specification_gap[association$specification_gap$shadow_scope == "unrestricted", c("outcome", "specification_gap"), drop = FALSE]
  names(temporal)[2L] <- "temporal_gap"; names(unrestricted)[2L] <- "unrestricted_gap"
  ledger <- merge(ledger, temporal, by = "outcome", all.x = TRUE, sort = FALSE)
  ledger <- merge(ledger, unrestricted, by = "outcome", all.x = TRUE, sort = FALSE)
  ledger$status <- "associational"; ledger
}

#' Return structural shadow-model specification gaps
#' @param association A `cssem_association` object.
#' @param scope Optional shadow scope.
#' @return A data frame of theory-minus-shadow cross-validated R-squared gaps.
#' @export
cssem_specification_gap <- function(association, scope = NULL) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  if (is.null(scope)) return(association$specification_gap)
  scope <- match.arg(scope, c("temporal", "unrestricted"))
  association$specification_gap[association$specification_gap$shadow_scope == scope, , drop = FALSE]
}

#' Print an associational CS-SEM structural fit
#' @param x A `cssem_association` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_association <- function(x, ...) {
  cat("CS-SEM associational fit:", length(x$full_models), "declared outcome(s); causal interpretation disabled\n")
  invisible(x)
}
