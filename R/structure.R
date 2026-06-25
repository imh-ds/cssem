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
.is_monotone_shape <- function(shape) shape %in% c("monotone_increasing", "monotone_decreasing")
.shape_complexity <- function(shape) {
  if (shape == "linear") return(1L)
  if (.is_monotone_shape(shape)) return(2L)
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
  knots <- unique(as.numeric(stats::quantile(x, c(.20, .50, .80), na.rm = TRUE, names = FALSE)))
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
    if (.is_monotone_shape(built$info$shape)) {
      constrained <- c(constrained, index[-1L])
      directions <- c(directions, rep(if (built$info$shape == "monotone_increasing") 1L else -1L, length(index) - 1L))
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

.selection_frequency <- function(base, candidates, candidate_meta, multiplier, folds_per_repeat) {
  repeats <- as.integer(length(base$fold_mse) / folds_per_repeat)
  # Stability means that a candidate repeatedly clears the same paired
  # baseline-improvement rule; it is not a winner-take-all contest between
  # nearly equivalent nonlinear bases.
  vapply(names(candidates), function(key) {
    candidate <- candidates[[key]]
    monotone <- .is_monotone_shape(candidate_meta[[key]]$shape)
    supported <- vapply(seq_len(repeats), function(repeat_index) {
      index <- ((repeat_index - 1L) * folds_per_repeat + 1L):(repeat_index * folds_per_repeat)
      difference <- base$fold_mse[index] - candidate$fold_mse[index]
      threshold <- if (monotone) 0 else multiplier * stats::sd(difference) / sqrt(length(difference))
      mean(difference) > threshold
    }, logical(1))
    mean(supported)
  }, numeric(1))
}

.pick_shape_winner <- function(candidate_keys, candidate_meta, improvement, improvement_se,
                               frequency, smooth_uncertainty, shape_stability_min) {
  valid <- which(is.finite(improvement) & is.finite(improvement_se))
  if (!length(valid)) return(NA_character_)
  best <- valid[which.max(improvement[valid])]
  best_improvement <- improvement[[candidate_keys[[best]]]]
  indistinguishable <- valid[improvement[valid] >= improvement[[best]] - smooth_uncertainty *
    sqrt(improvement_se[[best]]^2 + improvement_se[valid]^2)]
  stable_monotone <- candidate_keys[indistinguishable][
    vapply(candidate_keys[indistinguishable], function(key) {
      meta <- candidate_meta[[key]]
      .is_monotone_shape(meta$shape) && improvement[[key]] > 0 && frequency[[key]] >= shape_stability_min
    }, logical(1))
  ]
  if (length(stable_monotone) && best_improvement <= .05) {
    monotone_improvement <- improvement[stable_monotone]
    monotone_complexity <- vapply(stable_monotone, function(key) .shape_complexity(candidate_meta[[key]]$shape), integer(1))
    stable_monotone <- stable_monotone[monotone_complexity == min(monotone_complexity)]
    return(stable_monotone[[which.max(monotone_improvement[stable_monotone])]])
  }
  strongest <- candidate_keys[indistinguishable][improvement[candidate_keys[indistinguishable]] == max(improvement[candidate_keys[indistinguishable]])]
  complexity <- vapply(strongest, function(key) .shape_complexity(candidate_meta[[key]]$shape), integer(1))
  strongest[[which.min(complexity)]]
}

# Classical (Fuller) errors-in-variables correction on standardized construct
# states. Measurement error in the predictors attenuates structural slopes; the
# correction subtracts the predictor error covariance D_e = diag((1 - rho) * var)
# from the predictor covariance before solving the normal equations. Error in the
# outcome inflates residual variance but does not bias the slope, so only
# predictor reliabilities enter. Returns naive (uncorrected) and corrected slopes.
.eiv_coefficients <- function(scores, outcome, predictors, reliability, weights = NULL, posterior_var = NULL) {
  X <- as.matrix(scores[, predictors, drop = FALSE]); y <- scores[[outcome]]
  w <- if (is.null(weights)) rep(1, nrow(X)) else weights
  pv <- if (is.null(posterior_var)) NULL else as.matrix(posterior_var[, predictors, drop = FALSE])
  keep <- stats::complete.cases(X, y) & is.finite(w)
  X <- X[keep, , drop = FALSE]; y <- y[keep]; w <- w[keep]
  if (!is.null(pv)) pv <- pv[keep, , drop = FALSE]
  na <- stats::setNames(rep(NA_real_, length(predictors)), predictors)
  if (length(y) < length(predictors) + 2L || sum(w) <= 0) return(list(naive = na, corrected = na))
  # Inverse-variance weighted moments: respondents with wide posteriors carry
  # less weight, so heteroskedastic measurement information no longer biases the
  # structural estimate toward the noisiest respondents.
  sw <- sum(w)
  xbar <- colSums(X * w) / sw; ybar <- sum(y * w) / sw
  Xc <- sweep(X, 2L, xbar, "-"); yc <- y - ybar
  Szz <- crossprod(Xc * w, Xc) / sw
  Szy <- crossprod(Xc * w, yc) / sw
  naive <- tryCatch(drop(solve(Szz, Szy)), error = function(e) na)
  # Weighted marginal reliability when per-respondent posterior variances are
  # available, otherwise the construct-level reliability estimate.
  rel <- reliability[predictors]
  if (!is.null(pv)) {
    signal <- diag(Szz); error <- colSums(pv * w) / sw
    rel_w <- signal / (signal + error)
    rel <- ifelse(is.finite(rel_w), rel_w, rel)
  }
  if (anyNA(rel) || any(!is.finite(rel)) || any(rel <= 0)) return(list(naive = stats::setNames(naive, predictors), corrected = na))
  De <- diag((1 - rel) * diag(Szz), nrow = length(predictors))
  corrected_cov <- Szz - De
  # Guard against an ill-conditioned correction (low reliability can render the
  # corrected predictor covariance non-positive-definite); fall back to naive.
  pd <- tryCatch(all(eigen(corrected_cov, symmetric = TRUE, only.values = TRUE)$values > 1e-6), error = function(e) FALSE)
  corrected <- if (pd) tryCatch(drop(solve(corrected_cov, Szy)), error = function(e) na) else na
  list(naive = stats::setNames(naive, predictors), corrected = stats::setNames(corrected, predictors))
}

# Percentile bootstrap of the corrected slopes, resampling respondents. This
# reflects sampling variability of the errors-in-variables estimator; the
# reliability inputs are held fixed at their measurement-model estimates.
.eiv_bootstrap <- function(scores, outcome, predictors, reliability, replicates, seed,
                           weights = NULL, posterior_var = NULL) {
  if (replicates < 1L) return(NULL)
  set.seed(seed + 4242L); n <- nrow(scores)
  estimates <- matrix(NA_real_, replicates, length(predictors), dimnames = list(NULL, predictors))
  for (b in seq_len(replicates)) {
    idx <- sample.int(n, n, replace = TRUE)
    estimates[b, ] <- .eiv_coefficients(scores[idx, , drop = FALSE], outcome, predictors, reliability,
      weights = if (is.null(weights)) NULL else weights[idx],
      posterior_var = if (is.null(posterior_var)) NULL else posterior_var[idx, , drop = FALSE])$corrected
  }
  estimates
}

# Corrected structural effects for one outcome. Edges whose selected shape is
# linear or monotone are disattenuated; smooth edges are reported but not yet
# corrected (closed-form errors-in-variables for splines is out of scope).
.corrected_effects <- function(scores, outcome, selected_shapes, reliability, replicates, seed,
                               weights = NULL, posterior_var = NULL) {
  predictors <- names(selected_shapes)
  applicable <- vapply(predictors, function(p) selected_shapes[[p]] %in% c("linear", "monotone_increasing", "monotone_decreasing"), logical(1))
  fit <- .eiv_coefficients(scores, outcome, predictors, reliability, weights, posterior_var)
  boot <- if (any(applicable)) .eiv_bootstrap(scores, outcome, predictors, reliability, replicates, seed, weights, posterior_var) else NULL
  do.call(rbind, lapply(predictors, function(p) {
    ci <- if (!is.null(boot) && applicable[[p]] && any(is.finite(boot[, p]))) stats::quantile(boot[, p], c(.025, .975), na.rm = TRUE, names = FALSE) else c(NA_real_, NA_real_)
    data.frame(outcome = outcome, predictor = p,
      naive_estimate = if (applicable[[p]]) unname(fit$naive[[p]]) else NA_real_,
      corrected_estimate = if (applicable[[p]]) unname(fit$corrected[[p]]) else NA_real_,
      predictor_reliability = unname(reliability[[p]]),
      corrected_ci_low = ci[[1L]], corrected_ci_high = ci[[2L]],
      eiv_applicable = applicable[[p]], stringsAsFactors = FALSE)
  }))
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
#' @param reliability Optional named vector of per-construct reliabilities used
#'   by the errors-in-variables correction. Defaults to the posterior
#'   reliability carried on `fit`; when unavailable the corrected estimate is
#'   omitted rather than reported uncorrected.
#' @param eiv_bootstrap Number of percentile-bootstrap replicates for the
#'   corrected-estimate interval. Zero disables the interval.
#' @param respondent_weighting Experimental. `"information"` applies
#'   inverse-variance respondent weighting from the posterior SD. It is
#'   `"none"` by default: because posterior width is score-dependent, weighting
#'   induces range restriction and does not improve point-estimate bias in
#'   validation, so it is not recommended for confirmatory estimates.
#' @param preset Runtime preset. Use `"exploratory"` for lighter-weight
#'   structural selection defaults while iterating locally.
#' @return An object of class `cssem_association`.
#' @export
cssem_associate <- function(fit, structure, folds = NULL, spline_df = c(3L, 4L), smooth_uncertainty = 1,
                             shape_stability_min = .70, structural_repeats = 5L, seed = 1L,
                             shadow_scope = c("both", "temporal", "unrestricted"),
                             reliability = NULL, eiv_bootstrap = 0L,
                             respondent_weighting = c("none", "information"),
                             preset = c("default", "exploratory")) {
  if (!inherits(fit, "cssem_fit")) stop("fit must be a cssem_fit.", call. = FALSE)
  if (!inherits(structure, "cssem_structure")) stop("structure must be a cssem_structure.", call. = FALSE)
  preset <- match.arg(preset)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)
  respondent_weighting <- match.arg(respondent_weighting)
  if (preset == "exploratory") {
    if (missing(spline_df)) spline_df <- 3L
    if (missing(structural_repeats)) structural_repeats <- 2L
    if (missing(shadow_scope)) shadow_scope <- "temporal"
  }
  if (!is.numeric(smooth_uncertainty) || length(smooth_uncertainty) != 1L || !is.finite(smooth_uncertainty) || smooth_uncertainty < 0)
    stop("smooth_uncertainty must be a non-negative numeric scalar.", call. = FALSE)
  if (!is.numeric(shape_stability_min) || length(shape_stability_min) != 1L || shape_stability_min < 0 || shape_stability_min > 1)
    stop("shape_stability_min must be between zero and one.", call. = FALSE)
  spline_df <- unique(as.integer(spline_df))
  if (!length(spline_df) || any(is.na(spline_df)) || any(spline_df < 2L)) stop("spline_df must contain values of at least 2.", call. = FALSE)
  scores <- fit$locked_scores; all_names <- names(scores)
  declared <- unique(c(names(structure$effects), unlist(lapply(structure$effects, names), use.names = FALSE)))
  if (!all(declared %in% all_names)) stop("Structural declarations must use locked construct names.", call. = FALSE)
  # Per-construct reliability used by the errors-in-variables correction. Defaults
  # to the posterior reliability carried on the fit; NA where unavailable so the
  # corrected estimate is simply omitted rather than silently wrong.
  reliability_source <- if (is.null(reliability)) fit$reliability else reliability
  reliability_vec <- stats::setNames(rep(NA_real_, length(all_names)), all_names)
  if (!is.null(reliability_source)) {
    shared <- intersect(names(reliability_source), all_names)
    reliability_vec[shared] <- as.numeric(reliability_source[shared])
  }
  # Optional inverse-variance respondent weighting, drawn from the per-respondent
  # posterior SD carried on the fit. Unavailable for score-only engines, which
  # then fall back to unweighted estimation.
  respondent_weights <- NULL; posterior_var <- NULL
  if (respondent_weighting == "information" && !is.null(fit$score_posterior_sd)) {
    sd <- as.data.frame(fit$score_posterior_sd)
    modeled <- intersect(all_names, names(sd))
    if (length(modeled) && nrow(sd) == nrow(scores)) {
      respondent_weights <- .information_weights(sd[, modeled, drop = FALSE])
      posterior_var <- sd^2
    }
  }
  shadow_scope <- match.arg(shadow_scope); scopes <- if (shadow_scope == "both") c("temporal", "unrestricted") else shadow_scope
  temporal_order <- if ("temporal" %in% scopes) .resolve_temporal_order(structure, all_names) else NULL
  folds <- if (is.null(folds)) fit$folds else as.integer(folds)
  if (length(folds) != nrow(scores) || length(unique(folds)) < 2L) stop("folds must assign every row to at least two validation folds.", call. = FALSE)
  fold_sets <- .structural_fold_sets(folds, structural_repeats, seed)
  candidates <- list(); effects <- list(); predictions <- list(); gaps <- list(); models <- list(); contributions <- list(); corrected <- list()
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
    frequency <- if (length(nonlinear)) setNames(.selection_frequency(baseline, nonlinear, candidate_meta, smooth_uncertainty, length(unique(fold_sets[[1L]]))), candidate_keys) else numeric()
    improvement <- if (length(nonlinear)) setNames(vapply(nonlinear, function(x) mean(baseline$fold_mse - x$fold_mse), numeric(1)), candidate_keys) else numeric()
    improvement_se <- if (length(nonlinear)) setNames(vapply(nonlinear, function(x) stats::sd(baseline$fold_mse - x$fold_mse) / sqrt(length(x$fold_mse)), numeric(1)), candidate_keys) else numeric()
    winner <- NA_character_
    best_key <- NA_character_
    if (length(nonlinear)) {
      valid <- which(is.finite(improvement) & is.finite(improvement_se))
      if (length(valid)) best_key <- candidate_keys[valid[which.max(improvement[valid])]]
      winner <- .pick_shape_winner(candidate_keys, candidate_meta, improvement, improvement_se,
        frequency, smooth_uncertainty, shape_stability_min)
    }
    monotone_winner <- length(winner) == 1L && !is.na(winner) && .is_monotone_shape(candidate_meta[[winner]]$shape)
    best_significant <- length(best_key) == 1L && !is.na(best_key) && improvement[[best_key]] > smooth_uncertainty * improvement_se[[best_key]]
    select_nonlinear <- length(nonlinear) && length(winner) == 1L && !is.na(winner) && frequency[[winner]] >= shape_stability_min &&
      (improvement[[winner]] > smooth_uncertainty * improvement_se[[winner]] ||
        (monotone_winner && improvement[[winner]] > 0 && best_significant))
    selected_shapes <- if (select_nonlinear) candidate_meta[[winner]]$shapes else baseline_shapes
    selected <- if (select_nonlinear) nonlinear[[winner]] else baseline
    full_model <- .fit_shape_model(scores, outcome, selected_shapes)
    corrected[[outcome]] <- .corrected_effects(scores, outcome, selected_shapes, reliability_vec, eiv_bootstrap, seed,
      weights = respondent_weights, posterior_var = posterior_var)
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
    corrected_effects = do.call(rbind, corrected), reliability = reliability_vec, eiv_bootstrap = eiv_bootstrap,
    respondent_weighting = respondent_weighting,
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
  if (!is.null(association$corrected_effects)) {
    ledger <- merge(ledger, association$corrected_effects, by = c("outcome", "predictor"), all.x = TRUE, sort = FALSE)
  }
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
