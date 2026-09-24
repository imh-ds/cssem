# Shared validated construction for cssem_effect(), used internally so package
# code never triggers cssem_effect()'s own deprecation warning.
.build_effect <- function(shape) {
  shape <- match.arg(shape, c("auto", "linear", "auto_monotone",
                              "monotone_increasing", "monotone_decreasing", "smooth"))
  structure(list(shape = shape), class = "cssem_effect")
}

#' Declare an edge-level CS-SEM associational effect policy
#'
#' **Deprecated.** Use the shape-wrapper markers (`linear()`,
#' `auto_monotone()`, `monotone_increasing()`, `monotone_decreasing()`,
#' `smooth()`) inside a [specify_structure()] formula instead.
#' `cssem_structure()` still accepts `cssem_effect()` declarations unchanged.
#'
#' @param shape A declared shape policy: `"auto"`, `"linear"`,
#'   `"auto_monotone"`, `"monotone_increasing"`,
#'   `"monotone_decreasing"`, or `"smooth"`.
#' @return An object of class `cssem_effect`.
#' @export
cssem_effect <- function(shape = c("auto", "linear", "auto_monotone",
                                  "monotone_increasing", "monotone_decreasing", "smooth")) {
  .Deprecated("specify_structure", package = "cssem",
    msg = "cssem_effect() is deprecated; declare edge shapes with linear()/auto_monotone()/monotone_increasing()/monotone_decreasing()/smooth() inside a specify_structure() formula instead.")
  shape <- match.arg(shape)
  .build_effect(shape)
}

.effect_policy <- function(value) {
  if (inherits(value, "cssem_effect")) return(value)
  if (is.character(value) && length(value) == 1L) return(.build_effect(value))
  stop("Each named edge must be a cssem_effect() declaration or a supported shape string.", call. = FALSE)
}

.parse_effects <- function(value) {
  if (is.character(value)) {
    if (!length(value) || any(!nzchar(value)) || anyDuplicated(value))
      stop("Each outcome needs one or more unique predictor names.", call. = FALSE)
    result <- lapply(value, function(x) .build_effect("auto")); names(result) <- value
    return(result)
  }
  if (!is.list(value) || is.null(names(value)) || any(names(value) == "") || anyDuplicated(names(value)))
    stop("Edge declarations must be a non-empty character vector or a named list.", call. = FALSE)
  lapply(value, .effect_policy)
}

#' Declare a structural response family
#'
#' Structural families are explicit because measurement scale declarations do
#' not determine the likelihood used for an endogenous locked score.
#'
#' @param family One of `"gaussian"`, `"binomial"` (binary), or
#'   `"ordinal"`.
#' @param link The response link. Gaussian uses `"identity"`; binomial and
#'   ordinal use the cumulative/logistic `"logit"` link.
#' @param levels Optional numeric outcome levels. Binary levels must be `0, 1`;
#'   ordinal levels must be unique integer values in increasing order.
#' @return A validated `cssem_structural_outcome` declaration.
#' @export
structural_outcome <- function(family = c("gaussian", "binomial", "ordinal"),
                               link = NULL, levels = NULL) {
  if (!is.character(family) || length(family) != 1L || is.na(family))
    stop("family must be one of 'gaussian', 'binomial', or 'ordinal'.", call. = FALSE)
  allowed <- c("gaussian", "binomial", "ordinal")
  if (!family %in% allowed)
    stop("family must be one of 'gaussian', 'binomial', or 'ordinal'.", call. = FALSE)
  family <- match.arg(family, allowed)
  expected_link <- if (family == "gaussian") "identity" else "logit"
  if (is.null(link)) link <- expected_link
  if (!is.character(link) || length(link) != 1L || !identical(link, expected_link))
    stop(sprintf("family '%s' only supports link '%s'.", family, expected_link), call. = FALSE)
  if (family == "gaussian") {
    if (!is.null(levels)) stop("Gaussian structural outcomes cannot declare levels.", call. = FALSE)
  } else if (family == "binomial") {
    if (is.null(levels)) levels <- c(0, 1)
    if (!is.numeric(levels) || length(levels) != 2L || !all(is.finite(levels)) ||
        !identical(as.numeric(levels), c(0, 1)))
      stop("Binomial structural levels must be exactly c(0, 1).", call. = FALSE)
  } else {
    if (!is.null(levels)) {
      if (!is.numeric(levels) || length(levels) < 2L || any(!is.finite(levels)) ||
          any(levels != as.integer(levels)) || anyDuplicated(levels) ||
          !identical(as.numeric(levels), sort(as.numeric(levels))))
        stop("Ordinal structural levels must be at least two increasing integer values.", call. = FALSE)
      levels <- as.numeric(levels)
    }
  }
  structure(list(family = family, link = link, levels = levels),
    class = "cssem_structural_outcome")
}

#' @rdname structural_outcome
#' @export
binary_outcome <- function(levels = c(0, 1))
  structural_outcome("binomial", levels = levels)

#' @rdname structural_outcome
#' @export
ordinal_outcome <- function(levels = NULL)
  structural_outcome("ordinal", levels = levels)

.normalize_structural_outcome <- function(value) {
  if (inherits(value, "cssem_structural_outcome")) return(value)
  if (is.character(value) && length(value) == 1L)
    return(structural_outcome(value))
  if (is.list(value)) {
    family <- if (is.null(value$family)) "gaussian" else value$family
    link <- if (is.null(value$link)) NULL else value$link
    levels <- if (is.null(value$levels)) NULL else value$levels
    return(structural_outcome(family, link = link, levels = levels))
  }
  stop("Each structural family must be a structural_outcome() declaration or a supported family name.", call. = FALSE)
}

.normalize_response_families <- function(families, outcomes) {
  result <- lapply(outcomes, function(outcome) structural_outcome("gaussian"))
  names(result) <- outcomes
  if (is.null(families)) return(result)
  if (!is.list(families) || is.null(names(families)) || any(!nzchar(names(families))) ||
      anyDuplicated(names(families)))
    stop("families must be a named list keyed by structural outcome names.", call. = FALSE)
  unknown <- setdiff(names(families), outcomes)
  if (length(unknown))
    stop(sprintf("families names must match declared outcomes (unknown: %s).", paste(unknown, collapse = ", ")), call. = FALSE)
  for (outcome in names(families)) result[[outcome]] <- .normalize_structural_outcome(families[[outcome]])
  result
}

# Shared validated construction for cssem_structure() and specify_structure().
# Both front doors resolve to this identical internal representation, so every
# downstream consumer of a `cssem_structure` object (associate() and
# everything built on it) is unaffected by which front door built it.
.build_structure <- function(effects, order, families = NULL) {
  if (!is.list(effects) || is.null(names(effects)) || any(names(effects) == ""))
    stop("effects must be a named list of outcome-predictor declarations.", call. = FALSE)
  parsed <- lapply(effects, .parse_effects)
  for (outcome in names(parsed)) if (outcome %in% names(parsed[[outcome]]))
    stop("An outcome cannot be its own predictor.", call. = FALSE)
  if (!is.null(order) && (anyDuplicated(order) || any(!nzchar(order))))
    stop("order must contain unique, non-empty construct names.", call. = FALSE)
  response_families <- .normalize_response_families(families, names(parsed))
  structure(list(effects = parsed, order = if (is.null(order)) NULL else as.character(order),
    response_families = response_families, status = "associational"), class = "cssem_structure")
}

#' Declare an associational CS-SEM structural model
#'
#' **Deprecated.** Use [specify_structure()], which declares the same
#' specification with formulas instead of nested lists.
#'
#' Character-vector declarations remain supported. Named edge declarations use
#' [cssem_effect()] to state each predictor's shape policy.
#'
#' @param effects A named list of outcomes. Each value is either a non-empty
#'   character vector of predictors or a named list of [cssem_effect()] objects.
#' @param order Optional character vector giving the user-declared temporal
#'   order of all constructs.
#' @param families Optional named list of [structural_outcome()] declarations,
#'   keyed by endogenous outcome. Omitted outcomes use Gaussian identity.
#' @return An object of class `cssem_structure`.
#' @export
cssem_structure <- function(effects, order = NULL, families = NULL) {
  .Deprecated("specify_structure", package = "cssem",
    msg = "cssem_structure() is deprecated; use specify_structure() with formulas instead.")
  .build_structure(effects, order, families)
}

# Shape-wrapper names recognized inside specify_structure() formulas. These are
# never called as functions - stats::terms() only uses them as syntactic
# markers on the unevaluated formula, the same mechanism mgcv::s() relies on -
# so none of them need to be defined or exported, which keeps them from ever
# shadowing base/stats functions such as stats::smooth().
.shape_specials <- c("linear", "auto_monotone", "monotone_increasing", "monotone_decreasing", "smooth")

# Parse one `Outcome ~ predictors` formula into the named list of cssem_effect()
# declarations that .parse_effects() already accepts. A predictor wrapped in a
# recognized shape special (e.g. `smooth(Consc)`) declares that shape; every
# other term defaults to "auto". Interaction terms use the formula's native
# colon syntax and pass through term.labels unchanged, matching the existing
# "A:B" predictor-name convention.
.formula_effects <- function(f) {
  if (!inherits(f, "formula") || length(f) != 3L)
    stop("Every structural declaration must be a two-sided formula, e.g. Outcome ~ predictor1 + predictor2.", call. = FALSE)
  labels <- attr(stats::terms(f, specials = .shape_specials), "term.labels")
  if (!length(labels)) stop("Every structural formula needs at least one predictor.", call. = FALSE)
  effects <- list()
  for (label in labels) {
    expr <- str2lang(label)
    if (is.call(expr) && length(expr) == 2L && as.character(expr[[1L]]) %in% .shape_specials) {
      shape <- as.character(expr[[1L]]); predictor <- deparse(expr[[2L]])
      if (grepl(":", predictor, fixed = TRUE))
        stop("Shape wrappers cannot be applied to interaction terms.", call. = FALSE)
      effects[[predictor]] <- .build_effect(shape)
    } else {
      effects[[label]] <- .build_effect("auto")
    }
  }
  effects
}

#' Declare an associational CS-SEM structural model with formulas
#'
#' Friendly front door for [cssem_structure()]. Each outcome is declared with
#' a formula, `Outcome ~ predictor1 + predictor2`. Interaction predictors use
#' standard formula colon syntax (`A:B`). Wrap a predictor in `linear()`,
#' `auto_monotone()`, `monotone_increasing()`, `monotone_decreasing()`, or
#' `smooth()` to declare a non-default shape policy for that edge (these are
#' formula markers only, not callable functions); undeclared predictors
#' default to `"auto"`, matching [cssem_effect()].
#'
#' @param ... One formula per declared outcome.
#' @param order Optional character vector giving the user-declared temporal
#'   order of all constructs.
#' @param families Optional named list of [structural_outcome()] declarations,
#'   keyed by endogenous outcome. Omitted outcomes use Gaussian identity.
#' @return An object of class `cssem_structure`.
#' @examples
#' structure <- specify_structure(
#'   Satisfaction ~ Trust,
#'   Loyalty ~ Trust + monotone_increasing(Satisfaction),
#'   order = c("Trust", "Satisfaction", "Loyalty")
#' )
#' @family model specification functions
#' @export
specify_structure <- function(..., order = NULL, families = NULL) {
  formulas <- list(...)
  if (!length(formulas) || !all(vapply(formulas, inherits, logical(1), what = "formula")))
    stop("Every declaration must be a formula, e.g. specify_structure(Outcome ~ predictor1 + predictor2).", call. = FALSE)
  outcomes <- vapply(formulas, function(f) deparse(f[[2L]]), character(1))
  if (anyDuplicated(outcomes)) stop("Each outcome may appear in only one formula.", call. = FALSE)
  effects <- lapply(formulas, .formula_effects)
  names(effects) <- outcomes
  .build_structure(effects, order, families)
}

.effect_predictors <- function(effect_specs) names(effect_specs)

# Parent constructs of an outcome, expanding interaction predictors to their
# constituent constructs.
.outcome_parents <- function(effects, node) {
  if (!node %in% names(effects)) return(character())
  unique(unlist(lapply(names(effects[[node]]), .predictor_constructs), use.names = FALSE))
}

.derived_temporal_order <- function(effects, all_names) {
  graph_names <- unique(c(names(effects), unlist(lapply(names(effects), function(o) .outcome_parents(effects, o)), use.names = FALSE)))
  if (!setequal(graph_names, all_names)) return(NULL)
  remaining <- all_names; resolved <- character()
  while (length(remaining)) {
    available <- remaining[vapply(remaining, function(node) all(.outcome_parents(effects, node) %in% resolved), logical(1))]
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
    if (any(match(.outcome_parents(structure$effects, outcome), order) >= match(outcome, order)))
      stop("Every declared predictor must occur before its outcome in order.", call. = FALSE)
  }
  order
}

.validate_fixed_shapes <- function(fixed_shapes, structure) {
  if (!is.list(fixed_shapes) || is.null(names(fixed_shapes)) ||
      any(!nzchar(names(fixed_shapes))) || anyDuplicated(names(fixed_shapes)))
    stop("fixed_shapes must be a named list of selected structural shapes.", call. = FALSE)
  missing <- setdiff(names(structure$effects), names(fixed_shapes))
  extra <- setdiff(names(fixed_shapes), names(structure$effects))
  if (length(missing) || length(extra))
    stop(sprintf("fixed_shapes must name every structural outcome exactly once (missing: %s; extra: %s).",
      if (length(missing)) paste(missing, collapse = ", ") else "none",
      if (length(extra)) paste(extra, collapse = ", ") else "none"), call. = FALSE)
  out <- lapply(names(structure$effects), function(outcome) {
    shapes <- fixed_shapes[[outcome]]
    predictors <- names(structure$effects[[outcome]])
    if (!is.character(shapes) || is.null(names(shapes)) || !setequal(names(shapes), predictors))
      stop(sprintf("fixed_shapes[[\"%s\"]] must be a named shape vector for predictors: %s.",
        outcome, paste(predictors, collapse = ", ")), call. = FALSE)
    shapes <- as.character(shapes[predictors])
    valid <- shapes == "linear" | shapes == "product" |
      grepl("^smooth_df[0-9]+$", shapes) |
      shapes %in% c("monotone_increasing", "monotone_decreasing")
    if (any(!valid)) stop(sprintf("Unsupported fixed structural shape(s): %s.",
      paste(unique(shapes[!valid]), collapse = ", ")), call. = FALSE)
    interaction <- vapply(predictors, .is_interaction, logical(1))
    if (any(interaction & shapes != "product"))
      stop("Interaction predictors require fixed shape = \"product\".", call. = FALSE)
    stats::setNames(shapes, predictors)
  })
  names(out) <- names(structure$effects); out
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

# Interaction predictors are declared with R-style colon syntax ("A:B") and enter
# the effect surface as a linear-by-linear product term. These helpers parse them
# and recover the constituent constructs.
.is_interaction <- function(predictor) grepl(":", predictor, fixed = TRUE)
.interaction_terms <- function(predictor) strsplit(predictor, ":", fixed = TRUE)[[1L]]
.predictor_constructs <- function(predictor) if (.is_interaction(predictor)) .interaction_terms(predictor) else predictor

# Design matrix with one column per predictor, forming the product for
# interaction predictors from their constituent construct columns.
.predictor_matrix <- function(scores, predictors) {
  columns <- lapply(predictors, function(predictor) {
    if (.is_interaction(predictor)) {
      terms <- .interaction_terms(predictor); scores[[terms[[1L]]]] * scores[[terms[[2L]]]]
    } else scores[[predictor]]
  })
  matrix(unlist(columns, use.names = FALSE), ncol = length(predictors), dimnames = list(NULL, predictors))
}

# Monotone basis: at each knot, a convex hinge (x - knot)_+ and a concave hinge
# min(x - knot, 0). Both are nondecreasing in x, so any nonnegative combination is
# nondecreasing (and any nonpositive one nonincreasing), and together they span
# convex, concave, S-shaped, and linear increasing curves. A convex-only basis
# (the linear term plus (x - knot)_+ hinges) can only steepen, so it could not
# represent diminishing returns or saturation. The pair at one knot sums to
# x - knot, so no separate linear column is needed.
.hinge_basis <- function(x, knots) {
  if (!length(knots)) return(matrix(x, ncol = 1L))
  convex <- vapply(knots, function(knot) pmax(0, x - knot), numeric(length(x)))
  concave <- vapply(knots, function(knot) pmin(0, x - knot), numeric(length(x)))
  matrix(cbind(convex, concave), nrow = length(x))
}

.train_basis <- function(x, shape) {
  if (shape == "linear") return(list(values = matrix(x, ncol = 1L), info = list(shape = shape)))
  if (grepl("^smooth", shape)) {
    df <- .shape_df(shape)
    basis <- tryCatch(splines::ns(x, df = df), error = function(err) NULL)
    if (is.null(basis)) {
      # Heavily tied scores (e.g. composites of few ordinal items) can collapse
      # the df-based quantile knots onto a boundary knot. Retry with the
      # deduplicated interior quantiles; when none fall strictly inside the
      # range, degrade to the linear column so the smooth candidate still
      # competes (as linear) instead of aborting the fit.
      boundary <- range(x, na.rm = TRUE)
      knots <- unique(as.numeric(stats::quantile(x, seq_len(df - 1L) / df, na.rm = TRUE, names = FALSE)))
      knots <- knots[knots > boundary[[1L]] & knots < boundary[[2L]]]
      if (!length(knots))
        return(list(values = matrix(x, ncol = 1L), info = list(shape = shape, knots = NULL, boundary = NULL)))
      basis <- splines::ns(x, knots = knots, Boundary.knots = boundary)
    }
    return(list(values = basis, info = list(shape = shape, knots = attr(basis, "knots"), boundary = attr(basis, "Boundary.knots"))))
  }
  knots <- unique(as.numeric(stats::quantile(x, c(.20, .50, .80), na.rm = TRUE, names = FALSE)))
  knots <- knots[knots > min(x, na.rm = TRUE) & knots < max(x, na.rm = TRUE)]
  list(values = .hinge_basis(x, knots), info = list(shape = shape, knots = knots))
}

.predict_basis <- function(x, info) {
  if (info$shape == "linear") return(matrix(x, ncol = 1L))
  if (grepl("^smooth", info$shape)) {
    # A NULL boundary marks the degraded-to-linear training basis above.
    if (is.null(info$boundary)) return(matrix(x, ncol = 1L))
    return(splines::ns(x, knots = info$knots, Boundary.knots = info$boundary))
  }
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

.structural_family <- function(family = NULL) {
  if (is.null(family)) return(structural_outcome("gaussian"))
  .normalize_structural_outcome(family)
}

.validate_structural_response <- function(data, outcome, family) {
  family <- .structural_family(family)
  values <- data[[outcome]]
  if (family$family == "gaussian") return(list(values = values, levels = NULL))
  if (any(!is.finite(values)))
    stop(sprintf("Categorical structural outcome '%s' contains non-finite locked scores.", outcome), call. = FALSE)
  if (family$family == "binomial") {
    if (any(!values %in% family$levels))
      stop(sprintf("Binomial structural outcome '%s' must contain only 0/1 locked scores.", outcome), call. = FALSE)
    return(list(values = as.numeric(values), levels = family$levels))
  }
  if (any(values != as.integer(values)))
    stop(sprintf("Ordinal structural outcome '%s' must contain integer locked-score categories.", outcome), call. = FALSE)
  observed <- sort(unique(as.numeric(values)))
  levels <- if (is.null(family$levels)) observed else family$levels
  if (length(levels) < 2L || any(!values %in% levels))
    stop(sprintf("Ordinal structural outcome '%s' must use at least two declared integer levels.", outcome), call. = FALSE)
  list(values = as.numeric(values), levels = as.numeric(levels))
}

.categorical_metrics <- function(observed, probability, family, levels) {
  keep <- is.finite(observed) & apply(probability, 1L, function(x) all(is.finite(x)))
  if (!any(keep)) return(c(rmse = NA_real_, r_squared = NA_real_, log_loss = NA_real_, brier = NA_real_, accuracy = NA_real_))
  y <- observed[keep]; p <- probability[keep, , drop = FALSE]
  index <- match(y, levels)
  if (anyNA(index)) return(c(rmse = NA_real_, r_squared = NA_real_, log_loss = NA_real_, brier = NA_real_, accuracy = NA_real_))
  expected <- drop(p %*% as.numeric(levels))
  truth <- matrix(0, nrow(p), ncol(p)); truth[cbind(seq_len(nrow(p)), index)] <- 1
  selected <- p[cbind(seq_len(nrow(p)), index)]
  selected <- pmax(pmin(selected, 1 - 1e-12), 1e-12)
  sse <- sum((y - expected)^2); sst <- sum((y - mean(y))^2)
  c(rmse = sqrt(mean((y - expected)^2)), r_squared = if (sst > 0) 1 - sse / sst else NA_real_,
    log_loss = -mean(log(selected)), brier = mean(rowSums((p - truth)^2)),
    accuracy = mean(max.col(p, ties.method = "first") == index))
}

.metric_or_na <- function(metrics, name) {
  if (name %in% names(metrics)) unname(metrics[[name]]) else NA_real_
}

.binomial_glm_vcov <- function(fit) {
  p <- length(fit$coefficients)
  if (!isTRUE(fit$converged) || is.null(fit$qr) || fit$rank < p || any(!is.finite(fit$coefficients)))
    return(matrix(NA_real_, p, p, dimnames = list(names(fit$coefficients), names(fit$coefficients))))
  covariance <- tryCatch(chol2inv(qr.R(fit$qr)), error = function(e) NULL)
  if (is.null(covariance) || any(!is.finite(covariance)))
    return(matrix(NA_real_, p, p, dimnames = list(names(fit$coefficients), names(fit$coefficients))))
  pivot <- fit$qr$pivot
  restored <- matrix(NA_real_, p, p)
  restored[pivot, pivot] <- covariance
  dimnames(restored) <- list(names(fit$coefficients), names(fit$coefficients))
  restored
}

.categorical_coefficient_se <- function(family, fitted, predictors) {
  family <- .structural_family(family)
  covariance <- if (family$family == "binomial") .binomial_glm_vcov(fitted) else
    tryCatch(stats::vcov(fitted), error = function(e) NULL)
  result <- stats::setNames(rep(NA_real_, length(predictors)), predictors)
  if (is.null(covariance) || is.null(rownames(covariance))) return(result)
  shared <- intersect(predictors, rownames(covariance))
  variances <- diag(covariance)[shared]
  valid <- is.finite(variances) & variances >= 0
  if (length(shared)) result[shared[valid]] <- sqrt(variances[valid])
  result[!is.finite(result)] <- NA_real_
  result
}

.fit_shape_model <- function(data, outcome, shapes, family = NULL) {
  family <- .structural_family(family)
  if (family$family != "gaussian") {
    invalid <- names(shapes)[vapply(shapes, function(shape) !identical(shape, "linear"), logical(1))]
    if (length(invalid) || any(vapply(names(shapes), .is_interaction, logical(1))))
      stop("Categorical structural outcomes only support linear main-effect shapes.", call. = FALSE)
    response <- .validate_structural_response(data, outcome, family)
    predictors <- names(shapes)
    design <- cbind(`(Intercept)` = 1, as.matrix(data[, predictors, drop = FALSE]))
    if (family$family == "binomial") {
      fitted <- stats::glm.fit(design, response$values, family = stats::binomial(link = family$link))
      coefficient <- fitted$coefficients; names(coefficient) <- colnames(design)
      return(list(outcome = outcome, shapes = shapes,
        infos = stats::setNames(lapply(predictors, function(x) list(shape = "linear")), predictors),
        coefficient = coefficient, maps = stats::setNames(as.list(seq.int(2L, ncol(design))), predictors),
        coefficient_se = .categorical_coefficient_se(family, fitted, predictors),
        family = family, levels = response$levels, categorical_fit = fitted,
        predictor_names = predictors))
    }
    frame <- data.frame(.cssem_response = ordered(response$values, levels = response$levels),
      data[, predictors, drop = FALSE], check.names = FALSE)
    formula <- stats::reformulate(predictors, response = ".cssem_response")
    fitted <- MASS::polr(formula, data = frame, Hess = TRUE, method = "logistic", model = TRUE)
    coefficient <- fitted$coefficients; names(coefficient) <- predictors
    return(list(outcome = outcome, shapes = shapes,
      infos = stats::setNames(lapply(predictors, function(x) list(shape = "linear")), predictors),
      coefficient = coefficient, maps = stats::setNames(as.list(seq_along(predictors)), predictors),
      coefficient_se = .categorical_coefficient_se(family, fitted, predictors),
      family = family, levels = response$levels, categorical_fit = fitted,
      predictor_names = predictors))
  }
  predictors <- names(shapes); blocks <- list(); infos <- list(); constrained <- integer(); directions <- integer()
  for (predictor in predictors) {
    built <- if (.is_interaction(predictor)) {
      terms <- .interaction_terms(predictor)
      list(values = matrix(data[[terms[[1L]]]] * data[[terms[[2L]]]], ncol = 1L), info = list(shape = "product", terms = terms))
    } else .train_basis(data[[predictor]], shapes[[predictor]])
    blocks[[predictor]] <- built$values; infos[[predictor]] <- built$info
    index <- seq_len(ncol(built$values)) + sum(vapply(blocks[-length(blocks)], ncol, integer(1))) + 1L
    if (.is_monotone_shape(built$info$shape)) {
      # Constrain every basis coefficient to one sign so the fit is genuinely
      # monotone. Mixed signs would let a U-shape masquerade as a monotone
      # effect; see .hinge_basis() for why one-signed coefficients still span
      # both convex and concave monotone curves.
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
  list(outcome = outcome, shapes = shapes, infos = infos, coefficient = coefficient, maps = maps,
    family = family, levels = NULL, predictor_names = predictors)
}

.predict_shape_model <- function(model, data, type = c("expected", "probability", "class")) {
  type <- match.arg(type)
  family <- .structural_family(model$family)
  if (family$family != "gaussian") {
    design <- cbind(`(Intercept)` = 1, as.matrix(data[, model$predictor_names, drop = FALSE]))
    if (family$family == "binomial") {
      coefficient <- model$categorical_fit$coefficients
      eta <- drop(design %*% coefficient)
      probability <- cbind(`0` = 1 - stats::plogis(eta), `1` = stats::plogis(eta))
    } else {
      probability <- as.matrix(stats::predict(model$categorical_fit,
        newdata = data, type = "probs"))
      probability <- probability[, as.character(model$levels), drop = FALSE]
    }
    expected <- drop(probability %*% as.numeric(model$levels))
    if (type == "probability") return(probability)
    if (type == "class") return(model$levels[max.col(probability, ties.method = "first")])
    return(expected)
  }
  blocks <- lapply(names(model$shapes), function(predictor) {
    info <- model$infos[[predictor]]
    if (identical(info$shape, "product")) matrix(data[[info$terms[[1L]]]] * data[[info$terms[[2L]]]], ncol = 1L)
    else .predict_basis(data[[predictor]], info)
  })
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

.cv_shape_candidate <- function(scores, outcome, shapes, fold_sets, family = NULL) {
  family <- .structural_family(family)
  predictions <- vector("list", length(fold_sets)); losses <- numeric()
  probabilities <- if (family$family == "gaussian") NULL else vector("list", length(fold_sets))
  levels <- if (family$family == "gaussian") NULL else .validate_structural_response(scores, outcome, family)$levels
  for (repeat_index in seq_along(fold_sets)) {
    folds <- fold_sets[[repeat_index]]; prediction <- rep(NA_real_, nrow(scores))
    probability <- if (family$family == "gaussian") NULL else matrix(NA_real_, nrow(scores), length(levels),
      dimnames = list(NULL, as.character(levels)))
    for (fold in sort(unique(folds))) {
      train <- scores[folds != fold, , drop = FALSE]; test <- scores[folds == fold, , drop = FALSE]
      model <- .fit_shape_model(train, outcome, shapes, family)
      prediction[folds == fold] <- .predict_shape_model(model, test)
      if (family$family != "gaussian") probability[folds == fold, ] <- .predict_shape_model(model, test, "probability")
    }
    predictions[[repeat_index]] <- prediction
    if (family$family == "gaussian") losses <- c(losses, .foldwise_mse(scores[[outcome]], prediction, folds)) else {
      index <- match(scores[[outcome]], levels)
      losses <- c(losses, vapply(sort(unique(folds)), function(fold) {
        rows <- which(folds == fold); p <- pmax(pmin(probability[cbind(rows, index[rows])], 1 - 1e-12), 1e-12)
        -mean(log(p))
      }, numeric(1)))
      probabilities[[repeat_index]] <- probability
    }
  }
  metrics <- if (family$family == "gaussian") .prediction_metrics(scores[[outcome]], predictions[[1L]]) else
    .categorical_metrics(scores[[outcome]], probabilities[[1L]], family, levels)
  list(prediction = predictions[[1L]], probability = if (family$family == "gaussian") NULL else probabilities[[1L]],
    fold_mse = losses, metrics = metrics)
}

# Heteroskedasticity-robust (HC3) Wald test for curvature on one edge, holding
# every other declared predictor in its baseline (linear or product) form. The
# spline columns are residualised against the restricted design, so the tested
# coefficients are exactly the curvature a straight line cannot express, with
# df - 1 degrees of freedom. Robust standard errors matter here because the
# regressors are estimated posterior means whose precision varies by respondent,
# so the classical constant-variance assumption does not hold. Candidate spline
# sizes are searched, so their p-values are Bonferroni-combined.
.nonlinearity_p <- function(scores, outcome, predictor, baseline_shapes, spline_df) {
  y <- scores[[outcome]]; x <- scores[[predictor]]
  others <- setdiff(names(baseline_shapes), predictor)
  blocks <- lapply(others, function(other) {
    if (.is_interaction(other)) {
      terms <- .interaction_terms(other); scores[[terms[[1L]]]] * scores[[terms[[2L]]]]
    } else scores[[other]]
  })
  restricted <- cbind(1, x, if (length(blocks)) matrix(unlist(blocks, use.names = FALSE), ncol = length(blocks)))
  if (!all(is.finite(y)) || !all(is.finite(restricted))) return(NA_real_)
  decomposition <- qr(restricted)
  tests <- vapply(spline_df, function(df) {
    basis <- tryCatch(splines::ns(x, df = df), error = function(err) NULL)
    if (is.null(basis)) return(NA_real_)
    extra <- qr.resid(decomposition, as.matrix(basis))
    extra_qr <- qr(extra)
    if (extra_qr$rank < 1L) return(NA_real_)
    extra <- extra[, extra_qr$pivot[seq_len(extra_qr$rank)], drop = FALSE]
    design <- cbind(restricted, extra)
    fit <- stats::lm.fit(design, y)
    if (anyNA(fit$coefficients)) return(NA_real_)
    bread <- tryCatch(chol2inv(qr.R(qr(design))), error = function(err) NULL)
    if (is.null(bread)) return(NA_real_)
    leverage <- rowSums((design %*% bread) * design)
    weighted <- design * (fit$residuals / pmax(1 - leverage, 1e-8))
    covariance <- bread %*% crossprod(weighted) %*% bread
    tested <- seq.int(ncol(restricted) + 1L, ncol(design))
    block <- covariance[tested, tested, drop = FALSE]
    solved <- tryCatch(solve(block, fit$coefficients[tested]), error = function(err) NULL)
    if (is.null(solved)) return(NA_real_)
    statistic <- drop(crossprod(fit$coefficients[tested], solved))
    if (!is.finite(statistic) || statistic < 0) return(NA_real_)
    stats::pchisq(statistic, df = length(tested), lower.tail = FALSE)
  }, numeric(1))
  tests <- tests[is.finite(tests)]
  if (!length(tests)) return(NA_real_)
  min(1, min(tests) * length(tests))
}

# Reported diagnostic: how often across repeated cross-validation assignments a
# candidate clears the paired baseline-improvement margin. It no longer gates
# acceptance (.nonlinearity_p() does), but it still separates a shape supported
# in every split from one carried by a single lucky one, and it breaks ties
# between otherwise equivalent shapes.
.selection_frequency <- function(base, candidates, multiplier, folds_per_repeat) {
  repeats <- as.integer(length(base$fold_mse) / folds_per_repeat)
  # Stability means that a candidate repeatedly clears the same paired
  # baseline-improvement rule; it is not a winner-take-all contest between
  # nearly equivalent nonlinear bases. Every shape family faces the same
  # standard-error margin: a bare positive improvement is not support, since
  # with no signal it occurs in about half of all repeats.
  vapply(names(candidates), function(key) {
    candidate <- candidates[[key]]
    supported <- vapply(seq_len(repeats), function(repeat_index) {
      index <- ((repeat_index - 1L) * folds_per_repeat + 1L):(repeat_index * folds_per_repeat)
      difference <- base$fold_mse[index] - candidate$fold_mse[index]
      mean(difference) > multiplier * stats::sd(difference) / sqrt(length(difference))
    }, logical(1))
    mean(supported)
  }, numeric(1))
}

.pick_shape_winner <- function(candidate_keys, candidate_meta, improvement, improvement_se,
                               frequency, smooth_uncertainty, shape_stability_min) {
  valid <- which(is.finite(improvement) & is.finite(improvement_se))
  if (!length(valid)) return(NA_character_)
  best <- valid[which.max(improvement[valid])]
  indistinguishable <- valid[improvement[valid] >= improvement[[best]] - smooth_uncertainty *
    sqrt(improvement_se[[best]]^2 + improvement_se[valid]^2)]
  stable_monotone <- candidate_keys[indistinguishable][
    vapply(candidate_keys[indistinguishable], function(key) {
      meta <- candidate_meta[[key]]
      .is_monotone_shape(meta$shape) && improvement[[key]] > 0 && frequency[[key]] >= shape_stability_min
    }, logical(1))
  ]
  # A monotone shape carries a directional claim a spline does not, so it wins
  # whenever it predicts indistinguishably well. This used to apply only when
  # the best improvement was under an absolute .05, which had no justification
  # and sent strong monotone effects (thresholds, saturating curves) to the
  # spline label purely because they were strong.
  if (length(stable_monotone)) {
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
.eiv_coefficients <- function(scores, outcome, predictors, reliability, weights = NULL, posterior_var = NULL,
                              reliability_floor = 0.15, condition_ratio = 0.10) {
  interaction <- vapply(predictors, .is_interaction, logical(1))
  X <- .predictor_matrix(scores, predictors); y <- scores[[outcome]]
  w <- if (is.null(weights)) rep(1, nrow(X)) else weights
  # Posterior-variance reweighting is only defined for construct predictors; skip
  # it when an interaction term is present.
  pv <- if (is.null(posterior_var) || any(interaction)) NULL else as.matrix(posterior_var[, predictors, drop = FALSE])
  keep <- stats::complete.cases(X, y) & is.finite(w)
  X <- X[keep, , drop = FALSE]; y <- y[keep]; w <- w[keep]
  if (!is.null(pv)) pv <- pv[keep, , drop = FALSE]
  na <- stats::setNames(rep(NA_real_, length(predictors)), predictors)
  diagnostic <- list(n = length(y), rank = 0L, condition_number = NA_real_,
    corrected_condition_number = NA_real_, singular = TRUE, correction_shrink = NA_real_,
    correction_strength = NA_real_, reliability_floor_applied = NA, stable = NA,
    message = "Insufficient complete rows for a structural correction.")
  if (length(y) < length(predictors) + 2L || sum(w) <= 0)
    return(list(naive = na, corrected = na, diagnostics = diagnostic))
  # Inverse-variance weighted moments: respondents with wide posteriors carry
  # less weight, so heteroskedastic measurement information no longer biases the
  # structural estimate toward the noisiest respondents.
  sw <- sum(w)
  xbar <- colSums(X * w) / sw; ybar <- sum(y * w) / sw
  Xc <- sweep(X, 2L, xbar, "-"); yc <- y - ybar
  Szz <- crossprod(Xc * w, Xc) / sw
  Szy <- crossprod(Xc * w, yc) / sw
  diagnostic$rank <- as.integer(qr(Szz)$rank)
  diagnostic$condition_number <- tryCatch(unname(kappa(Szz)), error = function(e) NA_real_)
  diagnostic$singular <- diagnostic$rank < ncol(Szz) || !is.finite(diagnostic$condition_number)
  naive <- tryCatch(drop(solve(Szz, Szy)), error = function(e) na)
  # Weighted marginal reliability when per-respondent posterior variances are
  # available, otherwise the construct-level reliability estimate. An interaction
  # term's reliability is the product of its constituents' reliabilities (the
  # reliability of a product of standardized error-laden scores), so the
  # interaction coefficient is disattenuated too rather than left attenuated.
  rel <- vapply(predictors, function(predictor)
    if (.is_interaction(predictor)) prod(reliability[.interaction_terms(predictor)]) else unname(reliability[predictor]), numeric(1))
  names(rel) <- predictors
  if (!is.null(pv)) {
    signal <- diag(Szz); error <- colSums(pv * w) / sw
    rel_w <- signal / (signal + error)
    rel <- ifelse(is.finite(rel_w), rel_w, rel)
  }
  if (anyNA(rel) || any(!is.finite(rel)) || any(rel <= 0)) {
    diagnostic$message <- "A predictor reliability is unavailable or non-positive; corrected coefficients are omitted."
    return(list(naive = stats::setNames(naive, predictors), corrected = na,
      stable = NA, diagnostics = diagnostic))
  }
  # Adaptive regularization. At very low reliability a raw errors-in-variables
  # correction subtracts almost all the predictor variance, leaving a
  # near-singular covariance whose inverse explodes (the corrected estimate then
  # becomes more biased than the naive one). Floor the effective reliability so
  # the correction factor stays bounded, then shrink the error-covariance
  # subtraction further if the corrected covariance is still ill-conditioned. The
  # result interpolates between the full correction and the naive estimate.
  # This is numerical stabilization, not an accuracy guarantee: it bounds the
  # correction factor so the estimate cannot explode, and says nothing about
  # whether the corrected estimate is closer to any target than the naive one.
  # `stable` flags when the limiting engaged, so a caller can see that the
  # reported estimate is not the full correction.
  effective <- pmax(rel, reliability_floor)
  De <- diag((1 - effective) * diag(Szz), nrow = length(predictors))
  base <- tryCatch(min(eigen(Szz, symmetric = TRUE, only.values = TRUE)$values), error = function(e) NA_real_)
  shrink <- 1; corrected_cov <- Szz - De
  if (is.finite(base)) {
    iteration <- 0L
    while (iteration < 25L && min(eigen(corrected_cov, symmetric = TRUE, only.values = TRUE)$values) < condition_ratio * base) {
      shrink <- shrink * 0.8; corrected_cov <- Szz - shrink * De; iteration <- iteration + 1L
    }
  }
  diagnostic$corrected_condition_number <- tryCatch(unname(kappa(corrected_cov)), error = function(e) NA_real_)
  requested_strength <- mean(1 - pmin(pmax(rel, 0), 1))
  effective_strength <- mean(shrink * (1 - effective))
  diagnostic$correction_shrink <- shrink
  diagnostic$correction_strength <- if (requested_strength > 0)
    pmin(pmax(effective_strength / requested_strength, 0), 1) else 0
  diagnostic$reliability_floor_applied <- any(rel < reliability_floor)
  corrected <- tryCatch(drop(solve(corrected_cov, Szy)), error = function(e) na)
  stable <- all(rel >= reliability_floor) && shrink >= 1 - 1e-6
  diagnostic$stable <- stable
  diagnostic$message <- if (stable) "Full errors-in-variables correction was numerically stable." else
    "The correction was stabilized by a reliability floor or covariance shrinkage; this is not an accuracy guarantee."
  list(naive = stats::setNames(naive, predictors), corrected = stats::setNames(corrected, predictors),
    stable = stable, diagnostics = diagnostic)
}

# Percentile bootstrap of the corrected slopes, resampling respondents. This
# reflects sampling variability of the errors-in-variables estimator; the
# reliability inputs are held fixed at their measurement-model estimates.
.eiv_bootstrap <- function(scores, outcome, predictors, reliability, replicates, seed,
                           weights = NULL, posterior_var = NULL, level = .95) {
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
# linear, monotone, or product are disattenuated; smooth edges are reported but
# not yet corrected (closed-form errors-in-variables for splines is out of scope).
.corrected_effects <- function(scores, outcome, selected_shapes, reliability, replicates, seed,
                               weights = NULL, posterior_var = NULL, level = .95, family = NULL) {
  predictors <- names(selected_shapes)
  family <- .structural_family(family)
  if (family$family != "gaussian") {
    return(do.call(rbind, lapply(predictors, function(p) data.frame(
      outcome = outcome, predictor = p, naive_estimate = NA_real_, corrected_estimate = NA_real_,
      predictor_reliability = NA_real_, corrected_ci_low = NA_real_, corrected_ci_high = NA_real_,
      eiv_applicable = FALSE, eiv_stable = NA, eiv_n = nrow(scores), eiv_rank = NA_integer_,
      eiv_condition_number = NA_real_, eiv_corrected_condition_number = NA_real_, eiv_singular = NA,
      correction_shrink = NA_real_, correction_strength = NA_real_, reliability_floor_applied = NA,
      eiv_diagnostic = "Categorical structural outcomes do not support linear EIV correction.",
      stringsAsFactors = FALSE))))
  }
  applicable <- vapply(predictors, function(p) selected_shapes[[p]] %in%
    c("linear", "monotone_increasing", "monotone_decreasing", "product"), logical(1))
  fit <- .eiv_coefficients(scores, outcome, predictors, reliability, weights, posterior_var)
  boot <- if (any(applicable)) .eiv_bootstrap(scores, outcome, predictors, reliability, replicates, seed,
    weights, posterior_var, level) else NULL
  diagnostic <- fit$diagnostics
  do.call(rbind, lapply(predictors, function(p) {
    ci <- if (!is.null(boot) && applicable[[p]] && any(is.finite(boot[, p])))
      stats::quantile(boot[, p], c((1 - level) / 2, (1 + level) / 2), na.rm = TRUE, names = FALSE) else c(NA_real_, NA_real_)
    data.frame(outcome = outcome, predictor = p,
      naive_estimate = if (applicable[[p]]) unname(fit$naive[[p]]) else NA_real_,
      corrected_estimate = if (applicable[[p]]) unname(fit$corrected[[p]]) else NA_real_,
      predictor_reliability = if (.is_interaction(p))
        prod(reliability[.interaction_terms(p)]) else unname(reliability[[p]]),
      corrected_ci_low = ci[[1L]], corrected_ci_high = ci[[2L]],
      eiv_applicable = applicable[[p]], eiv_stable = applicable[[p]] && isTRUE(fit$stable),
      eiv_n = diagnostic$n, eiv_rank = diagnostic$rank,
      eiv_condition_number = diagnostic$condition_number,
      eiv_corrected_condition_number = diagnostic$corrected_condition_number,
      eiv_singular = diagnostic$singular, correction_shrink = diagnostic$correction_shrink,
      correction_strength = diagnostic$correction_strength,
      reliability_floor_applied = diagnostic$reliability_floor_applied,
      eiv_diagnostic = diagnostic$message,
      stringsAsFactors = FALSE)
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
    if (shape %in% c("linear", "product")) return(data.frame(outcome = outcome, predictor = predictor, shape = shape,
      estimate = model$coefficient[model$maps[[predictor]]][1L], x = NA_real_, fitted = NA_real_, strongest_region = NA_character_))
    grid <- seq(stats::quantile(scores[[predictor]], .05), stats::quantile(scores[[predictor]], .95), length.out = 50L)
    # The grid holds every other predictor at its mean, but an interaction
    # predictor is not a column of `scores`: .predict_shape_model() rebuilds the
    # product from its constituents, so the frame is built over those instead.
    # Looking the interaction name up directly produced a mean of NULL and a
    # "not numeric or logical: returning NA" warning on every curve.
    constructs <- unique(unlist(lapply(names(model$shapes), .predictor_constructs), use.names = FALSE))
    new_data <- as.data.frame(lapply(constructs, function(name) rep(mean(scores[[name]]), length(grid))))
    names(new_data) <- constructs; new_data[[predictor]] <- grid
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
#' @param fit A `fit_states` object.
#' @param structure A `cssem_structure` object.
#' @param folds Optional structural validation folds.
#' @param spline_df Degrees of freedom for low-complexity unconstrained spline
#'   candidates. Defaults to 3 and 4.
#' @param smooth_uncertainty Paired foldwise-loss standard-error multiplier used
#'   when deciding which candidate shapes are predictively indistinguishable.
#'   It governs the reported shape, not whether an edge is nonlinear.
#' @param shape_stability_min Minimum repeated-CV selection frequency for a
#'   monotone candidate to be reported in place of an equally predictive
#'   spline. Like `smooth_uncertainty`, it affects the reported shape only.
#' @param shape_alpha Family-wise error rate for the curvature test that decides
#'   whether any edge of an outcome is nonlinear. The test is a
#'   heteroskedasticity-robust Wald test of the spline terms against the linear
#'   fit, Bonferroni-combined over `spline_df` and Holm-adjusted across the
#'   outcome's shape-searched predictors.
#' @param shape_min_gain Smallest share of the linear model's out-of-fold error
#'   a curved shape must remove before it is reported in place of the straight
#'   line. Guards against curvature that is real but negligible, such as the
#'   mild score-level curvature strongly skewed indicators induce even when the
#'   relation between the constructs is linear.
#' @param structural_repeats Number of deterministic structural CV assignments.
#' @param seed Seed used only for repeated structural folds.
#' @param shadow_scope Shadow benchmark scope.
#' @param reliability Optional numeric vector of reliabilities in `(0, 1]`,
#'   named by locked construct, used by the errors-in-variables correction in
#'   place of the fit's values for the named constructs (for example, for a
#'   sensitivity analysis). Constructs not named keep the posterior (or
#'   asserted) reliability carried on `fit`; when no reliability is available
#'   the corrected estimate is omitted rather than reported uncorrected.
#' @param eiv_bootstrap Number of percentile-bootstrap replicates for the
#'   corrected-estimate interval. Zero disables the interval.
#' @param level Confidence level for the corrected-estimate interval. This is
#'   independent of the structural shape-selection error rate.
#' @param respondent_weighting Experimental. `"information"` applies
#'   inverse-variance respondent weighting from the posterior SD. It is
#'   `"none"` by default: because posterior width is score-dependent, weighting
#'   induces range restriction and does not improve point-estimate bias in
#'   validation, so it is not recommended for confirmatory estimates.
#' @param preset Runtime preset. Use `"exploratory"` for lighter-weight
#'   structural selection defaults while iterating locally.
#' @param missing_policy How missing locked scores are handled. The default
#'   `"complete"` excludes rows with any non-finite or prior-only score from
#'   structural fitting and records them in [sample_accounting()]. `"error"`
#'   rejects such rows before fitting.
#' @param fixed_shapes Optional named list of selected shape vectors used to
#'   refit an existing structural shape selection without searching again.
#' @param constraints Optional [cssem_constraint()] declaration. Constraints
#'   apply only to selected linear locked-score edges; EIV overrides,
#'   information weighting, interactions, and nonlinear selected shapes are
#'   rejected explicitly.
#' @return An object of class `cssem_association`.
#' @export
associate <- function(fit, structure, folds = NULL, spline_df = c(3L, 4L), smooth_uncertainty = 1,
                             shape_stability_min = .70, shape_alpha = .05, shape_min_gain = .005,
                             structural_repeats = 5L, seed = 1L,
                             shadow_scope = c("both", "temporal", "unrestricted"),
                             reliability = NULL, eiv_bootstrap = 0L,
                             respondent_weighting = c("none", "information"),
                             preset = c("default", "exploratory"), level = .95,
                             missing_policy = c("complete", "error"), fixed_shapes = NULL,
                             constraints = NULL) {
  associate_call <- match.call()
  .preserve_seed()
  if (!inherits(fit, "fit_states")) stop("fit must be a fit_states.", call. = FALSE)
  if (!inherits(structure, "cssem_structure")) stop("structure must be a cssem_structure.", call. = FALSE)
  preset <- match.arg(preset)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)
  level <- .bootstrap_validate_level(level)
  respondent_weighting <- match.arg(respondent_weighting)
  missing_policy <- match.arg(missing_policy)
  constraints <- .validate_constraint_targets(constraints, structure)
  if (!is.null(constraints) && isTRUE(constraints$active)) {
    if (respondent_weighting != "none") stop("Constraints reject information weighting; use respondent_weighting = \"none\".", call. = FALSE)
    if (eiv_bootstrap > 0L || !is.null(reliability)) stop("Constraints reject explicit EIV correction or EIV bootstrap requests.", call. = FALSE)
  }
  if (preset == "exploratory") {
    if (missing(spline_df)) spline_df <- 3L
    if (missing(structural_repeats)) structural_repeats <- 2L
    if (missing(shadow_scope)) shadow_scope <- "temporal"
  }
  if (!is.numeric(smooth_uncertainty) || length(smooth_uncertainty) != 1L || !is.finite(smooth_uncertainty) || smooth_uncertainty < 0)
    stop("smooth_uncertainty must be a non-negative numeric scalar.", call. = FALSE)
  if (!is.numeric(shape_stability_min) || length(shape_stability_min) != 1L || shape_stability_min < 0 || shape_stability_min > 1)
    stop("shape_stability_min must be between zero and one.", call. = FALSE)
  if (!is.numeric(shape_alpha) || length(shape_alpha) != 1L || !is.finite(shape_alpha) || shape_alpha <= 0 || shape_alpha >= 1)
    stop("shape_alpha must be between zero and one.", call. = FALSE)
  if (!is.numeric(shape_min_gain) || length(shape_min_gain) != 1L || !is.finite(shape_min_gain) || shape_min_gain < 0)
    stop("shape_min_gain must be a non-negative numeric scalar.", call. = FALSE)
  spline_df <- unique(as.integer(spline_df))
  if (!length(spline_df) || any(is.na(spline_df)) || any(spline_df < 2L)) stop("spline_df must contain values of at least 2.", call. = FALSE)
  scores <- fit$locked_scores; all_names <- names(scores)
  response_families <- structure$response_families
  if (is.null(response_families)) {
    response_families <- lapply(names(structure$effects), function(x) structural_outcome("gaussian"))
    names(response_families) <- names(structure$effects)
  }
  predictor_names <- unlist(lapply(structure$effects, names), use.names = FALSE)
  declared <- unique(c(names(structure$effects), unlist(lapply(predictor_names, .predictor_constructs), use.names = FALSE)))
  if (!all(declared %in% all_names)) stop("Structural declarations must use locked construct names.", call. = FALSE)
  fixed_shapes <- if (is.null(fixed_shapes)) NULL else .validate_fixed_shapes(fixed_shapes, structure)
  categorical_outcomes <- names(response_families)[vapply(response_families, function(x) .structural_family(x)$family != "gaussian", logical(1))]
  if (length(categorical_outcomes)) {
    if (!is.null(constraints) && isTRUE(constraints$active))
      stop("Constraints are not supported for categorical structural outcomes; use a Gaussian response or omit constraints.", call. = FALSE)
    if (respondent_weighting != "none" || eiv_bootstrap > 0L || !is.null(reliability))
      stop("categorical structural outcomes do not support reliability/EIV correction or information weighting.", call. = FALSE)
    for (outcome in categorical_outcomes) {
      declared_shapes <- if (is.null(fixed_shapes)) structure$effects[[outcome]] else fixed_shapes[[outcome]]
      if (any(vapply(declared_shapes, function(x) !identical(if (inherits(x, "cssem_effect")) x$shape else x, "linear"), logical(1))) ||
          any(vapply(names(declared_shapes), .is_interaction, logical(1))))
        stop("Categorical structural outcomes only support linear main-effect shapes.", call. = FALSE)
    }
  }
  score_row_ids <- if (!is.null(fit$row_ids)) as.integer(fit$row_ids) else seq_len(nrow(scores))
  measurement_status <- .measurement_status(fit, score_row_ids, declared)
  observed_status <- measurement_status == "complete" | measurement_status == "partial"
  score_complete <- apply(as.matrix(scores[, declared, drop = FALSE]), 1L, function(x) all(is.finite(x))) &
    apply(observed_status, 1L, all)
  if (missing_policy == "error" && any(!score_complete))
    stop(sprintf("missing_policy = \"error\" found %d row(s) with missing or prior-only locked scores; use `sample_accounting()` to inspect coverage or choose `complete`.",
      sum(!score_complete)), call. = FALSE)
  if (missing_policy == "complete" && any(!score_complete)) {
    scores <- scores[score_complete, , drop = FALSE]
    score_row_ids <- score_row_ids[score_complete]
  }
  if (nrow(scores) < 2L) stop("Missing-score handling left fewer than two rows for structural fitting.", call. = FALSE)
  for (outcome in categorical_outcomes)
    .validate_structural_response(scores, outcome, response_families[[outcome]])
  # Per-construct reliability used by the errors-in-variables correction: the
  # posterior reliability carried on the fit, overridden construct by construct
  # by any user-supplied values. NA where unavailable so the corrected estimate is
  # simply omitted rather than silently wrong.
  reliability_vec <- stats::setNames(rep(NA_real_, length(all_names)), all_names)
  if (!is.null(fit$reliability)) {
    shared <- intersect(names(fit$reliability), all_names)
    reliability_vec[shared] <- as.numeric(fit$reliability[shared])
  }
  overridden <- character(0)
  if (!is.null(reliability)) {
    if (!is.numeric(reliability) || is.null(names(reliability)) || any(!names(reliability) %in% all_names))
      stop("reliability must be a numeric vector named by locked construct names.", call. = FALSE)
    if (any(!is.finite(reliability) | reliability <= 0 | reliability > 1))
      stop("reliability values must be in (0, 1].", call. = FALSE)
    reliability_vec[names(reliability)] <- as.numeric(reliability)
    overridden <- names(reliability)
  }
  # Optional inverse-variance respondent weighting, drawn from the per-respondent
  # posterior SD carried on the fit. Unavailable for score-only engines, which
  # then fall back to unweighted estimation.
  # Per-respondent posterior variances feed the errors-in-variables correction
  # directly: passing them through (even unweighted) lets the bootstrap
  # re-estimate reliability on each resample, so its interval reflects
  # reliability-estimation uncertainty. This matters most at low reliability,
  # where a fixed reliability produces over-narrow intervals.
  respondent_weights <- NULL; posterior_var <- NULL
  if (!is.null(fit$score_posterior_sd)) {
    sd <- as.data.frame(fit$score_posterior_sd)
    if (nrow(sd) == length(score_complete) && length(score_complete) != nrow(scores))
      sd <- sd[score_complete, , drop = FALSE]
    modeled <- intersect(all_names, names(sd))
    if (length(modeled) && nrow(sd) == nrow(scores)) {
      posterior_var <- sd^2
      if (respondent_weighting == "information") respondent_weights <- .information_weights(sd[, modeled, drop = FALSE])
      # .eiv_coefficients() re-estimates reliability from posterior variances
      # wherever they exist, which would silently replace a user-supplied value.
      # Blank those constructs' variances so the supplied reliability is the one
      # actually used (and the one reported).
      for (nm in intersect(overridden, names(posterior_var))) posterior_var[[nm]] <- NA_real_
      # The fit-level reliability averages over every fitted row, including
      # prior-only rows whose posterior variance is the prior's. Structural
      # rows exclude those, so re-estimate reliability on the retained rows;
      # this reliability also feeds the mediation, causal, and moderation
      # corrections. With no exclusions it equals the fit-level value.
      for (nm in setdiff(modeled, overridden)) {
        variance <- posterior_var[[nm]]
        if (!any(is.finite(variance))) next
        signal <- stats::var(scores[[nm]], na.rm = TRUE)
        error <- mean(variance, na.rm = TRUE)
        if (is.finite(signal) && is.finite(error) && signal > 0)
          reliability_vec[[nm]] <- signal / (signal + error)
      }
    }
  }
  shadow_scope <- match.arg(shadow_scope); scopes <- if (shadow_scope == "both") c("temporal", "unrestricted") else shadow_scope
  temporal_order <- if ("temporal" %in% scopes) .resolve_temporal_order(structure, all_names) else NULL
  folds <- if (is.null(folds)) fit$folds else as.integer(folds)
  if (length(folds) == nrow(fit$locked_scores) && length(folds) != nrow(scores)) folds <- folds[score_complete]
  if (length(folds) != nrow(scores) || length(unique(folds)) < 2L) stop("folds must assign every row to at least two validation folds.", call. = FALSE)
  fold_sets <- .structural_fold_sets(folds, structural_repeats, seed)
  candidates <- list(); effects <- list(); predictions <- list(); gaps <- list(); models <- list(); contributions <- list(); corrected <- list()
  for (outcome in names(structure$effects)) {
    policies <- structure$effects[[outcome]]; predictors <- names(policies)
    family <- .structural_family(response_families[[outcome]])
    categorical <- family$family != "gaussian"
    if (!is.null(fixed_shapes)) {
      selected_shapes <- fixed_shapes[[outcome]]
      baseline_shapes <- selected_shapes
      baseline <- .cv_shape_candidate(scores, outcome, selected_shapes, fold_sets, family)
      nonlinear <- list(); candidate_meta <- list(); candidate_keys <- character()
      frequency <- improvement <- improvement_se <- shape_p <- adjusted <- numeric()
      flagged <- winner <- NA_character_; relative_gain <- NA_real_; select_nonlinear <- FALSE
      selected <- baseline
    } else {
    # Declared interaction predictors enter as fixed linear-by-linear product
    # terms; only main-effect predictors are shape-searched.
    baseline_shapes <- stats::setNames(vapply(predictors, function(p) if (.is_interaction(p)) "product" else "linear", character(1)), predictors)
    if (categorical) baseline_shapes[] <- "linear"
    baseline <- .cv_shape_candidate(scores, outcome, baseline_shapes, fold_sets, family)
    nonlinear <- list(); candidate_meta <- list()
    for (predictor in predictors) if (!categorical && !.is_interaction(predictor)) for (shape in setdiff(.shape_candidates(policies[[predictor]]$shape, spline_df), "linear")) {
      shapes <- baseline_shapes; shapes[[predictor]] <- shape; key <- paste(predictor, shape, sep = "::")
      nonlinear[[key]] <- .cv_shape_candidate(scores, outcome, shapes, fold_sets, family)
      candidate_meta[[key]] <- list(predictor = predictor, shape = shape, shapes = shapes)
    }
    candidate_keys <- names(nonlinear)
    frequency <- if (length(nonlinear)) stats::setNames(.selection_frequency(baseline, nonlinear, smooth_uncertainty, length(unique(fold_sets[[1L]]))), candidate_keys) else numeric()
    improvement <- if (length(nonlinear)) stats::setNames(vapply(nonlinear, function(x) mean(baseline$fold_mse - x$fold_mse), numeric(1)), candidate_keys) else numeric()
    improvement_se <- if (length(nonlinear)) stats::setNames(vapply(nonlinear, function(x) stats::sd(baseline$fold_mse - x$fold_mse) / sqrt(length(x$fold_mse)), numeric(1)), candidate_keys) else numeric()
    # Curvature is decided by a test with a stated error rate, not by the
    # cross-validated loss itself: a paired loss improvement has no calibrated
    # null distribution, so any threshold on it is tuned rather than justified,
    # and the tuned thresholds that hold the false-curve rate down also discard
    # real curvature. Cross-validation then only chooses which shape to report.
    # At most one edge per outcome may be nonlinear, so only the most
    # significant predictor is eligible, and family-wise error across the
    # searched predictors is controlled by Holm.
    searched <- unique(vapply(candidate_meta, `[[`, character(1), "predictor"))
    shape_p <- if (length(searched)) stats::setNames(vapply(searched, function(predictor)
      .nonlinearity_p(scores, outcome, predictor, baseline_shapes, spline_df), numeric(1)), searched) else numeric()
    adjusted <- if (length(shape_p)) stats::p.adjust(shape_p, method = "holm") else numeric()
    flagged <- if (any(is.finite(adjusted) & adjusted < shape_alpha)) names(adjusted)[[which.min(adjusted)]] else NA_character_
    winner <- NA_character_
    if (!is.na(flagged)) {
      eligible <- candidate_keys[vapply(candidate_meta[candidate_keys], function(meta) identical(meta$predictor, flagged), logical(1))]
      winner <- .pick_shape_winner(eligible, candidate_meta, improvement[eligible], improvement_se[eligible],
        frequency[eligible], smooth_uncertainty, shape_stability_min)
    }
    # A flagged edge still has to predict materially better out of fold than the
    # straight line it would replace. The test answers whether curvature is
    # real, not whether it is worth reporting: strongly skewed indicators, for
    # instance, make the scores themselves mildly curved even when the latent
    # relation is straight, and at large n that curvature is detectable but
    # negligible. The floor is a share of the linear model's out-of-fold error.
    relative_gain <- if (length(winner) == 1L && !is.na(winner)) improvement[[winner]] / mean(baseline$fold_mse) else NA_real_
    select_nonlinear <- is.finite(relative_gain) && relative_gain > shape_min_gain
    selected_shapes <- if (select_nonlinear) candidate_meta[[winner]]$shapes else baseline_shapes
    selected <- if (select_nonlinear) nonlinear[[winner]] else baseline
    }
    full_model <- .fit_shape_model(scores, outcome, selected_shapes, family)
    corrected[[outcome]] <- .corrected_effects(scores, outcome, selected_shapes, reliability_vec, eiv_bootstrap, seed,
      weights = respondent_weights, posterior_var = posterior_var, level = level, family = family)
    edge_p <- function(predictor) if (predictor %in% names(adjusted)) unname(adjusted[[predictor]]) else NA_real_
    # The baseline shape of an interaction predictor is "product", not "linear":
    # effect_card() has always said so, and the ledger reading "linear" made the
    # same edge look like two different things in two reports.
    candidate_rows <- lapply(predictors, function(predictor) data.frame(outcome = outcome, predictor = predictor,
      candidate = baseline_shapes[[predictor]], shape = baseline_shapes[[predictor]],
      rmse = baseline$metrics[["rmse"]], r_squared = baseline$metrics[["r_squared"]],
      log_loss = .metric_or_na(baseline$metrics, "log_loss"),
      brier = .metric_or_na(baseline$metrics, "brier"),
      accuracy = .metric_or_na(baseline$metrics, "accuracy"),
      mean_mse_improvement = 0, mse_improvement_se = NA_real_, selection_frequency = if (select_nonlinear && candidate_meta[[winner]]$predictor == predictor) 0 else 1,
      nonlinearity_p = edge_p(predictor),
      selected = !select_nonlinear || candidate_meta[[winner]]$predictor != predictor, stringsAsFactors = FALSE))
    if (length(nonlinear)) for (key in names(nonlinear)) {
      meta <- candidate_meta[[key]]; candidate_rows[[length(candidate_rows) + 1L]] <- data.frame(outcome = outcome, predictor = meta$predictor,
        candidate = meta$shape, shape = meta$shape, rmse = nonlinear[[key]]$metrics[["rmse"]], r_squared = nonlinear[[key]]$metrics[["r_squared"]],
        log_loss = .metric_or_na(nonlinear[[key]]$metrics, "log_loss"),
        brier = .metric_or_na(nonlinear[[key]]$metrics, "brier"),
        accuracy = .metric_or_na(nonlinear[[key]]$metrics, "accuracy"),
        mean_mse_improvement = improvement[[key]], mse_improvement_se = improvement_se[[key]], selection_frequency = frequency[[key]],
        nonlinearity_p = edge_p(meta$predictor),
        selected = isTRUE(select_nonlinear) && identical(key, winner), stringsAsFactors = FALSE)
    }
    candidates[[outcome]] <- do.call(rbind, candidate_rows)
    for (predictor in predictors) {
      dropped_shapes <- selected_shapes[setdiff(names(selected_shapes), predictor)]
      dropped <- .cv_shape_candidate(scores, outcome, dropped_shapes, fold_sets, family)
      difference <- dropped$fold_mse - selected$fold_mse
      contributions[[paste(outcome, predictor, sep = "::")]] <- data.frame(outcome = outcome, predictor = predictor,
        edge_drop_mse_increase = mean(difference), edge_drop_mse_se = stats::sd(difference) / sqrt(length(difference)), stringsAsFactors = FALSE)
    }
    shadow_predictions <- list(); shadow_rows <- list()
    for (scope in scopes) {
      shadow_predictors <- if (scope == "temporal") temporal_order[match(temporal_order, temporal_order) < match(outcome, temporal_order)] else setdiff(all_names, outcome)
      shadow_prediction <- if (categorical) rep(NA_real_, nrow(scores)) else
        .shadow_predictions(scores, outcome, shadow_predictors, folds)
      shadow_metrics <- if (categorical) c(r_squared = NA_real_) else
        .prediction_metrics(scores[[outcome]], shadow_prediction)
      shadow_predictions[[scope]] <- shadow_prediction
      shadow_rows[[scope]] <- data.frame(outcome = outcome, shadow_scope = scope, eligible_predictors = paste(shadow_predictors, collapse = " + "),
        theory_r_squared = if (categorical) NA_real_ else selected$metrics[["r_squared"]],
        shadow_r_squared = shadow_metrics[["r_squared"]],
        specification_gap = if (categorical) NA_real_ else selected$metrics[["r_squared"]] - shadow_metrics[["r_squared"]],
        diagnostic_status = if (categorical) "unavailable_categorical_shadow" else "available", stringsAsFactors = FALSE)
    }
    effect_data <- .effect_rows(full_model, scores, outcome)
    effect_data$selection_stability <- vapply(effect_data$predictor, function(predictor) {
      row <- candidates[[outcome]][candidates[[outcome]]$predictor == predictor & candidates[[outcome]]$selected, , drop = FALSE]
      if (nrow(row) && row$shape[[1L]] != "linear") row$selection_frequency[[1L]] else 1
    }, numeric(1))
    effects[[outcome]] <- effect_data; predictions[[outcome]] <- as.data.frame(c(list(observed = scores[[outcome]], theory = selected$prediction), shadow_predictions))
    gaps[[outcome]] <- do.call(rbind, shadow_rows); models[[outcome]] <- full_model
  }
  constraint_diagnostics <- list(active = FALSE)
  if (!is.null(constraints) && isTRUE(constraints$active)) {
    applied <- .constraint_apply(models, scores, constraints)
    models <- applied$models; constraint_diagnostics <- applied$diagnostics
    for (outcome in names(models)) {
      full_model <- models[[outcome]]
      effect_data <- .effect_rows(full_model, scores, outcome)
      effect_data$selection_stability <- vapply(effect_data$predictor, function(predictor) {
        row <- candidates[[outcome]][candidates[[outcome]]$predictor == predictor & candidates[[outcome]]$selected, , drop = FALSE]
        if (nrow(row) && row$shape[[1L]] != "linear") row$selection_frequency[[1L]] else 1
      }, numeric(1))
      effects[[outcome]] <- effect_data
      predictions[[outcome]]$theory <- NA_real_
      corrected[[outcome]] <- .constraint_effect_rows(corrected[[outcome]], full_model)
      candidates[[outcome]][, intersect(c("rmse", "r_squared", "mean_mse_improvement", "mse_improvement_se", "nonlinearity_p"), names(candidates[[outcome]]))] <- NA_real_
      for (predictor in names(full_model$shapes)) {
        key <- paste(outcome, predictor, sep = "::")
        if (key %in% names(contributions)) {
          contributions[[key]]$edge_drop_mse_increase <- NA_real_
          contributions[[key]]$edge_drop_mse_se <- NA_real_
        }
      }
      gaps[[outcome]]$theory_r_squared <- NA_real_
      gaps[[outcome]]$specification_gap <- NA_real_
    }
    constraint_diagnostics$predictive_status <- "unavailable: cross-validated constrained diagnostics are not retained; shape selection metrics describe the unconstrained selection stage"
  }
  corrected_table <- do.call(rbind, corrected)
  association_settings <- list(folds = folds, spline_df = spline_df,
    smooth_uncertainty = smooth_uncertainty, shape_stability_min = shape_stability_min,
    shape_alpha = shape_alpha, shape_min_gain = shape_min_gain,
    structural_repeats = structural_repeats, seed = seed, shadow_scope = shadow_scope,
    reliability = reliability, eiv_bootstrap = eiv_bootstrap,
    respondent_weighting = respondent_weighting, preset = preset, level = level,
    missing_policy = missing_policy, fixed_shapes = fixed_shapes, constraints = constraints)
  fit_provenance <- fit$provenance_record
  parent <- if (is.null(fit_provenance)) list() else list(fit = fit_provenance)
  if (!is.null(fit_provenance)) {
    association_input <- fit_provenance$input
  } else {
    source_data <- if (!is.null(fit$input_data)) fit$input_data else fit$data
    source_rows <- if (is.data.frame(source_data) && nrow(source_data) >= max(score_row_ids))
      score_row_ids else seq_len(nrow(scores))
    association_input <- .cssem_provenance_input_summary(source_data, retained_rows = source_rows)
  }
  association_input$n_retained <- as.integer(length(score_row_ids))
  association_input$row_positions <- as.integer(score_row_ids)
  association_input$split_ids <- list(structural_fold = as.integer(folds))
  if (!is.null(fit$measurement_split$assignment) && !is.null(fit$measurement_split$row_ids)) {
    measurement_rows <- match(score_row_ids, fit$measurement_split$row_ids)
    if (!anyNA(measurement_rows))
      association_input$split_ids$measurement_fold <-
        as.integer(fit$measurement_split$assignment[measurement_rows])
  }
  association_provenance <- .cssem_provenance_record("associate",
    .cssem_provenance_call(associate_call, c("fit", "structure", "constraints")),
    settings = list(model = .cssem_provenance_model_specification(fit$model),
      structure = .cssem_provenance_structure_specification(structure),
      options = .cssem_provenance_association_settings(association_settings)),
    input = association_input, parent = parent, packages = c("MASS", "rpart", "splines"))
  structure(list(structure = structure, response_families = response_families, fit = fit, candidate_metrics = do.call(rbind, candidates), effects = do.call(rbind, effects),
    contributions = do.call(rbind, contributions), predictions = predictions, specification_gap = do.call(rbind, gaps), full_models = models,
    corrected_effects = corrected_table, numerical_diagnostics = .structural_numerical_diagnostics(corrected_table),
    reliability = reliability_vec, eiv_bootstrap = eiv_bootstrap,
    respondent_weighting = respondent_weighting, level = level, scores = scores,
    constraints = constraints, constraint_diagnostics = constraint_diagnostics,
    # Retain the resolved declaration and controls so update.cssem_association()
    # can rebuild the association through the public associate() contract.
    association_settings = association_settings,
    row_ids = score_row_ids, missing_policy = missing_policy,
    folds = folds, structural_repeats = structural_repeats, temporal_order = temporal_order, shadow_scope = scopes,
    provenance_record = association_provenance,
    status = "associational"), class = "cssem_association")
}

#' Return a declared effect's associational evidence card
#' @param association A `cssem_association` object.
#' @param outcome Declared endogenous construct name.
#' @return A list of selected effects, candidates, contributions, and shadow gaps.
#' @export
effect_card <- function(association, outcome) {
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
effect_ledger <- function(association) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  selected <- association$candidate_metrics[association$candidate_metrics$selected, c("outcome", "predictor", "shape", "r_squared", "mean_mse_improvement", "mse_improvement_se", "selection_frequency", "nonlinearity_p"), drop = FALSE]
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
specification_gap <- function(association, scope = NULL) {
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
