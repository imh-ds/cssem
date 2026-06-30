# CS-SEM associational mediation: simulation-based path propagation over the
# declared structural DAG. The engine generalizes single, parallel, serial, and
# mixed mediation by enumerating directed paths from the predictor to the
# outcome and isolating each effect through edge-masked forward propagation.
#
# This file currently implements the propagation engine and decomposition
# (development step 1). Disattenuation (step 2) and bootstrap intervals (step 3)
# build on these internals.

.edge <- function(from, to) paste(from, to, sep = "->")

.path_edges <- function(path) {
  if (length(path) < 2L) return(character(0))
  .edge(path[-length(path)], path[-1L])
}

# Forward adjacency implied by a structure: an edge points from each declared
# predictor to its outcome.
.forward_edges <- function(structure) {
  adjacency <- list()
  for (outcome in names(structure$effects)) {
    for (predictor in names(structure$effects[[outcome]])) {
      adjacency[[predictor]] <- c(adjacency[[predictor]], outcome)
    }
  }
  adjacency
}

# Enumerate every simple directed path from x to y. The declared graph is
# acyclic, so the recursion terminates; each returned element is the node
# sequence of one path (including both endpoints).
.structure_paths <- function(structure, x, y) {
  adjacency <- .forward_edges(structure)
  paths <- list()
  walk <- function(node, trail) {
    if (identical(node, y)) {
      paths[[length(paths) + 1L]] <<- trail
      return(invisible())
    }
    for (child in adjacency[[node]]) {
      if (!child %in% trail) walk(child, c(trail, child))
    }
  }
  walk(x, x)
  paths
}

# Full edge set carried by a set of fitted outcome models.
.model_edges <- function(models) {
  edges <- character(0)
  for (outcome in names(models)) {
    if (!is.null(models[[outcome]])) edges <- c(edges, .edge(names(models[[outcome]]$shapes), outcome))
  }
  unique(edges)
}

# Predicted outcome vector after shifting x by delta and propagating the shift
# forward only along the active edges. A node sees a parent's shifted value when
# the parent->node edge is active and its observed value otherwise, so the active
# set selects which structural paths transmit the shift.
.propagate_y <- function(models, scores, order, x, y, delta, active) {
  baseline <- as.list(scores)
  shifted <- baseline
  shifted[[x]] <- baseline[[x]] + delta
  for (node in order) {
    model <- models[[node]]
    if (is.null(model)) next
    predictors <- names(model$shapes)
    frame <- as.data.frame(stats::setNames(lapply(predictors, function(predictor) {
      if (.edge(predictor, node) %in% active) shifted[[predictor]] else baseline[[predictor]]
    }), predictors), stringsAsFactors = FALSE)
    shifted[[node]] <- .predict_shape_model(model, frame)
  }
  shifted[[y]]
}

# Average effect transmitted along a chosen active edge set, expressed per unit
# of delta. Differencing against the all-observed prediction cancels the model
# intercept; averaging over respondents recovers the coefficient-product
# decomposition for linear edges and the average mediated effect otherwise.
.mediation_effect <- function(models, scores, order, x, y, delta, active, baseline_y) {
  shifted_y <- .propagate_y(models, scores, order, x, y, delta, active)
  mean(shifted_y - baseline_y, na.rm = TRUE) / delta
}

# Raw total, direct, total-indirect, and per-path effects for one model set.
# The per-path vector is aligned with `paths`; direct paths (length two) are
# returned as NA there and summarized separately.
.decompose_effects <- function(models, scores, order, x, y, paths, delta) {
  baseline_y <- .propagate_y(models, scores, order, x, y, 0, character(0))
  all_edges <- .model_edges(models)
  effect <- function(active) .mediation_effect(models, scores, order, x, y, delta, active, baseline_y)
  total <- effect(all_edges)
  direct <- if (.edge(x, y) %in% all_edges) effect(.edge(x, y)) else 0
  path_eff <- vapply(paths, function(path) if (length(path) > 2L) effect(.path_edges(path)) else NA_real_, numeric(1))
  list(total = total, direct = direct, indirect_total = total - direct, path_eff = path_eff)
}

# A purely linear model object compatible with .predict_shape_model(), built from
# named edge slopes. The intercept is irrelevant because every reported effect is
# a difference of predictions.
.linear_model_from_slopes <- function(predictors, slopes) {
  list(
    shapes = stats::setNames(rep("linear", length(predictors)), predictors),
    infos = stats::setNames(lapply(predictors, function(predictor) list(shape = "linear")), predictors),
    coefficient = c(0, slopes[predictors])
  )
}

# Replace each outcome model with a linear model carrying the errors-in-variables
# disattenuated slope for every linear/monotone edge. Smooth edges (and edges
# without a usable correction) keep their naive slope and are marked ineligible,
# so any path passing through them is reported without disattenuation.
.corrected_models <- function(models, scores, reliability) {
  corrected <- vector("list", length(models)); names(corrected) <- names(models)
  eligible <- logical()
  for (outcome in names(models)) {
    model <- models[[outcome]]; if (is.null(model)) next
    predictors <- names(model$shapes)
    coefficients <- .eiv_coefficients(scores, outcome, predictors, reliability)
    slopes <- vapply(predictors, function(predictor) {
      linear_or_monotone <- model$shapes[[predictor]] %in% c("linear", "monotone_increasing", "monotone_decreasing")
      usable <- linear_or_monotone && is.finite(coefficients$corrected[[predictor]])
      eligible[[.edge(predictor, outcome)]] <<- usable
      if (usable) unname(coefficients$corrected[[predictor]]) else unname(coefficients$naive[[predictor]])
    }, numeric(1))
    corrected[[outcome]] <- .linear_model_from_slopes(predictors, stats::setNames(slopes, predictors))
  }
  list(models = corrected, eligible = eligible)
}

# Assemble the naive (and, when reliability is supplied, disattenuated)
# decomposition into reporting tables. A path's disattenuated effect is reported
# only when every edge it traverses is correction-eligible.
.assemble_mediation <- function(paths, naive, disattenuated, eligible, x, y) {
  path_ok <- function(path) if (is.null(eligible)) NA else isTRUE(all(eligible[.path_edges(path)]))
  mediating <- which(vapply(paths, length, integer(1)) > 2L)
  direct_ok <- if (is.null(eligible)) NA else isTRUE(eligible[[.edge(x, y)]])
  paths_ok <- if (is.null(eligible)) NA else all(vapply(paths[mediating], path_ok, logical(1)))

  summary <- data.frame(
    component = c("total", "direct", "indirect_total"),
    naive_effect = c(naive$total, naive$direct, naive$indirect_total),
    disattenuated_effect = if (is.null(disattenuated)) NA_real_ else c(
      if (isTRUE(direct_ok) && isTRUE(paths_ok)) disattenuated$total else NA_real_,
      if (isTRUE(direct_ok)) disattenuated$direct else NA_real_,
      if (isTRUE(paths_ok)) disattenuated$indirect_total else NA_real_
    ),
    stringsAsFactors = FALSE
  )

  path_rows <- lapply(mediating, function(j) {
    path <- paths[[j]]; usable <- path_ok(path)
    data.frame(
      path = paste(path, collapse = " -> "),
      mediators = paste(path[-c(1L, length(path))], collapse = ", "),
      naive_effect = naive$path_eff[[j]],
      disattenuated_effect = if (!is.null(disattenuated) && isTRUE(usable)) disattenuated$path_eff[[j]] else NA_real_,
      disattenuated = usable,
      stringsAsFactors = FALSE
    )
  })
  path_specific <- if (length(path_rows)) do.call(rbind, path_rows) else
    data.frame(path = character(0), mediators = character(0), naive_effect = numeric(0),
      disattenuated_effect = numeric(0), disattenuated = logical(0))

  list(
    summary = summary,
    path_specific = path_specific,
    proportion_mediated = if (is.finite(naive$total) && abs(naive$total) > 1e-8) naive$indirect_total / naive$total else NA_real_,
    path_sum_residual = naive$indirect_total - sum(naive$path_eff[mediating])
  )
}

# Internal entry point for development steps 1-2: decompose the effect of x on y,
# adding errors-in-variables disattenuated effects when per-construct reliability
# is supplied. Public cssem_mediation() (bootstrap intervals and reporting) is
# added in step 3.
.cssem_mediation_core <- function(models, scores, structure, x, y, reliability = NULL, delta = 1) {
  order <- .resolve_temporal_order(structure, names(scores))
  if (match(x, order) >= match(y, order)) stop("x must precede y in the temporal order.", call. = FALSE)
  paths <- .structure_paths(structure, x, y)
  if (!length(paths)) stop("No directed path connects x to y in the declared structure.", call. = FALSE)
  naive <- .decompose_effects(models, scores, order, x, y, paths, delta)
  disattenuated <- NULL; eligible <- NULL
  if (!is.null(reliability)) {
    corrected <- .corrected_models(models, scores, reliability)
    disattenuated <- .decompose_effects(corrected$models, scores, order, x, y, paths, delta)
    eligible <- corrected$eligible
  }
  .assemble_mediation(paths, naive, disattenuated, eligible, x, y)
}
