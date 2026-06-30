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

# Decompose the effect of x on y into total, direct, total-indirect, and one
# path-specific indirect effect per enumerated mediating path.
.mediation_decompose <- function(models, scores, order, x, y, paths, delta) {
  baseline_y <- .propagate_y(models, scores, order, x, y, 0, character(0))
  all_edges <- .model_edges(models)
  effect <- function(active) .mediation_effect(models, scores, order, x, y, delta, active, baseline_y)

  total <- effect(all_edges)
  direct_edge <- .edge(x, y)
  direct <- if (direct_edge %in% all_edges) effect(direct_edge) else 0
  total_indirect <- total - direct

  mediating <- Filter(function(path) length(path) > 2L, paths)
  path_rows <- lapply(mediating, function(path) {
    data.frame(
      path = paste(path, collapse = " -> "),
      mediators = paste(path[-c(1L, length(path))], collapse = ", "),
      effect = effect(.path_edges(path)),
      stringsAsFactors = FALSE
    )
  })
  path_specific <- if (length(path_rows)) do.call(rbind, path_rows) else
    data.frame(path = character(0), mediators = character(0), effect = numeric(0))

  list(
    summary = data.frame(
      component = c("total", "direct", "indirect_total"),
      effect = c(total, direct, total_indirect),
      stringsAsFactors = FALSE
    ),
    path_specific = path_specific,
    proportion_mediated = if (is.finite(total) && abs(total) > 1e-8) total_indirect / total else NA_real_,
    path_sum_residual = total_indirect - sum(path_specific$effect)
  )
}

# Internal entry point for step-1 validation: fit one linear/selected-shape model
# per endogenous construct on the path set and decompose. Public cssem_mediation()
# (with disattenuation, bootstrap, and reporting) is added in later steps.
.cssem_mediation_core <- function(models, scores, structure, x, y, delta = 1) {
  order <- .resolve_temporal_order(structure, names(scores))
  if (match(x, order) >= match(y, order)) stop("x must precede y in the temporal order.", call. = FALSE)
  paths <- .structure_paths(structure, x, y)
  if (!length(paths)) stop("No directed path connects x to y in the declared structure.", call. = FALSE)
  .mediation_decompose(models, scores, order, x, y, paths, delta)
}
