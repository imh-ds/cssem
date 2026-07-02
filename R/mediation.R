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
    constructs <- unique(unlist(lapply(names(structure$effects[[outcome]]), .predictor_constructs), use.names = FALSE))
    for (construct in constructs) adjacency[[construct]] <- c(adjacency[[construct]], outcome)
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
    if (!is.null(models[[outcome]])) {
      constructs <- unique(unlist(lapply(names(models[[outcome]]$shapes), .predictor_constructs), use.names = FALSE))
      edges <- c(edges, .edge(constructs, outcome))
    }
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
    # Build the frame over the model's input constructs (interaction predictors
    # contribute their constituents) so edge-masking applies per construct.
    constructs <- unique(unlist(lapply(names(model$shapes), .predictor_constructs), use.names = FALSE))
    frame <- as.data.frame(stats::setNames(lapply(constructs, function(construct) {
      if (.edge(construct, node) %in% active) shifted[[construct]] else baseline[[construct]]
    }), constructs), stringsAsFactors = FALSE)
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
    shapes = stats::setNames(vapply(predictors, function(p) if (.is_interaction(p)) "product" else "linear", character(1)), predictors),
    infos = stats::setNames(lapply(predictors, function(p)
      if (.is_interaction(p)) list(shape = "product", terms = .interaction_terms(p)) else list(shape = "linear")), predictors),
    coefficient = c(0, slopes[predictors])
  )
}

# Replace each outcome model with a linear model carrying the errors-in-variables
# disattenuated slope for every linear/monotone edge. Smooth edges (and edges
# without a usable correction) keep their naive slope and are marked ineligible,
# so any path passing through them is reported without disattenuation.
.corrected_models <- function(models, scores, reliability, posterior_var = NULL) {
  corrected <- vector("list", length(models)); names(corrected) <- names(models)
  eligible <- logical(); stable <- logical()
  for (outcome in names(models)) {
    model <- models[[outcome]]; if (is.null(model)) next
    predictors <- names(model$shapes)
    coefficients <- .eiv_coefficients(scores, outcome, predictors, reliability, posterior_var = posterior_var)
    slopes <- vapply(predictors, function(predictor) {
      linear_or_monotone <- model$shapes[[predictor]] %in% c("linear", "monotone_increasing", "monotone_decreasing")
      usable <- linear_or_monotone && is.finite(coefficients$corrected[[predictor]])
      eligible[[.edge(predictor, outcome)]] <<- usable
      # An edge's correction is stable only when it was usable and the adaptive
      # regularization did not have to limit the outcome model's correction.
      stable[[.edge(predictor, outcome)]] <<- usable && isTRUE(coefficients$stable)
      if (usable) unname(coefficients$corrected[[predictor]]) else unname(coefficients$naive[[predictor]])
    }, numeric(1))
    corrected[[outcome]] <- .linear_model_from_slopes(predictors, stats::setNames(slopes, predictors))
  }
  list(models = corrected, eligible = eligible, stable = stable)
}

# Assemble the naive (and, when reliability is supplied, disattenuated)
# decomposition into reporting tables. A path's disattenuated effect is reported
# only when every edge it traverses is correction-eligible.
.assemble_mediation <- function(paths, naive, disattenuated, eligible, stable, reliability, x, y) {
  path_ok <- function(path) if (is.null(eligible)) NA else isTRUE(all(eligible[.path_edges(path)]))
  path_stable <- function(path) if (is.null(stable)) NA else isTRUE(all(stable[.path_edges(path)]))
  path_min_reliability <- function(path) {
    if (is.null(reliability)) return(NA_real_)
    values <- reliability[intersect(path, names(reliability))]
    if (!length(values) || all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
  }
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
      disattenuation_stable = path_stable(path),
      min_reliability = path_min_reliability(path),
      stringsAsFactors = FALSE
    )
  })
  path_specific <- if (length(path_rows)) do.call(rbind, path_rows) else
    data.frame(path = character(0), mediators = character(0), naive_effect = numeric(0),
      disattenuated_effect = numeric(0), disattenuated = logical(0),
      disattenuation_stable = logical(0), min_reliability = numeric(0))

  list(
    summary = summary,
    path_specific = path_specific,
    proportion_mediated = if (is.finite(naive$total) && abs(naive$total) > 1e-8) naive$indirect_total / naive$total else NA_real_,
    path_sum_residual = naive$indirect_total - sum(naive$path_eff[mediating])
  )
}

# Refit one shape model per endogenous construct with shapes held fixed. Used by
# the bootstrap so model selection is conditioned on and only the coefficients
# vary across resamples.
.refit_models <- function(scores, shapes_by_outcome) {
  refit <- vector("list", length(shapes_by_outcome)); names(refit) <- names(shapes_by_outcome)
  for (outcome in names(shapes_by_outcome)) {
    shapes <- shapes_by_outcome[[outcome]]
    if (!is.null(shapes)) refit[[outcome]] <- .fit_shape_model(scores, outcome, shapes)
  }
  refit
}

# Percentile bootstrap of every decomposition quantity. Respondents are
# resampled, stage-model coefficients refit at the fixed shapes, and the naive
# (and, when reliability is supplied, disattenuated) effects recomputed. The
# reliability inputs are held fixed at their estimates.
.add_mediation_intervals <- function(out, models, scores, order, paths, x, y,
                                      reliability, delta, replicates, seed) {
  shapes_by_outcome <- lapply(models, function(model) if (is.null(model)) NULL else model$shapes)
  mediating <- which(vapply(paths, length, integer(1)) > 2L)
  n <- nrow(scores)
  naive_summary <- matrix(NA_real_, replicates, 3L)
  dis_summary <- matrix(NA_real_, replicates, 3L)
  naive_path <- matrix(NA_real_, replicates, length(mediating))
  dis_path <- matrix(NA_real_, replicates, length(mediating))
  set.seed(seed + 808L)
  for (b in seq_len(replicates)) {
    resampled <- scores[sample.int(n, n, replace = TRUE), , drop = FALSE]
    refit <- .refit_models(resampled, shapes_by_outcome)
    naive <- .decompose_effects(refit, resampled, order, x, y, paths, delta)
    naive_summary[b, ] <- c(naive$total, naive$direct, naive$indirect_total)
    naive_path[b, ] <- naive$path_eff[mediating]
    if (!is.null(reliability)) {
      corrected <- .corrected_models(refit, resampled, reliability)
      dis <- .decompose_effects(corrected$models, resampled, order, x, y, paths, delta)
      dis_summary[b, ] <- c(dis$total, dis$direct, dis$indirect_total)
      dis_path[b, ] <- dis$path_eff[mediating]
    }
  }
  ci <- function(m) t(apply(m, 2L, function(column) {
    column <- column[is.finite(column)]
    if (!length(column)) c(NA_real_, NA_real_) else stats::quantile(column, c(.025, .975), names = FALSE)
  }))
  mask <- function(bounds, point) ifelse(is.na(point), NA_real_, bounds)

  ns <- ci(naive_summary)
  out$summary$naive_ci_low <- ns[, 1L]; out$summary$naive_ci_high <- ns[, 2L]
  if (!is.null(reliability)) {
    ds <- ci(dis_summary)
    out$summary$disattenuated_ci_low <- mask(ds[, 1L], out$summary$disattenuated_effect)
    out$summary$disattenuated_ci_high <- mask(ds[, 2L], out$summary$disattenuated_effect)
  }
  if (length(mediating)) {
    np <- ci(naive_path)
    out$path_specific$naive_ci_low <- np[, 1L]; out$path_specific$naive_ci_high <- np[, 2L]
    if (!is.null(reliability)) {
      dp <- ci(dis_path)
      out$path_specific$disattenuated_ci_low <- mask(dp[, 1L], out$path_specific$disattenuated_effect)
      out$path_specific$disattenuated_ci_high <- mask(dp[, 2L], out$path_specific$disattenuated_effect)
    }
  }
  out
}

# Internal entry point for development steps 1-3: decompose the effect of x on y,
# add errors-in-variables disattenuated effects when per-construct reliability is
# supplied, and attach percentile bootstrap intervals when requested. Public
# cssem_mediation() (validation and reporting) is added in step 4.
.cssem_mediation_core <- function(models, scores, structure, x, y, reliability = NULL,
                                  delta = 1, eiv_bootstrap = 0L, seed = 1L) {
  order <- .resolve_temporal_order(structure, names(scores))
  if (match(x, order) >= match(y, order)) stop("x must precede y in the temporal order.", call. = FALSE)
  paths <- .structure_paths(structure, x, y)
  if (!length(paths)) stop("No directed path connects x to y in the declared structure.", call. = FALSE)
  naive <- .decompose_effects(models, scores, order, x, y, paths, delta)
  disattenuated <- NULL; eligible <- NULL; stable <- NULL
  if (!is.null(reliability)) {
    corrected <- .corrected_models(models, scores, reliability)
    disattenuated <- .decompose_effects(corrected$models, scores, order, x, y, paths, delta)
    eligible <- corrected$eligible; stable <- corrected$stable
  }
  out <- .assemble_mediation(paths, naive, disattenuated, eligible, stable, reliability, x, y)
  if (as.integer(eiv_bootstrap) > 0L) {
    out <- .add_mediation_intervals(out, models, scores, order, paths, x, y,
      reliability, delta, as.integer(eiv_bootstrap), seed)
  }
  out
}

# Choose the reported effect and interval for each row: the disattenuated value
# when disattenuation was requested and available for that row, otherwise naive.
.mediation_reported <- function(table, disattenuated) {
  use <- disattenuated & !is.na(table$disattenuated_effect)
  table$reported_effect <- ifelse(use, table$disattenuated_effect, table$naive_effect)
  table$reported_ci_low <- if ("naive_ci_low" %in% names(table))
    ifelse(use, table$disattenuated_ci_low, table$naive_ci_low) else NA_real_
  table$reported_ci_high <- if ("naive_ci_high" %in% names(table))
    ifelse(use, table$disattenuated_ci_high, table$naive_ci_high) else NA_real_
  table$basis <- ifelse(use, "disattenuated", "naive")
  table
}

#' Decompose an associational mediation effect on locked construct states
#'
#' Decomposes the effect of `x` on `y` into total, direct, and indirect
#' components, plus one path-specific indirect effect for every directed path
#' from `x` to `y` declared in the association's structure. Single, parallel, and
#' serial mediation are handled uniformly: effects are obtained by simulating an
#' `x` shift and propagating it through the fitted construct-level effect models,
#' so the decomposition is correct for nonlinear edges, not only linear paths.
#'
#' Linear and monotone edges are disattenuated with the same errors-in-variables
#' correction used by [cssem_associate()], recovering the latent-scale indirect
#' effect that measurement error attenuates (the bias is largest for indirect
#' effects, which compound error across paths). Any path through a smooth edge is
#' reported without disattenuation. The decomposition is associational: it
#' requires the structure's declared temporal order and makes no causal claim.
#'
#' @param association A `cssem_association` from [cssem_associate()].
#' @param x Name of the locked predictor construct.
#' @param y Name of the locked outcome construct.
#' @param mediators Optional character vector restricting the displayed
#'   path-specific effects to paths whose mediators all lie in this set. Total,
#'   direct, and indirect effects always reflect the full declared structure.
#' @param disattenuate Whether to report errors-in-variables disattenuated
#'   effects when the association carries reliability. Defaults to `TRUE`.
#' @param eiv_bootstrap Number of percentile-bootstrap resamples for the
#'   intervals. Zero (default) omits intervals.
#' @param delta Size of the `x` contrast, in standard-deviation units of the
#'   standardized construct states. Defaults to one. Effects are reported per
#'   `delta`; for linear models they are scale-invariant.
#' @param seed Bootstrap seed.
#' @return An object of class `cssem_mediation`.
#' @examples
#' # association <- cssem_associate(fit, structure)
#' # cssem_mediation(association, x = "Trust", y = "Loyalty", eiv_bootstrap = 200)
#' @export
cssem_mediation <- function(association, x, y, mediators = NULL, disattenuate = TRUE,
                            eiv_bootstrap = 0L, delta = 1, seed = 1L) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  scores <- association$scores
  if (is.null(scores)) stop("association does not carry locked scores; re-run cssem_associate().", call. = FALSE)
  all_names <- names(scores)
  if (length(x) != 1L || length(y) != 1L || !all(c(x, y) %in% all_names)) stop("x and y must be locked construct names.", call. = FALSE)
  if (identical(x, y)) stop("x and y must differ.", call. = FALSE)
  if (!is.null(mediators) && !all(mediators %in% all_names)) stop("mediators must be locked construct names.", call. = FALSE)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)
  if (!is.numeric(delta) || length(delta) != 1L || !is.finite(delta) || delta == 0) stop("delta must be a non-zero numeric scalar.", call. = FALSE)

  models <- stats::setNames(vector("list", length(all_names)), all_names)
  for (outcome in names(association$full_models)) models[[outcome]] <- association$full_models[[outcome]]

  reliability <- NULL
  if (isTRUE(disattenuate)) {
    reliability <- association$reliability
    if (is.null(reliability) || all(is.na(reliability))) reliability <- NULL
  }

  core <- .cssem_mediation_core(models, scores, association$structure, x, y,
    reliability = reliability, delta = delta, eiv_bootstrap = eiv_bootstrap, seed = seed)

  if (!is.null(mediators) && nrow(core$path_specific)) {
    keep <- vapply(strsplit(core$path_specific$mediators, ", ", fixed = TRUE),
      function(path_mediators) all(path_mediators %in% mediators), logical(1))
    core$path_specific <- core$path_specific[keep, , drop = FALSE]
    if (!nrow(core$path_specific)) stop("No mediating path passes only through the requested mediators.", call. = FALSE)
  }

  structure(c(core, list(x = x, y = y, n = nrow(scores), delta = delta,
    disattenuated = !is.null(reliability), bootstrap = eiv_bootstrap, status = "associational")),
    class = "cssem_mediation")
}

#' Return a tidy mediation effect ledger
#'
#' @param mediation A `cssem_mediation` object.
#' @return A data frame with one row per total, direct, indirect, and
#'   path-specific effect, carrying the reported effect, interval, estimation
#'   basis, and associational status.
#' @examples
#' # cssem_mediation_ledger(mediation)
#' @export
cssem_mediation_ledger <- function(mediation) {
  if (!inherits(mediation, "cssem_mediation")) stop("mediation must be a cssem_mediation.", call. = FALSE)
  summary <- .mediation_reported(mediation$summary, isTRUE(mediation$disattenuated))
  ledger <- data.frame(component = summary$component, path = NA_character_,
    effect = summary$reported_effect, ci_low = summary$reported_ci_low, ci_high = summary$reported_ci_high,
    basis = summary$basis, stringsAsFactors = FALSE)
  if (nrow(mediation$path_specific)) {
    paths <- .mediation_reported(mediation$path_specific, isTRUE(mediation$disattenuated))
    ledger <- rbind(ledger, data.frame(component = "path_indirect", path = paths$path,
      effect = paths$reported_effect, ci_low = paths$reported_ci_low, ci_high = paths$reported_ci_high,
      basis = paths$basis, stringsAsFactors = FALSE))
  }
  ledger$status <- "associational"
  ledger
}

#' Print an associational CS-SEM mediation decomposition
#'
#' @param x A `cssem_mediation` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_mediation <- function(x, ...) {
  paths <- nrow(x$path_specific)
  cat(sprintf("CS-SEM associational mediation: %s -> %s  (n = %d, %d mediating path%s)\n",
    x$x, x$y, x$n, paths, if (paths == 1L) "" else "s"))
  basis <- if (isTRUE(x$disattenuated)) "disattenuated (errors-in-variables)" else "naive (not disattenuated)"
  intervals <- if (x$bootstrap > 0L) sprintf("; 95%% bootstrap intervals (%d resamples)", x$bootstrap) else ""
  cat("Effects are ", basis, intervals, ".\n\n", sep = "")
  format_effect <- function(effect, low, high) if (is.na(low)) sprintf("% .3f", effect) else
    sprintf("% .3f  [% .3f, % .3f]", effect, low, high)
  labels <- c(total = "total effect", direct = "direct effect", indirect_total = "indirect effect")
  summary <- .mediation_reported(x$summary, isTRUE(x$disattenuated))
  for (i in seq_len(nrow(summary))) {
    cat(sprintf("  %-16s %s\n", labels[[summary$component[i]]],
      format_effect(summary$reported_effect[i], summary$reported_ci_low[i], summary$reported_ci_high[i])))
  }
  if (is.finite(x$proportion_mediated)) cat(sprintf("  %-16s %.3f\n", "prop. mediated", x$proportion_mediated))
  limited <- FALSE
  if (paths) {
    cat("\n  path-specific indirect effects:\n")
    rows <- .mediation_reported(x$path_specific, isTRUE(x$disattenuated))
    for (i in seq_len(nrow(rows))) {
      # Flag a path when the correction was numerically regularized, or when a
      # construct it passes through has low reliability (where disattenuation is
      # incomplete and intervals can under-cover even if numerically stable).
      low_reliability <- is.finite(rows$min_reliability[i]) && rows$min_reliability[i] < 0.5
      flag <- if (isTRUE(x$disattenuated) && !isTRUE(rows$disattenuated[i])) {
        "  (smooth edge: not disattenuated)"
      } else if (isTRUE(x$disattenuated) && (identical(rows$disattenuation_stable[i], FALSE) || low_reliability)) {
        limited <- TRUE
        sprintf("  (limited: reliability %.2f)", rows$min_reliability[i])
      } else ""
      cat(sprintf("  %-26s %s%s\n", rows$path[i],
        format_effect(rows$reported_effect[i], rows$reported_ci_low[i], rows$reported_ci_high[i]), flag))
    }
  }
  if (limited) cat("\nSome corrections were limited by low construct reliability; those intervals can under-cover. Interpret the flagged effects with caution.\n")
  cat("\nAssociational decomposition under the declared temporal order; not a causal mediation claim.\n")
  invisible(x)
}
