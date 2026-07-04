# Interventional (causal) mediation. This elevates the disattenuated,
# edge-masked g-computation of R/mediation.R to a causal estimand under a
# declared adjustment set and (for a causal label) a declared temporal order,
# with a causal-admissibility panel. The point estimate reuses the associational
# engine: for additive mediator models the disattenuated edge-masked indirect
# effect is the interventional indirect effect, so the two agree numerically. The
# causal content is the discipline layer -- adjustment-set gating, a
# post-treatment-adjustment guard, overlap / nuisance / sensitivity diagnostics,
# and an explicit causal-admissibility label -- exactly as cssem_causal_effect()
# adds to a single structural coefficient. Exposure-induced mediator-outcome
# confounding and mediator interactions are not yet handled and are flagged.

# Every construct reachable from a node by following declared forward edges (its
# descendants), used to reject adjusting for post-treatment constructs.
.descendants <- function(structure, node) {
  adjacency <- .forward_edges(structure)
  seen <- character(0); frontier <- adjacency[[node]]
  while (length(frontier)) {
    current <- frontier[[1L]]; frontier <- frontier[-1L]
    if (!current %in% seen) { seen <- c(seen, current); frontier <- c(frontier, adjacency[[current]]) }
  }
  seen
}

# Constituent constructs declared as predictors of an outcome (interaction terms
# contribute their constituents).
.declared_predictors <- function(structure, outcome) {
  predictors <- names(structure$effects[[outcome]])
  if (is.null(predictors)) return(character(0))
  unique(unlist(lapply(predictors, .predictor_constructs), use.names = FALSE))
}

# Cinelli-Hazlett robustness value for the mediator -> outcome (b) path of each
# mediator that is a direct predictor of the outcome, taken from a linear outcome
# model on the locked scores. The minimum across mediators is the weakest link.
.mediation_robustness <- function(scores, y, y_predictors, mediators) {
  direct <- intersect(mediators, y_predictors)
  if (!length(direct)) return(NA_real_)
  model <- stats::lm(stats::reformulate(y_predictors, y), scores)
  table <- summary(model)$coefficients
  values <- vapply(direct, function(m)
    if (m %in% rownames(table)) .robustness_value(table[m, "t value"], model$df.residual) else NA_real_, numeric(1))
  if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
}

.mediation_admissibility <- function(scores, structure, x, y, adjust, mediators, reliability) {
  # Identification: residual treatment variation after the adjustment set.
  treatment_r2 <- summary(stats::lm(stats::reformulate(adjust, x), scores))$r.squared
  identification_strength <- 1 - treatment_r2
  # Nuisance quality: how well each stage model's declared predictors explain it.
  y_predictors <- .declared_predictors(structure, y)
  outcome_r2 <- summary(stats::lm(stats::reformulate(y_predictors, y), scores))$r.squared
  mediator_r2 <- vapply(mediators, function(m) {
    predictors <- .declared_predictors(structure, m)
    if (!length(predictors)) NA_real_ else summary(stats::lm(stats::reformulate(predictors, m), scores))$r.squared
  }, numeric(1))
  robustness_value <- .mediation_robustness(scores, y, y_predictors, mediators)
  path_constructs <- unique(c(x, mediators, y, adjust))
  values <- if (is.null(reliability)) NA_real_ else reliability[intersect(path_constructs, names(reliability))]
  min_reliability <- if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
  list(identification_strength = identification_strength, treatment_r2 = treatment_r2,
    outcome_r2 = outcome_r2, mediator_r2_min = if (all(is.na(mediator_r2))) NA_real_ else min(mediator_r2, na.rm = TRUE),
    robustness_value = robustness_value, min_path_reliability = min_reliability)
}

#' Estimate an interventional (causal) mediation effect on locked construct states
#'
#' Elevates the disattenuated mediation decomposition of [cssem_mediation()] to a
#' causal estimand. The effect of `x` on `y` is decomposed into interventional
#' total, direct, and indirect effects by simulating an `x` shift and propagating
#' it through the disattenuated construct-level models while holding the declared
#' `adjust` set fixed, so the indirect effect is adjusted for the confounders you
#' declare. For additive mediator models this g-computed indirect effect equals
#' the interventional indirect effect.
#'
#' Unlike [cssem_mediation()], this estimator enforces causal discipline: it
#' requires a non-empty adjustment set, requires those constructs to be declared
#' predictors of the outcome and of each mediator (so every stage model conditions
#' on them), refuses to adjust for any construct that is downstream of the
#' treatment (a post-treatment adjustment would bias the effect), and reports a
#' causal-admissibility panel. A `causal_under_assumptions` label additionally
#' requires a declared `temporal_order`; otherwise the effect is reported as an
#' adjusted association. Exposure-induced mediator-outcome confounding and
#' mediator interactions are not yet modeled.
#'
#' @param association A `cssem_association` from [cssem_associate()].
#' @param x Name of the locked treatment construct.
#' @param y Name of the locked outcome construct.
#' @param adjust Character vector of adjustment-set (confounder) construct names.
#'   Must be declared predictors of the outcome and of each mediator.
#' @param mediators Optional character vector of mediator constructs. Defaults to
#'   every intermediate construct on a declared directed path from `x` to `y`.
#' @param temporal_order Optional character vector asserting the temporal order of
#'   the constructs; required for a `causal_under_assumptions` label.
#' @param estimand Causal estimand. Currently `"interventional"`.
#' @param disattenuate Whether to disattenuate using the association reliability.
#'   Defaults to `TRUE`.
#' @param eiv_bootstrap Number of percentile-bootstrap resamples for the
#'   intervals. Zero (default) omits intervals.
#' @param delta Size of the `x` contrast in standardized units. Defaults to one.
#' @param seed Bootstrap seed.
#' @return An object of class `cssem_causal_mediation`.
#' @examples
#' # cssem_causal_mediation(association, x = "Satisfaction", y = "Loyalty",
#' #   adjust = "Trust", temporal_order = c("Trust", "Satisfaction", "Commitment", "Loyalty"))
#' @export
cssem_causal_mediation <- function(association, x, y, adjust, mediators = NULL,
                                   temporal_order = NULL, estimand = c("interventional"),
                                   disattenuate = TRUE, eiv_bootstrap = 0L, delta = 1, seed = 1L) {
  estimand <- match.arg(estimand)
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  scores <- association$scores
  if (is.null(scores)) stop("association does not carry locked scores; re-run cssem_associate().", call. = FALSE)
  all_names <- names(scores)
  if (length(x) != 1L || length(y) != 1L || !all(c(x, y) %in% all_names)) stop("x and y must be locked construct names.", call. = FALSE)
  if (identical(x, y)) stop("x and y must differ.", call. = FALSE)
  if (missing(adjust) || !length(adjust)) stop("cssem_causal_mediation() requires a non-empty adjustment set; use cssem_mediation() for an associational decomposition.", call. = FALSE)
  if (!all(adjust %in% all_names)) stop("adjust must be locked construct names.", call. = FALSE)
  if (any(adjust %in% c(x, y))) stop("adjust must be distinct from x and y.", call. = FALSE)
  if (!is.null(mediators) && !all(mediators %in% all_names)) stop("mediators must be locked construct names.", call. = FALSE)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)
  if (!is.numeric(delta) || length(delta) != 1L || !is.finite(delta) || delta == 0) stop("delta must be a non-zero numeric scalar.", call. = FALSE)

  structure <- association$structure
  paths <- .structure_paths(structure, x, y)
  if (!length(paths)) stop("No directed path connects x to y in the declared structure.", call. = FALSE)
  path_mediators <- unique(unlist(lapply(paths, function(path)
    if (length(path) > 2L) path[-c(1L, length(path))] else character(0)), use.names = FALSE))
  if (is.null(mediators)) {
    mediators <- path_mediators
  } else if (!all(mediators %in% path_mediators)) {
    stop("mediators must be intermediate constructs on a declared x -> y path.", call. = FALSE)
  }
  if (!length(mediators)) stop("No mediating path connects x to y; there is no indirect effect to route.", call. = FALSE)

  # Discipline: never adjust for a construct downstream of the treatment.
  offenders <- intersect(adjust, .descendants(structure, x))
  if (length(offenders)) stop(sprintf("Cannot adjust for %s: downstream of the treatment %s (post-treatment adjustment biases the mediation effect).",
    paste(offenders, collapse = ", "), x), call. = FALSE)

  # Discipline: every confounder must be a declared predictor of the outcome and
  # of each mediator, so every stage model conditions on it.
  needs <- c(list(y), as.list(mediators))
  for (node in needs) {
    missing_adjust <- setdiff(adjust, .declared_predictors(structure, node))
    if (length(missing_adjust)) stop(sprintf("Adjustment construct(s) %s must be declared as predictor(s) of %s to adjust the mediation effect for them.",
      paste(missing_adjust, collapse = ", "), node), call. = FALSE)
  }

  if (!is.null(temporal_order)) {
    constructs <- unique(c(x, y, mediators, adjust))
    if (!all(constructs %in% temporal_order)) stop("temporal_order must contain the treatment, outcome, mediator, and adjust constructs.", call. = FALSE)
    if (match(x, temporal_order) >= match(y, temporal_order)) stop("x must precede y in temporal_order.", call. = FALSE)
  }

  reliability <- NULL
  if (isTRUE(disattenuate)) {
    reliability <- association$reliability
    if (is.null(reliability) || all(is.na(reliability))) reliability <- NULL
  }

  models <- stats::setNames(vector("list", length(all_names)), all_names)
  for (outcome in names(association$full_models)) models[[outcome]] <- association$full_models[[outcome]]
  core <- .cssem_mediation_core(models, scores, structure, x, y,
    reliability = reliability, delta = delta, eiv_bootstrap = eiv_bootstrap, seed = seed)

  panel <- .mediation_admissibility(scores, structure, x, y, adjust, mediators, reliability)
  has_order <- !is.null(temporal_order)
  label <- if (has_order && panel$identification_strength >= .10) "causal_under_assumptions" else "adjusted_association"

  structure(c(core, list(x = x, y = y, adjust = adjust, mediators = mediators, estimand = estimand,
    n = nrow(scores), delta = delta, disattenuated = !is.null(reliability), bootstrap = eiv_bootstrap,
    temporal_order_declared = has_order, label = label, status = label), panel),
    class = "cssem_causal_mediation")
}

#' Print an interventional CS-SEM mediation decomposition
#'
#' @param x A `cssem_causal_mediation` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_causal_mediation <- function(x, ...) {
  cat(sprintf("CS-SEM interventional mediation: %s -> %s  (n = %d, mediator%s: %s)\n",
    x$x, x$y, x$n, if (length(x$mediators) == 1L) "" else "s", paste(x$mediators, collapse = ", ")))
  labels <- c(causal_under_assumptions = "Causal under assumptions",
    adjusted_association = "Adjusted association (not causal: no declared temporal order)")
  cat("Interpretation: ", labels[[x$label]], "\n", sep = "")
  cat("Adjustment set: ", paste(x$adjust, collapse = ", "), "\n", sep = "")
  basis <- if (isTRUE(x$disattenuated)) "disattenuated (errors-in-variables)" else "naive (not disattenuated)"
  intervals <- if (x$bootstrap > 0L) sprintf("; 95%% bootstrap intervals (%d resamples)", x$bootstrap) else ""
  cat("Effects are ", basis, intervals, ".\n\n", sep = "")
  format_effect <- function(effect, low, high) if (is.na(low)) sprintf("% .3f", effect) else
    sprintf("% .3f  [% .3f, % .3f]", effect, low, high)
  labels_eff <- c(total = "total effect", direct = "direct effect", indirect_total = "indirect effect")
  summary <- .mediation_reported(x$summary, isTRUE(x$disattenuated))
  for (i in seq_len(nrow(summary))) {
    cat(sprintf("  %-18s %s\n", labels_eff[[summary$component[i]]],
      format_effect(summary$reported_effect[i], summary$reported_ci_low[i], summary$reported_ci_high[i])))
  }
  if (is.finite(x$proportion_mediated)) cat(sprintf("  %-18s %.3f\n", "prop. mediated", x$proportion_mediated))

  cat("\n  Causal admissibility panel:\n")
  cat(sprintf("    identification strength  %.2f  (residual treatment variation after adjustment)\n", x$identification_strength))
  cat(sprintf("    outcome model R2         %.2f\n", x$outcome_r2))
  if (is.finite(x$mediator_r2_min)) cat(sprintf("    mediator model R2 (min)  %.2f\n", x$mediator_r2_min))
  if (is.finite(x$robustness_value)) cat(sprintf("    robustness value (b-path)%.2f  (an unmeasured mediator-outcome confounder explaining %.0f%% of residual variance in both would null the weakest indirect edge)\n",
    x$robustness_value, 100 * x$robustness_value))
  if (is.finite(x$min_path_reliability)) cat(sprintf("    min path reliability     %.2f\n", x$min_path_reliability))
  if (x$identification_strength < .10) cat("    WARNING: treatment is largely explained by the adjustment set; the effect is weakly identified.\n")

  cat("\nInterventional decomposition under declared assumptions; not a randomized-experiment guarantee.\n")
  cat("Point estimate equals the disattenuated g-computed indirect effect (exact for additive mediator models).\n")
  invisible(x)
}
