# Associational-to-causal estimation for a single declared edge. CS-SEM does not
# let a path be interpreted causally by default: a causal label requires a
# declared adjustment set and temporal order, and every effect ships with
# identification diagnostics and sensitivity analysis. The estimator is the
# errors-in-variables adjusted coefficient (disattenuated and confounding
# adjusted under a linear adjustment), which recovers the effect that naive
# regression leaves confounded and adjusted regression leaves attenuated.
# Flexible (nonlinear) confounder adjustment is a later extension.

# Cinelli-Hazlett robustness value: the minimum share of residual variance in
# both treatment and outcome that an unmeasured confounder would need to explain
# to drive the adjusted effect to zero.
.robustness_value <- function(t_value, df) {
  if (!is.finite(t_value) || !is.finite(df) || df <= 0) return(NA_real_)
  f <- abs(t_value) / sqrt(df)
  0.5 * (sqrt(f^4 + 4 * f^2) - f^2)
}

.eiv_adjusted <- function(scores, outcome, treatment, adjust, reliability) {
  coefficients <- .eiv_coefficients(scores, outcome, c(treatment, adjust), reliability)
  list(naive = unname(coefficients$naive[[treatment]]), corrected = unname(coefficients$corrected[[treatment]]),
       stable = isTRUE(coefficients$stable))
}

# Cross-fitted partially-linear DML: flexible spline nuisances remove nonlinear
# confounding (which a linear adjustment cannot), and the orthogonal score yields
# an analytic interval. Estimated on the denoised construct states; not
# disattenuated, since the correction over-corrects in the nonlinear setting.
.dml_partial_linear <- function(scores, outcome, treatment, adjust, folds, spline_df = 5L) {
  n <- nrow(scores)
  nuisance <- paste(sprintf("splines::ns(%s, df = %d)", adjust, spline_df), collapse = " + ")
  formula_x <- stats::as.formula(paste(treatment, "~", nuisance))
  formula_y <- stats::as.formula(paste(outcome, "~", nuisance))
  x_residual <- y_residual <- rep(NA_real_, n)
  for (fold in sort(unique(folds))) {
    train <- folds != fold; test <- folds == fold
    model_x <- stats::lm(formula_x, scores[train, , drop = FALSE])
    model_y <- stats::lm(formula_y, scores[train, , drop = FALSE])
    x_residual[test] <- scores[[treatment]][test] - stats::predict(model_x, scores[test, , drop = FALSE])
    y_residual[test] <- scores[[outcome]][test] - stats::predict(model_y, scores[test, , drop = FALSE])
  }
  keep <- is.finite(x_residual) & is.finite(y_residual)
  x_residual <- x_residual[keep]; y_residual <- y_residual[keep]; m <- length(x_residual)
  theta <- sum(x_residual * y_residual) / sum(x_residual^2)
  score <- x_residual * (y_residual - theta * x_residual)
  se <- sqrt(mean(score^2) / mean(x_residual^2)^2 / m)
  list(estimate = theta, se = se, ci_low = theta - 1.96 * se, ci_high = theta + 1.96 * se, df = m - spline_df * length(adjust) - 2L)
}

#' Estimate a declared causal effect on locked construct states
#'
#' Estimates the effect of a `treatment` construct on an `outcome` construct
#' adjusting for a declared `adjust` set, using the errors-in-variables corrected
#' coefficient: disattenuated for measurement error and adjusted for (linear)
#' confounding. The result reports the unadjusted, adjusted-naive, and
#' adjusted-disattenuated estimates, an identification and sensitivity panel, and
#' an explicit causal-admissibility label. A causal label requires both an
#' adjustment set and a declared temporal order; otherwise the effect is reported
#' as an association.
#'
#' @param association A `cssem_association` from [cssem_associate()].
#' @param treatment Treatment construct name.
#' @param outcome Outcome construct name.
#' @param adjust Character vector of adjustment-set construct names.
#' @param estimand Adjustment strategy. `"adjusted_linear"` (default) is the
#'   errors-in-variables corrected coefficient: disattenuated and adjusted for
#'   *linear* confounding. `"adjusted_dml"` is a cross-fitted partially-linear
#'   double-machine-learning estimate with flexible spline nuisances, which
#'   removes *nonlinear* confounding that a linear adjustment leaves behind (a
#'   gain over CB-SEM's linear structural adjustment). The DML estimand is
#'   computed on the denoised construct states and is not disattenuated.
#' @param temporal_order Optional character vector asserting the temporal order
#'   of the constructs; required (with `adjust`) for a causal label.
#' @param disattenuate Whether to disattenuate using the association reliability
#'   (`"adjusted_linear"` only).
#' @param eiv_bootstrap Percentile-bootstrap resamples for the interval
#'   (`"adjusted_linear"` only; the DML interval is the analytic orthogonal-score
#'   interval).
#' @param reliability_grid Assumed treatment reliabilities for the reliability
#'   sensitivity curve (`"adjusted_linear"` only).
#' @param spline_df Spline degrees of freedom per confounder for the DML
#'   nuisances (`"adjusted_dml"` only).
#' @param seed Bootstrap seed.
#' @return An object of class `cssem_causal_effect`.
#' @examples
#' # cssem_causal_effect(association, "Satisfaction", "Loyalty",
#' #   adjust = c("Trust", "PriorLoyalty"),
#' #   temporal_order = c("Trust", "PriorLoyalty", "Satisfaction", "Loyalty"),
#' #   eiv_bootstrap = 500)
#' # # Flexible adjustment for nonlinear confounding:
#' # cssem_causal_effect(association, "Satisfaction", "Loyalty",
#' #   adjust = "Trust", estimand = "adjusted_dml",
#' #   temporal_order = c("Trust", "Satisfaction", "Loyalty"))
#' @export
cssem_causal_effect <- function(association, treatment, outcome, adjust = character(0),
                                estimand = c("adjusted_linear", "adjusted_dml"),
                                temporal_order = NULL, disattenuate = TRUE, eiv_bootstrap = 0L,
                                reliability_grid = c(.5, .6, .7, .8, .9, 1), spline_df = 5L, seed = 1L) {
  estimand <- match.arg(estimand)
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  scores <- association$scores
  if (is.null(scores)) stop("association does not carry locked scores; re-run cssem_associate().", call. = FALSE)
  constructs <- c(treatment, outcome, adjust)
  if (!all(constructs %in% names(scores))) stop("treatment, outcome, and adjust must be locked construct names.", call. = FALSE)
  if (treatment %in% c(outcome, adjust) || outcome %in% adjust) stop("treatment, outcome, and adjust must be distinct.", call. = FALSE)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)
  if (estimand == "adjusted_dml" && !length(adjust)) stop("estimand = \"adjusted_dml\" requires an adjustment set to residualize against.", call. = FALSE)
  if (!is.null(temporal_order)) {
    if (!all(constructs %in% temporal_order)) stop("temporal_order must contain the treatment, outcome, and adjust constructs.", call. = FALSE)
    if (match(treatment, temporal_order) >= match(outcome, temporal_order)) stop("treatment must precede outcome in temporal_order.", call. = FALSE)
  }

  # Identification: how much treatment variation survives adjustment, and how
  # well the adjustment set predicts each. Little residual treatment variation
  # means the effect is weakly identified. (Common to both estimands.)
  treatment_r2 <- if (length(adjust)) summary(stats::lm(stats::reformulate(adjust, treatment), scores))$r.squared else 0
  outcome_r2 <- if (length(adjust)) summary(stats::lm(stats::reformulate(adjust, outcome), scores))$r.squared else 0
  identification_strength <- 1 - treatment_r2

  if (estimand == "adjusted_dml") {
    folds <- association$folds
    if (is.null(folds) || length(folds) != nrow(scores)) {
      set.seed(seed); folds <- sample(rep_len(1:5, nrow(scores)))
    }
    dml <- .dml_partial_linear(scores, outcome, treatment, adjust, folds, spline_df)
    unadjusted <- unname(stats::coef(stats::lm(stats::reformulate(treatment, outcome), scores))[treatment])
    adjusted_naive <- unname(stats::coef(stats::lm(stats::reformulate(c(treatment, adjust), outcome), scores))[treatment])
    adjusted_effect <- dml$estimate; interval <- c(dml$ci_low, dml$ci_high)
    disattenuated <- FALSE; stable <- TRUE
    robustness_value <- .robustness_value(dml$estimate / dml$se, dml$df)
    reliability_sensitivity <- NULL
  } else {
    reliability <- if (isTRUE(disattenuate)) association$reliability else NULL
    if (!is.null(reliability) && all(is.na(reliability))) reliability <- NULL
    disattenuated <- !is.null(reliability)
    identity_reliability <- stats::setNames(rep(1, length(names(scores))), names(scores))
    used_reliability <- if (disattenuated) reliability else identity_reliability

    unadjusted_fit <- .eiv_adjusted(scores, outcome, treatment, character(0), used_reliability)
    adjusted_fit <- .eiv_adjusted(scores, outcome, treatment, adjust, used_reliability)
    unadjusted <- unadjusted_fit$corrected; adjusted_naive <- adjusted_fit$naive
    adjusted_effect <- adjusted_fit$corrected; stable <- adjusted_fit$stable

    interval <- c(NA_real_, NA_real_)
    if (eiv_bootstrap > 0L) {
      draws <- .eiv_bootstrap(scores, outcome, c(treatment, adjust), used_reliability, eiv_bootstrap, seed)
      if (!is.null(draws) && any(is.finite(draws[, treatment]))) interval <- stats::quantile(draws[, treatment], c(.025, .975), na.rm = TRUE, names = FALSE)
    }

    # Sensitivity: robustness value from the adjusted OLS t-statistic.
    adjusted_ols <- stats::lm(stats::reformulate(c(treatment, adjust), outcome), scores)
    coefficient_table <- summary(adjusted_ols)$coefficients
    robustness_value <- .robustness_value(coefficient_table[treatment, "t value"], adjusted_ols$df.residual)

    # Reliability sensitivity: the adjusted-disattenuated effect under a range of
    # assumed treatment reliabilities.
    reliability_sensitivity <- data.frame(reliability = reliability_grid,
      effect = vapply(reliability_grid, function(rho) {
        trial <- used_reliability; trial[[treatment]] <- rho
        .eiv_adjusted(scores, outcome, treatment, adjust, trial)$corrected
      }, numeric(1)), stringsAsFactors = FALSE)
  }

  has_adjust <- length(adjust) > 0L; has_order <- !is.null(temporal_order)
  label <- if (has_adjust && has_order && identification_strength >= .10) "causal_under_assumptions"
    else if (has_adjust) "adjusted_association" else "unadjusted_association"

  structure(list(treatment = treatment, outcome = outcome, adjust = adjust, estimand = estimand,
    unadjusted = unadjusted, adjusted_naive = adjusted_naive, adjusted_effect = adjusted_effect,
    ci_low = interval[[1L]], ci_high = interval[[2L]], disattenuated = disattenuated, stable = stable,
    bootstrap = eiv_bootstrap, n = nrow(scores), temporal_order_declared = has_order,
    identification_strength = identification_strength, treatment_r2 = treatment_r2, outcome_r2 = outcome_r2,
    robustness_value = robustness_value, reliability_sensitivity = reliability_sensitivity,
    label = label, status = label), class = "cssem_causal_effect")
}

#' Print a declared causal effect
#'
#' @param x A `cssem_causal_effect` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_causal_effect <- function(x, ...) {
  cat(sprintf("CS-SEM effect: %s -> %s  (n = %d)\n", x$treatment, x$outcome, x$n))
  labels <- c(causal_under_assumptions = "Causal under assumptions",
    adjusted_association = "Adjusted association (not causal: no declared temporal order)",
    unadjusted_association = "Unadjusted association (not causal: no adjustment set)")
  cat("Interpretation: ", labels[[x$label]], "\n", sep = "")
  cat("Adjustment set: ", if (length(x$adjust)) paste(x$adjust, collapse = ", ") else "(none)", "\n")
  dml <- identical(x$estimand, "adjusted_dml")
  cat("Estimand:       ", if (dml) "adjusted_dml (flexible spline adjustment for nonlinear confounding)" else "adjusted_linear (disattenuated, linear adjustment)", "\n\n", sep = "")
  basis <- if (dml) "flexible-adjusted, not disattenuated" else if (isTRUE(x$disattenuated)) "disattenuated" else "naive"
  format_effect <- function(effect, low, high) if (is.na(low)) sprintf("% .3f", effect) else sprintf("% .3f  [% .3f, % .3f]", effect, low, high)
  cat(sprintf("  unadjusted effect     % .3f  (confounded if the adjustment set matters)\n", x$unadjusted))
  if (dml) cat(sprintf("  linear-adjusted       % .3f  (leaves residual nonlinear confounding)\n", x$adjusted_naive))
  else cat(sprintf("  adjusted, attenuated  % .3f  (adjusted but not disattenuated)\n", x$adjusted_naive))
  cat(sprintf("  adjusted effect       %s  (%s)\n", format_effect(x$adjusted_effect, x$ci_low, x$ci_high), basis))
  cat("\n  identification strength ", sprintf("%.2f", x$identification_strength),
    " (residual treatment variation after adjustment)\n", sep = "")
  if (is.finite(x$robustness_value)) cat(sprintf("  robustness value        %.2f  (an unmeasured confounder explaining %.0f%% of residual variance in both treatment and outcome would null the effect)\n",
    x$robustness_value, 100 * x$robustness_value))
  if (x$identification_strength < .10) cat("  WARNING: treatment is largely explained by the adjustment set; the effect is weakly identified.\n")
  cat("\nAssociational-to-causal estimate under declared assumptions; not a randomized-experiment guarantee.\n")
  invisible(x)
}
