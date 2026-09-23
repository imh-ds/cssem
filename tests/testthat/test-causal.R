.causal_fixture <- function(n = 3000, seed = 11) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  C <- stats::rnorm(n); X <- std(0.7 * C + stats::rnorm(n, sd = .7)); Y <- std(0.3 * X + 0.6 * C + stats::rnorm(n, sd = .5))
  scores <- data.frame(X = std(X), C = std(C), Y = Y)
  structure(list(scores = scores, reliability = stats::setNames(c(.70, .90, .85), c("X", "C", "Y")),
    full_models = list(), structure = cssem_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))),
    class = "cssem_association")
}

test_that("causal_effect decomposes confounding and attenuation", {
  association <- .causal_fixture()
  effect <- causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"), eiv_bootstrap = 60, seed = 11)
  expect_s3_class(effect, "causal_effect")
  expect_cssem_provenance(effect, "causal_effect")
  # Adjustment removes upward confounding.
  expect_gt(effect$unadjusted, effect$adjusted_effect)
  # Disattenuation increases the adjusted effect relative to the attenuated one.
  expect_gt(effect$adjusted_effect, effect$adjusted_naive)
  expect_true(is.finite(effect$robustness_value) && effect$robustness_value > 0)
  expect_equal(nrow(effect$reliability_sensitivity), 6L)
  expect_true(is.finite(effect$ci_low) && is.finite(effect$ci_high))
})

test_that("causal effect provenance links its fitted association", {
  data <- simulate_states(n = 48, seed = 731, missing = 0)
  model <- specify_measurement(A = ordinal("a1", "a2"),
    B = ordinal("b1", "b2"), folds = 3)
  fit <- fit_states(model, data, seed = 732, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, specify_structure(B ~ linear(A)),
    structural_repeats = 1, seed = 733)
  effect <- causal_effect(association, "A", "B")
  provenance <- expect_cssem_provenance(effect, "causal_effect", "associate")
  expect_identical(provenance$parent$association$operation, "associate")
})

.causal_nonlinear_fixture <- function(n = 4000, seed = 7) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  C <- stats::rnorm(n)
  X <- std(sin(1.3 * C) + stats::rnorm(n, sd = .7))
  Y <- std(0.3 * X + sin(1.3 * C) + stats::rnorm(n, sd = .5))
  scores <- data.frame(X = std(X), C = std(C), Y = Y)
  structure(list(scores = scores, folds = sample(rep_len(1:5, n)),
    reliability = stats::setNames(c(.75, .85, .85), c("X", "C", "Y")),
    full_models = list(), structure = cssem_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))),
    class = "cssem_association")
}

test_that("the DML estimand removes nonlinear confounding a linear adjustment leaves", {
  association <- .causal_nonlinear_fixture()
  dml <- causal_effect(association, "X", "Y", adjust = "C", estimand = "adjusted_dml",
    temporal_order = c("C", "X", "Y"))
  expect_identical(dml$estimand, "adjusted_dml")
  expect_false(dml$disattenuated)
  expect_null(dml$reliability_sensitivity)
  # Flexible adjustment recovers the ~0.3 effect that the linear adjustment
  # leaves upward-confounded (nonlinear confounder).
  expect_lt(dml$adjusted_effect, dml$adjusted_naive)
  expect_lt(abs(dml$adjusted_effect - 0.3), abs(dml$adjusted_naive - 0.3))
  expect_lt(abs(dml$adjusted_effect - 0.3), 0.06)
  # Analytic orthogonal-score interval.
  expect_true(is.finite(dml$ci_low) && is.finite(dml$ci_high) && dml$ci_low < dml$ci_high)
  expect_true(is.finite(dml$robustness_value) && dml$robustness_value > 0)
  expect_output(print(dml), "nonlinear confounding")
})

test_that("flexible causal identification uses nonlinear residual treatment variation", {
  # Regression: flexible estimands used the linear X ~ C R2 for their causal
  # label, even when a spline nuisance model could predict X almost perfectly.
  set.seed(56)
  n <- 1200
  C <- stats::runif(n, -2, 2)
  X <- as.numeric(splines::ns(C, df = 5L)[, 1L])
  Y <- C + stats::rnorm(n, sd = .15)
  scores <- data.frame(X = as.numeric(scale(X)), C = as.numeric(scale(C)),
    Y = as.numeric(scale(Y)))
  association <- structure(list(scores = scores,
    folds = sample(rep_len(1:5, n)), reliability = NULL, full_models = list(),
    structure = cssem_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))),
    class = "cssem_association")

  for (estimand in c("adjusted_dml", "adjusted_ame")) {
    effect <- causal_effect(association, "X", "Y", adjust = "C",
      estimand = estimand, temporal_order = c("C", "X", "Y"))
    expect_lt(effect$identification_strength, .10, info = estimand)
    expect_identical(effect$label, "adjusted_association")
    expect_false(isTRUE(effect$stable))
  }
})

test_that("the DML estimand requires an adjustment set", {
  association <- .causal_nonlinear_fixture(n = 500)
  expect_error(causal_effect(association, "X", "Y", estimand = "adjusted_dml"), "requires an adjustment set")
})

.causal_ame_fixture <- function(n = 5000, seed = 4) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  C <- stats::rnorm(n)
  X <- std(0.8 * C + (stats::rgamma(n, 2, 1) - 2))       # skewed treatment
  Y <- 0.3 * X + 0.2 * X^2 + 0.7 * C + stats::rnorm(n, sd = .5)
  scores <- data.frame(X = X, C = C, Y = Y)
  list(truth = mean(0.3 + 0.4 * X), association = structure(list(scores = scores,
    folds = sample(rep_len(1:5, n)), reliability = stats::setNames(c(.75, .85, .85), c("X", "C", "Y")),
    full_models = list(), structure = cssem_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))),
    class = "cssem_association"))
}

test_that("the AME estimand recovers the average derivative under a nonlinear dose-response", {
  fixture <- .causal_ame_fixture()
  ame <- causal_effect(fixture$association, "X", "Y", adjust = "C", estimand = "adjusted_ame",
    temporal_order = c("C", "X", "Y"))
  pl <- causal_effect(fixture$association, "X", "Y", adjust = "C", estimand = "adjusted_dml",
    temporal_order = c("C", "X", "Y"))
  expect_identical(ame$estimand, "adjusted_ame")
  expect_false(ame$disattenuated)
  expect_null(ame$reliability_sensitivity)
  # The average marginal effect recovers the true average derivative ...
  expect_lt(abs(ame$adjusted_effect - fixture$truth), 0.04)
  # ... where the constant-slope partially-linear estimand does not.
  expect_gt(abs(pl$adjusted_effect - fixture$truth), abs(ame$adjusted_effect - fixture$truth))
  expect_true(is.finite(ame$ci_low) && is.finite(ame$ci_high) && ame$ci_low < ame$ci_high)
  expect_output(print(ame), "average marginal effect")
  expect_output(print(ame), "avg marginal effect")
})

test_that("the AME estimand requires an adjustment set and routes through an edge", {
  fixture <- .causal_ame_fixture(n = 1500)
  expect_error(causal_effect(fixture$association, "X", "Y", estimand = "adjusted_ame"), "requires an adjustment set")
  routing <- route(fixture$association,
    causal = list(causal_edge("X", "Y", adjust = "C", estimand = "adjusted_ame")),
    temporal_order = c("C", "X", "Y"))
  expect_identical(routing$causal_effects[[1L]]$estimand, "adjusted_ame")
})

test_that("route carries the DML estimand through to a causal edge", {
  association <- .causal_nonlinear_fixture(n = 1500)
  routing <- route(association,
    causal = list(causal_edge("X", "Y", adjust = "C", estimand = "adjusted_dml")),
    temporal_order = c("C", "X", "Y"))
  causal_row <- routing$table[routing$table$status == "causal", ]
  expect_equal(causal_row$estimand, "adjusted_dml")
  expect_identical(routing$causal_effects[[1L]]$estimand, "adjusted_dml")
  expect_error(causal_edge("X", "Y", adjust = "C", estimand = "bogus"), "should be one of")
})

test_that("a causal label requires both an adjustment set and a temporal order", {
  association <- .causal_fixture()
  expect_identical(causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"))$label, "causal_under_assumptions")
  expect_identical(causal_effect(association, "X", "Y", adjust = "C")$label, "adjusted_association")
  expect_identical(causal_effect(association, "X", "Y")$label, "unadjusted_association")
})

test_that("causal_effect prints the reason for a non-causal label", {
  skeleton <- function(label, declared, strength) structure(list(
    treatment = "X", outcome = "Y", adjust = "C", estimand = "adjusted_linear",
    claim_type = "total (adjusted)", unadjusted = .5, adjusted_naive = .3,
    adjusted_effect = .35, ci_low = NA_real_, ci_high = NA_real_,
    disattenuated = FALSE, stable = TRUE, bootstrap = 0L, n = 300L,
    temporal_order_declared = declared, identification_strength = strength,
    treatment_r2 = 1 - strength, outcome_r2 = .2, robustness_value = .2,
    reliability_sensitivity = NULL, label = label, status = label),
    class = "causal_effect")

  weak <- skeleton("adjusted_association", TRUE, .04)
  expect_output(print(weak), "weak identification")
  expect_false(any(grepl("no declared temporal order", capture.output(print(weak)))))
  expect_output(print(skeleton("adjusted_association", FALSE, .8)),
    "no declared temporal order")
  expect_output(print(skeleton("causal_under_assumptions", TRUE, .8)),
    "Causal under assumptions")
})

test_that("route builds a Path Routing Table with honest defaults", {
  generated <- cssem:::.structural_validation_data("linear", 400, 5, items = 4L)
  fit <- fit_states(generated$model, generated$data, seed = 5, iterations = 4, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 2L, seed = 5, shadow_scope = "temporal")
  routing <- route(association,
    causal = list(causal_edge("Quality", "Loyalty", adjust = "Trust")),
    temporal_order = c("Trust", "Quality", "Loyalty"))
  expect_s3_class(routing, "cssem_routing")
  expect_true(all(c("path", "status", "effect", "interpretation") %in% names(routing$table)))
  expect_true(any(routing$table$status == "associational"))
  causal_row <- routing$table[routing$table$status == "causal", ]
  expect_equal(nrow(causal_row), 1L)
  expect_true(is.finite(causal_row$robustness_value))
  # Discipline: a causal edge without a temporal order is rejected.
  expect_error(route(association, causal = list(causal_edge("Quality", "Loyalty", adjust = "Trust"))), "temporal_order")
  expect_error(causal_edge("Quality", "Loyalty"), "adjustment set")
  expect_output(print(routing), "Path Routing Table")
})

test_that("causal_effect guards reject bad inputs", {
  association <- .causal_fixture()
  expect_error(causal_effect(association, "X", "X"), "distinct")
  expect_error(causal_effect(association, "X", "Q"), "locked construct")
  expect_error(causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "Y", "X")), "must precede")
  expect_output(print(causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"))), "Causal under assumptions")
})

test_that("post-treatment adjustment is refused under a declared temporal order", {
  set.seed(91)
  n <- 120
  scores <- data.frame(X = rnorm(n))
  scores$M <- .6 * scores$X + rnorm(n, sd = .7)
  scores$Y <- .5 * scores$X + .4 * scores$M + rnorm(n, sd = .7)
  fit <- structure(list(locked_scores = scores, folds = sample(rep(1:3, length.out = n)),
    reliability = c(X = .85, M = .85, Y = .9)), class = "fit_states")
  assoc <- associate(fit,
    cssem_structure(list(M = "X", Y = c("X", "M")), order = c("X", "M", "Y")),
    structural_repeats = 2L, seed = 91, shadow_scope = "temporal")
  expect_error(
    causal_effect(assoc, "X", "Y", adjust = "M", temporal_order = c("X", "M", "Y")),
    "post-treatment")
  expect_error(
    route(assoc,
      causal = list(causal_edge("X", "Y", adjust = "M")),
      temporal_order = c("X", "M", "Y")),
    "post-treatment")
  # A pre-treatment adjustment set still routes.
  ok <- causal_effect(assoc, "M", "Y", adjust = "X", temporal_order = c("X", "M", "Y"))
  expect_identical(ok$label, "causal_under_assumptions")
})

test_that("a weakly identified declared causal edge is not routed as causal", {
  # Regression: route() set status "causal" before estimating, so an edge whose
  # causal_effect() label came back adjusted_association (identification
  # strength below .10) still read as a causal pathway in the routing table and
  # in evidence_report().
  set.seed(77)
  n <- 300
  control <- stats::rnorm(n)
  # The adjuster explains almost all of the treatment, so almost no treatment
  # variation survives adjustment.
  treatment <- .99 * control + stats::rnorm(n, sd = .14)
  outcome <- .4 * treatment + .3 * control + stats::rnorm(n, sd = .6)
  fit <- structure(list(locked_scores = data.frame(C = as.numeric(scale(control)),
    X = as.numeric(scale(treatment)), Y = as.numeric(scale(outcome))),
    folds = sample(rep(1:3, length.out = n)),
    reliability = c(C = .85, X = .85, Y = .85)), class = "fit_states")
  association <- associate(fit, specify_structure(Y ~ linear(X) + linear(C),
    order = c("C", "X", "Y")), seed = 77)

  effect <- causal_effect(association, treatment = "X", outcome = "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"))
  expect_lt(effect$identification_strength, .10)
  expect_identical(effect$label, "adjusted_association")

  routing <- route(association, causal = list(causal_edge("X", "Y", adjust = "C")),
    temporal_order = c("C", "X", "Y"))
  row <- routing$table[routing$table$path == "X→Y", , drop = FALSE]
  expect_identical(row$status[[1L]], "causal_weak")
  # The declaration is kept: estimand, adjustment set, and estimate still report.
  expect_identical(row$estimand[[1L]], "adjusted_linear")
  expect_identical(row$adjustment_set[[1L]], "C")
  expect_true(is.finite(row$effect[[1L]]))

  report <- evidence_report(association, routing = routing)
  edge <- report$effects[report$effects$path == "X→Y", , drop = FALSE]
  expect_identical(edge$causal_status[[1L]], "causal_weak")
  expect_false(grepl("causal pathway", edge$verdict[[1L]]))
})

test_that("the edge verdict names weak identification instead of claiming a pathway", {
  strong <- list(stability = 1, estimate = .45, contribution = .08, gap = .05)
  routed <- cssem:::.edge_verdict(strong$stability, strong$estimate, strong$contribution,
    strong$gap, "causal")
  weak <- cssem:::.edge_verdict(strong$stability, strong$estimate, strong$contribution,
    strong$gap, "causal_weak")
  expect_match(routed, "causal pathway")
  expect_false(grepl("causal pathway", weak))
  expect_match(weak, "weakly identified")
})

test_that("a causal claim is typed from its adjustment set, not named direct by default", {
  # Regression: every causal_effect row was typed "direct". With a
  # pre-treatment adjustment set that excludes the declared mediators, the
  # estimand is a total-effect contrast, which the paper had to explain away.
  set.seed(21)
  n <- 400
  covariate <- stats::rnorm(n)
  treatment <- .3 * covariate + stats::rnorm(n, sd = .9)
  mediator <- .5 * treatment + stats::rnorm(n, sd = .8)
  outcome <- .4 * mediator + .25 * treatment + .2 * covariate + stats::rnorm(n, sd = .8)
  fit <- structure(list(locked_scores = data.frame(C = as.numeric(scale(covariate)),
    X = as.numeric(scale(treatment)), M = as.numeric(scale(mediator)),
    Y = as.numeric(scale(outcome))), folds = sample(rep(1:3, length.out = n)),
    reliability = c(C = .85, X = .85, M = .85, Y = .85)), class = "fit_states")
  association <- associate(fit, specify_structure(M ~ linear(X),
    Y ~ linear(X) + linear(M) + linear(C), order = c("C", "X", "M", "Y")), seed = 21)

  pre_treatment <- causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "M", "Y"))
  expect_identical(pre_treatment$claim_type, "total (adjusted)")
  expect_length(pre_treatment$adjusted_mediators, 0L)

  # Adjusting a declared mediator blocks the mediated paths, so the same
  # estimand becomes a direct contrast. (A declared temporal order refuses this
  # adjustment outright, so it is reachable only without one.)
  with_mediator <- causal_effect(association, "X", "Y", adjust = c("C", "M"))
  expect_identical(with_mediator$claim_type, "direct (adjusted)")
  expect_identical(with_mediator$adjusted_mediators, "M")

  report <- evidence_report(association, causal = list(pre_treatment))
  expect_identical(report$causal_claims$type[[1L]], "total (adjusted)")
})

test_that("a claim arriving through both routing and causal is listed once", {
  # Regression: claims from routing$causal_effects and from causal= were
  # concatenated, so an effect passed through both appeared twice and read as
  # two independent pieces of evidence.
  set.seed(22)
  n <- 300
  x <- stats::rnorm(n); c0 <- stats::rnorm(n)
  y <- .4 * x + .2 * c0 + stats::rnorm(n, sd = .7)
  fit <- structure(list(locked_scores = data.frame(C = as.numeric(scale(c0)),
    X = as.numeric(scale(x)), Y = as.numeric(scale(y))),
    folds = sample(rep(1:3, length.out = n)),
    reliability = c(C = .85, X = .85, Y = .85)), class = "fit_states")
  association <- associate(fit, specify_structure(Y ~ linear(X) + linear(C),
    order = c("C", "X", "Y")), seed = 22)
  routing <- route(association, causal = list(causal_edge("X", "Y", adjust = "C")),
    temporal_order = c("C", "X", "Y"))
  explicit <- causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"), eiv_bootstrap = 50L)

  both <- evidence_report(association, routing = routing, causal = list(explicit))
  expect_equal(nrow(both$causal_claims), 1L)
  # The explicitly passed object wins, so its interval survives.
  expect_true(is.finite(both$causal_claims$ci_low[[1L]]))
  routed_only <- evidence_report(association, routing = routing)
  expect_true(is.na(routed_only$causal_claims$ci_low[[1L]]))
})

test_that("an interventional mediation names the actual reason it is not causal", {
  # Regression: the non-causal interpretation line was hard-coded to "no
  # declared temporal order", but the same label is assigned when the order was
  # declared and identification strength fell below .10.
  skeleton <- function(label, declared, strength) structure(list(
    x = "X", y = "Y", mediators = "M", adjust = "C", estimand = "interventional",
    n = 300L, disattenuated = TRUE, bootstrap = 0L, temporal_order_declared = declared,
    label = label, status = label, identification_strength = strength, outcome_r2 = .3,
    mediator_r2_min = .2, robustness_value = .1, min_path_reliability = .8,
    proportion_mediated = .5, proportion_mediated_basis = "disattenuated",
    summary = data.frame(component = c("total", "direct", "indirect_total"),
      naive_effect = c(.4, .2, .2), disattenuated_effect = c(.5, .25, .25),
      stringsAsFactors = FALSE)), class = "causal_indirect_effect")

  expect_output(print(skeleton("adjusted_association", FALSE, .8)), "no declared temporal order")
  weak <- skeleton("adjusted_association", TRUE, .04)
  expect_output(print(weak), "weak identification")
  expect_false(any(grepl("no declared temporal order", capture.output(print(weak)))))
  expect_output(print(skeleton("causal_under_assumptions", TRUE, .8)), "Causal under assumptions")
})
