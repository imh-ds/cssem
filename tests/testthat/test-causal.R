.causal_fixture <- function(n = 3000, seed = 11) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  C <- stats::rnorm(n); X <- std(0.7 * C + stats::rnorm(n, sd = .7)); Y <- std(0.3 * X + 0.6 * C + stats::rnorm(n, sd = .5))
  scores <- data.frame(X = std(X), C = std(C), Y = Y)
  structure(list(scores = scores, reliability = stats::setNames(c(.70, .90, .85), c("X", "C", "Y")),
    full_models = list(), structure = cssem_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))),
    class = "cssem_association")
}

test_that("cssem_causal_effect decomposes confounding and attenuation", {
  association <- .causal_fixture()
  effect <- cssem_causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"), eiv_bootstrap = 60, seed = 11)
  expect_s3_class(effect, "cssem_causal_effect")
  # Adjustment removes upward confounding.
  expect_gt(effect$unadjusted, effect$adjusted_effect)
  # Disattenuation increases the adjusted effect relative to the attenuated one.
  expect_gt(effect$adjusted_effect, effect$adjusted_naive)
  expect_true(is.finite(effect$robustness_value) && effect$robustness_value > 0)
  expect_equal(nrow(effect$reliability_sensitivity), 6L)
  expect_true(is.finite(effect$ci_low) && is.finite(effect$ci_high))
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
  dml <- cssem_causal_effect(association, "X", "Y", adjust = "C", estimand = "adjusted_dml",
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

test_that("the DML estimand requires an adjustment set", {
  association <- .causal_nonlinear_fixture(n = 500)
  expect_error(cssem_causal_effect(association, "X", "Y", estimand = "adjusted_dml"), "requires an adjustment set")
})

test_that("cssem_route carries the DML estimand through to a causal edge", {
  association <- .causal_nonlinear_fixture(n = 1500)
  routing <- cssem_route(association,
    causal = list(cssem_causal_edge("X", "Y", adjust = "C", estimand = "adjusted_dml")),
    temporal_order = c("C", "X", "Y"))
  causal_row <- routing$table[routing$table$status == "causal", ]
  expect_equal(causal_row$estimand, "adjusted_dml")
  expect_identical(routing$causal_effects[[1L]]$estimand, "adjusted_dml")
  expect_error(cssem_causal_edge("X", "Y", adjust = "C", estimand = "bogus"), "should be one of")
})

test_that("a causal label requires both an adjustment set and a temporal order", {
  association <- .causal_fixture()
  expect_identical(cssem_causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"))$label, "causal_under_assumptions")
  expect_identical(cssem_causal_effect(association, "X", "Y", adjust = "C")$label, "adjusted_association")
  expect_identical(cssem_causal_effect(association, "X", "Y")$label, "unadjusted_association")
})

test_that("cssem_route builds a Path Routing Table with honest defaults", {
  generated <- cssem:::.structural_validation_data("linear", 400, 5, items = 4L)
  fit <- cssem_fit(generated$model, generated$data, seed = 5, iterations = 4, diagnostics = FALSE)
  association <- cssem_associate(fit, generated$structure, structural_repeats = 2L, seed = 5, shadow_scope = "temporal")
  routing <- cssem_route(association,
    causal = list(cssem_causal_edge("Quality", "Loyalty", adjust = "Trust")),
    temporal_order = c("Trust", "Quality", "Loyalty"))
  expect_s3_class(routing, "cssem_routing")
  expect_true(all(c("path", "status", "effect", "interpretation") %in% names(routing$table)))
  expect_true(any(routing$table$status == "associational"))
  causal_row <- routing$table[routing$table$status == "causal", ]
  expect_equal(nrow(causal_row), 1L)
  expect_true(is.finite(causal_row$robustness_value))
  # Discipline: a causal edge without a temporal order is rejected.
  expect_error(cssem_route(association, causal = list(cssem_causal_edge("Quality", "Loyalty", adjust = "Trust"))), "temporal_order")
  expect_error(cssem_causal_edge("Quality", "Loyalty"), "adjustment set")
  expect_output(print(routing), "Path Routing Table")
})

test_that("cssem_causal_effect guards reject bad inputs", {
  association <- .causal_fixture()
  expect_error(cssem_causal_effect(association, "X", "X"), "distinct")
  expect_error(cssem_causal_effect(association, "X", "Q"), "locked construct")
  expect_error(cssem_causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "Y", "X")), "must precede")
  expect_output(print(cssem_causal_effect(association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "Y"))), "Causal under assumptions")
})
