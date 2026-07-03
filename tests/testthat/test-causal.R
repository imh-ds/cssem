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
