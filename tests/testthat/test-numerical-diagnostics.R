test_that("measurement diagnostics expose controls, objectives, and optimizer status", {
  set.seed(44)
  n <- 90
  z <- rnorm(n)
  data <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n, .7)) + 3, 1), 5),
    a2 = pmin(pmax(round(z + rnorm(n, .7)) + 3, 1), 5),
    a3 = pmin(pmax(round(z + rnorm(n, .7)) + 3, 1), 5),
    c1 = z + rnorm(n, .5), c2 = .8 * z + rnorm(n, .5)
  )
  fit <- fit_states(specify_measurement(
    Ord = ordinal("a1", "a2", "a3"), Cont = continuous("c1", "c2"), folds = 3
  ), data, seed = 2, iterations = 3, tolerance = 1e-3,
  quadrature = seq(-3, 3, length.out = 15), diagnostics = FALSE)
  diagnostics <- numerical_diagnostics(fit)

  expect_s3_class(diagnostics, "cssem_numerical_diagnostics")
  expect_true(all(c("component", "construct", "item", "objective_initial",
    "objective_final", "objective_change", "optimizer_status", "rank",
    "condition_number", "singular", "converged") %in% names(diagnostics)))
  expect_true(any(diagnostics$component == "measurement"))
  expect_true(all(is.finite(diagnostics$condition_number)))
  expect_true(all(diagnostics$rank >= 1L))
  expect_equal(fit$fit_settings$tolerance, 1e-3)
  expect_equal(length(fit$fit_settings$quadrature), 15L)
  expect_error(fit_states(specify_measurement(A = ordinal("a1", "a2", "a3")),
    data[c("a1", "a2", "a3")], tolerance = 0), "tolerance")
  expect_error(fit_states(specify_measurement(A = ordinal("a1", "a2", "a3")),
    data[c("a1", "a2", "a3")], quadrature = c(0, 0)), "quadrature")
})

test_that("structural diagnostics expose correction strength and conditioning", {
  set.seed(45)
  n <- 100
  z <- rnorm(n)
  data <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a2 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a3 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    b1 = pmin(pmax(round(.6 * z + rnorm(n)) + 3, 1), 5),
    b2 = pmin(pmax(round(.6 * z + rnorm(n)) + 3, 1), 5),
    b3 = pmin(pmax(round(.6 * z + rnorm(n)) + 3, 1), 5)
  )
  fit <- fit_states(specify_measurement(
    A = ordinal("a1", "a2", "a3"), B = ordinal("b1", "b2", "b3"), folds = 3
  ), data, seed = 4, iterations = 3, diagnostics = FALSE)
  association <- associate(fit, specify_structure(B ~ A, order = c("A", "B")),
    structural_repeats = 1, eiv_bootstrap = 0)
  diagnostics <- numerical_diagnostics(fit, association)
  structural <- diagnostics[diagnostics$component == "structural_eiv", , drop = FALSE]

  expect_true(nrow(structural) >= 1L)
  expect_true(all(c("correction_shrink", "correction_strength", "condition_number",
    "corrected_condition_number", "reliability_floor_applied") %in% names(structural)))
  expect_true(all(is.finite(structural$condition_number)))
  expect_true(all(structural$correction_strength >= 0 & structural$correction_strength <= 1))
  expect_equal(unname(association$numerical_diagnostics), unname(structural))
})
