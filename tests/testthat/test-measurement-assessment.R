test_that("measurement parameters expose scale-aware support and response curves", {
  set.seed(32)
  n <- 90
  z <- rnorm(n)
  data <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n, sd = .7)) + 3, 1), 5),
    a2 = pmin(pmax(round(z + rnorm(n, sd = .7)) + 3, 1), 5),
    a3 = pmin(pmax(round(z + rnorm(n, sd = .7)) + 3, 1), 5),
    c1 = 1.2 * z + rnorm(n, sd = .6),
    c2 = .9 * z + rnorm(n, sd = .6),
    m = z
  )
  model <- specify_measurement(
    Ord = ordinal("a1", "a2", "a3", keys = c(-1, 1, 1)),
    Cont = continuous("c1", "c2"),
    Manifest = manifest("m", standardize = FALSE), folds = 3
  )
  fit <- fit_states(model, data, seed = 4, iterations = 3, diagnostics = FALSE)
  parameters <- measurement_parameters(fit)

  expect_s3_class(parameters, "cssem_measurement_parameters")
  expect_true(all(c("construct", "item", "scale", "parameter", "estimate", "units",
    "observed_n", "missing_n", "category_support", "posterior_sd_median",
    "posterior_information", "loss_metric") %in% names(parameters)))
  expect_true(any(parameters$parameter == "discrimination" & parameters$scale == "ordinal"))
  expect_true(any(parameters$parameter == "slope" & parameters$scale == "continuous"))
  expect_true(any(parameters$parameter == "manifest_scale" & parameters$scale == "manifest"))
  expect_true(any(parameters$item == "a1" & parameters$reverse_key))
  expect_true(all(parameters$observed_n + parameters$missing_n == n))

  curve <- item_response_curve(fit, "Ord", "a1", latent = seq(-2, 2, length.out = 9))
  expect_s3_class(curve, "cssem_item_response_curve")
  expect_true(all(c("latent", "category", "probability", "expected_response") %in% names(curve)))
  probabilities <- split(curve$probability, curve$latent)
  expect_true(all(vapply(probabilities, sum, numeric(1)) == 1, tolerance = 1e-8))

  continuous_curve <- item_response_curve(fit, "Cont", "c1", latent = c(-1, 0, 1))
  encoder <- fit$full_encoders$Cont$encoders[[1L]]
  expect_equal(continuous_curve$expected_response, encoder$intercept + encoder$slope * c(-1, 0, 1))
  expect_error(item_response_curve(fit, "Manifest", "m"), "manifest")
})

test_that("measurement assessment reports scale-aware validity diagnostics", {
  set.seed(33)
  n <- 100
  z <- rnorm(n)
  data <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a2 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    a3 = pmin(pmax(round(z + rnorm(n)) + 3, 1), 5),
    b1 = pmin(pmax(round(.5 * z + rnorm(n)) + 3, 1), 5),
    b2 = pmin(pmax(round(.5 * z + rnorm(n)) + 3, 1), 5),
    b3 = pmin(pmax(round(.5 * z + rnorm(n)) + 3, 1), 5)
  )
  fit <- fit_states(specify_measurement(
    A = ordinal("a1", "a2", "a3"), B = ordinal("b1", "b2", "b3"), folds = 3
  ), data, seed = 5, iterations = 3, diagnostics = FALSE)
  assessment <- measurement_assessment(fit)

  expect_s3_class(assessment, "cssem_measurement_assessment")
  expect_cssem_provenance(assessment, "measurement_assessment", "fit_states")
  expect_true(all(c("construct", "eap_reliability", "descriptive_convergent_r2",
    "ave", "ave_reason") %in% names(assessment$constructs)))
  expect_true(all(is.na(assessment$constructs$ave)))
  expect_true(all(grepl("not defined", assessment$constructs$ave_reason, fixed = TRUE)))
  expect_true(all(c("construct_a", "construct_b", "htmt", "method", "interpretation") %in% names(assessment$validity)))
  expect_true(any(assessment$validity$method == "descriptive_spearman_item_correlations"))
  expect_true(all(grepl("descriptive", assessment$validity$interpretation,
    ignore.case = TRUE)))
  expect_true(is.data.frame(assessment$item_score_correlations))
  expect_true(is.data.frame(assessment$collinearity))
})
