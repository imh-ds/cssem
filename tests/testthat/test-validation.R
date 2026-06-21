test_that("validation manifests are deterministic and expose required scenarios", {
  measurement <- cssem_measurement_validation_manifest("screening")
  structural <- cssem_structural_validation_manifest("screening")
  expect_true(all(c("clean", "moderate_missing", "sparse_categories") %in% measurement$scenario))
  expect_true(all(c("diagnostic_clean", "diagnostic_local_dependence") %in% cssem_measurement_validation_manifest("diagnostic")$scenario))
  expect_true(all(c("linear", "monotone_increasing", "monotone_decreasing", "smooth_strong", "null", "interaction", "omitted", "downstream") %in% structural$scenario))
  expect_equal(cssem_supported_envelope()$minimum_n, 200)
})

test_that("measurement validation is deterministic for a fixed seed", {
  manifest <- cssem_measurement_validation_manifest("screening")[1, ]
  first <- cssem_run_measurement_validation(manifest, reps = 1, seed = 7, folds = 2, iterations = 2, max_iterations = 2)
  second <- cssem_run_measurement_validation(manifest, reps = 1, seed = 7, folds = 2, iterations = 2, max_iterations = 2)
  expect_equal(first$cssem_recovery, second$cssem_recovery)
  expect_true(is.finite(first$held_out_loss))
})

test_that("release report applies the declared gap sign convention", {
  measurement <- data.frame(n = 200, loading = .8, missing = 0, cross_loading = 0, local_dependence = 0,
    sparse = FALSE, overlap = 0, converged = TRUE, cssem_recovery = .90, ordinal_factor_proxy_recovery = .90, composite_proxy_recovery = .90)
  structural <- rbind(
    data.frame(scenario = "linear", outcome = "Quality", predictor = "Trust", selected_shape = "linear", temporal_gap = 0, unrestricted_minus_temporal = 0),
    data.frame(scenario = "monotone_increasing", outcome = "Quality", predictor = "Trust", selected_shape = "monotone_increasing", temporal_gap = 0, unrestricted_minus_temporal = 0),
    data.frame(scenario = "monotone_decreasing", outcome = "Quality", predictor = "Trust", selected_shape = "monotone_decreasing", temporal_gap = 0, unrestricted_minus_temporal = 0),
    data.frame(scenario = "smooth_strong", outcome = "Quality", predictor = "Trust", selected_shape = "smooth_df3", temporal_gap = 0, unrestricted_minus_temporal = 0),
    data.frame(scenario = "null", outcome = "Quality", predictor = "Trust", selected_shape = "linear", temporal_gap = 0, unrestricted_minus_temporal = 0),
    data.frame(scenario = "omitted", outcome = "Quality", predictor = "Trust", selected_shape = "linear", temporal_gap = -.04, unrestricted_minus_temporal = 0),
    data.frame(scenario = "downstream", outcome = "Quality", predictor = "Trust", selected_shape = "linear", temporal_gap = 0, unrestricted_minus_temporal = -.04)
  )
  report <- cssem_validation_report(measurement, structural)
  expect_true(report$passed)
})

test_that("structural validation records both shadow scopes", {
  manifest <- data.frame(scenario = "linear", n = 80L)
  result <- cssem_run_structural_validation(manifest, reps = 1, seed = 9, folds = 2, iterations = 2, max_iterations = 2)
  expect_true(all(c("temporal_gap", "unrestricted_gap", "selected_shape") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_true(all(is.finite(result$temporal_gap)))
})
