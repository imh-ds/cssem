test_that("causal validation manifest declares independent estimands and targets", {
  manifest <- causal_validation_manifest("screening")

  expect_identical(manifest, causal_validation_manifest("screening"))
  expect_equal(manifest$scenario, c("confounding", "weak_overlap",
    "nuisance_misspecification", "measurement_error",
    "nonlinear_treatment", "mediation"))
  expect_true(all(is.finite(manifest$target)))
  expect_true(all(manifest$n >= 100L))
  expect_equal(manifest$estimand[manifest$scenario == "mediation"],
    "interventional_mediation")
})

test_that("causal validation retains bias and graph admissibility for each stress case", {
  manifest <- causal_validation_manifest("screening")
  result <- validate_causal(manifest, reps = 1, seed = 1201, bootstrap = 0)

  expect_equal(nrow(result), nrow(manifest))
  expect_true(all(result$status == "ok"), info = paste(result$error, collapse = "; "))
  expect_true(all(is.finite(result$truth)))
  expect_true(all(is.finite(result$estimate)))
  expect_equal(result$bias, result$estimate - result$truth)
  expect_true(all(c("ci_low", "ci_high", "ci_covers_truth", "graph_valid",
    "adjustment_valid", "causal_admissible", "overlap_status",
    "nuisance_method") %in% names(result)))

  confounding <- result[result$scenario == "confounding", , drop = FALSE]
  expect_false(confounding$causal_admissible)
  expect_identical(confounding$label, "adjusted_association")
  weak <- result[result$scenario == "weak_overlap", , drop = FALSE]
  expect_identical(weak$overlap_status, "weak")
  expect_false(weak$label == "causal_under_assumptions")
  nuisance <- result[result$scenario == "nuisance_misspecification", , drop = FALSE]
  expect_identical(nuisance$nuisance_method, "cross_fitted_spline_nuisance")
  mediation <- result[result$scenario == "mediation", , drop = FALSE]
  expect_identical(mediation$truth_method, "product_of_structural_paths")
})

test_that("causal validation reports target coverage when intervals are requested", {
  manifest <- causal_validation_manifest("screening")
  manifest <- manifest[manifest$scenario == "measurement_error", , drop = FALSE]
  result <- validate_causal(manifest, reps = 1, seed = 1202, bootstrap = 20)

  expect_identical(result$status, "ok")
  expect_true(is.finite(result$ci_low) && is.finite(result$ci_high))
  expect_true(is.logical(result$ci_covers_truth))
})

test_that("causal validation protects independent truths and the caller RNG", {
  manifest <- causal_validation_manifest("screening")
  altered <- manifest
  altered$target[[1L]] <- altered$target[[1L]] + .1
  expect_error(validate_causal(altered, reps = 1, bootstrap = 0),
    "truth_method and target")
  expect_error(validate_causal(manifest, reps = 1, seed = -1, bootstrap = 0),
    "non-negative integer")

  set.seed(2014)
  expected <- sample.int(1000, 5)
  set.seed(2014)
  validate_causal(manifest[manifest$scenario == "nuisance_misspecification", , drop = FALSE],
    reps = 1, seed = 77, bootstrap = 0)
  expect_identical(sample.int(1000, 5), expected)
})
