test_that("validation manifests are deterministic and expose required scenarios", {
  measurement <- measurement_manifest("screening")
  structural <- structural_manifest("screening")
  expect_true(all(c("clean", "moderate_missing", "sparse_categories") %in% measurement$scenario))
  expect_true(all(c("diagnostic_clean", "diagnostic_local_dependence") %in% measurement_manifest("diagnostic")$scenario))
  expect_true(all(c("linear", "monotone_increasing", "monotone_decreasing", "smooth_strong", "null", "interaction", "omitted", "downstream") %in% structural$scenario))
  expect_equal(supported_envelope()$minimum_n, 200)
})

test_that("measurement validation is deterministic for a fixed seed", {
  manifest <- measurement_manifest("screening")[1, ]
  first <- validate_measurement(manifest, reps = 1, seed = 7, folds = 2, iterations = 2, max_iterations = 2)
  second <- validate_measurement(manifest, reps = 1, seed = 7, folds = 2, iterations = 2, max_iterations = 2)
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
  report <- validation_report(measurement, structural)
  expect_true(report$passed)
})

test_that("structural validation records both shadow scopes", {
  manifest <- data.frame(scenario = "linear", n = 80L)
  result <- validate_structure(manifest, reps = 1, seed = 9, folds = 2, iterations = 2, max_iterations = 2)
  expect_true(all(c("temporal_gap", "unrestricted_gap", "selected_shape") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_true(all(is.finite(result$temporal_gap)))
})

test_that("comparator validation skips optional engines cleanly when unavailable", {
  manifest <- measurement_manifest("screening")[1, ]
  result <- validate_comparator(manifest, reps = 1, seed = 11, folds = 2, iterations = 2, workers = 1)
  expect_true(all(c("cssem_locked", "ordinal_factor_proxy", "composite_proxy", "lavaan_dwls", "seminr_pls") %in% result$engine))
  expect_true(all(c("downstream_rmse", "downstream_r_squared") %in% names(result)))
  expect_true(all(result$status[result$engine %in% c("cssem_locked", "ordinal_factor_proxy", "composite_proxy")] == "success"))
  expect_true(all(result$status[result$engine %in% c("lavaan_dwls", "seminr_pls")] %in% c("success", "skipped_not_installed", "error")))
})

test_that("structural comparator validation returns structural benchmark columns", {
  manifest <- structural_manifest("screening")[1, , drop = FALSE]
  result <- validate_structure_comparator(
    manifest,
    reps = 1,
    seed = 13,
    folds = 2,
    iterations = 2,
    max_iterations = 2,
    structural_repeats = 1,
    workers = 1
  )
  expect_true(all(c("cssem_locked", "ordinal_factor_proxy", "composite_proxy", "lavaan_dwls", "seminr_pls",
    "lavaan_native", "seminr_native", "lavaan_sam") %in% result$engine))
  expect_true(all(c("selected_shape", "temporal_gap", "unrestricted_gap", "score_coverage",
    "truth_slope", "naive_estimate", "corrected_estimate", "predictor_reliability",
    "structural_bias", "ci_covers_truth", "shape_correct") %in% names(result)))
  expect_true(all(result$status[result$engine == "lavaan_sam"] %in%
    c("success", "skipped_not_installed", "skipped_no_sam", "error")))
  # The corrected estimate is a CS-SEM capability driven by posterior reliability;
  # score-only proxies report the naive slope and leave the correction empty.
  cssem_rows <- result[result$engine == "cssem_locked" & result$outcome == "Quality" & result$predictor == "Trust", , drop = FALSE]
  expect_true(any(is.finite(cssem_rows$corrected_estimate)))
  built_in <- result$engine %in% c("cssem_locked", "ordinal_factor_proxy", "composite_proxy")
  expect_true(all(result$status[built_in] == "success"))
})

test_that("repo-owned validation scripts use the shared library helper", {
  scripts <- c(
    "inst/scripts/run-v03-ci.R",
    "inst/scripts/run-v03-screening.R",
    "inst/scripts/generate-v03-release-artifacts.R",
    "inst/scripts/generate-v04-comparator-artifacts.R",
    "inst/scripts/generate-v04-structural-comparator-artifacts.R",
    "inst/scripts/run-benchmark.R",
    "inst/scripts/run-validation-suite.R",
    "inst/scripts/smoke-test.R"
  )
  script_paths <- file.path(testthat::test_path("..", ".."), scripts)
  lines <- lapply(script_paths, readLines, warn = FALSE)
  expect_true(all(vapply(lines, function(x) any(grepl("script-utils.R", x, fixed = TRUE)), logical(1))))
  expect_true(all(vapply(lines, function(x) any(grepl("prefer_workspace_library\\(", x)), logical(1))))
})

test_that("workspace library helper is opt-in", {
  script <- file.path(testthat::test_path("..", ".."), "inst", "scripts", "script-utils.R")
  e <- new.env(parent = globalenv())
  source(script, local = e)

  original <- .libPaths()
  on.exit({
    Sys.unsetenv("CSSEM_USE_LOCAL_R_LIB")
    Sys.unsetenv("CSSEM_R_LIB_PATH")
    .libPaths(original)
  }, add = TRUE)

  Sys.unsetenv("CSSEM_USE_LOCAL_R_LIB")
  Sys.unsetenv("CSSEM_R_LIB_PATH")
  e$prefer_workspace_library()
  expect_equal(.libPaths(), original)

  local_path <- file.path(tempdir(), "cssem-test-lib")
  dir.create(local_path, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(CSSEM_R_LIB_PATH = local_path)
  e$prefer_workspace_library()
  expect_equal(normalizePath(.libPaths()[1], winslash = "/", mustWork = TRUE),
    normalizePath(local_path, winslash = "/", mustWork = TRUE))
})

test_that("public docs refer to seminr instead of plspm", {
  files <- c("README.md", "docs/validation.md")
  paths <- file.path(testthat::test_path("..", ".."), files)
  lines <- unlist(lapply(paths, readLines, warn = FALSE))
  expect_false(any(grepl("plspm", lines, fixed = TRUE)))
  expect_true(any(grepl("seminr", lines, fixed = TRUE)))
})

test_that("proxy first-PC scores are sign-aligned with the item composite", {
  set.seed(31)
  latent <- rnorm(120)
  block <- data.frame(
    a1 = latent + rnorm(120, sd = .5), a2 = latent + rnorm(120, sd = .5),
    a3 = latent + rnorm(120, sd = .5), a4 = latent + rnorm(120, sd = .5)
  )
  # Alignment must hold regardless of prcomp()'s arbitrary rotation sign, which
  # negating the data flips.
  expect_gt(cor(cssem:::.aligned_first_pc(block), rowMeans(block)), 0)
  expect_gt(cor(cssem:::.aligned_first_pc(-block), rowMeans(-block)), 0)
})

test_that("a sparse item schedule survives a skew shift", {
  # Regression: the skew branch rebuilt the non-sparse cutpoints, so a scenario
  # asking for both got a skewed item with ordinary thresholds. No shipped
  # manifest combines them, but the generator silently ignored the request.
  set.seed(1)
  z <- stats::rnorm(4000)
  share <- function(...) mean(cssem:::.validation_items(z, "a", .8, 0, items = 1L, ...)$a1 == 1)
  sparse_only <- share(sparse = TRUE)
  sparse_skewed <- share(sparse = TRUE, skew = 1.2)
  plain_skewed <- share(sparse = FALSE, skew = 1.2)
  # Sparse thresholds put almost nothing in the lowest category; the skew shift
  # moves mass into it, but the sparse schedule must still be visible.
  expect_lt(sparse_only, .05)
  expect_lt(sparse_skewed, plain_skewed - .10)
})

test_that("comparator scores are refused rather than misaligned when rows are dropped", {
  # Regression: with no case index the scores were written to the first
  # nrow(scores) rows, attributing each to the wrong respondent.
  scores <- data.frame(A = c(10, 20, 30), B = c(1, 2, 3))
  expect_error(cssem:::.fill_score_frame(scores, c("A", "B"), 6L), "cannot be matched to respondents")
  # An explicit index still places them, and a complete frame is unaffected.
  placed <- cssem:::.fill_score_frame(scores, c("A", "B"), 6L, case_idx = 4:6)
  expect_equal(which(!is.na(placed$A)), 4:6)
  expect_equal(cssem:::.fill_score_frame(scores, c("A", "B"), 3L)$A, scores$A)
})
