.comparison_fixture <- function(shift = 0, row_shift = NULL) {
  train <- list(c(1L, 2L), c(1L, 3L)); test <- list(c(3L, 4L), c(2L, 4L))
  if (!is.null(row_shift)) test <- lapply(test, function(ids) ids + row_shift)
  provenance <- data.frame(outer_id = 1:2, method = "random", seed = 1,
    train_n = 2L, test_n = 2L, train_unit_n = NA_integer_, test_unit_n = NA_integer_,
    status = "success", status_detail = "", selected_shapes = NA_character_,
    stringsAsFactors = FALSE)
  provenance$train_ids <- I(train); provenance$test_ids <- I(test)
  metrics <- do.call(rbind, lapply(seq_len(2L), function(i) data.frame(
    outer_id = i, outcome = "Y", n = 2L, rmse = c(1, 2)[i] + shift,
    mae = c(.8, 1.6)[i] + shift, r_squared = c(.4, .2)[i] + shift,
    metric_scope = "outer_test", stringsAsFactors = FALSE)))
  structure(list(predictions = data.frame(), selection_metrics = data.frame(),
    test_metrics = metrics, provenance = provenance, failures = data.frame(),
    settings = list(split_fingerprint = "fixture", observation_fingerprint = "rows:4",
      target_fingerprint = "Y", score_basis_fingerprint = "A->Y"), status = "complete"),
    class = c("cssem_outer_validation", "list"))
}

test_that("paired outer comparison computes partition deltas and intervals", {
  first <- .comparison_fixture(); second <- .comparison_fixture(shift = -.2)
  result <- compare_outer(first, second, metrics = c("rmse", "r_squared"), reps = 25L, seed = 88L)
  expect_s3_class(result, "cssem_model_comparison")
  expect_cssem_provenance(result, "compare_outer")
  expect_equal(result$differences$rmse[result$differences$outer_id == 1L], -.2)
  expect_equal(result$differences$r_squared[result$differences$outer_id == 1L], -.2)
  expect_true(all(c("metric", "estimate", "ci_low", "ci_high") %in% names(result$intervals)))
  expect_equal(result$differences, compare_outer(first, second, metrics = c("rmse", "r_squared"), reps = 25L, seed = 88L)$differences)
})

test_that("paired comparison rejects equal-sized but different partitions", {
  first <- .comparison_fixture(); second <- .comparison_fixture(row_shift = 10L)
  expect_error(compare_outer(first, second, reps = 10L), "partition row IDs")
})

test_that("paired comparison rejects different target availability", {
  first <- .comparison_fixture(); second <- .comparison_fixture()
  second$test_metrics <- second$test_metrics[second$test_metrics$outer_id != 2L, , drop = FALSE]
  expect_error(compare_outer(first, second, reps = 10L), "target availability")
})

test_that("paired comparison checks metric scope and accepts explicit construct maps", {
  first <- .comparison_fixture(); second <- .comparison_fixture()
  second$test_metrics$metric_scope <- "internal_selection"
  expect_error(compare_outer(first, second, reps = 10L), "metric_scope")
  second <- .comparison_fixture(); second$test_metrics$outcome <- "Y_B"
  mapped <- compare_outer(first, second, alignment = c(Y_B = "Y"), reps = 10L)
  expect_s3_class(mapped, "cssem_model_comparison")
})

test_that("compare_models provenance records both outer-validation parents", {
  data <- simulate_states(n = 60, seed = 404, missing = 0)
  model <- specify_measurement(A = ordinal("a1", "a2"),
    B = ordinal("b1", "b2"), folds = 3)
  structure <- specify_structure(B ~ linear(A))
  splits <- make_splits(data, method = "random", folds = 3, seed = 405)
  result <- compare_models(model, structure, model, structure, data, splits,
    seed = 406, iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1, shadow_scope = "temporal"))
  provenance <- expect_cssem_provenance(result, "compare_models")
  expect_length(provenance$parent, 2L)
  expect_true(all(vapply(provenance$parent,
    function(parent) identical(parent$operation, "validate_outer"), logical(1))))
})

test_that("compare_models compares competing predictor sets for one outcome", {
  # Regression: the score-basis fingerprint covered every structural node, so
  # two theories for the same outcome with different predictors were rejected
  # as having "different score bases".
  data <- simulate_states(n = 90, seed = 407, missing = 0)
  model <- specify_measurement(A = ordinal("a1", "a2"), C = ordinal("a3", "a4"),
    B = ordinal("b1", "b2"), folds = 3)
  splits <- make_splits(data, method = "random", folds = 3, seed = 408)
  result <- compare_models(model, specify_structure(B ~ linear(A), order = c("A", "C", "B")),
    model, specify_structure(B ~ linear(A) + linear(C), order = c("A", "C", "B")),
    data, splits, seed = 409, iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1, shadow_scope = "temporal"))
  expect_s3_class(result, "cssem_model_comparison")
  expect_true(all(result$intervals$interval_status == "descriptive_uncalibrated"))

  other_outcome <- specify_measurement(A = ordinal("a1", "a2"), C = ordinal("a3", "a4"),
    B = ordinal("b3", "b4"), folds = 3)
  expect_error(compare_models(model, specify_structure(B ~ linear(A), order = c("A", "C", "B")),
    other_outcome, specify_structure(B ~ linear(A), order = c("A", "C", "B")),
    data, splits, seed = 409, iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1, shadow_scope = "temporal")),
    "same observed indicators")
})

test_that("construct alignment renames the outcome identities it is checked by", {
  # Regression: the character alignment renamed outcome rows but kept the
  # target fingerprints built from the old names, so aligned outcomes with
  # identical indicators were still rejected.
  map <- function(outcome) stats::setNames(list(list(indicators = c("y1", "y2"),
    scales = c("ordinal", "ordinal"))), outcome)
  fixture <- function(outcome) {
    result <- .comparison_fixture()
    result$test_metrics$outcome <- outcome
    result$settings$target_map <- map(outcome)
    result$settings$target_fingerprint <- cssem:::.outer_target_fingerprint(map(outcome))
    result$settings$score_basis_fingerprint <- cssem:::.outer_score_basis_fingerprint(map(outcome))
    result
  }
  mapped <- compare_outer(fixture("Y"), fixture("Y_B"), alignment = c(Y_B = "Y"), reps = 10L)
  expect_s3_class(mapped, "cssem_model_comparison")
  expect_error(compare_outer(fixture("Y"), fixture("Y_B"), reps = 10L), "target")
})
