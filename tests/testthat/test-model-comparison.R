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
