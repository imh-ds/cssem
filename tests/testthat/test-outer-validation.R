test_that("outer validation separates training selection from test metrics", {
  generated <- cssem:::.structural_validation_data("linear", n = 72, seed = 21, items = 3, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 8)
  result <- validate_outer(generated$model, generated$structure, generated$data, splits,
    iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1L, shadow_scope = "temporal"))
  expect_s3_class(result, "cssem_outer_validation")
  expect_true(all(result$test_metrics$metric_scope == "outer_test"))
  expect_true(all(result$selection_metrics$metric_scope == "internal_selection"))
  expect_true(all(result$provenance$train_n > 0 & result$provenance$test_n > 0))
  expect_true(all(vapply(result$provenance$train_ids, function(x) length(x) > 0, logical(1))))
})

test_that("group and time outer partitions preserve their constraints", {
  generated <- cssem:::.structural_validation_data("linear", n = 72, seed = 22, items = 3, missing = 0)
  generated$model$folds <- 3L
  generated$data$entity <- rep(seq_len(18), each = 4)
  grouped <- make_splits(generated$data, method = "group", group = "entity", folds = 3, seed = 4)
  expect_true(all(vapply(grouped$outer$test_ids, function(test) {
    !any(generated$data$entity[test] %in% generated$data$entity[setdiff(seq_len(nrow(generated$data)), test)])
  }, logical(1))))
  generated$data$time <- seq_len(nrow(generated$data))
  timed <- make_splits(generated$data, method = "time", time = "time", folds = 3)
  expect_true(all(vapply(seq_len(nrow(timed$outer)), function(i) {
    max(generated$data$time[timed$outer$train_ids[[i]]]) < min(generated$data$time[timed$outer$test_ids[[i]]])
  }, logical(1))))
  vector_group <- make_splits(generated$data, method = "group", group = generated$data$entity,
    folds = 3, seed = 4)
  vector_group$group_values <- NULL
  expect_error(validate_outer(generated$model, generated$structure, generated$data, vector_group), "cannot be verified")
})

test_that("failed outer partitions are retained", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 23, items = 3, missing = 0)
  generated$model$folds <- 2L
  splits <- make_splits(generated$data, method = "random", folds = 2, seed = 5)
  generated$data[splits$outer$test_ids[[1]], "a1"] <- NA
  result <- validate_outer(generated$model, generated$structure, generated$data, splits,
    iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1L, shadow_scope = "temporal"),
    measurement_missing_policy = "error")
  expect_true(nrow(result$failures) >= 1L)
  expect_true(all(c("outer_id", "stage", "message") %in% names(result$failures)))
})

test_that("outer selection is isolated from held-out outcomes", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 24, items = 3, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 6)
  altered <- generated$data
  altered[splits$outer$test_ids[[1]], c("loyalty1", "loyalty2", "loyalty3")] <- 1L
  args <- list(iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1L, shadow_scope = "temporal"))
  original <- do.call(validate_outer, c(list(generated$model, generated$structure, generated$data, splits), args))
  changed <- do.call(validate_outer, c(list(generated$model, generated$structure, altered, splits), args))
  first_original <- original$selection_metrics[original$selection_metrics$outer_id == 1L, , drop = FALSE]
  first_changed <- changed$selection_metrics[changed$selection_metrics$outer_id == 1L, , drop = FALSE]
  expect_identical(first_original, first_changed)
})

test_that("malformed outer partitions fail before fitting", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 25, items = 3, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 7)
  splits$outer$test_ids[[1L]] <- c(splits$outer$test_ids[[1L]], splits$outer$train_ids[[1L]][[1L]])
  expect_error(validate_outer(generated$model, generated$structure, generated$data, splits), "overlapping")
})

test_that("outer validation reports the G7 outcome-column limitation", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 26, items = 3, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 8)
  incomplete <- generated$data[, setdiff(names(generated$data), "loyalty1"), drop = FALSE]
  expect_error(validate_outer(generated$model, generated$structure, incomplete, splits), "G7 limitation")
})

test_that("outer validation rejects missing predictor columns before fitting", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 28, items = 3, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 8)
  incomplete <- generated$data[, setdiff(names(generated$data), "trust1"), drop = FALSE]
  expect_error(validate_outer(generated$model, generated$structure, incomplete, splits), "before outer fitting")
})

test_that("outer metrics exclude prior-only held-out states", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 27, items = 3, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 9)
  first_test <- splits$outer$test_ids[[1L]]
  generated$data[first_test, c("loyalty1", "loyalty2", "loyalty3")] <- NA_integer_
  result <- validate_outer(generated$model, generated$structure, generated$data, splits,
    iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1L, shadow_scope = "temporal"))
  loyalty_metric <- result$test_metrics[result$test_metrics$outer_id == 1L &
    result$test_metrics$outcome == "Loyalty", , drop = FALSE]
  expect_equal(loyalty_metric$n, 0L)
  expect_false(any(result$predictions$outer_id == 1L & result$predictions$outcome == "Loyalty"))
})
