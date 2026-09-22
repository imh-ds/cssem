test_that("parameter tables expose stable structural parameter identities", {
  n <- 40L
  scores <- data.frame(A = seq_len(n) / n, B = seq_len(n)^2 / n)
  fit <- structure(list(locked_scores = scores, folds = rep(1:4, length.out = n),
    reliability = c(A = NA_real_, B = NA_real_)), class = "fit_states")
  association <- associate(fit,
    specify_structure(B ~ linear(A), order = c("A", "B")),
    structural_repeats = 1L, seed = 1L)
  table <- parameter_table(association)

  expect_true(all(c("parameter_id", "estimate_basis", "shape", "available", "status") %in% names(table)))
  expect_equal(length(unique(table$parameter_id)), nrow(table))
  expect_true(any(table$parameter_id == "edge:B~A:naive"))
  expect_true(all(table$status == "available"))
})

test_that("contrast specifications evaluate safe arithmetic over stable IDs", {
  n <- 40L
  scores <- data.frame(A = seq_len(n) / n, B = seq_len(n)^2 / n)
  fit <- structure(list(locked_scores = scores, folds = rep(1:4, length.out = n),
    reliability = c(A = NA_real_, B = NA_real_)), class = "fit_states")
  association <- associate(fit,
    specify_structure(B ~ linear(A), order = c("A", "B")),
    structural_repeats = 1L, seed = 1L)
  table <- parameter_table(association)
  estimate <- table$estimate[table$parameter_id == "edge:B~A:naive"]
  spec <- contrast_spec(list(double = "edge:B~A:naive * 2",
    squared = "(edge:B~A:naive) * (edge:B~A:naive)"))

  expect_s3_class(spec, "cssem_contrast_spec")
  expect_error(contrast(association, contrast_spec(list(unknown = "missing_id + 1"))),
    "Unknown parameter_id")
  expect_error(contrast_spec(list(call = "log(edge:B~A:naive)")), "function calls")
  result <- contrast(association, spec)
  expect_s3_class(result, "cssem_contrast")
  expect_equal(result$estimates[["double"]], 2 * estimate)
  expect_equal(result$estimates[["squared"]], estimate^2)
})
