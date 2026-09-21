test_that("model and data preflight return structured issues", {
  model <- specify_measurement(
    A = ordinal("a1", "a2", "a3"),
    B = continuous("b1", "b2"),
    folds = 3
  )
  model_check <- check_model(model)
  expect_s3_class(model_check, "cssem_model_check")
  expect_true(is.data.frame(model_check))
  expect_false(any(model_check$severity == "error"))

  data <- data.frame(
    a1 = c(1, 2, 3, 4, 5, 1), a2 = c(1, 2, 3, 4, 5, 1),
    a3 = c(1, 2, 3, 4, 5, 1), b1 = 1:6, b2 = 2:7
  )
  data_check <- check_data(data, model, folds = rep(1:3, each = 2))
  expect_s3_class(data_check, "cssem_data_check")
  expect_true(all(c("severity", "construct", "item", "row", "fold", "code",
    "message", "action") %in% names(data_check)))
  expect_false(any(data_check$severity == "error"))
})

test_that("preflight identifies graph, schema, and sparse-fold failures", {
  malformed <- structure(list(
    constructs = list(A = list(indicators = c("x", "x"), scales = c("ordinal", "bad"), keys = c(1, 2))),
    folds = 1L
  ), class = "cssem_model")
  model_check <- check_model(malformed)
  expect_true(all(c("invalid_folds", "duplicate_indicator", "invalid_scale", "invalid_key") %in% model_check$code))

  model <- specify_measurement(A = ordinal("a1", "a2", "a3"), B = ordinal("b1", "b2", "b3"), folds = 3)
  data <- data.frame(
    a1 = c(1, 1, 1, 1, 2, 2), a2 = c(1, 1, 1, 1, 2, 2), a3 = c(1, 1, 1, 1, 2, 2),
    b1 = c(1, 2, 1, 2, 1, 2), b2 = c(1, 2, 1, 2, 1, 2), b3 = c(1, 2, 1, 2, 1, 2)
  )
  structure <- specify_structure(B ~ C, order = c("A", "B", "C"))
  graph_check <- check_model(model, structure = structure)
  expect_true(any(graph_check$code == "unknown_structural_node"))
  data_check <- check_data(data, model, folds = rep(1:3, each = 2))
  expect_true(any(data_check$code == "sparse_category" | data_check$code == "fold_missing_category"))
  expect_false(any(data_check$severity == "error"))
})

test_that("fit_states stops before fitting on preflight errors", {
  model <- specify_measurement(A = ordinal("a1", "a2", "a3"), folds = 2)
  data <- data.frame(a1 = 1:20, a2 = 1:20)
  expect_error(fit_states(model, data, diagnostics = FALSE), "missing declared indicators")

  bad <- data.frame(a1 = seq(1, 20) + .5, a2 = 1:20, a3 = 1:20)
  expect_error(fit_states(model, bad, diagnostics = FALSE), "whole-number")
})
