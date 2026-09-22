test_that("observed prediction does not require outcome indicators", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 31, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  outcome_items <- generated$model$constructs$Loyalty$indicators
  predictor_only <- generated$data[, setdiff(names(generated$data), outcome_items), drop = FALSE]

  result <- predict(association, predictor_only, outcomes = "Loyalty")
  expect_s3_class(result, "cssem_prediction")
  expect_equal(nrow(result$predictions), nrow(predictor_only))
  expect_true(all(result$predictions$outcome == "Loyalty"))
  expect_true(all(is.finite(result$predictions$prediction)))

  changed <- generated$data
  changed[outcome_items] <- lapply(changed[outcome_items], function(x) rev(x))
  original <- predict(association, generated$data, outcomes = "Loyalty")
  altered <- predict(association, changed, outcomes = "Loyalty")
  expect_equal(original$predictions$prediction, altered$predictions$prediction)
})

test_that("observed prediction validates requested outcomes and required parents", {
  generated <- cssem:::.structural_validation_data("linear", n = 36, seed = 32, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)

  expect_error(
    predict(association, generated$data, outcomes = "Unknown"),
    "Unknown structural outcome"
  )
  missing_parent <- generated$data[, setdiff(names(generated$data), "trust1"), drop = FALSE]
  expect_error(
    predict(association, missing_parent, outcomes = "Loyalty"),
    "requires indicator"
  )
})

test_that("prediction reports support extrapolation and converts to a data frame", {
  generated <- cssem:::.structural_validation_data("linear", n = 42, seed = 33, items = 3, missing = 0)
  model <- specify_measurement(
    Trust = continuous("trust1", "trust2", "trust3"),
    Quality = continuous("quality1", "quality2", "quality3"),
    Loyalty = continuous("loyalty1", "loyalty2", "loyalty3"),
    folds = 3L)
  fit <- fit_states(model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  out <- generated$data[seq_len(3), setdiff(names(generated$data),
    model$constructs$Loyalty$indicators), drop = FALSE]
  out$trust1 <- 100
  result <- predict(association, out, outcomes = "Loyalty")
  expect_true(any(result$predictions$extrapolated))
  expect_equal(as.data.frame(result), result$predictions)
  expect_output(print(result), "CS-SEM structural prediction")
})
