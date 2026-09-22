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

test_that("recursive prediction resolves an unavailable endogenous mediator", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 34, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  unavailable <- generated$model$constructs$Quality$indicators
  target <- generated$model$constructs$Loyalty$indicators
  predictor_only <- generated$data[, setdiff(names(generated$data), c(unavailable, target)), drop = FALSE]

  result <- predict(association, predictor_only, outcomes = "Loyalty", mode = "recursive")
  expect_s3_class(result, "cssem_prediction")
  expect_true(all(is.finite(result$predictions$prediction)))
  expect_true(any(result$predictions$source == "recursive"))
  expect_true(any(grepl("Quality", result$availability$required_constructs)))
})

test_that("recursive prediction uses observed mediators without unused ancestors", {
  generated <- cssem:::.structural_validation_data("linear", n = 42, seed = 39, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  chain <- specify_structure(
    Quality ~ linear(Trust),
    Loyalty ~ linear(Quality),
    order = c("Trust", "Quality", "Loyalty"))
  association <- associate(fit, chain, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  quality_items <- generated$model$constructs$Quality$indicators
  target_items <- generated$model$constructs$Loyalty$indicators
  mediator_only <- generated$data[, setdiff(quality_items, target_items), drop = FALSE]

  result <- predict(association, mediator_only, outcomes = "Loyalty", mode = "recursive")
  expect_true(all(is.finite(result$predictions$prediction)))
  expect_true(all(result$predictions$source == "observed"))
})

test_that("recursive prediction follows row-specific upstream demand", {
  set.seed(41)
  n <- 30L
  data <- data.frame(
    a1 = sample(1:3, n, replace = TRUE), a2 = sample(1:3, n, replace = TRUE),
    b1 = rnorm(n), b2 = rnorm(n),
    c1 = rnorm(n), c2 = rnorm(n), d1 = rnorm(n), d2 = rnorm(n),
    check.names = FALSE)
  model <- specify_measurement(
    A = ordinal("a1", "a2"), B = continuous("b1", "b2"),
    C = continuous("c1", "c2"), D = continuous("d1", "d2"), folds = 3L)
  fit <- fit_states(model, data, seed = 2, iterations = 1, diagnostics = FALSE)
  chain <- specify_structure(
    B ~ linear(A), C ~ linear(B), D ~ linear(C),
    order = c("A", "B", "C", "D"))
  association <- associate(fit, chain, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  newdata <- data.frame(a1 = c("irrelevant", "1"),
    a2 = c("irrelevant", "1"),
    b1 = c(NA_real_, data$b1[[2L]]),
    b2 = c(NA_real_, data$b2[[2L]]),
    c1 = c(data$c1[[1L]], NA_real_), c2 = c(data$c2[[1L]], NA_real_))

  result <- predict(association, newdata, outcomes = "D", mode = "recursive")
  expect_true(all(is.finite(result$predictions$prediction)))
})

test_that("recursive prediction ignores invalid unused ancestor columns", {
  set.seed(42)
  n <- 30L
  data <- data.frame(
    a1 = rnorm(n), a2 = rnorm(n), b1 = rnorm(n), b2 = rnorm(n),
    c1 = rnorm(n), c2 = rnorm(n), d1 = rnorm(n), d2 = rnorm(n))
  model <- specify_measurement(
    A = continuous("a1", "a2"), B = continuous("b1", "b2"),
    C = continuous("c1", "c2"), D = continuous("d1", "d2"), folds = 3L)
  fit <- fit_states(model, data, seed = 2, iterations = 1, diagnostics = FALSE)
  chain <- specify_structure(
    B ~ linear(A), C ~ linear(B), D ~ linear(C),
    order = c("A", "B", "C", "D"))
  association <- associate(fit, chain, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  newdata <- data.frame(a1 = rep(Inf, n), a2 = rep(Inf, n),
    b1 = data$b1, b2 = data$b2, c1 = data$c1, c2 = data$c2)

  expect_no_error(predict(association, newdata, outcomes = "D", mode = "recursive"))
})

test_that("recursive prediction does not promote prior-only states to inputs", {
  generated <- cssem:::.structural_validation_data("linear", n = 42, seed = 40, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  chain <- specify_structure(
    Quality ~ linear(Trust),
    Loyalty ~ linear(Quality),
    order = c("Trust", "Quality", "Loyalty"))
  association <- associate(fit, chain, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  trust_items <- generated$model$constructs$Trust$indicators
  predictor_only <- generated$data[, trust_items, drop = FALSE]
  predictor_only[] <- NA_real_

  result <- predict(association, predictor_only, outcomes = "Loyalty", mode = "recursive",
    missing_policy = "na")
  expect_true(all(is.na(result$predictions$prediction)))
  expect_true(all(result$predictions$status %in% c("unavailable", "missing_input")))
})

test_that("recursive prediction reports unavailable exogenous inputs", {
  generated <- cssem:::.structural_validation_data("linear", n = 36, seed = 35, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  missing <- generated$data[, setdiff(names(generated$data), c(
    generated$model$constructs$Trust$indicators,
    generated$model$constructs$Quality$indicators,
    generated$model$constructs$Loyalty$indicators)), drop = FALSE]

  expect_error(
    predict(association, missing, outcomes = "Loyalty", mode = "recursive"),
    "unavailable exogenous|requires indicator"
  )
})

test_that("recursive prediction rejects structural cycles", {
  generated <- cssem:::.structural_validation_data("linear", n = 30, seed = 36, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  linear_model <- function(parent, outcome) list(
    outcome = outcome,
    shapes = stats::setNames("linear", parent),
    infos = stats::setNames(list(list(shape = "linear")), parent),
    coefficient = c(0, 1),
    maps = stats::setNames(list(2L), parent)
  )
  association$full_models <- list(A = linear_model("B", "A"), B = linear_model("A", "B"))
  expect_error(
    predict(association, generated$data, outcomes = "A", mode = "recursive"),
    "cycle"
  )
})

test_that("prediction assessment reports errors, calibration, and a training baseline", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 37, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)

  assessment <- prediction_assessment(association, generated$data, outcomes = "Loyalty")
  expect_s3_class(assessment, "cssem_prediction_assessment")
  expect_true(all(c("outcome", "status", "n", "rmse", "mae", "r_squared",
    "calibration_intercept", "calibration_slope", "baseline_rmse") %in%
    names(assessment$metrics)))
  expect_identical(assessment$metrics$status, "ok")
  expect_gt(assessment$metrics$n, 0)
  expect_true(is.finite(assessment$metrics$rmse))
  expect_true(is.finite(assessment$metrics$baseline_rmse))
  expect_output(print(assessment), "CS-SEM prediction assessment")
})

test_that("prediction assessment makes unavailable targets explicit", {
  generated <- cssem:::.structural_validation_data("linear", n = 36, seed = 38, items = 3, missing = 0)
  generated$model$folds <- 3L
  fit <- fit_states(generated$model, generated$data, seed = 2, iterations = 1, diagnostics = FALSE)
  association <- associate(fit, generated$structure, structural_repeats = 1L,
    shadow_scope = "temporal", seed = 3)
  target_items <- generated$model$constructs$Loyalty$indicators
  predictor_only <- generated$data[, setdiff(names(generated$data), target_items), drop = FALSE]

  assessment <- prediction_assessment(association, predictor_only, outcomes = "Loyalty")
  expect_identical(assessment$metrics$status, "target_unavailable")
  expect_identical(assessment$metrics$n, 0L)
  expect_true(is.na(assessment$metrics$rmse))

  no_baseline <- prediction_assessment(association, generated$data, outcomes = "Loyalty",
    baseline = "none")
  expect_true(is.na(no_baseline$metrics$baseline_rmse))
})
