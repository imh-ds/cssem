test_that("G1 exposes stable summaries and effect parameter tables", {
  set.seed(10)
  n <- 60
  scores <- data.frame(A = rnorm(n), B = rnorm(n))
  fit <- structure(list(
    locked_scores = scores,
    folds = rep(1:3, length.out = n),
    reliability = c(A = .8, B = .9),
    measurement_engine = list(), item_metrics = data.frame(),
    stability = c(A = .9, B = .9),
    redundancy = cor(scores),
    warnings = data.frame(type = character(), target = character(), detail = character()),
    residual_dependence = data.frame(),
    model = structure(list(constructs = list()), class = "cssem_model")
  ), class = "fit_states")
  association <- associate(fit, cssem_structure(list(B = "A"), order = c("A", "B")),
    structural_repeats = 1L, seed = 1L)

  fit_summary <- summary(fit)
  association_summary <- summary(association)
  table <- parameter_table(association)

  expect_s3_class(fit_summary, "summary.fit_states")
  expect_s3_class(association_summary, "summary.cssem_association")
  expect_true(is.data.frame(fit_summary$constructs))
  expect_true(is.data.frame(association_summary$effects))
  expect_true(all(c("outcome", "predictor", "estimate", "estimate_basis", "units",
    "uncertainty_method", "n", "available", "availability_reason") %in% names(table)))
  expect_equal(nrow(table), nrow(effect_ledger(association)))
  expect_equal(table$naive_estimate, effect_ledger(association)$naive_estimate)
  expect_equal(table$corrected_estimate, effect_ledger(association)$corrected_estimate)
  expect_equal(nobs(fit), n)
  expect_equal(nobs(association), n)
})
test_that("G1 extractors preserve unavailable uncertainty and OOF prediction labels", {
  n <- 50
  scores <- data.frame(A = seq_len(n), B = seq_len(n)^2)
  fit <- structure(list(locked_scores = scores, folds = rep(1:5, length.out = n)), class = "fit_states")
  association <- associate(fit, cssem_structure(list(B = "A"), order = c("A", "B")),
    structural_repeats = 1L, seed = 2L)

  table <- parameter_table(association)
  expect_true(all(table$available))
  expect_true(all(table$estimate_basis == "naive"))
  expect_true(all(is.na(table$corrected_estimate)))
  expect_true(all(is.na(table$se)))
  expect_true(all(nzchar(table$availability_reason)))
  expect_true(all(is.na(vcov(association))))
  expect_equal(ncol(confint(association)), 2L)
  expect_identical(attr(fitted(association), "prediction_type"), "out_of_fold")
  expect_identical(attr(residuals(association), "prediction_type"), "out_of_fold")
  expect_equal(nrow(fitted(association)), n)
  expect_equal(nrow(residuals(association)), n)
})

test_that("G1 update refits from the stored reproducible specification", {
  data <- simulate_states(n = 48, seed = 12)
  model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  fit <- fit_states(model, data, seed = 3, iterations = 2, diagnostics = FALSE)
  updated <- update(fit, iterations = 3, diagnostics = FALSE)
  expect_s3_class(updated, "fit_states")
  expect_equal(as.integer(updated$fit_settings$iterations), 3L)
  expect_equal(nrow(updated$locked_scores), nrow(data))
})
