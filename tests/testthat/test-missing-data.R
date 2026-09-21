test_that("measurement missing-data policies and row accounting are explicit", {
  set.seed(71)
  n <- 48L
  data <- data.frame(
    a1 = sample(1:5, n, replace = TRUE),
    a2 = sample(1:5, n, replace = TRUE),
    a3 = sample(1:5, n, replace = TRUE),
    x = rnorm(n)
  )
  data[3L, c("a1", "a2", "a3")] <- NA
  data[7L, "a1"] <- NA
  data[11L, "x"] <- NA
  model <- specify_measurement(
    A = ordinal("a1", "a2", "a3"),
    X = manifest("x"), folds = 3L
  )

  partial <- fit_states(model, data, seed = 3L, iterations = 1L,
    diagnostics = FALSE, missing_policy = "partial")
  accounting <- sample_accounting(partial)
  expect_s3_class(accounting, "cssem_sample_accounting")
  expect_equal(accounting$policy, "partial")
  expect_true(all(c("stage", "target", "n_total", "n_effective", "n_prior_only",
    "n_excluded") %in% names(accounting$summary)))
  expect_true(any(accounting$rows$target == "A" & accounting$rows$status == "prior_only"))
  expect_true(any(accounting$rows$target == "A" & accounting$rows$status == "partial"))
  expect_true(any(accounting$rows$target == "X" & accounting$rows$status == "prior_only"))
  expect_true(any(accounting$rows$target == "X" &
    grepl("^missing_indicator", accounting$rows$reason)))

  listwise <- fit_states(model, data, seed = 3L, iterations = 1L,
    diagnostics = FALSE, missing_policy = "listwise")
  expect_equal(listwise$missing_policy, "listwise")
  expect_lt(nrow(listwise$data), nrow(data))
  listwise_accounting <- sample_accounting(listwise)
  expect_true(any(listwise_accounting$rows$status == "excluded"))
  expect_equal(listwise_accounting$input_n, nrow(data))
  expect_equal(listwise_accounting$retained_n, nrow(listwise$data))

  expect_error(
    fit_states(model, data, seed = 3L, iterations = 1L,
      diagnostics = FALSE, missing_policy = "error"),
    "missing_policy = \\\"error\\\""
  )
})

test_that("structural accounting reports complete-case exclusions by outcome", {
  set.seed(72)
  n <- 54L
  data <- data.frame(
    a1 = sample(1:5, n, replace = TRUE),
    a2 = sample(1:5, n, replace = TRUE),
    a3 = sample(1:5, n, replace = TRUE),
    x = rnorm(n), y = rnorm(n)
  )
  data[c(4L, 9L), "x"] <- NA
  fit <- fit_states(specify_measurement(A = ordinal("a1", "a2", "a3"),
    X = manifest("x"), Y = manifest("y"), folds = 3L), data, seed = 4L,
    iterations = 1L, diagnostics = FALSE)
  association <- associate(fit, specify_structure(Y ~ linear(X), order = c("A", "X", "Y")),
    structural_repeats = 1L, shadow_scope = "temporal", missing_policy = "complete")
  expect_equal(association$missing_policy, "complete")
  accounting <- sample_accounting(association)
  expect_true(any(accounting$summary$stage == "structural" & accounting$summary$target == "Y"))
  structural <- accounting$rows[accounting$rows$stage == "structural" & accounting$rows$target == "Y", , drop = FALSE]
  expect_true(any(structural$status == "excluded"))
  expect_true(any(structural$reason %in% c("measurement_excluded", "missing_score")))
  expect_true(all(accounting$summary$n_effective[accounting$summary$stage == "structural"] <
    accounting$summary$n_total[accounting$summary$stage == "structural"]))

  expect_error(
    associate(fit, specify_structure(Y ~ linear(X), order = c("A", "X", "Y")),
      structural_repeats = 1L, shadow_scope = "temporal", missing_policy = "error"),
    "missing_policy = \\\"error\\\""
  )

  effect <- causal_effect(association, treatment = "X", outcome = "Y",
    disattenuate = FALSE)
  effect_accounting <- sample_accounting(effect)
  expect_true(any(effect_accounting$summary$stage == "causal" &
    effect_accounting$summary$target == "X -> Y"))
  expect_equal(effect_accounting$input_n, nrow(data))
})
