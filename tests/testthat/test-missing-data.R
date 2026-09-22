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
  data[4L, c("a1", "a2", "a3")] <- NA
  fit <- fit_states(specify_measurement(A = ordinal("a1", "a2", "a3"),
    X = manifest("x"), Y = manifest("y"), folds = 3L), data, seed = 4L,
    iterations = 1L, diagnostics = FALSE)
  association <- associate(fit, specify_structure(Y ~ linear(X) + linear(A), order = c("A", "X", "Y")),
    structural_repeats = 1L, shadow_scope = "temporal", missing_policy = "complete")
  expect_equal(association$missing_policy, "complete")
  accounting <- sample_accounting(association)
  expect_true(any(accounting$summary$stage == "structural" & accounting$summary$target == "Y"))
  structural <- accounting$rows[accounting$rows$stage == "structural" & accounting$rows$target == "Y", , drop = FALSE]
  expect_true(any(structural$status == "excluded"))
  expect_true(any(structural$reason %in% c("prior_only_measurement", "measurement_excluded", "missing_score")))
  expect_true(all(accounting$summary$n_effective[accounting$summary$stage == "structural"] <
    accounting$summary$n_total[accounting$summary$stage == "structural"]))

  expect_error(
    associate(fit, specify_structure(Y ~ linear(X) + linear(A), order = c("A", "X", "Y")),
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

test_that("sample accounting reports independent units after listwise filtering", {
  set.seed(73)
  n <- 36L
  data <- data.frame(
    a1 = sample(1:5, n, replace = TRUE),
    a2 = sample(1:5, n, replace = TRUE),
    a3 = sample(1:5, n, replace = TRUE),
    x = rnorm(n), y = rnorm(n), subject = rep(seq_len(12), each = 3)
  )
  data[c(2L, 20L), "a1"] <- NA
  model <- specify_measurement(
    A = ordinal("a1", "a2", "a3"),
    X = manifest("x"), Y = manifest("y"), folds = 3L
  )
  fit <- fit_states(model, data, cluster = "subject", missing_policy = "listwise",
    iterations = 1L, diagnostics = FALSE)
  accounting <- sample_accounting(fit)
  expect_equal(accounting$independent_unit_n, length(unique(fit$cluster_ids)))
  expect_true(all(c("cluster_id", "n_rows", "retained_rows") %in% names(accounting$unit_summary)))
  expect_equal(sum(accounting$unit_summary$retained_rows), nrow(fit$data))

  association <- associate(fit,
    specify_structure(Y ~ linear(X) + linear(A), order = c("A", "X", "Y")),
    structural_repeats = 1L, shadow_scope = "temporal", missing_policy = "complete")
  association_accounting <- sample_accounting(association)
  expect_true(is.numeric(association_accounting$independent_unit_n))
  expect_true(nrow(association_accounting$unit_summary) > 0L)
  structural_units <- association_accounting$summary$independent_unit_n[
    association_accounting$summary$stage == "structural"]
  expect_true(all(is.finite(structural_units)))

  effect <- causal_effect(association, treatment = "X", outcome = "Y", disattenuate = FALSE)
  effect_accounting <- sample_accounting(effect)
  expect_true(is.numeric(effect_accounting$independent_unit_n))
  expect_true(nrow(effect_accounting$unit_summary) > 0L)
})

test_that("unit accounting deduplicates row ledgers across targets", {
  ids <- rep(seq_len(3L), each = 2L)
  units <- cssem:::.unit_accounting(ids, input_n = length(ids),
    retained_ids = seq_along(ids), effective_ids = c(1L, 2L, 3L, 4L, 1L, 4L))
  expect_equal(units$unit_summary$effective_rows, c(2L, 2L, 0L))
})

test_that("unsupported survey design metadata is rejected explicitly", {
  data <- simulate_states(n = 36, seed = 74, missing = 0)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3L)
  expect_error(
    fit_states(model, data, design = list(weights = rep(1, nrow(data))),
      iterations = 1L, diagnostics = FALSE),
    "survey design"
  )
})
