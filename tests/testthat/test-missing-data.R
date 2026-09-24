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

test_that("association reliability excludes prior-only rows", {
  # Regression: fit-level reliability averaged over prior-only rows, whose
  # posterior variance is the prior's, and the association passed that
  # deflated value to mediation, causal, and moderation corrections although
  # those rows are excluded from structural estimation.
  set.seed(72)
  n <- 360L
  latent <- matrix(stats::rnorm(n * 2L), n, 2L)
  item <- function(z) as.integer(cut(.8 * z + stats::rnorm(n, sd = .6), c(-Inf, -.8, 0, .8, Inf)))
  data <- data.frame(a1 = item(latent[, 1]), a2 = item(latent[, 1]), a3 = item(latent[, 1]),
    b1 = item(latent[, 2]), b2 = item(latent[, 2]), b3 = item(latent[, 2]))
  model <- specify_measurement(A = ordinal("a1", "a2", "a3"), B = ordinal("b1", "b2", "b3"), folds = 3L)
  structure <- specify_structure(B ~ linear(A), order = c("A", "B"))

  complete_fit <- cssem:::.fit_states_quiet(model, data, seed = 3L, iterations = 4L, diagnostics = FALSE)
  complete <- associate(complete_fit, structure, structural_repeats = 1L, seed = 1L)
  expect_equal(unname(complete$reliability[c("A", "B")]),
    unname(complete_fit$reliability[c("A", "B")]), tolerance = 1e-10)

  data[1:60, c("a1", "a2", "a3")] <- NA
  fit <- cssem:::.fit_states_quiet(model, data, seed = 3L, iterations = 4L, diagnostics = FALSE)
  association <- associate(fit, structure, structural_repeats = 1L, seed = 1L)
  retained <- as.integer(association$row_ids)
  scores <- fit$locked_scores$A[retained]; variance <- fit$score_posterior_sd$A[retained]^2
  expected <- stats::var(scores) / (stats::var(scores) + mean(variance))
  expect_equal(association$reliability[["A"]], expected, tolerance = 1e-10)
  expect_gt(association$reliability[["A"]], fit$reliability[["A"]])
})

test_that("structural ledgers name cross-outcome complete-case exclusions", {
  # Regression: associate() excludes a row when any declared construct is
  # incomplete, but the ledger for an unaffected outcome reported
  # "missing_score" although that outcome's own scores were complete.
  set.seed(73)
  n <- 54L
  data <- data.frame(a1 = sample(1:5, n, replace = TRUE), a2 = sample(1:5, n, replace = TRUE),
    a3 = sample(1:5, n, replace = TRUE), x = rnorm(n), y = rnorm(n), z = rnorm(n))
  data[5L, c("a1", "a2", "a3")] <- NA
  fit <- fit_states(specify_measurement(A = ordinal("a1", "a2", "a3"), X = manifest("x"),
    Y = manifest("y"), Z = manifest("z"), folds = 3L), data, seed = 4L,
    iterations = 1L, diagnostics = FALSE)
  association <- associate(fit, specify_structure(Y ~ linear(X), Z ~ linear(A),
    order = c("A", "X", "Y", "Z")), structural_repeats = 1L, shadow_scope = "temporal")
  rows <- sample_accounting(association)$rows
  row_y <- rows[rows$stage == "structural" & rows$target == "Y" & rows$row_id == 5L, , drop = FALSE]
  row_z <- rows[rows$stage == "structural" & rows$target == "Z" & rows$row_id == 5L, , drop = FALSE]
  expect_identical(row_y$status, "excluded")
  expect_identical(row_y$reason, "other_declared_construct_incomplete")
  expect_identical(row_z$reason, "prior_only_measurement")
})
