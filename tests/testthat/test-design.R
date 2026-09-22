test_that("fit_states resolves cluster columns and preserves unit metadata", {
  data <- simulate_states(n = 48, seed = 901, missing = 0)
  data$subject <- rep(seq_len(16), each = 3)
  data[5L, "a1"] <- NA
  model <- specify_measurement(
    A = ordinal(paste0("a", 1:4)),
    folds = 3L
  )

  fit <- fit_states(model, data, cluster = "subject", missing_policy = "listwise",
    iterations = 1L, diagnostics = FALSE)

  expect_identical(fit$cluster_ids, data$subject[fit$row_ids])
  expect_equal(fit$independent_unit_n, length(unique(fit$cluster_ids)))
  expect_equal(
    fit$cluster_summary$n_rows[match(names(table(fit$cluster_ids)), fit$cluster_summary$cluster_id)],
    as.integer(table(fit$cluster_ids))
  )
  expect_false(5L %in% fit$row_ids)
  expect_equal(fit$fit_settings$cluster, "subject")
})

test_that("fit_states accepts a full-length cluster vector", {
  data <- simulate_states(n = 42, seed = 902, missing = 0)
  ids <- rep(seq_len(14), each = 3)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3L)

  fit <- fit_states(model, data, cluster = ids, iterations = 1L,
    diagnostics = FALSE)

  expect_identical(fit$cluster_ids, ids)
  expect_equal(fit$independent_unit_n, 14L)
  expect_equal(fit$measurement_split$unit_n, 14L)
})

test_that("cluster labels fail with actionable validation errors", {
  data <- simulate_states(n = 36, seed = 903, missing = 0)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)), folds = 3L)

  expect_error(
    fit_states(model, data, cluster = rep(NA_character_, nrow(data)),
      iterations = 1L, diagnostics = FALSE),
    "missing"
  )
  bad <- rep(seq_len(12), each = 3)
  bad[2L] <- Inf
  expect_error(
    fit_states(model, data, cluster = bad, iterations = 1L, diagnostics = FALSE),
    "finite"
  )
  expect_error(
    cssem:::.reject_unsupported_design_fields(list(weights = rep(1, nrow(data)))),
    "survey design"
  )
})
