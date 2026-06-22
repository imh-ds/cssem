test_that("fit locks scores and scoring rejects schema mismatch", {
  d <- simulate_cssem_data(n = 60, seed = 3)
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  f <- cssem_fit(m, d, seed = 4, iterations = 3)
  expect_equal(nrow(f$locked_scores), 60)
  expect_true(all(is.finite(f$locked_scores$A)))
  expect_identical(f$measurement_engine$A$estimator, "marginal_graded_response")
  expect_equal(nrow(cssem_residual_diagnostics(f, "A")), 6)
  expect_error(cssem_score(f, d[paste0("a", 4:1)]))
  expect_error(cssem_score(f, d[paste0("a", 1:3)]))
})

test_that("cross-fitting retains a rare ordinal category schema", {
  d <- simulate_cssem_data(n = 45, seed = 12)
  d$a1 <- 2L
  d$a1[1] <- 1L
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  expect_silent(f <- cssem_fit(m, d, seed = 8, iterations = 2, diagnostics = FALSE))
  expect_true(all(is.finite(f$locked_scores$A)))
})

test_that("exploratory presets lighten model and fit defaults", {
  d <- simulate_cssem_data(n = 60, seed = 9)
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), preset = "exploratory")
  f <- cssem_fit(m, d, seed = 5, diagnostics = FALSE, preset = "exploratory")
  expect_equal(m$folds, 2L)
  expect_equal(f$measurement_engine$A$iterations, 8L)
  expect_true(all(is.finite(f$locked_scores$A)))
})
