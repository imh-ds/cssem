test_that("fit locks scores and scoring rejects schema mismatch", {
  d <- simulate_states(n = 60, seed = 3)
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  f <- fit_states(m, d, seed = 4, iterations = 3)
  expect_equal(nrow(f$locked_scores), 60)
  expect_true(all(is.finite(f$locked_scores$A)))
  expect_identical(f$measurement_engine$A$estimator, "marginal_graded_response")
  expect_equal(nrow(residual_diagnostics(f, "A")), 6)
  expect_error(score_states(f, d[paste0("a", 4:1)]))
  expect_error(score_states(f, d[paste0("a", 1:3)]))
})

test_that("fit reports reliability, posterior SD, and respondent information", {
  d <- simulate_states(n = 120, seed = 7)
  m <- cssem_model(list(
    A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
    B = list(indicators = paste0("b", 1:4), scales = "ordinal")
  ), folds = 3)
  f <- fit_states(m, d, seed = 4, iterations = 4, draws = 5L, diagnostics = FALSE)
  expect_true(all(f$reliability > 0 & f$reliability <= 1))
  expect_equal(dim(f$score_posterior_sd), c(120L, 2L))
  expect_equal(dim(f$uncertainty_draws), c(120L, 2L, 5L))
  info <- respondent_information(f)
  expect_equal(nrow(info), 120L)
  expect_true("information_weight" %in% names(info))
  expect_equal(mean(info$information_weight), 1, tolerance = 1e-6)
  card <- construct_card(f, "A")
  expect_true(is.data.frame(card$respondent_information))
  expect_true(card$respondent_information$reliability > 0)
})

test_that("structural validation manifest exposes v0.4 realistic scenarios", {
  screening <- structural_manifest("screening")
  expect_true(all(c("loading", "careless", "skew") %in% names(screening)))
  expect_true(all(c("plateau", "threshold", "diminishing", "low_reliability", "careless", "skewed") %in% screening$scenario))
  expect_equal(screening$loading[screening$scenario == "low_reliability"], .55)
  expect_true(screening$careless[screening$scenario == "careless"] > 0)
})

test_that("cross-fitting retains a rare ordinal category schema", {
  d <- simulate_states(n = 45, seed = 12)
  d$a1 <- 2L
  d$a1[1] <- 1L
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  expect_silent(f <- fit_states(m, d, seed = 8, iterations = 2, diagnostics = FALSE))
  expect_true(all(is.finite(f$locked_scores$A)))
})

test_that("exploratory presets lighten model and fit defaults", {
  d <- simulate_states(n = 60, seed = 9)
  m <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), preset = "exploratory")
  f <- fit_states(m, d, seed = 5, diagnostics = FALSE, preset = "exploratory")
  expect_equal(m$folds, 2L)
  expect_equal(f$measurement_engine$A$iterations, 8L)
  expect_true(all(is.finite(f$locked_scores$A)))
})

test_that("continuous-only and mixed constructs get a real posterior and reliability", {
  set.seed(21)
  n <- 150
  z <- rnorm(n)
  d <- data.frame(
    a1 = pmin(pmax(round(z + rnorm(n)), -2), 2) + 3,
    a2 = pmin(pmax(round(z + rnorm(n)), -2), 2) + 3,
    a3 = pmin(pmax(round(z + rnorm(n)), -2), 2) + 3,
    c1 = 1.2 * z + rnorm(n, sd = .6),
    c2 = 1.0 * z + rnorm(n, sd = .6)
  )
  m <- specify_measurement(
    Ord = ordinal("a1", "a2", "a3"),
    Cont = continuous("c1", "c2"),
    folds = 3
  )
  f <- fit_states(m, d, seed = 3, iterations = 3, diagnostics = FALSE)
  expect_identical(f$measurement_engine$Ord$estimator, "marginal_graded_response")
  expect_identical(f$measurement_engine$Cont$estimator, "marginal_linear_factor")
  expect_true(is.finite(f$reliability[["Cont"]]))
  expect_true(f$reliability[["Cont"]] > 0 && f$reliability[["Cont"]] <= 1)
  expect_true(all(is.finite(f$score_posterior_sd$Cont)))
})

test_that("manifest() constructs pass through standardized (or raw) with asserted reliability", {
  set.seed(22)
  n <- 80
  d <- data.frame(
    a1 = sample(1:5, n, replace = TRUE), a2 = sample(1:5, n, replace = TRUE),
    age = round(rnorm(n, 40, 10))
  )
  m <- specify_measurement(A = ordinal("a1", "a2"), Age = manifest("age"), folds = 2)
  f <- fit_states(m, d, seed = 1, iterations = 2, diagnostics = FALSE)
  expect_equal(f$reliability[["Age"]], 1)
  expect_equal(mean(f$locked_scores$Age), 0, tolerance = 1e-8)
  expect_equal(sd(f$locked_scores$Age), 1, tolerance = 1e-6)
  expect_identical(f$measurement_engine$Age$estimator, "manifest")
  expect_true(all(is.na(f$score_posterior_sd$Age)))

  m_raw <- specify_measurement(A = ordinal("a1", "a2"), Age = manifest("age", standardize = FALSE), folds = 2)
  f_raw <- fit_states(m_raw, d, seed = 1, iterations = 2, diagnostics = FALSE)
  expect_equal(range(f_raw$locked_scores$Age), range(d$age))
})

test_that("ordinal() rejects non-integer category codes instead of truncating", {
  d <- simulate_states(n = 40, seed = 5)
  d$a1 <- d$a1 + 0.5
  m <- specify_measurement(A = ordinal("a1", "a2", "a3", "a4"))
  expect_error(fit_states(m, d, seed = 1, iterations = 2, diagnostics = FALSE), "whole-number")
})
