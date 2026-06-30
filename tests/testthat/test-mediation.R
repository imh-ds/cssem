.mediation_fixture <- function(scenario = c("single", "parallel", "serial"), n = 4000, seed = 1) {
  scenario <- match.arg(scenario)
  set.seed(seed)
  std <- function(v) as.numeric(scale(v))
  X <- std(stats::rnorm(n))
  if (scenario == "single") {
    M <- std(.5 * X + stats::rnorm(n, sd = .4)); Y <- std(.3 * X + .4 * M + stats::rnorm(n, sd = .4))
    scores <- data.frame(X = X, M = M, Y = Y)
    structure <- cssem_structure(list(M = "X", Y = c("X", "M")), order = c("X", "M", "Y"))
  } else if (scenario == "parallel") {
    M1 <- std(.5 * X + stats::rnorm(n, sd = .4)); M2 <- std(-.3 * X + stats::rnorm(n, sd = .4))
    Y <- std(.2 * X + .4 * M1 + .6 * M2 + stats::rnorm(n, sd = .4))
    scores <- data.frame(X = X, M1 = M1, M2 = M2, Y = Y)
    structure <- cssem_structure(list(M1 = "X", M2 = "X", Y = c("X", "M1", "M2")), order = c("X", "M1", "M2", "Y"))
  } else {
    M1 <- std(.5 * X + stats::rnorm(n, sd = .4)); M2 <- std(.3 * X + .7 * M1 + stats::rnorm(n, sd = .4))
    Y <- std(.2 * X + .1 * M1 + .6 * M2 + stats::rnorm(n, sd = .4))
    scores <- data.frame(X = X, M1 = M1, M2 = M2, Y = Y)
    structure <- cssem_structure(list(M1 = "X", M2 = c("X", "M1"), Y = c("X", "M1", "M2")), order = c("X", "M1", "M2", "Y"))
  }
  models <- stats::setNames(vector("list", length(scores)), names(scores))
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    models[[outcome]] <- cssem:::.fit_shape_model(scores, outcome, stats::setNames(rep("linear", length(predictors)), predictors))
  }
  list(scores = scores, structure = structure, models = models)
}

test_that("linear single mediation reproduces the product of paths", {
  fx <- .mediation_fixture("single")
  out <- cssem:::.cssem_mediation_core(fx$models, fx$scores, fx$structure, "X", "Y")
  # Reference: standard linear path decomposition on the same scores.
  a <- unname(stats::coef(stats::lm(M ~ X, fx$scores))[2L])
  y_coef <- stats::coef(stats::lm(Y ~ X + M, fx$scores))
  indirect <- out$summary$naive_effect[out$summary$component == "indirect_total"]
  direct <- out$summary$naive_effect[out$summary$component == "direct"]
  expect_equal(indirect, a * unname(y_coef[["M"]]), tolerance = 0.01)
  expect_equal(direct, unname(y_coef[["X"]]), tolerance = 0.01)
  expect_equal(nrow(out$path_specific), 1L)
})

test_that("parallel and serial paths are enumerated and sum to the indirect total", {
  parallel <- cssem:::.cssem_mediation_core(.mediation_fixture("parallel")$models, .mediation_fixture("parallel")$scores,
    .mediation_fixture("parallel")$structure, "X", "Y")
  expect_equal(nrow(parallel$path_specific), 2L)
  expect_equal(parallel$path_sum_residual, 0, tolerance = 1e-6)

  fx <- .mediation_fixture("serial")
  serial <- cssem:::.cssem_mediation_core(fx$models, fx$scores, fx$structure, "X", "Y")
  expect_equal(nrow(serial$path_specific), 3L)
  expect_equal(serial$path_sum_residual, 0, tolerance = 1e-6)
})

test_that("disattenuation increases the indirect effect under measurement error", {
  set.seed(3)
  std <- function(v) as.numeric(scale(v))
  attenuate <- function(latent, rho) std(sqrt(rho) * std(latent) + sqrt(1 - rho) * stats::rnorm(length(latent)))
  n <- 5000; rho <- 0.8
  X <- std(stats::rnorm(n)); M <- std(.5 * X + stats::rnorm(n, sd = .4)); Y <- std(.3 * X + .4 * M + stats::rnorm(n, sd = .4))
  # Latent-scale indirect effect the disattenuation should recover.
  latent_indirect <- unname(stats::coef(stats::lm(M ~ X))[2L]) * unname(stats::coef(stats::lm(Y ~ X + M))[["M"]])
  obs <- data.frame(X = attenuate(X, rho), M = attenuate(M, rho), Y = attenuate(Y, rho))
  structure <- cssem_structure(list(M = "X", Y = c("X", "M")), order = c("X", "M", "Y"))
  models <- list(X = NULL,
    M = cssem:::.fit_shape_model(obs, "M", c(X = "linear")),
    Y = cssem:::.fit_shape_model(obs, "Y", c(X = "linear", M = "linear")))
  out <- cssem:::.cssem_mediation_core(models, obs, structure, "X", "Y", reliability = c(X = rho, M = rho, Y = rho))
  row <- out$summary[out$summary$component == "indirect_total", ]
  expect_true(row$disattenuated_effect > row$naive_effect)
  expect_equal(row$disattenuated_effect, latent_indirect, tolerance = 0.05)
})

test_that("a path through a smooth edge is not disattenuated", {
  fx <- .mediation_fixture("single")
  fx$models$Y <- cssem:::.fit_shape_model(fx$scores, "Y", c(X = "linear", M = "smooth_df3"))
  out <- cssem:::.cssem_mediation_core(fx$models, fx$scores, fx$structure, "X", "Y", reliability = c(X = .8, M = .8, Y = .8))
  expect_false(out$path_specific$disattenuated[[1L]])
  expect_true(is.na(out$path_specific$disattenuated_effect[[1L]]))
})

test_that("public cssem_mediation runs end to end through the real pipeline", {
  generated <- cssem:::.structural_validation_data("linear", 400, 5, items = 4L)
  fit <- cssem_fit(generated$model, generated$data, seed = 5, iterations = 4, diagnostics = FALSE)
  association <- cssem_associate(fit, generated$structure, structural_repeats = 2L, seed = 5, shadow_scope = "temporal")
  med <- cssem_mediation(association, "Trust", "Loyalty", eiv_bootstrap = 50L, seed = 5)
  expect_s3_class(med, "cssem_mediation")
  # Trust -> Quality -> Loyalty is the declared mediating path.
  expect_true(nrow(med$path_specific) >= 1L)
  expect_true(all(c("total", "direct", "indirect_total") %in% med$summary$component))
  expect_true(all(c("disattenuated_ci_low", "disattenuated_ci_high") %in% names(med$summary)))
})

test_that("mediation validation harness recovers the latent indirect effect", {
  manifest <- cssem_mediation_validation_manifest("screening")
  expect_true(all(c("scenario", "n", "loading", "items") %in% names(manifest)))
  results <- cssem_run_mediation_validation(manifest[1, ], reps = 1, seed = 4026, iterations = 4, eiv_bootstrap = 60)
  expect_true(all(c("true_indirect", "naive_indirect", "disattenuated_indirect",
    "naive_abs_bias", "disattenuated_abs_bias", "disattenuated_covers_truth") %in% names(results)))
  expect_lt(results$disattenuated_abs_bias, results$naive_abs_bias)
})

test_that("mediation benchmark scores every engine against the latent truth", {
  manifest <- cssem_mediation_validation_manifest("benchmark")
  expect_true(all(c("scenario", "n", "loading", "items", "edge_shape") %in% names(manifest)))
  expect_true(all(manifest$edge_shape == "linear"))
  row <- manifest[manifest$scenario == "single" & manifest$loading == .80 & manifest$n == 400L, ]
  results <- cssem_run_mediation_comparator_validation(row, reps = 1, seed = 4026, iterations = 4,
    eiv_bootstrap = 40, seminr_bootstrap = 40)
  expect_setequal(unique(results$engine), c("cssem_disattenuated", "cssem_naive", "lavaan_native", "seminr_native"))
  disattenuated <- results[results$engine == "cssem_disattenuated", ]
  naive <- results[results$engine == "cssem_naive", ]
  # Disattenuation reduces indirect-effect bias relative to the naive estimate.
  expect_lt(disattenuated$abs_bias, naive$abs_bias)
})

test_that("mediation guards reject bad inputs", {
  fx <- .mediation_fixture("single")
  models <- stats::setNames(fx$models, names(fx$scores))
  association <- structure(list(structure = fx$structure, full_models = Filter(Negate(is.null), models),
    reliability = stats::setNames(rep(0.8, 3), c("X", "M", "Y")), scores = fx$scores), class = "cssem_association")
  expect_error(cssem_mediation(association, "Y", "X"), "precede")
  expect_error(cssem_mediation(association, "X", "Q"), "locked construct")
  expect_error(cssem_mediation(association, "X", "X"), "differ")

  med <- cssem_mediation(association, "X", "Y", eiv_bootstrap = 50L, seed = 1L)
  expect_s3_class(med, "cssem_mediation")
  ledger <- cssem_mediation_ledger(med)
  expect_true(all(c("total", "direct", "indirect_total", "path_indirect") %in% ledger$component))
  expect_true(all(c("ci_low", "ci_high", "basis", "status") %in% names(ledger)))
  expect_output(print(med), "associational mediation")
})
