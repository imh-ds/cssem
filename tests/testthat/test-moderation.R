.moderation_fixture <- function(n = 4000, seed = 2) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  X <- std(stats::rnorm(n)); W <- std(stats::rnorm(n)); M <- std(.5 * X + stats::rnorm(n, sd = .4))
  Y <- std(.2 * X + .4 * M + .1 * W + .5 * (M * W) + stats::rnorm(n, sd = .4))
  scores <- data.frame(X = X, W = W, M = M, Y = Y)
  fit <- structure(list(locked_scores = scores, folds = sample(rep(1:3, length.out = n)),
    reliability = stats::setNames(rep(.99, 4L), c("X", "W", "M", "Y"))), class = "fit_states")
  linear <- function(predictors) stats::setNames(lapply(predictors, function(p) cssem_effect("linear")), predictors)
  list(scores = scores, fit = fit,
    structure = cssem_structure(list(M = linear("X"), Y = linear(c("X", "M", "W", "M:W"))), order = c("X", "W", "M", "Y")))
}

test_that("a declared interaction fits as a product moderation term", {
  fx <- .moderation_fixture()
  association <- associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  model <- association$full_models$Y
  expect_identical(unname(model$shapes[["M:W"]]), "product")
  # Recovers a positive interaction (true 0.5 before Y standardization).
  expect_gt(model$coefficient[model$maps[["M:W"]]], 0.3)
})

test_that("conditional_indirect_effect reports conditional effects and the index", {
  fx <- .moderation_fixture()
  association <- associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  mm <- conditional_indirect_effect(association, "X", "Y", "W", eiv_bootstrap = 100L, seed = 2)
  expect_s3_class(mm, "conditional_indirect_effect")
  expect_equal(nrow(mm$conditional), 3L)
  expect_true(all(c("indirect", "ci_low", "ci_high") %in% names(mm$conditional)))
  # Indirect effect strengthens with the moderator: positive index, CI above zero.
  expect_gt(mm$index$estimate, 0)
  expect_gt(mm$index$ci[[1L]], 0)
  expect_lt(mm$conditional$indirect[[1L]], mm$conditional$indirect[[3L]])
  expect_output(print(mm), "index of moderated mediation")
})

test_that("moderated mediation benchmark scores every engine against the latent truth", {
  skip_if_not_installed("seminr")
  manifest <- conditional_indirect_effect_manifest("screening")
  row <- manifest[manifest$scenario == "b_path" & manifest$loading == .80, ][1, ]
  results <- validate_conditional_indirect_effect_comparator(row, reps = 1, seed = 6000, iterations = 4,
    eiv_bootstrap = 40, seminr_bootstrap = 40)
  expect_setequal(unique(results$engine), c("cssem_disattenuated", "cssem_naive", "lavaan_native", "seminr_native"))
  disattenuated <- results[results$engine == "cssem_disattenuated", ]
  naive <- results[results$engine == "cssem_naive", ]
  # Disattenuation reduces the bias of the index of moderated mediation.
  expect_lt(disattenuated$abs_bias, naive$abs_bias)
})

test_that("conditional_slopes reports conditional slopes and a Johnson-Neyman region", {
  fx <- .moderation_fixture()
  association <- associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  ss <- conditional_slopes(association, "Y", "M", "W", eiv_bootstrap = 100L, seed = 2)
  expect_s3_class(ss, "conditional_slopes")
  expect_equal(nrow(ss$slopes), 3L)
  expect_true(all(c("slope", "ci_low", "ci_high") %in% names(ss$slopes)))
  # The slope of M on Y increases with the moderator (positive interaction).
  expect_lt(ss$slopes$slope[[1L]], ss$slopes$slope[[3L]])
  expect_gt(ss$interaction, 0)
  expect_false(is.null(ss$johnson_neyman))
  expect_error(conditional_slopes(association, "Y", "M", "X"), "No declared interaction")
  expect_output(print(ss), "Johnson-Neyman")
})

test_that("moderated mediation guards reject bad inputs", {
  fx <- .moderation_fixture()
  association <- associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  expect_error(conditional_indirect_effect(association, "X", "Y", "X"), "distinct")
  no_interaction <- associate(fx$fit,
    cssem_structure(list(M = "X", Y = c("X", "M", "W")), order = c("X", "W", "M", "Y")),
    structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  expect_error(conditional_indirect_effect(no_interaction, "X", "Y", "W"), "interaction")
})

test_that("moderated mediation harness recovers the index of moderated mediation", {
  manifest <- conditional_indirect_effect_manifest("screening")
  expect_true(all(c("scenario", "n", "loading", "items") %in% names(manifest)))
  results <- validate_conditional_indirect_effect(manifest[1, ], reps = 1, seed = 5000, iterations = 4, eiv_bootstrap = 60)
  expect_true(all(c("true_index", "naive_index", "disattenuated_index",
    "naive_abs_bias", "disattenuated_abs_bias", "index_covers_truth",
    "truth_method", "sample_oracle_index", "naive_sample_oracle_abs_error",
    "disattenuated_sample_oracle_abs_error") %in% names(results)))
  expect_identical(results$truth_method, "analytic_interaction")
  # Disattenuation reduces the bias of the index of moderated mediation.
  expect_lt(results$disattenuated_abs_bias, results$naive_abs_bias)
})

test_that("moderated mediation validation uses an independent interaction truth", {
  fx <- .moderation_fixture()
  truth <- cssem:::.moderated_mediation_truth(fx$scores, fx$structure, "W", c(-1, 0, 1))
  a <- unname(stats::coef(stats::lm(M ~ X, fx$scores))[["X"]])
  outcome <- stats::coef(stats::lm(Y ~ X + M + W + M:W, fx$scores))
  b <- unname(outcome[["M"]]); interaction <- unname(outcome[["M:W"]])
  values <- cssem:::.moderator_values(fx$scores, "W", c(-1, 0, 1))
  expected <- a * (b + interaction * values)
  expect_identical(attr(truth, "method"), "analytic_interaction")
  expect_equal(truth$conditional, expected, tolerance = 1e-10)
  expect_equal(truth$index, cssem:::.moderated_index(expected, c(-1, 0, 1), sd(fx$scores$W)),
    tolerance = 1e-10)
})

test_that("conditional indirect effects match analytic moderated mediation", {
  fx <- .moderation_fixture()
  association <- associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  a_hat <- unname(stats::coef(stats::lm(M ~ X, fx$scores))[2L])
  y_coef <- stats::coef(stats::lm(Y ~ X + M + W + M:W, fx$scores))
  models <- stats::setNames(vector("list", 4L), c("X", "W", "M", "Y"))
  for (outcome in names(association$full_models)) models[[outcome]] <- association$full_models[[outcome]]
  order <- cssem:::.resolve_temporal_order(fx$structure, names(fx$scores))
  paths <- cssem:::.structure_paths(fx$structure, "X", "Y")
  conditional <- function(w) {
    at_w <- fx$scores; at_w$W <- w
    cssem:::.decompose_effects(models, at_w, order, "X", "Y", paths, 1)$indirect_total
  }
  for (w in c(-1, 0, 1)) {
    expect_equal(conditional(w), a_hat * (y_coef[["M"]] + y_coef[["M:W"]] * w), tolerance = 1e-6)
  }
  # A positive index of moderated mediation: the indirect effect grows with W.
  expect_gt(conditional(1) - conditional(-1), 0)
})

test_that("an endogenous treatment propagates through moderated mediation", {
  # Regression: X is itself endogenous (X = f(P)), so it carries a stage model.
  # The propagation engine must preserve X's injected shift or every conditional
  # indirect effect collapses to zero.
  set.seed(22); std <- function(v) as.numeric(scale(v)); n <- 6000
  P <- stats::rnorm(n); W <- std(stats::rnorm(n))
  X <- std(0.5 * P + stats::rnorm(n, sd = .8))
  M <- std(0.5 * X + stats::rnorm(n, sd = .6))
  Y <- std(0.2 * X + 0.4 * M + 0.1 * W + 0.35 * (M * W) + stats::rnorm(n, sd = .6))
  scores <- data.frame(P = P, W = W, X = X, M = M, Y = Y)
  structure <- cssem_structure(
    list(X = "P", M = "X", Y = c("X", "M", "W", "M:W")),
    order = c("P", "W", "X", "M", "Y"))
  full_models <- list(
    X = cssem:::.fit_shape_model(scores, "X", c(P = "linear")),
    M = cssem:::.fit_shape_model(scores, "M", c(X = "linear")),
    Y = cssem:::.fit_shape_model(scores, "Y",
      stats::setNames(c("linear", "linear", "linear", "product"), c("X", "M", "W", "M:W"))))
  association <- structure(list(scores = scores,
    reliability = stats::setNames(rep(1, 5), c("P", "W", "X", "M", "Y")),
    full_models = full_models, structure = structure), class = "cssem_association")
  mm <- conditional_indirect_effect(association, "X", "Y", "W", disattenuate = FALSE)
  # Conditional indirect effects are non-zero and increase with W (positive M:W).
  expect_gt(mean(abs(mm$conditional$indirect)), 0.05)
  expect_gt(mm$index$estimate, 0)
})

test_that("moderator levels are evaluated on the moderator's own scale", {
  # Regression: levels are documented in SD units but were used as raw values.
  # For a manifest() covariate kept in natural units (age in years) the "+1 SD"
  # row was evaluated at the raw value 1, i.e. at one year old.
  set.seed(32)
  n <- 400
  age <- round(stats::rnorm(n, 45, 12))
  x <- stats::rnorm(n)
  y <- .3 * x + .02 * age + .01 * x * age + stats::rnorm(n, sd = .8)
  fit <- structure(list(
    locked_scores = data.frame(X = as.numeric(scale(x)), Age = age, Y = as.numeric(scale(y))),
    folds = sample(rep(1:3, length.out = n)),
    reliability = c(X = .85, Age = 1, Y = .85)), class = "fit_states")
  association <- associate(fit, specify_structure(Y ~ linear(X) + linear(Age) + X:Age,
    order = c("Age", "X", "Y")), seed = 32)
  slopes <- conditional_slopes(association, outcome = "Y", predictor = "X", moderator = "Age")

  centre <- mean(fit$locked_scores$Age); spread <- stats::sd(fit$locked_scores$Age)
  expect_equal(slopes$slopes$moderator_value, centre + c(-1, 0, 1) * spread, tolerance = 1e-8)
  # The reported slope is the one implied by the fitted coefficients at that value.
  expect_equal(slopes$slopes$slope,
    unname(vapply(centre + c(-1, 0, 1) * spread, function(w) slopes$slopes$slope[[2L]] +
      slopes$interaction * (w - centre), numeric(1))), tolerance = 1e-8)
  # The slope really varies across the range: the old raw -1/0/1 rows spanned
  # three years and were nearly identical.
  expect_gt(diff(range(slopes$slopes$slope)), 4 * abs(slopes$interaction))
})

test_that("standardized moderators are unaffected by the scale conversion", {
  set.seed(33)
  n <- 300
  w <- stats::rnorm(n); x <- stats::rnorm(n)
  y <- .3 * x + .2 * w + .25 * x * w + stats::rnorm(n, sd = .7)
  fit <- structure(list(
    locked_scores = data.frame(W = as.numeric(scale(w)), X = as.numeric(scale(x)),
      Y = as.numeric(scale(y))),
    folds = sample(rep(1:3, length.out = n)),
    reliability = c(W = .8, X = .8, Y = .8)), class = "fit_states")
  association <- associate(fit, specify_structure(Y ~ linear(X) + linear(W) + X:W,
    order = c("W", "X", "Y")), seed = 33)
  slopes <- conditional_slopes(association, outcome = "Y", predictor = "X", moderator = "W")
  expect_equal(slopes$slopes$moderator_value, c(-1, 0, 1), tolerance = 1e-8)
})

test_that("printing a moderated mediation handles a zero or missing index", {
  # Regression: the narrative read `if (x$index$estimate > 0)`, which errored
  # with "missing value where TRUE/FALSE needed" on an NA index and described a
  # zero index as "weakens".
  skeleton <- function(estimate) structure(list(
    x = "X", y = "Y", moderator = "W", levels = c(-1, 0, 1),
    conditional = data.frame(level = c("-1 SD", "mean", "+1 SD"), moderator_value = c(-1, 0, 1),
      naive_indirect = c(.2, .2, .2), disattenuated_indirect = c(.25, .25, .25),
      indirect = c(.25, .25, .25), stringsAsFactors = FALSE),
    index = list(estimate = estimate, basis = "disattenuated", ci = c(NA_real_, NA_real_)),
    n = 300L, disattenuated = TRUE, bootstrap = 0L, min_reliability = .8,
    status = "associational"), class = "conditional_indirect_effect")

  expect_output(print(skeleton(0)), "does not vary detectably")
  expect_output(print(skeleton(NA_real_)), "could not be computed")
  expect_output(print(skeleton(NA_real_)), "index of moderated mediation: not available")
  expect_output(print(skeleton(.12)), "strengthens")
  expect_output(print(skeleton(-.12)), "weakens")
})
