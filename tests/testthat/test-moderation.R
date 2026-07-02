.moderation_fixture <- function(n = 4000, seed = 2) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  X <- std(stats::rnorm(n)); W <- std(stats::rnorm(n)); M <- std(.5 * X + stats::rnorm(n, sd = .4))
  Y <- std(.2 * X + .4 * M + .1 * W + .5 * (M * W) + stats::rnorm(n, sd = .4))
  scores <- data.frame(X = X, W = W, M = M, Y = Y)
  fit <- structure(list(locked_scores = scores, folds = sample(rep(1:3, length.out = n)),
    reliability = stats::setNames(rep(.99, 4L), c("X", "W", "M", "Y"))), class = "cssem_fit")
  linear <- function(predictors) stats::setNames(lapply(predictors, function(p) cssem_effect("linear")), predictors)
  list(scores = scores, fit = fit,
    structure = cssem_structure(list(M = linear("X"), Y = linear(c("X", "M", "W", "M:W"))), order = c("X", "W", "M", "Y")))
}

test_that("a declared interaction fits as a product moderation term", {
  fx <- .moderation_fixture()
  association <- cssem_associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  model <- association$full_models$Y
  expect_identical(unname(model$shapes[["M:W"]]), "product")
  # Recovers a positive interaction (true 0.5 before Y standardization).
  expect_gt(model$coefficient[model$maps[["M:W"]]], 0.3)
})

test_that("cssem_moderated_mediation reports conditional effects and the index", {
  fx <- .moderation_fixture()
  association <- cssem_associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  mm <- cssem_moderated_mediation(association, "X", "Y", "W", eiv_bootstrap = 100L, seed = 2)
  expect_s3_class(mm, "cssem_moderated_mediation")
  expect_equal(nrow(mm$conditional), 3L)
  expect_true(all(c("indirect", "ci_low", "ci_high") %in% names(mm$conditional)))
  # Indirect effect strengthens with the moderator: positive index, CI above zero.
  expect_gt(mm$index$estimate, 0)
  expect_gt(mm$index$ci[[1L]], 0)
  expect_lt(mm$conditional$indirect[[1L]], mm$conditional$indirect[[3L]])
  expect_output(print(mm), "index of moderated mediation")
})

test_that("moderated mediation guards reject bad inputs", {
  fx <- .moderation_fixture()
  association <- cssem_associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  expect_error(cssem_moderated_mediation(association, "X", "Y", "X"), "distinct")
  no_interaction <- cssem_associate(fx$fit,
    cssem_structure(list(M = "X", Y = c("X", "M", "W")), order = c("X", "W", "M", "Y")),
    structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
  expect_error(cssem_moderated_mediation(no_interaction, "X", "Y", "W"), "interaction")
})

test_that("conditional indirect effects match analytic moderated mediation", {
  fx <- .moderation_fixture()
  association <- cssem_associate(fx$fit, fx$structure, structural_repeats = 2L, seed = 2, shadow_scope = "temporal")
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
