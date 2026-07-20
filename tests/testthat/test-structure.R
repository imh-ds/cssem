test_that("associational layer uses locked scores and reports a shadow gap", {
  set.seed(10)
  n <- 120
  trust <- rnorm(n)
  satisfaction <- .65 * trust + rnorm(n, sd = .7)
  loyalty <- .35 * trust + .60 * satisfaction + rnorm(n, sd = .7)
  fit <- structure(list(
    locked_scores = data.frame(Trust = trust, Satisfaction = satisfaction, Loyalty = loyalty),
    folds = rep(1:3, length.out = n)
  ), class = "cssem_fit")
  structure_spec <- cssem_structure(list(
    Satisfaction = "Trust",
    Loyalty = c("Trust", "Satisfaction")
  ), order = c("Trust", "Satisfaction", "Loyalty"))
  association <- cssem_associate(fit, structure_spec)
  expect_s3_class(association, "cssem_association")
  expect_equal(nrow(cssem_specification_gap(association)), 4)
  expect_equal(nrow(cssem_specification_gap(association, "temporal")), 2)
  expect_true(all(cssem_specification_gap(association)$specification_gap ==
    cssem_specification_gap(association)$theory_r_squared - cssem_specification_gap(association)$shadow_r_squared))
  expect_equal(sum(association$candidate_metrics$selected), 3)
  expect_true(all(c("mean_mse_improvement", "mse_improvement_se") %in% names(association$candidate_metrics)))
  expect_equal(association$structural_repeats, 5L)
  expect_equal(nrow(cssem_effect_ledger(association)), 3L)
  expect_identical(association$status, "associational")
})

test_that("edge declarations support policies while preserving character vectors", {
  declared <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("monotone_increasing")),
    Loyalty = c("Trust", "Quality")
  ), order = c("Trust", "Quality", "Loyalty"))
  expect_equal(declared$effects$Quality$Trust$shape, "monotone_increasing")
  expect_equal(declared$effects$Loyalty$Trust$shape, "auto")
  expect_error(cssem_effect("not_a_shape"))
})

test_that("monotone candidates are constrained and selected edge by edge", {
  set.seed(44)
  n <- 220
  trust <- rnorm(n)
  quality <- .70 * trust + .85 * pmax(trust, 0) + rnorm(n, sd = .30)
  loyalty <- .45 * trust + .35 * quality + rnorm(n, sd = .60)
  fit <- structure(list(locked_scores = data.frame(Trust = trust, Quality = quality, Loyalty = loyalty),
    folds = sample(rep(1:3, length.out = n))), class = "cssem_fit")
  specification <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("auto")),
    Loyalty = list(Trust = cssem_effect("auto"), Quality = cssem_effect("auto"))
  ), order = c("Trust", "Quality", "Loyalty"))
  association <- cssem_associate(fit, specification, structural_repeats = 3, seed = 44, shape_stability_min = .50)
  quality <- association$candidate_metrics[association$candidate_metrics$outcome == "Quality", , drop = FALSE]
  expect_true(any(quality$shape == "monotone_increasing"))
  expect_lte(sum(association$candidate_metrics$selected & association$candidate_metrics$outcome == "Loyalty" & association$candidate_metrics$shape != "linear"), 1L)
  expect_true(all(c("edge_drop_mse_increase", "selection_frequency", "status") %in% names(cssem_effect_ledger(association))))
})

test_that("default selector retains a clear monotone-increasing edge", {
  set.seed(144)
  n <- 320
  trust <- rnorm(n)
  quality <- .85 * trust + 1.10 * pmax(trust + .20, 0) + rnorm(n, sd = .22)
  loyalty <- .30 * trust + .55 * quality + rnorm(n, sd = .45)
  fit <- structure(list(
    locked_scores = data.frame(Trust = trust, Quality = quality, Loyalty = loyalty),
    folds = sample(rep(1:3, length.out = n))
  ), class = "cssem_fit")
  specification <- cssem_structure(list(
    Quality = list(Trust = cssem_effect("monotone_increasing")),
    Loyalty = list(Trust = cssem_effect("auto"), Quality = cssem_effect("auto"))
  ), order = c("Trust", "Quality", "Loyalty"))
  association <- cssem_associate(fit, specification, structural_repeats = 5, seed = 144)
  quality <- association$candidate_metrics[association$candidate_metrics$outcome == "Quality" &
    association$candidate_metrics$predictor == "Trust" & association$candidate_metrics$selected, , drop = FALSE]
  expect_equal(quality$shape[[1L]], "monotone_increasing")
  expect_gte(quality$selection_frequency[[1L]], .70)
})

test_that("winner selection prefers stable monotone candidates within uncertainty band", {
  candidate_keys <- c("Trust::monotone_increasing", "Trust::smooth_df3")
  candidate_meta <- list(
    `Trust::monotone_increasing` = list(predictor = "Trust", shape = "monotone_increasing"),
    `Trust::smooth_df3` = list(predictor = "Trust", shape = "smooth_df3")
  )
  improvement <- c(`Trust::monotone_increasing` = .020, `Trust::smooth_df3` = .022)
  improvement_se <- c(`Trust::monotone_increasing` = .004, `Trust::smooth_df3` = .004)
  frequency <- c(`Trust::monotone_increasing` = .80, `Trust::smooth_df3` = 1)
  winner <- cssem:::.pick_shape_winner(candidate_keys, candidate_meta, improvement, improvement_se,
    frequency, smooth_uncertainty = 1, shape_stability_min = .70)
  expect_equal(winner, "Trust::monotone_increasing")
})

test_that("monotone basis retains training-fold knots for scoring", {
  trained <- cssem:::.train_basis(c(-2, -1, 0, 1, 2), "monotone_increasing")
  scored <- cssem:::.predict_basis(c(-10, 10), trained$info)
  expect_equal(ncol(scored), ncol(trained$values))
  expect_equal(ncol(trained$values), 4L)
  expect_false(any(trained$info$knots %in% c(-10, 10)))
})

test_that("structural repeated CV validates its repeat count", {
  fit <- structure(list(locked_scores = data.frame(A = rnorm(30), B = rnorm(30)), folds = rep(1:3, 10)), class = "cssem_fit")
  specification <- cssem_structure(list(B = "A"), order = c("A", "B"))
  expect_error(cssem_associate(fit, specification, structural_repeats = 0L), "at least 1")
})

test_that("structural declarations reject invalid self-effects", {
  expect_error(cssem_structure(list(Trust = "Trust")))
})

test_that("temporal shadows require a valid ordering for cyclic declarations", {
  fit <- structure(list(locked_scores = data.frame(A = rnorm(30), B = rnorm(30)), folds = rep(1:3, 10)), class = "cssem_fit")
  cyclic <- cssem_structure(list(A = "B", B = "A"))
  expect_error(cssem_associate(fit, cyclic, shadow_scope = "temporal"))
  expect_s3_class(cssem_associate(fit, cyclic, shadow_scope = "unrestricted"), "cssem_association")
})

test_that("exploratory preset lightens structural defaults", {
  set.seed(21)
  fit <- structure(list(
    locked_scores = data.frame(Trust = rnorm(60), Quality = rnorm(60), Loyalty = rnorm(60)),
    folds = sample(rep(1:3, length.out = 60))
  ), class = "cssem_fit")
  specification <- cssem_structure(list(
    Quality = "Trust",
    Loyalty = c("Trust", "Quality")
  ), order = c("Trust", "Quality", "Loyalty"))
  association <- cssem_associate(fit, specification, preset = "exploratory")
  expect_equal(association$structural_repeats, 2L)
  expect_identical(association$shadow_scope, "temporal")
})

test_that("smooth basis degrades gracefully when quantile knots collapse", {
  # Enough mass on one value that every df-based quantile knot equals the left
  # boundary, which errors in splines::ns() and previously aborted the fit.
  x <- c(rep(0, 90), seq(.1, 1, length.out = 10))
  expect_error(splines::ns(x, df = 4L))
  trained <- cssem:::.train_basis(x, "smooth_df4")
  reconstructed <- cssem:::.predict_basis(x, trained$info)
  expect_equal(ncol(as.matrix(reconstructed)), ncol(as.matrix(trained$values)))
  # Fully degenerate scores degrade to the single linear column.
  constant <- cssem:::.train_basis(rep(1, 50), "smooth_df4")
  expect_identical(ncol(as.matrix(constant$values)), 1L)
  expect_identical(ncol(as.matrix(cssem:::.predict_basis(rep(1, 50), constant$info))), 1L)
})
