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
  expect_equal(sum(association$candidate_metrics$selected), 2)
  expect_true(all(c("mean_mse_improvement", "mse_improvement_se") %in% names(association$candidate_metrics)))
  expect_equal(association$structural_repeats, 3L)
  expect_identical(association$status, "associational")
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
