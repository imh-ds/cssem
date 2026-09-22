test_that("structural response families are explicit and validated", {
  binary <- structural_outcome("binomial")
  expect_s3_class(binary, "cssem_structural_outcome")
  expect_equal(binary$family, "binomial")
  expect_equal(binary$link, "logit")
  ordinal <- ordinal_outcome(levels = 1:5)
  expect_equal(ordinal$family, "ordinal")
  expect_equal(ordinal$levels, 1:5)

  specification <- specify_structure(
    Y ~ linear(X),
    families = list(Y = binary_outcome()),
    order = c("X", "Y"))
  expect_equal(specification$response_families$Y$family, "binomial")
  expect_equal(specification$response_families$X$family, "gaussian")
  expect_error(structural_outcome("nominal"), "family")
  expect_error(structural_outcome("binomial", link = "probit"), "link")
  expect_error(specify_structure(Y ~ X, families = list(X = binary_outcome())), "outcome")
})

test_that("binary structural outcomes use probability-scale diagnostics", {
  set.seed(1101)
  n <- 150
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(.7 * x))
  fit <- structure(list(locked_scores = data.frame(X = x, Y = y),
    folds = rep(1:3, length.out = n)), class = "fit_states")
  specification <- specify_structure(Y ~ linear(X),
    families = list(Y = binary_outcome()), order = c("X", "Y"))
  association <- associate(fit, specification, structural_repeats = 2L,
    shadow_scope = "temporal", seed = 1101)
  expect_equal(association$response_families$Y$family, "binomial")
  expect_true(all(association$predictions$Y$theory >= 0 & association$predictions$Y$theory <= 1))
  expect_true(all(c("log_loss", "brier", "accuracy") %in% names(association$candidate_metrics)))
  expect_true(all(is.finite(association$candidate_metrics$log_loss)))
  expect_error(associate(fit, specification, reliability = c(X = .8)), "categorical")
})

test_that("ordinal structural outcomes return ordered probabilities", {
  set.seed(1102)
  n <- 180
  x <- rnorm(n)
  latent <- .8 * x + rlogis(n)
  y <- cut(latent, breaks = c(-Inf, -.7, .7, Inf), labels = FALSE)
  fit <- structure(list(locked_scores = data.frame(X = x, Y = y),
    folds = rep(1:3, length.out = n)), class = "fit_states")
  specification <- specify_structure(Y ~ linear(X),
    families = list(Y = ordinal_outcome(levels = 1:3)), order = c("X", "Y"))
  association <- associate(fit, specification, structural_repeats = 2L,
    shadow_scope = "temporal", seed = 1102)
  expect_equal(association$response_families$Y$family, "ordinal")
  expect_true(all(association$predictions$Y$theory >= 1 & association$predictions$Y$theory <= 3))
  expect_true(all(c("log_loss", "accuracy") %in% names(association$candidate_metrics)))
  expect_true(all(is.finite(association$candidate_metrics$log_loss)))

  bad <- fit
  bad$locked_scores$Y <- bad$locked_scores$Y + .25
  expect_error(associate(bad, specification, shadow_scope = "temporal"), "integer")
})

test_that("categorical structural outcomes reject unsupported shapes", {
  specification <- specify_structure(Y ~ smooth(X),
    families = list(Y = binary_outcome()), order = c("X", "Y"))
  fit <- structure(list(locked_scores = data.frame(X = rnorm(30), Y = rbinom(30, 1, .5)),
    folds = rep(1:3, length.out = 30)), class = "fit_states")
  expect_error(associate(fit, specification, shadow_scope = "temporal"), "linear")
})
