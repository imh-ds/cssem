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
  expect_false("X" %in% names(specification$response_families))
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
  parameters <- parameter_table(association)
  expect_match(parameters$units[[1L]], "log-odds")
  expect_equal(parameters$estimate_basis[[1L]], "maximum_likelihood")
  expect_true(is.finite(parameters$se[[1L]]))
  expect_true(is.finite(parameters$ci_low[[1L]]) && is.finite(parameters$ci_high[[1L]]))
  expect_match(parameters$uncertainty_method[[1L]], "Wald")
  expect_match(parameters$availability_reason[[1L]], "Categorical.*correction")
  set.seed(3100L)
  caller_rng <- .Random.seed
  contrast <- marginal_contrast(association, "Y", "X", c(-1, 1), reps = 80L,
    level = .90, seed = 3101L)
  expect_identical(.Random.seed, caller_rng)
  repeated_contrast <- marginal_contrast(association, "Y", "X", c(-1, 1),
    reps = 80L, level = .90, seed = 3101L)
  expect_identical(contrast$draws, repeated_contrast$draws)
  point_contrast <- marginal_contrast(association, "Y", "X", c(-1, 1))
  expect_equal(point_contrast$interval_status, "not_requested")
  expect_true(all(is.na(point_contrast$contrast$estimate_ci_low)))
  expect_error(marginal_contrast(association, "Y", "X", c(-1, 1), reps = 1.5),
    "non-negative whole-number")
  expect_s3_class(contrast, "cssem_marginal_contrast")
  expect_true(all(is.finite(contrast$contrast$probability_contrast)))
  expect_true(all(is.finite(contrast$contrast$estimate_ci_low)))
  expect_true(all(contrast$contrast$estimate_ci_low <= contrast$contrast$estimate_ci_high))
  expect_true(all(contrast$contrast$probability_contrast_ci_low <= contrast$contrast$probability_contrast_ci_high))
  expect_equal(contrast$successful_replicates, 80L)
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
  ordinal_parameter <- parameter_table(association)
  expect_match(ordinal_parameter$units[[1L]], "cumulative log-odds")
  expect_true(is.finite(ordinal_parameter$se[[1L]]))
  expect_true(is.finite(ordinal_parameter$ci_low[[1L]]) && is.finite(ordinal_parameter$ci_high[[1L]]))

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

test_that("categorical coefficient Wald intervals recover their declared slopes", {
  set.seed(3102)
  simulations <- 80L
  n <- 500L
  beta <- .65
  binary_covered <- logical(simulations)
  ordinal_covered <- logical(simulations)
  for (i in seq_len(simulations)) {
    x <- rnorm(n)
    y_binary <- rbinom(n, 1L, plogis(beta * x))
    binary_model <- cssem:::.fit_shape_model(data.frame(X = x, Y = y_binary), "Y",
      c(X = "linear"), binary_outcome())
    binary_ci <- beta + c(-1, 1) * qnorm(.95) * binary_model$coefficient_se[["X"]]
    binary_covered[[i]] <- binary_ci[[1L]] <= beta && beta <= binary_ci[[2L]]

    thresholds <- c(-.7, .7)
    cdf <- cbind(plogis(thresholds[[1L]] - beta * x), plogis(thresholds[[2L]] - beta * x))
    category_probabilities <- cbind(cdf[, 1L], cdf[, 2L] - cdf[, 1L], 1 - cdf[, 2L])
    u <- runif(n)
    y_ordinal <- 1L + (u > category_probabilities[, 1L]) +
      (u > rowSums(category_probabilities[, 1:2, drop = FALSE]))
    ordinal_model <- cssem:::.fit_shape_model(data.frame(X = x, Y = y_ordinal), "Y",
      c(X = "linear"), ordinal_outcome(1:3))
    ordinal_ci <- beta + c(-1, 1) * qnorm(.95) * ordinal_model$coefficient_se[["X"]]
    ordinal_covered[[i]] <- ordinal_ci[[1L]] <= beta && beta <= ordinal_ci[[2L]]
  }
  expect_gte(mean(binary_covered), .84)
  expect_gte(mean(ordinal_covered), .84)
})

test_that("marginal contrast bootstrap intervals cover binary and ordinal effects", {
  set.seed(3103)
  simulations <- 40L
  n <- 240L
  reps <- 120L
  beta <- .70
  thresholds <- c(-.7, .7)
  binary_truth <- plogis(beta) - plogis(-beta)
  ordinal_truth <- sum(plogis(thresholds + beta) - plogis(thresholds - beta))
  binary_probability_truth <- c("0" = -binary_truth, "1" = binary_truth)
  ordinal_low_cdf <- plogis(thresholds + beta)
  ordinal_high_cdf <- plogis(thresholds - beta)
  ordinal_low_probabilities <- c(ordinal_low_cdf[[1L]], diff(ordinal_low_cdf),
    1 - ordinal_low_cdf[[2L]])
  ordinal_high_probabilities <- c(ordinal_high_cdf[[1L]], diff(ordinal_high_cdf),
    1 - ordinal_high_cdf[[2L]])
  ordinal_probability_truth <- setNames(ordinal_high_probabilities - ordinal_low_probabilities,
    as.character(1:3))
  covered <- matrix(FALSE, simulations, 7L, dimnames = list(NULL, c(
    "binary_expected", paste0("binary_probability_", names(binary_probability_truth)),
    "ordinal_expected", paste0("ordinal_probability_", names(ordinal_probability_truth)))))

  for (i in seq_len(simulations)) {
    x <- rnorm(n)
    y_binary <- rbinom(n, 1L, plogis(beta * x))
    binary_scores <- data.frame(X = x, Y = y_binary)
    binary_family <- binary_outcome()
    binary_model <- cssem:::.fit_shape_model(binary_scores, "Y", c(X = "linear"), binary_family)
    binary_association <- structure(list(full_models = list(Y = binary_model),
      scores = binary_scores, response_families = list(Y = binary_family)),
      class = "cssem_association")
    binary_interval <- marginal_contrast(binary_association, "Y", "X", c(-1, 1),
      reps = reps, level = .90, seed = 4000L + i)$contrast
    covered[i, "binary_expected"] <- binary_interval$estimate_ci_low[[1L]] <= binary_truth &&
      binary_truth <= binary_interval$estimate_ci_high[[1L]]
    for (category in names(binary_probability_truth)) {
      row <- match(category, binary_interval$category)
      covered[i, paste0("binary_probability_", category)] <-
        binary_interval$probability_contrast_ci_low[[row]] <= binary_probability_truth[[category]] &&
          binary_probability_truth[[category]] <= binary_interval$probability_contrast_ci_high[[row]]
    }

    cdf <- cbind(plogis(thresholds[[1L]] - beta * x), plogis(thresholds[[2L]] - beta * x))
    probabilities <- cbind(cdf[, 1L], cdf[, 2L] - cdf[, 1L], 1 - cdf[, 2L])
    u <- runif(n)
    y_ordinal <- 1L + (u > probabilities[, 1L]) + (u > rowSums(probabilities[, 1:2, drop = FALSE]))
    ordinal_scores <- data.frame(X = x, Y = y_ordinal)
    ordinal_family <- ordinal_outcome(1:3)
    ordinal_model <- cssem:::.fit_shape_model(ordinal_scores, "Y", c(X = "linear"), ordinal_family)
    ordinal_association <- structure(list(full_models = list(Y = ordinal_model),
      scores = ordinal_scores, response_families = list(Y = ordinal_family)),
      class = "cssem_association")
    ordinal_interval <- marginal_contrast(ordinal_association, "Y", "X", c(-1, 1),
      reps = reps, level = .90, seed = 5000L + i)$contrast
    covered[i, "ordinal_expected"] <- ordinal_interval$estimate_ci_low[[1L]] <= ordinal_truth &&
      ordinal_truth <= ordinal_interval$estimate_ci_high[[1L]]
    for (category in names(ordinal_probability_truth)) {
      row <- match(category, ordinal_interval$category)
      covered[i, paste0("ordinal_probability_", category)] <-
        ordinal_interval$probability_contrast_ci_low[[row]] <= ordinal_probability_truth[[category]] &&
          ordinal_probability_truth[[category]] <= ordinal_interval$probability_contrast_ci_high[[row]]
    }
  }
  expect_true(all(colMeans(covered) >= .70))
})
