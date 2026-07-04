.causal_mediation_fixture <- function(n = 6000, seed = 3) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  add_error <- function(latent, rho) std(latent + stats::rnorm(length(latent), sd = sqrt((1 - rho) / rho)))
  C <- stats::rnorm(n); X <- stats::rnorm(n)
  M <- std(0.50 * X + 0.40 * C + stats::rnorm(n, sd = .8))
  Y <- std(0.20 * X + 0.45 * M + 0.35 * C + stats::rnorm(n, sd = .7))
  truth <- unname(stats::coef(stats::lm(M ~ X + C))["X"] * stats::coef(stats::lm(Y ~ X + M + C))["M"])
  rho <- c(X = .75, C = .80, M = .72, Y = .78)
  scores <- data.frame(X = add_error(X, rho["X"]), C = add_error(C, rho["C"]),
                       M = add_error(M, rho["M"]), Y = add_error(Y, rho["Y"]))
  structure <- cssem_structure(list(M = c("X", "C"), Y = c("X", "M", "C")), order = c("C", "X", "M", "Y"))
  full_models <- list(
    M = cssem:::.fit_shape_model(scores, "M", c(X = "linear", C = "linear")),
    Y = cssem:::.fit_shape_model(scores, "Y", c(X = "linear", M = "linear", C = "linear")))
  list(truth = truth, association = structure(list(scores = scores,
    reliability = stats::setNames(as.numeric(rho), c("X", "C", "M", "Y")),
    full_models = full_models, structure = structure), class = "cssem_association"))
}

test_that("interventional mediation recovers the indirect effect composites attenuate", {
  fixture <- .causal_mediation_fixture()
  cm <- cssem_causal_mediation(fixture$association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "M", "Y"))
  expect_s3_class(cm, "cssem_causal_mediation")
  indirect <- cm$summary
  disattenuated <- indirect$disattenuated_effect[indirect$component == "indirect_total"]
  naive <- indirect$naive_effect[indirect$component == "indirect_total"]
  # Disattenuation recovers the truth that the naive composite attenuates.
  expect_lt(abs(disattenuated - fixture$truth), abs(naive - fixture$truth))
  expect_lt(abs(disattenuated - fixture$truth), 0.03)
  expect_lt(naive, fixture$truth - 0.05)
})

test_that("a causal label requires a declared temporal order", {
  fixture <- .causal_mediation_fixture(n = 2000)
  expect_identical(cssem_causal_mediation(fixture$association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "M", "Y"))$label, "causal_under_assumptions")
  expect_identical(cssem_causal_mediation(fixture$association, "X", "Y", adjust = "C")$label,
    "adjusted_association")
})

test_that("the admissibility panel is populated", {
  fixture <- .causal_mediation_fixture(n = 2000)
  cm <- cssem_causal_mediation(fixture$association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "M", "Y"))
  expect_true(is.finite(cm$identification_strength) && cm$identification_strength > 0.5)
  expect_true(is.finite(cm$outcome_r2) && cm$outcome_r2 > 0)
  expect_true(is.finite(cm$robustness_value) && cm$robustness_value > 0)
  expect_equal(cm$mediators, "M")
  expect_output(print(cm), "Causal admissibility panel")
  expect_output(print(cm), "Causal under assumptions")
})

test_that("causal-mediation discipline guards reject bad specifications", {
  fixture <- .causal_mediation_fixture(n = 1500)
  association <- fixture$association
  # No adjustment set.
  expect_error(cssem_causal_mediation(association, "X", "Y"), "adjustment set")
  # Adjusting for a post-treatment (downstream) construct.
  expect_error(cssem_causal_mediation(association, "X", "Y", adjust = "M"), "downstream of the treatment")
  # A confounder that is not a declared predictor of the outcome/mediator cannot
  # adjust the effect. Build a structure where C does not predict M.
  scores <- association$scores
  structure2 <- cssem_structure(list(M = "X", Y = c("X", "M", "C")), order = c("C", "X", "M", "Y"))
  full2 <- list(M = cssem:::.fit_shape_model(scores, "M", c(X = "linear")),
    Y = cssem:::.fit_shape_model(scores, "Y", c(X = "linear", M = "linear", C = "linear")))
  association2 <- structure(list(scores = scores, reliability = association$reliability,
    full_models = full2, structure = structure2), class = "cssem_association")
  expect_error(cssem_causal_mediation(association2, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "M", "Y")), "declared as predictor")
  # Requesting a non-mediator as a mediator.
  expect_error(cssem_causal_mediation(association, "X", "Y", adjust = "C", mediators = "C"),
    "intermediate constructs")
})

test_that("bootstrap intervals attach to the interventional decomposition", {
  fixture <- .causal_mediation_fixture(n = 1500)
  cm <- cssem_causal_mediation(fixture$association, "X", "Y", adjust = "C",
    temporal_order = c("C", "X", "M", "Y"), eiv_bootstrap = 60, seed = 5)
  indirect <- cm$summary[cm$summary$component == "indirect_total", ]
  expect_true(is.finite(indirect$disattenuated_ci_low) && is.finite(indirect$disattenuated_ci_high))
  expect_lt(indirect$disattenuated_ci_low, indirect$disattenuated_ci_high)
})
