.evidence_pipeline <- function(n = 500, seed = 5) {
  generated <- cssem:::.structural_validation_data("linear", n, 5, items = 4L)
  fit <- cssem_fit(generated$model, generated$data, seed = seed, iterations = 4, diagnostics = FALSE)
  association <- cssem_associate(fit, generated$structure, structural_repeats = 2L, seed = seed, shadow_scope = "temporal")
  routing <- cssem_route(association,
    causal = list(cssem_causal_edge("Quality", "Loyalty", adjust = "Trust")),
    temporal_order = c("Trust", "Quality", "Loyalty"))
  list(fit = fit, association = association, routing = routing)
}

.evidence_mediation <- function(n = 4000, seed = 3) {
  set.seed(seed); std <- function(v) as.numeric(scale(v))
  add_error <- function(l, rho) std(l + stats::rnorm(length(l), sd = sqrt((1 - rho) / rho)))
  C <- stats::rnorm(n); X <- stats::rnorm(n)
  M <- std(0.50 * X + 0.40 * C + stats::rnorm(n, sd = .8))
  Y <- std(0.20 * X + 0.45 * M + 0.35 * C + stats::rnorm(n, sd = .7))
  rho <- c(X = .75, C = .80, M = .72, Y = .78)
  scores <- data.frame(X = add_error(X, rho["X"]), C = add_error(C, rho["C"]),
                       M = add_error(M, rho["M"]), Y = add_error(Y, rho["Y"]))
  structure <- cssem_structure(list(M = c("X", "C"), Y = c("X", "M", "C")), order = c("C", "X", "M", "Y"))
  full <- list(M = cssem:::.fit_shape_model(scores, "M", c(X = "linear", C = "linear")),
    Y = cssem:::.fit_shape_model(scores, "Y", c(X = "linear", M = "linear", C = "linear")))
  assoc <- structure(list(scores = scores, reliability = stats::setNames(as.numeric(rho), c("X", "C", "M", "Y")),
    full_models = full, structure = structure), class = "cssem_association")
  cssem_causal_mediation(assoc, "X", "Y", adjust = "C", temporal_order = c("C", "X", "M", "Y"))
}

test_that("evidence report composes constructs, effects, and causal claims", {
  p <- .evidence_pipeline()
  report <- cssem_evidence_report(p$association, fit = p$fit, routing = p$routing)
  expect_s3_class(report, "cssem_evidence_report")
  expect_equal(nrow(report$constructs), 3L)
  expect_true(all(c("path", "shape", "estimate", "causal_status", "verdict") %in% names(report$effects)))
  # Causal status is routed in, not hardcoded associational.
  causal_edge <- report$effects[report$effects$path == "Quality→Loyalty", ]
  expect_identical(causal_edge$causal_status, "causal")
  expect_match(causal_edge$verdict, "causal")
  expect_true(is.finite(causal_edge$robustness_value))
  # Other edges default to associational.
  expect_true(all(report$effects$causal_status[report$effects$path != "Quality→Loyalty"] == "associational"))
  # The routing's direct causal claim is carried into the causal-claims section.
  expect_equal(nrow(report$causal_claims), 1L)
  expect_identical(report$causal_claims$type, "direct")
})

test_that("a smooth edge (no scalar estimate) is scored on contribution", {
  # Stable, high predictive contribution, no scalar coefficient: not weak.
  expect_false(cssem:::.edge_verdict(1.0, NA_real_, 0.20, 0.03, "associational") == "Weak / unstable")
  # Negligible predictive contribution is weak even when stable.
  expect_identical(cssem:::.edge_verdict(1.0, NA_real_, 0.001, 0.03, "associational"), "Weak / unstable")
  # A causal edge earns a causal-pathway verdict; a representational edge does not.
  expect_match(cssem:::.edge_verdict(1.0, 0.4, 0.2, 0.03, "causal"), "causal pathway")
  expect_match(cssem:::.edge_verdict(1.0, 0.4, 0.2, 0.03, "representational"), "Representational")
})

test_that("without routing, all effect edges are associational and optional sections are absent", {
  p <- .evidence_pipeline()
  report <- cssem_evidence_report(p$association)
  expect_true(all(report$effects$causal_status == "associational"))
  expect_null(report$causal_claims)
  expect_null(report$constructs)
})

test_that("interventional-mediation claims appear in the causal-claims section", {
  p <- .evidence_pipeline()
  med <- .evidence_mediation()
  report <- cssem_evidence_report(p$association, causal = list(med))
  expect_equal(nrow(report$causal_claims), 1L)
  expect_match(report$causal_claims$type, "interventional")
  expect_identical(report$causal_claims$estimand, "interventional")
  expect_identical(report$causal_claims$verdict, "Supported under assumptions")
})

test_that("evidence report guards reject bad inputs", {
  p <- .evidence_pipeline()
  expect_error(cssem_evidence_report(list()), "cssem_association")
  expect_error(cssem_evidence_report(p$association, fit = list()), "cssem_fit")
  expect_error(cssem_evidence_report(p$association, routing = list()), "cssem_routing")
  expect_error(cssem_evidence_report(p$association, causal = list(1)), "cssem_causal")
  expect_output(print(cssem_evidence_report(p$association, fit = p$fit, routing = p$routing)), "Evidence Report")
})
