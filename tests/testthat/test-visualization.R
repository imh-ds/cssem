visualization_fixture <- function() {
  set.seed(62)
  n <- 120
  scores <- data.frame(X = stats::rnorm(n), M = stats::rnorm(n))
  scores$W <- stats::rnorm(n)
  scores$M <- 0.5 * scores$X + stats::rnorm(n, sd = 0.5)
  scores$Y <- scores$M^2 + stats::rnorm(n, sd = 0.2)
  structure <- specify_structure(M ~ linear(X), Y ~ smooth(M), order = c("X", "M", "Y"))
  models <- list(
    M = cssem:::.fit_shape_model(scores, "M", c(X = "linear")),
    Y = cssem:::.fit_shape_model(scores, "Y", c(M = "smooth_df3"))
  )
  candidate_metrics <- data.frame(
    outcome = c("M", "Y"), predictor = c("X", "M"),
    shape = c("linear", "smooth_df3"), r_squared = c(.3, .7),
    mean_mse_improvement = c(.3, .7), mse_improvement_se = c(.02, .03),
    selection_frequency = c(1, .9), nonlinearity_p = c(NA, .01),
    selected = TRUE, stringsAsFactors = FALSE
  )
  contributions <- data.frame(outcome = c("M", "Y"), predictor = c("X", "M"),
    edge_drop_mse_increase = c(.2, .4), edge_drop_mse_se = c(.02, .04))
  gaps <- data.frame(outcome = c("M", "Y"), shadow_scope = "temporal",
    specification_gap = 0)
  association <- structure(list(structure = structure, response_families = structure$response_families,
    scores = scores, full_models = models, candidate_metrics = candidate_metrics,
    contributions = contributions, specification_gap = gaps,
    corrected_effects = data.frame(outcome = c("M", "Y"), predictor = c("X", "M"),
      corrected_estimate = NA_real_, naive_estimate = c(.5, NA_real_),
      corrected_ci_low = NA_real_, corrected_ci_high = NA_real_),
    effects = data.frame(outcome = c("M", "Y"), predictor = c("X", "M"),
      x = NA_real_, estimate = c(.5, NA_real_)),
    reliability = NULL), class = "cssem_association")
  list(scores = scores, association = association)
}

.visualization_pdf <- function(code) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  device <- grDevices::dev.cur()
  on.exit(if (grDevices::dev.cur() == device) grDevices::dev.off(), add = TRUE)
  on.exit(unlink(file), add = TRUE)
  force(code)
  grDevices::dev.off()
  invisible(file.info(file)$size)
}

test_that("association plot data exposes directed edges and routed status", {
  fx <- visualization_fixture()
  path_data <- plot_data(fx$association, type = "paths")
  expect_equal(nrow(path_data), 2L)
  expect_true(all(c("from", "to", "x", "y", "xend", "yend", "status",
    "estimate", "basis", "units", "label") %in% names(path_data)))
  expect_true(all(path_data$status == "associational"))
  expect_true(all(grepl("not a causal claim", path_data$interpretation, fixed = TRUE)))
  expect_identical(path_data$basis, c("naive", "unavailable"))

  routing <- structure(list(table = data.frame(
    path = c("X\u2192M", "M\u2192Y"), status = c("causal_weak", "causal"),
    estimand = c("adjusted_linear", "adjusted_linear"), adjustment_set = c("", "X"),
    effect = c(.2, .3), ci_low = c(.1, .15), ci_high = c(.3, .45),
    robustness_value = c(NA, .2), interpretation = c("Adjusted association (weak identification)", "Causal under assumptions"),
    stringsAsFactors = FALSE), temporal_order = c("X", "M", "Y"), status = "routed"),
    class = "cssem_routing")
  routed <- plot_data(fx$association, type = "paths", routing = routing)
  expect_identical(routed$status, c("causal_weak", "causal"))
  expect_equal(routed$estimate, c(.2, .3))
  expect_match(routed$interpretation[[1L]], "weak identification")

  interaction_fixture <- fx
  interaction_fixture$association$structure <- specify_structure(
    M ~ linear(X), Y ~ M + M:W, order = c("X", "W", "M", "Y"))
  interaction_data <- plot_data(interaction_fixture$association, type = "paths")
  product_inputs <- interaction_data[interaction_data$edge_type == "interaction_input", , drop = FALSE]
  expect_equal(nrow(product_inputs), 2L)
  expect_setequal(product_inputs$from, c("M", "W"))
  expect_true(all(product_inputs$status == "derived_term"))
  expect_true(any(interaction_data$from == "M:W" & interaction_data$to == "Y"))
})

test_that("response-curve data is restricted to observed support and has no invented bands", {
  fx <- visualization_fixture()
  curve <- plot_data(fx$association, type = "curve", outcome = "Y", predictor = "M", n = 31L)
  expect_equal(nrow(curve), 31L)
  expect_equal(range(curve$x), range(fx$scores$M))
  expect_true(all(curve$in_support))
  expect_true(all(is.na(curve$ci_low) & is.na(curve$ci_high)))
  expect_true(all(curve$interval_basis == "unavailable"))
  expect_error(plot_data(fx$association, type = "curve", outcome = "Y", predictor = "X"),
    "selected structural edge")
  expect_error(plot_data(fx$association, type = "curve", outcome = "Y", predictor = "M", n = 1),
    "at least 2")
})

test_that("conditional slope plots retain their interval basis and unavailable JN intervals", {
  fx <- visualization_fixture()
  slopes <- data.frame(level = c("-1 SD", "Mean", "+1 SD"), moderator_value = c(-1, 0, 1),
    slope = c(.2, .4, .6), ci_low = c(.1, .3, .5), ci_high = c(.3, .5, .7))
  ss <- structure(list(association = fx$association, outcome = "Y", predictor = "M", moderator = "W",
    slopes = slopes, bootstrap = 40L, disattenuated = TRUE,
    johnson_neyman = list(grid = c(-1, 0, 1), significant = c(FALSE, TRUE, TRUE),
      intervals = list(c(0, 1)), ci_low = c(-.1, .1, .3), ci_high = c(.5, .7, .9))),
    class = "conditional_slopes")
  slope_data <- plot_data(ss, type = "slopes")
  expect_identical(slope_data$basis, rep("disattenuated_eiv", 3L))
  expect_true(all(slope_data$interval_basis == "percentile_bootstrap"))
  jn_data <- plot_data(ss, type = "johnson_neyman")
  expect_equal(jn_data$moderator_value,
    cssem:::.moderator_values(fx$association$scores, "W", c(-1, 0, 1)))
  expect_identical(jn_data$significant, c(FALSE, TRUE, TRUE))
  expect_true(all(is.finite(jn_data$ci_low) & is.finite(jn_data$ci_high)))

  ss$bootstrap <- 0L
  ss$johnson_neyman <- NULL
  unavailable <- plot_data(ss, type = "johnson_neyman")
  expect_equal(nrow(unavailable), 1L)
  expect_false(unavailable$available)
  expect_true(is.na(unavailable$ci_low) && is.na(unavailable$ci_high))
  expect_silent(.visualization_pdf(plot(ss, type = "slopes")))
  expect_silent(.visualization_pdf(plot(ss, type = "johnson_neyman")))
})

test_that("mediation and evidence plot data preserve estimands and uncertainty", {
  mediation <- structure(list(
    summary = data.frame(component = c("total", "direct", "indirect_total"),
      naive_effect = c(.5, .2, .3), disattenuated_effect = c(NA, NA, NA),
      naive_ci_low = c(.2, NA, .1), naive_ci_high = c(.8, NA, .5)),
    path_specific = data.frame(path = "X\u2192M\u2192Y", naive_effect = .3,
      disattenuated_effect = NA_real_, naive_ci_low = .1, naive_ci_high = .5),
    disattenuated = FALSE, x = "X", y = "Y", n = 120L, status = "associational"),
    class = "indirect_effect")
  med_data <- plot_data(mediation)
  expect_equal(nrow(med_data), 4L)
  expect_true(all(med_data$status == "associational"))
  expect_true(all(med_data$basis == "naive"))
  expect_true(all(is.na(med_data$ci_low[med_data$component == "direct"])))
  expect_identical(med_data$interval_basis[med_data$component == "direct"], "unavailable")
  expect_silent(.visualization_pdf(plot(mediation)))

  fx <- visualization_fixture()
  report <- evidence_report(fx$association)
  report_data <- plot_data(report, type = "effects")
  expect_equal(nrow(report_data), 2L)
  expect_true(all(report_data$causal_status == "associational"))
  expect_true(all(nzchar(report_data$basis)))
  expect_true(all(nzchar(report_data$units)))
  expect_silent(.visualization_pdf(plot(report, type = "effects")))
  expect_silent(.visualization_pdf(plot(report, type = "constructs")))
  expect_silent(.visualization_pdf(plot(report, type = "causal_claims")))
  report$causal_claims <- data.frame(claim = c("X\u2192M", "M\u2192Y"),
    label = c("causal_under_assumptions", "adjusted_association"),
    effect = c(.3, .2), ci_low = c(.1, NA_real_), ci_high = c(.5, NA_real_))
  expect_silent(.visualization_pdf(plot(report, type = "causal_claims")))
  report$effects$contribution[] <- NA_real_
  expect_silent(.visualization_pdf(plot(report, type = "effects")))
})

test_that("fit convergence plot data keeps full and fold convergence distinct", {
  fit <- structure(list(measurement_engine = list(
    A = list(estimator = "ordinal_mml", converged = TRUE, iterations = 5L, folds_converged = 3L, folds = 3L),
    B = list(estimator = "ordinal_mml", converged = FALSE, iterations = 8L, folds_converged = 1L, folds = 3L),
    C = list(estimator = "manifest", converged = NA, iterations = 0L, folds_converged = NA, folds = 3L))),
    class = "fit_states")
  convergence <- plot_data(fit, type = "convergence")
  expect_identical(convergence$construct, c("A", "B", "C"))
  expect_identical(convergence$folds_converged, c(3L, 1L, NA_integer_))
  expect_identical(convergence$status, c("converged", "not_converged", "not_applicable"))
  expect_silent(.visualization_pdf(plot(fit, type = "convergence")))
  manifest_fit <- fit
  manifest_fit$measurement_engine <- list(
    A = list(estimator = "manifest", converged = NA, iterations = 0L, folds_converged = NA_integer_, folds = 3L))
  expect_silent(.visualization_pdf(plot(manifest_fit, type = "convergence")))
})

test_that("base graphics plot methods support static file export", {
  fx <- visualization_fixture()
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  device <- grDevices::dev.cur()
  on.exit(if (grDevices::dev.cur() == device) grDevices::dev.off(), add = TRUE)
  on.exit(unlink(output), add = TRUE)
  expect_silent(plot(fx$association, type = "paths"))
  expect_silent(plot(fx$association, type = "curve", outcome = "Y", predictor = "M"))
  grDevices::dev.off()
  expect_gt(file.info(output)$size, 0)
})
