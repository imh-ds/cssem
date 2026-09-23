make_study_spec <- function() {
  calls <- new.env(parent = emptyenv())
  calls$generate <- 0L
  calls$truth <- 0L
  calls$analyze <- 0L
  scenarios <- data.frame(scenario = c("base", "stress"), beta = c(.30, .30))
  generate <- function(scenario, n, seed) {
    calls$generate <- calls$generate + 1L
    data.frame(y = rep(0, n))
  }
  truth <- function(scenario, n) {
    calls$truth <- calls$truth + 1L
    c(beta = scenario$beta, risk = .5 + 1 / n)
  }
  analyze <- function(data, scenario, seed) {
    calls$analyze <- calls$analyze + 1L
    list(status = "completed", converged = TRUE, failure_reason = "",
      estimates = data.frame(
        estimand = c("beta", "risk"), estimate = c(.30, .51),
        estimate_status = "available", lower = c(.1, .2), upper = c(.5, .8),
        interval_status = "available", status_reason = "",
        stringsAsFactors = FALSE
      ))
  }
  spec <- study_spec(scenarios, c(80L, 160L), generate, truth, analyze,
    metadata = list(description = "Known linear study", analysis_scope = "locked_score"))
  list(spec = spec, calls = calls, scenarios = scenarios,
    generate = generate, truth = truth, analyze = analyze)
}

test_that("study_spec preserves the callback contract without invoking callbacks", {
  fixture <- make_study_spec()
  spec <- fixture$spec
  expect_s3_class(spec, "cssem_study_spec")
  expect_equal(spec$sample_sizes, c(80L, 160L))
  expect_equal(spec$scenarios$scenario, c("base", "stress"))
  expect_identical(spec$metadata,
    list(description = "Known linear study", analysis_scope = "locked_score"))
  expect_identical(spec$generate, fixture$generate)
  expect_identical(spec$truth, fixture$truth)
  expect_identical(spec$analyze, fixture$analyze)
  expect_identical(spec$callback_labels,
    c(generate = "generate", truth = "truth", analyze = "analyze"))
  expect_true(all(vapply(as.list(fixture$calls), identical, logical(1), 0L)))
})

test_that("study_spec rejects invalid scenario grids", {
  fixture <- make_study_spec()
  scenarios <- fixture$scenarios
  expect_error(study_spec(data.frame(), 10L, fixture$generate, fixture$truth,
    fixture$analyze, fixture$spec$metadata), "non-empty data frame")
  expect_error(study_spec(data.frame(id = "base"), 10L, fixture$generate,
    fixture$truth, fixture$analyze, fixture$spec$metadata), "scenario")
  duplicate <- scenarios
  duplicate$scenario[[2L]] <- duplicate$scenario[[1L]]
  expect_error(study_spec(duplicate, 10L, fixture$generate, fixture$truth,
    fixture$analyze, fixture$spec$metadata), "unique")
  missing_id <- scenarios
  missing_id$scenario[[2L]] <- NA_character_
  expect_error(study_spec(missing_id, 10L, fixture$generate, fixture$truth,
    fixture$analyze, fixture$spec$metadata), "missing")
  factor_id <- scenarios
  factor_id$scenario <- factor(factor_id$scenario)
  expect_error(study_spec(factor_id, 10L, fixture$generate, fixture$truth,
    fixture$analyze, fixture$spec$metadata), "character")
})

test_that("study_spec rejects invalid sample-size grids", {
  fixture <- make_study_spec()
  for (sizes in list(integer(), 0L, -1L, c(10, 10.5), c(10L, 10L), NA_integer_)) {
    expect_error(study_spec(fixture$scenarios, sizes, fixture$generate,
      fixture$truth, fixture$analyze, fixture$spec$metadata), "sample_sizes")
  }
  expect_error(study_spec(fixture$scenarios, TRUE, fixture$generate,
    fixture$truth, fixture$analyze, fixture$spec$metadata), "sample_sizes")
})

test_that("study_spec validates callback functions and signatures", {
  fixture <- make_study_spec()
  expect_error(study_spec(fixture$scenarios, 10L, NULL, fixture$truth,
    fixture$analyze, fixture$spec$metadata), "generate")
  expect_error(study_spec(fixture$scenarios, 10L, function(data) data,
    fixture$truth, fixture$analyze, fixture$spec$metadata), "scenario.*n.*seed")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    function(scenario) 1, fixture$analyze, fixture$spec$metadata), "scenario.*n")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, function(data) data, fixture$spec$metadata), "data.*scenario.*seed")
  expect_s3_class(study_spec(fixture$scenarios, 10L,
    function(...) NULL, function(...) NULL, function(...) NULL,
    fixture$spec$metadata), "cssem_study_spec")
})

test_that("study_spec requires named, plain metadata with study labels", {
  fixture <- make_study_spec()
  base <- fixture$spec$metadata
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, list(analysis_scope = "locked")), "description")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, list(description = "", analysis_scope = "locked")), "description")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, list(description = "study", analysis_scope = "")), "analysis_scope")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, list(description = "study", analysis_scope = "locked", callback = fixture$generate)), "metadata")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, list(description = "study", analysis_scope = "locked", extra = new.env())), "metadata")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, c(base, list(assumptions = data.frame(n = 10L)))), "metadata")
  classed <- base
  class(classed) <- "study_metadata"
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, classed), "metadata")
  expect_error(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, unname(base)), "named list")
  expect_s3_class(study_spec(fixture$scenarios, 10L, fixture$generate,
    fixture$truth, fixture$analyze, c(base, list(assumptions = list(nested = c(1L, 2L))))),
    "cssem_study_spec")
})

make_failure_study_spec <- function() {
  scenarios <- data.frame(scenario = c("base", "generation_error", "bad_generator_output",
    "analysis_error", "malformed_analysis", "partial", "no_interval"),
    beta = rep(.30, 7L), stringsAsFactors = FALSE)
  generate <- function(scenario, n, seed) {
    if (scenario$scenario == "generation_error") stop("generator exploded")
    if (scenario$scenario == "bad_generator_output") return(data.frame(y = rep(.2, n - 1L)))
    data.frame(y = rep(.2, n))
  }
  truth <- function(scenario, n) c(beta = scenario$beta, risk = .5 + 1 / n)
  analyze <- function(data, scenario, seed) {
    if (scenario$scenario == "analysis_error") stop("analysis exploded")
    if (scenario$scenario == "malformed_analysis") {
      return(list(status = "completed", converged = TRUE,
        estimates = data.frame(estimand = "beta", estimate = .30,
          estimate_status = "available", lower = .1, upper = .5,
          interval_status = "available")))
    }
    interval_status <- if (scenario$scenario %in% c("partial", "no_interval"))
      "unavailable" else "available"
    status <- if (scenario$scenario == "partial") "partial" else "completed"
    estimates <- data.frame(
      estimand = c("beta", "risk"), estimate = c(.30, .51),
      estimate_status = "available", lower = if (interval_status == "available") c(.1, .2) else NA_real_,
      upper = if (interval_status == "available") c(.5, .8) else NA_real_,
      interval_status = interval_status,
      status_reason = if (interval_status == "unavailable") "interval was not available" else "",
      stringsAsFactors = FALSE
    )
    list(status = status, converged = TRUE,
      failure_reason = if (status == "partial") "one interval unavailable" else "",
      estimates = estimates)
  }
  study_spec(scenarios, 40L, generate, truth, analyze,
    metadata = list(description = "Failure accounting fixture", analysis_scope = "locked"))
}

test_that("simulate_study schedules the grid and attaches sample-size-specific truths", {
  fixture <- make_study_spec()
  spec <- fixture$spec
  simulation <- simulate_study(spec, reps = 2L, seed = 19L)
  reps <- simulation$replications
  replication_keys <- unique(reps[c("scenario", "n", "replication")])
  expect_equal(nrow(replication_keys), 8L)
  expect_equal(nrow(reps), 16L)
  expect_equal(reps$truth[reps$estimand == "risk"],
    .5 + 1 / reps$n[reps$estimand == "risk"])
  expect_identical(replication_keys$scenario,
    rep(c("base", "stress"), each = 4L))
  expect_identical(replication_keys$n, rep(c(80L, 160L), each = 2L, times = 2L))
  expect_identical(replication_keys$replication, rep(1:2, times = 4L))
  expect_identical(unique(reps$estimand), c("beta", "risk"))
  expect_identical(simulation$callback_labels,
    c(generate = "generate", truth = "truth", analyze = "analyze"))
  expect_equal(fixture$calls$truth, 4L)
  expect_equal(fixture$calls$generate, 8L)
  expect_equal(fixture$calls$analyze, 8L)
})

test_that("simulate_study retains errors, partial output, and unavailable intervals", {
  spec <- make_failure_study_spec()
  set.seed(808L)
  prior_seed <- .Random.seed
  simulation <- simulate_study(spec, reps = 1L, seed = 71L)
  expect_identical(.Random.seed, prior_seed)
  reps <- simulation$replications
  expect_equal(nrow(reps), 14L)
  generation_failure <- reps[reps$scenario == "generation_error", , drop = FALSE]
  expect_true(all(generation_failure$run_status == "generation_failed"))
  expect_true(all(generation_failure$generation_status == "failed"))
  expect_true(all(generation_failure$analysis_status == "not_run"))
  expect_true(all(generation_failure$failure_stage == "generation"))
  expect_true(all(grepl("generator exploded", generation_failure$failure_reason, fixed = TRUE)))
  malformed_generation <- reps[reps$scenario == "bad_generator_output", , drop = FALSE]
  expect_true(all(malformed_generation$run_status == "generation_failed"))
  expect_true(all(grepl("exactly n rows", malformed_generation$failure_reason, fixed = TRUE)))
  analysis_failure <- reps[reps$scenario == "analysis_error", , drop = FALSE]
  expect_true(all(analysis_failure$run_status == "analysis_failed"))
  expect_true(all(analysis_failure$generation_status == "completed"))
  expect_true(all(analysis_failure$failure_stage == "analysis"))
  expect_true(all(grepl("analysis exploded", analysis_failure$failure_reason, fixed = TRUE)))
  malformed_analysis <- reps[reps$scenario == "malformed_analysis", , drop = FALSE]
  expect_true(all(malformed_analysis$run_status == "analysis_failed"))
  expect_true(all(grepl("exactly one row for every declared estimand",
    malformed_analysis$failure_reason, fixed = TRUE)))
  partial <- reps[reps$scenario == "partial", , drop = FALSE]
  expect_true(all(partial$run_status == "partial"))
  expect_true(all(partial$analysis_status == "partial"))
  expect_true(all(partial$failure_stage == "analysis"))
  expect_true(all(partial$failure_reason == "one interval unavailable"))
  unavailable <- reps[reps$scenario == "no_interval", , drop = FALSE]
  expect_true(all(unavailable$estimate_status == "available"))
  expect_true(all(unavailable$interval_status == "unavailable"))
  expect_true(all(unavailable$status_reason == "interval was not available"))
  expect_identical(simulate_study(spec, reps = 1L, seed = 71L)$replications, reps)
})

test_that("simulate_study validates run controls and preflights truth", {
  spec <- make_study_spec()$spec
  expect_error(simulate_study(spec, reps = 0L), "reps")
  expect_error(simulate_study(spec, reps = 1.5), "reps")
  expect_error(simulate_study(spec, reps = 1L, seed = -1L), "seed")
  expect_error(simulate_study(spec, reps = 1L, workers = 0L), "workers")
  calls <- 0L
  invalid_truth <- function(scenario, n) {
    calls <<- calls + 1L
    if (n == 160L) c(beta = scenario$beta, other = .5) else c(beta = scenario$beta)
  }
  invalid_spec <- study_spec(spec$scenarios, spec$sample_sizes, spec$generate,
    invalid_truth, spec$analyze, spec$metadata)
  expect_error(simulate_study(invalid_spec, reps = 1L), "truth.*estimand")
  expect_equal(calls, 2L)
})

make_worker_study_spec <- function() {
  generate_env <- new.env(parent = baseenv())
  generate_env$noise_sd <- .8
  generate <- eval(quote(function(scenario, n, seed) {
    data.frame(y = scenario$mu + noise_sd * stats::rnorm(n))
  }), envir = generate_env)
  analysis_env <- new.env(parent = baseenv())
  analysis_env$offset <- .05
  analyze <- eval(quote(function(data, scenario, seed) {
    estimate <- mean(data$y) - offset
    data.frame_result <- data.frame(estimand = "mean", estimate = estimate,
      estimate_status = "available", lower = estimate - 1 / sqrt(nrow(data)),
      upper = estimate + 1 / sqrt(nrow(data)), interval_status = "available",
      status_reason = "")
    list(status = "completed", converged = TRUE, failure_reason = "",
      estimates = data.frame_result)
  }), envir = analysis_env)
  scenarios <- data.frame(scenario = c("low", "high"), mu = c(.2, .7))
  truth <- function(scenario, n) c(mean = scenario$mu - .05)
  study_spec(scenarios, c(30L, 60L), generate, truth, analyze,
    metadata = list(description = "Worker reproducibility", analysis_scope = "mean"))
}

test_that("PSOCK workers preserve ordered results, RNG, and cleanup", {
  spec <- make_worker_study_spec()
  set.seed(918L)
  prior_seed <- .Random.seed
  prior_connections <- rownames(showConnections(all = TRUE))
  sequential <- simulate_study(spec, reps = 2L, seed = 63L, workers = 1L)
  expect_identical(.Random.seed, prior_seed)
  expect_identical(rownames(showConnections(all = TRUE)), prior_connections)
  parallel <- simulate_study(spec, reps = 2L, seed = 63L, workers = 2L)
  expect_identical(.Random.seed, prior_seed)
  expect_identical(rownames(showConnections(all = TRUE)), prior_connections)
  expect_identical(parallel$replications, sequential$replications)

  failed <- simulate_study(make_failure_study_spec(), reps = 1L, seed = 64L,
    workers = 2L)
  expect_true(all(failed$replications$run_status[
    failed$replications$scenario == "generation_error"] == "generation_failed"))
  expect_true(all(failed$replications$run_status[
    failed$replications$scenario == "base"] == "completed"))
  expect_identical(rownames(showConnections(all = TRUE)), prior_connections)
})

make_summary_fixture <- function() {
  rows <- data.frame(
    scenario = rep("base", 4L), n = rep(100L, 4L), replication = 1:4,
    generate_seed = 11:14, analyze_seed = 21:24,
    estimand = rep("beta", 4L), truth = rep(.30, 4L),
    estimate = c(.20, .40, .50, NA_real_),
    estimate_status = c("available", "available", "available", "unavailable"),
    lower = c(.10, .20, .35, NA_real_), upper = c(.50, .40, .45, NA_real_),
    interval_status = c("available", "available", "available", "unavailable"),
    generation_status = c("completed", "completed", "completed", "failed"),
    analysis_status = c("completed", "partial", "completed", "not_run"),
    converged = c(TRUE, TRUE, FALSE, NA),
    run_status = c("completed", "partial", "completed", "generation_failed"),
    failure_stage = c("", "analysis", "", "generation"),
    failure_reason = c("", "interval unavailable", "", "generator failed"),
    status_reason = c("", "interval unavailable", "", "generator failed"),
    stringsAsFactors = FALSE
  )
  structure(list(replications = rows), class = "cssem_simulation")
}

summary_metric <- function(summary, metric, estimand = "beta", scope = NULL) {
  rows <- summary$metrics[summary$metrics$metric == metric &
    summary$metrics$estimand == estimand, , drop = FALSE]
  if (!is.null(scope)) rows <- rows[rows$scope == scope, , drop = FALSE]
  rows
}

test_that("summarize_study reports operating characteristics and MC uncertainty", {
  simulation <- make_summary_fixture()
  summary <- summarize_study(simulation, null_values = c(beta = .30), level = .95)
  expect_s3_class(summary, "cssem_study_summary")

  bias <- summary_metric(summary, "bias")
  expect_equal(bias$estimate, mean(c(-.10, .10, .20)))
  expect_equal(bias$n_valid, 3L)
  expect_equal(bias$planned_reps, 4L)
  errors <- c(-.10, .10, .20)
  expect_equal(bias$mcse, sd(errors) / sqrt(3))
  bias_half <- qt(.975, df = 2) * sd(errors) / sqrt(3)
  expect_equal(bias$mc_lower, mean(errors) - bias_half)
  expect_equal(bias$mc_upper, mean(errors) + bias_half)

  rmse <- summary_metric(summary, "rmse")
  expect_equal(rmse$estimate, sqrt(mean(errors^2)))
  mse_half <- qt(.975, df = 2) * sd(errors^2) / sqrt(3)
  expect_equal(rmse$mcse, sd(errors^2) / sqrt(3) / (2 * sqrt(mean(errors^2))))
  expect_equal(rmse$mc_lower, sqrt(max(0, mean(errors^2) - mse_half)))
  expect_equal(rmse$mc_upper, sqrt(mean(errors^2) + mse_half))

  width <- summary_metric(summary, "mean_interval_width")
  widths <- c(.40, .20, .10)
  expect_equal(width$estimate, mean(widths))
  expect_equal(width$mcse, sd(widths) / sqrt(3))

  coverage <- summary_metric(summary, "coverage")
  expect_equal(coverage$estimate[coverage$scope == "conditional"], 2 / 3)
  expect_equal(coverage$estimate[coverage$scope == "unconditional"], 2 / 4)
  expect_equal(coverage$numerator, c(2L, 2L))
  expect_equal(coverage$denominator, c(3L, 4L))

  detection <- summary_metric(summary, "detection")
  expect_equal(detection$estimate[detection$scope == "conditional"], 1 / 3)
  expect_equal(detection$estimate[detection$scope == "unconditional"], 1 / 4)
  rate <- coverage[coverage$scope == "conditional", , drop = FALSE]
  p <- 2 / 3
  z <- qnorm(.975)
  wilson_denominator <- 1 + z^2 / 3
  wilson_center <- (p + z^2 / 6) / wilson_denominator
  wilson_half <- z * sqrt(p * (1 - p) / 3 + z^2 / 36) / wilson_denominator
  expect_equal(rate$mcse, sqrt(p * (1 - p) / 3))
  expect_equal(rate$mc_lower, wilson_center - wilson_half)
  expect_equal(rate$mc_upper, wilson_center + wilson_half)

  expect_equal(summary_metric(summary, "convergence", "*", "conditional")$estimate, 2 / 3)
  expect_equal(summary_metric(summary, "convergence", "*", "unconditional")$estimate, 2 / 4)
  expect_equal(summary_metric(summary, "failure", "*")$estimate, 1 / 4)
  expect_equal(summary_metric(summary, "partial", "*")$estimate, 1 / 4)
  expect_equal(summary_metric(summary, "interval_availability")$estimate, 3 / 4)
})

test_that("summarize_study marks absent nulls and zero denominators unavailable", {
  simulation <- make_summary_fixture()
  without_null <- summarize_study(simulation)
  detection <- summary_metric(without_null, "detection")
  expect_true(all(detection$availability_status == "unavailable"))
  expect_true(all(is.na(detection$estimate)))

  rows <- simulation$replications
  rows$estimate <- NA_real_
  rows$estimate_status <- "unavailable"
  rows$lower <- NA_real_
  rows$upper <- NA_real_
  rows$interval_status <- "unavailable"
  rows$status_reason <- "not available"
  simulation$replications <- rows
  empty <- summarize_study(simulation, null_values = c(beta = .30))
  expect_identical(summary_metric(empty, "bias")$availability_status, "unavailable")
  expect_identical(summary_metric(empty, "coverage", scope = "conditional")$availability_status,
    "unavailable")
  expect_equal(summary_metric(empty, "coverage", scope = "unconditional")$estimate, 0)
  expect_error(summarize_study(simulation, level = 1), "level")
})

test_that("RMSE and its Monte Carlo error are zero when every valid error is zero", {
  simulation <- make_summary_fixture()
  simulation$replications$estimate[1:3] <- simulation$replications$truth[1:3]
  summary <- summarize_study(simulation)
  rmse <- summary_metric(summary, "rmse")
  expect_equal(rmse$estimate, 0)
  expect_equal(rmse$mcse, 0)
  expect_equal(rmse$mc_lower, 0)
  expect_equal(rmse$mc_upper, 0)
})

test_that("unconditional rates count analysis failures as uncovered and non-detections", {
  simulation <- make_summary_fixture()
  rows <- simulation$replications
  rows$run_status[2:3] <- "analysis_failed"
  rows$analysis_status[2:3] <- "failed"
  simulation$replications <- rows
  summary <- summarize_study(simulation, null_values = c(beta = .30))
  coverage <- summary_metric(summary, "coverage")
  expect_equal(coverage$estimate[coverage$scope == "conditional"], 2 / 3)
  expect_equal(coverage$estimate[coverage$scope == "unconditional"], 1 / 4)
  detection <- summary_metric(summary, "detection")
  expect_equal(detection$estimate[detection$scope == "conditional"], 1 / 3)
  expect_equal(detection$estimate[detection$scope == "unconditional"], 0)
  convergence <- summary_metric(summary, "convergence", "*")
  expect_equal(convergence$estimate[convergence$scope == "conditional"], 2 / 3)
  expect_equal(convergence$estimate[convergence$scope == "unconditional"], 1 / 4)
})

test_that("a known Gaussian slope agrees with its independent analytic oracle", {
  n <- 120L
  reps <- 200L
  simulation <- simulate_study(make_gaussian_slope_spec(n), reps = reps, seed = 407L)
  estimates <- simulation$replications$estimate[
    simulation$replications$estimand == "beta" &
      simulation$replications$estimate_status == "available"]
  expect_length(estimates, reps)
  expect_lt(abs(mean(estimates) - .30), 4 / sqrt(n * reps))
})

test_that("study design fixtures exercise mixed indicators and missingness mechanisms", {
  set.seed(77L)
  prior_seed <- .Random.seed
  design <- make_design_fixture(n = 1000L, seed = 502L)
  expect_identical(.Random.seed, prior_seed)
  expect_identical(design, make_design_fixture(n = 1000L, seed = 502L))
  expect_type(design$continuous_item_1, "double")
  expect_true(is.ordered(design$ordinal_item_1))
  expect_type(design$manifest_control, "double")
  expect_equal(design$product_term,
    design$manifest_control * design$latent_proxy)
  skew <- mean((design$skewed_state - mean(design$skewed_state))^3) /
    stats::sd(design$skewed_state)^3
  expect_gt(skew, .2)
  mcar <- is.na(design$mcar_item)
  mar <- is.na(design$mar_item)
  expect_gt(sum(mcar), 0L)
  expect_gt(sum(mar), 0L)
  expect_lt(abs(stats::cor(as.numeric(mcar), design$manifest_control)), .12)
  expect_gt(stats::cor(as.numeric(mar), design$manifest_control), .15)
})
