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
