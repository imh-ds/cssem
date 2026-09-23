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
