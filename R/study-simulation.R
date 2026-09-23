.study_scalar_integer <- function(value, name, minimum = 1L) {
  valid <- is.numeric(value) && is.null(dim(value)) && length(value) == 1L &&
    is.finite(value) && value == floor(value) && value >= minimum &&
    value <= .Machine$integer.max
  if (!valid) {
    label <- if (minimum == 0L) "a non-negative integer" else "a positive integer"
    stop(sprintf("%s must be %s.", name, label), call. = FALSE)
  }
  as.integer(value)
}

.study_scenario_row <- function(scenarios, index) {
  as.list(scenarios[index, , drop = FALSE])
}

.study_truth_values <- function(spec) {
  truth_by_scenario <- vector("list", nrow(spec$scenarios))
  estimand_names <- NULL
  for (scenario_index in seq_len(nrow(spec$scenarios))) {
    scenario <- .study_scenario_row(spec$scenarios, scenario_index)
    scenario_id <- scenario$scenario[[1L]]
    truth_by_scenario[[scenario_index]] <- vector("list", length(spec$sample_sizes))
    for (size_index in seq_along(spec$sample_sizes)) {
      n <- spec$sample_sizes[[size_index]]
      value <- tryCatch(
        do.call(spec$truth, list(scenario = scenario, n = n)),
        error = function(error) {
          stop(sprintf("truth callback failed for scenario '%s', n = %d: %s",
            scenario_id, n, conditionMessage(error)), call. = FALSE)
        }
      )
      valid <- is.numeric(value) && is.null(dim(value)) && length(value) > 0L &&
        !is.null(names(value)) && !anyNA(names(value)) &&
        all(nzchar(names(value))) && !anyDuplicated(names(value)) &&
        all(is.finite(value))
      if (!valid) {
        stop(sprintf("truth must return a finite named numeric vector with unique estimand names (scenario '%s', n = %d).",
          scenario_id, n), call. = FALSE)
      }
      if (is.null(estimand_names)) {
        estimand_names <- names(value)
      } else if (!setequal(names(value), estimand_names)) {
        stop(sprintf("truth must return the same estimand names for every scenario and sample size (scenario '%s', n = %d).",
          scenario_id, n), call. = FALSE)
      }
      truth_by_scenario[[scenario_index]][[size_index]] <- value[estimand_names]
    }
  }
  list(values = truth_by_scenario, estimands = estimand_names)
}

.study_build_jobs <- function(spec, reps, seed, truth) {
  job_count <- as.double(nrow(spec$scenarios)) * length(spec$sample_sizes) * reps
  if (!is.finite(job_count) || job_count > .Machine$integer.max / 2) {
    stop("The requested study has too many scheduled replications.", call. = FALSE)
  }
  set.seed(seed)
  job_seeds <- sample.int(.Machine$integer.max, size = as.integer(job_count * 2))
  jobs <- vector("list", as.integer(job_count))
  job_index <- 0L
  for (scenario_index in seq_len(nrow(spec$scenarios))) {
    scenario <- .study_scenario_row(spec$scenarios, scenario_index)
    for (size_index in seq_along(spec$sample_sizes)) {
      n <- spec$sample_sizes[[size_index]]
      for (replication in seq_len(reps)) {
        job_index <- job_index + 1L
        jobs[[job_index]] <- list(
          scenario = scenario,
          scenario_id = as.character(scenario$scenario[[1L]]),
          sample_size_index = size_index,
          n = n,
          replication = as.integer(replication),
          generate_seed = as.integer(job_seeds[[2L * job_index - 1L]]),
          analyze_seed = as.integer(job_seeds[[2L * job_index]]),
          truth = truth$values[[scenario_index]][[size_index]]
        )
      }
    }
  }
  jobs
}

.study_failed_rows <- function(job, stage, message, estimands) {
  data.frame(
    scenario = rep(job$scenario_id, length(estimands)),
    n = rep(job$n, length(estimands)),
    replication = rep(job$replication, length(estimands)),
    generate_seed = rep(job$generate_seed, length(estimands)),
    analyze_seed = rep(job$analyze_seed, length(estimands)),
    estimand = estimands,
    truth = unname(job$truth[estimands]),
    estimate = rep(NA_real_, length(estimands)),
    estimate_status = rep("unavailable", length(estimands)),
    lower = rep(NA_real_, length(estimands)),
    upper = rep(NA_real_, length(estimands)),
    interval_status = rep("unavailable", length(estimands)),
    generation_status = if (stage == "generation") "failed" else "completed",
    analysis_status = if (stage == "generation") "not_run" else "failed",
    converged = NA,
    run_status = if (stage == "generation") "generation_failed" else "analysis_failed",
    failure_stage = stage,
    failure_reason = message,
    status_reason = message,
    stringsAsFactors = FALSE
  )
}

.study_normalize_analysis <- function(value, estimands) {
  if (!is.list(value) || is.data.frame(value)) {
    stop("analyze() must return a named list.", call. = FALSE)
  }
  if (is.null(value$status) || !is.character(value$status) ||
      length(value$status) != 1L || is.na(value$status) ||
      !value$status %in% c("completed", "partial", "failed")) {
    stop("analyze()$status must be 'completed', 'partial', or 'failed'.", call. = FALSE)
  }
  if (!is.logical(value$converged) || length(value$converged) != 1L) {
    stop("analyze()$converged must be TRUE, FALSE, or NA.", call. = FALSE)
  }
  failure_reason <- value$failure_reason
  if (is.null(failure_reason)) failure_reason <- ""
  if (!is.character(failure_reason) || length(failure_reason) != 1L || is.na(failure_reason)) {
    stop("analyze()$failure_reason must be a character scalar when supplied.", call. = FALSE)
  }
  if (value$status == "failed" && !nzchar(trimws(failure_reason))) {
    stop("analyze() must supply failure_reason when status is 'failed'.", call. = FALSE)
  }
  estimates <- value$estimates
  required <- c("estimand", "estimate", "estimate_status", "lower", "upper", "interval_status")
  if (!is.data.frame(estimates) || !all(required %in% names(estimates))) {
    stop("analyze()$estimates must be a data frame with the required estimate and interval columns.",
      call. = FALSE)
  }
  if (nrow(estimates) != length(estimands) || !is.character(estimates$estimand) ||
      anyNA(estimates$estimand) || anyDuplicated(estimates$estimand) ||
      !setequal(estimates$estimand, estimands)) {
    stop("analyze()$estimates must contain exactly one row for every declared estimand.",
      call. = FALSE)
  }
  if (!is.numeric(estimates$estimate) || !is.numeric(estimates$lower) ||
      !is.numeric(estimates$upper)) {
    stop("analyze() estimate and interval columns must be numeric.", call. = FALSE)
  }
  if (!is.character(estimates$estimate_status) ||
      anyNA(estimates$estimate_status) ||
      any(!estimates$estimate_status %in% c("available", "unavailable"))) {
    stop("estimate_status must contain only 'available' or 'unavailable'.", call. = FALSE)
  }
  if (!is.character(estimates$interval_status) ||
      anyNA(estimates$interval_status) ||
      any(!estimates$interval_status %in% c("available", "unavailable"))) {
    stop("interval_status must contain only 'available' or 'unavailable'.", call. = FALSE)
  }
  estimate_available <- estimates$estimate_status == "available"
  interval_available <- estimates$interval_status == "available"
  if (any(!is.finite(estimates$estimate[estimate_available])) ||
      any(!is.na(estimates$estimate[!estimate_available]))) {
    stop("available estimates must be finite and unavailable estimates must be NA.", call. = FALSE)
  }
  if (any(!is.finite(estimates$lower[interval_available])) ||
      any(!is.finite(estimates$upper[interval_available])) ||
      any(estimates$lower[interval_available] > estimates$upper[interval_available])) {
    stop("available interval bounds must be finite and ordered.", call. = FALSE)
  }
  if (any(!is.na(estimates$lower[!interval_available])) ||
      any(!is.na(estimates$upper[!interval_available]))) {
    stop("unavailable interval bounds must be NA.", call. = FALSE)
  }
  if (any(interval_available & !estimate_available)) {
    stop("an interval cannot be available when its estimate is unavailable.", call. = FALSE)
  }
  if (!"status_reason" %in% names(estimates)) estimates$status_reason <- ""
  if (!is.character(estimates$status_reason) || anyNA(estimates$status_reason)) {
    stop("analyze()$estimates$status_reason must contain non-missing character values.",
      call. = FALSE)
  }
  estimates <- estimates[match(estimands, estimates$estimand), , drop = FALSE]
  needs_reason <- estimates$estimate_status == "unavailable" |
    estimates$interval_status == "unavailable"
  reason_missing <- !nzchar(trimws(estimates$status_reason))
  if (any(needs_reason & reason_missing & !nzchar(trimws(failure_reason)))) {
    stop("unavailable estimates and intervals must include a status_reason.", call. = FALSE)
  }
  estimates$status_reason[needs_reason & reason_missing] <- failure_reason
  list(status = value$status, converged = value$converged,
    failure_reason = failure_reason, estimates = estimates)
}

.study_run_one <- function(job, spec, estimands) {
  generated <- tryCatch({
    set.seed(job$generate_seed)
    data <- do.call(spec$generate, list(
      scenario = job$scenario, n = job$n, seed = job$generate_seed))
    if (!is.data.frame(data) || nrow(data) != job$n) {
      stop("generate() must return a data frame with exactly n rows.", call. = FALSE)
    }
    data
  }, error = identity)
  if (inherits(generated, "error")) {
    return(.study_failed_rows(job, "generation", conditionMessage(generated), estimands))
  }

  analysis <- tryCatch({
    set.seed(job$analyze_seed)
    raw <- do.call(spec$analyze, list(
      data = generated, scenario = job$scenario, seed = job$analyze_seed))
    .study_normalize_analysis(raw, estimands)
  }, error = identity)
  if (inherits(analysis, "error")) {
    return(.study_failed_rows(job, "analysis", conditionMessage(analysis), estimands))
  }

  status <- if (analysis$status == "failed") "analysis_failed" else analysis$status
  out <- analysis$estimates
  data.frame(
    scenario = rep(job$scenario_id, nrow(out)),
    n = rep(job$n, nrow(out)),
    replication = rep(job$replication, nrow(out)),
    generate_seed = rep(job$generate_seed, nrow(out)),
    analyze_seed = rep(job$analyze_seed, nrow(out)),
    estimand = out$estimand,
    truth = unname(job$truth[out$estimand]),
    estimate = out$estimate,
    estimate_status = out$estimate_status,
    lower = out$lower,
    upper = out$upper,
    interval_status = out$interval_status,
    generation_status = "completed",
    analysis_status = analysis$status,
    converged = rep(analysis$converged, nrow(out)),
    run_status = status,
    failure_stage = if (analysis$status %in% c("partial", "failed")) "analysis" else "",
    failure_reason = analysis$failure_reason,
    status_reason = out$status_reason,
    stringsAsFactors = FALSE
  )
}

#' Run a callback-driven simulation study
#'
#' @param spec A `cssem_study_spec` created by [study_spec()].
#' @param reps Number of replications for each scenario and sample size.
#' @param seed Non-negative integer base seed used to assign callback seeds.
#' @param workers Positive integer worker count. This release runs sequentially;
#'   PSOCK execution is enabled when the supported worker path is available.
#'
#' @return A `cssem_simulation` object. Its `replications` data frame has one
#'   row per scheduled replication and estimand, including independent truth,
#'   estimates, intervals, seeds, callback statuses, convergence, and failure
#'   stage/reason. The object also records the design grid, metadata, callback
#'   labels, base seed, and replication/worker counts. Callback environments are
#'   not stored.
#' @export
simulate_study <- function(spec, reps, seed = 1L, workers = 1L) {
  .preserve_seed()
  if (!inherits(spec, "cssem_study_spec")) {
    stop("spec must be a cssem_study_spec created by study_spec().", call. = FALSE)
  }
  reps <- .study_scalar_integer(reps, "reps")
  seed <- .study_scalar_integer(seed, "seed", minimum = 0L)
  workers <- .study_scalar_integer(workers, "workers")
  if (workers > 1L) {
    stop("workers greater than one are not supported yet.", call. = FALSE)
  }

  truth <- .study_truth_values(spec)
  jobs <- .study_build_jobs(spec, reps, seed, truth)
  rows <- lapply(jobs, .study_run_one, spec = spec, estimands = truth$estimands)
  structure(list(
    replications = do.call(rbind, rows),
    scenarios = spec$scenarios,
    sample_sizes = spec$sample_sizes,
    reps = reps,
    seed = seed,
    workers = workers,
    metadata = spec$metadata,
    callback_labels = spec$callback_labels
  ), class = "cssem_simulation")
}
