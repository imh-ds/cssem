# G15/G4 calibration study for reliability and shape-selection uncertainty.
# Run from the package root after installing this checkout into the library
# used by Rscript (set R_LIBS_USER when the package is installed locally):
#   Rscript tools/validation/g15-inference-studies.R --tier=screening
#   Rscript tools/validation/g15-inference-studies.R --tier=confirmation
#   Rscript tools/validation/g15-inference-studies.R --tier=confirmation --shard=1 --shards=20
# Confirmation is intentionally a long run (500 outer x 199 inner replicates).

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the cssem package root.", call. = FALSE)
}
if (!requireNamespace("cssem", quietly = TRUE)) {
  stop("Install the current cssem checkout in the library used by Rscript before running this study.", call. = FALSE)
}
if (!"study_spec" %in% getNamespaceExports("cssem")) {
  stop("The installed cssem package does not export study_spec(); install this checkout before running the study.", call. = FALSE)
}

.g15_parse_args <- function(args) {
  values <- list(tier = "screening", reps = NULL, inner_reps = NULL,
    workers = 1L, shard = 1L, shards = 1L)
  for (arg in args) {
    pieces <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    if (length(pieces) != 2L || !pieces[[1L]] %in% names(values))
      stop(sprintf("Unsupported argument: %s", arg), call. = FALSE)
    values[[pieces[[1L]]]] <- pieces[[2L]]
  }
  if (!values$tier %in% c("screening", "confirmation"))
    stop("tier must be screening or confirmation.", call. = FALSE)
  defaults <- if (values$tier == "screening") c(reps = 100L, inner_reps = 50L) else
    c(reps = 500L, inner_reps = 199L)
  values$reps <- as.integer(if (is.null(values$reps)) defaults[["reps"]] else values$reps)
  values$inner_reps <- as.integer(if (is.null(values$inner_reps)) defaults[["inner_reps"]] else values$inner_reps)
  values$workers <- as.integer(values$workers)
  values$shard <- as.integer(values$shard)
  values$shards <- as.integer(values$shards)
  if (anyNA(c(values$reps, values$inner_reps, values$workers,
      values$shard, values$shards)) ||
      any(c(values$reps, values$inner_reps, values$workers,
        values$shard, values$shards) < 1L))
    stop("reps, inner_reps, workers, shard, and shards must be positive integers.", call. = FALSE)
  if (values$shard > values$shards) stop("shard must be between 1 and shards.", call. = FALSE)
  if (values$shards > 1L && values$tier != "confirmation")
    stop("only the confirmation tier can be sharded.", call. = FALSE)
  values
}

.g15_config <- .g15_parse_args(commandArgs(trailingOnly = TRUE))
.g15_parent_pid <- Sys.getpid()
.g15_output_dir <- Sys.getenv("CSSEM_G15_OUTDIR",
  unset = file.path("tests", "internal", "validation_results"))
dir.create(.g15_output_dir, recursive = TRUE, showWarnings = FALSE)
.g15_run_id <- sprintf("%d-shard-%02d", .g15_parent_pid, .g15_config$shard)
Sys.setenv(CSSEM_G15_RUN_ID = .g15_run_id,
  CSSEM_G15_INNER_REPS = .g15_config$inner_reps,
  CSSEM_G15_LEVEL = .95,
  CSSEM_G15_MIN_BOOT_SUCCESS = .90)
.g15_seed <- 150415L
.g15_level <- .95
.g15_thresholds <- list(
  absolute_bias = .10,
  coverage_nominal = .95,
  coverage_tolerance = .04,
  unconditional_failure_max = .05,
  minimum_inner_bootstrap_success = .90
)
.g15_scenarios <- expand.grid(
  reliability_x = c(.40, .80),
  response_shape = c("linear", "curved"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
.g15_scenarios$scenario <- sprintf("rho%.2f_%s", .g15_scenarios$reliability_x,
  .g15_scenarios$response_shape)
.g15_scenarios$beta <- .50
.g15_scenarios$gamma <- .45
.g15_scenarios$correlation_xz <- .35
.g15_scenarios$curvature <- .35
.g15_scenarios <- .g15_scenarios[c("scenario", "reliability_x", "response_shape",
  "beta", "gamma", "correlation_xz", "curvature")]

.g15_generate <- function(scenario, n, seed) {
  set.seed(seed)
  z <- stats::rnorm(n)
  x <- scenario$correlation_xz * z +
    sqrt(1 - scenario$correlation_xz^2) * stats::rnorm(n)
  measurement_variance <- (1 - scenario$reliability_x) / scenario$reliability_x
  x_observed <- x + stats::rnorm(n, sd = sqrt(measurement_variance))
  z_effect <- if (scenario$response_shape == "curved")
    z + scenario$curvature * z^3 else z
  y <- scenario$beta * x + scenario$gamma * z_effect + stats::rnorm(n)
  data.frame(x_observed = x_observed, z_observed = z, y_observed = y)
}

.g15_truth <- function(scenario, n) {
  # X and Z are jointly Gaussian, so X's residual after conditioning on Z is
  # independent of every function of Z. The score-scale target is the exact
  # linear projection coefficient after adding classical X measurement error.
  residual_signal_variance <- 1 - scenario$correlation_xz^2
  measurement_variance <- (1 - scenario$reliability_x) / scenario$reliability_x
  observed_target <- scenario$beta * residual_signal_variance /
    (residual_signal_variance + measurement_variance)
  stats::setNames(c(scenario$beta, scenario$beta, observed_target, observed_target),
    c("latent_path_fixed", "latent_path_repeat",
      "observed_score_path_fixed", "observed_score_path_repeat"))
}

.g15_analyze <- local({
  inner_reps <- as.integer(Sys.getenv("CSSEM_G15_INNER_REPS"))
  level <- as.numeric(Sys.getenv("CSSEM_G15_LEVEL"))
  min_success_rate <- as.numeric(Sys.getenv("CSSEM_G15_MIN_BOOT_SUCCESS"))
  function(data, scenario, seed) {
  make_estimate_rows <- function(association) {
    spec <- cssem::contrast_spec(list(
      latent_path = "edge:Y~X:corrected",
      observed_score_path = "edge:Y~X:naive"
    ))
    point_table <- cssem::parameter_table(association)
    edge <- point_table[point_table$outcome == "Y" & point_table$predictor == "X", , drop = FALSE]
    if (nrow(edge) != 1L) stop("The focal X -> Y edge is not uniquely available.", call. = FALSE)
    point <- c(latent_path = edge$corrected_estimate[[1L]],
      observed_score_path = edge$naive_estimate[[1L]])
    selected_shape <- paste(names(association$full_models$Y$shapes),
      unname(association$full_models$Y$shapes), sep = "=", collapse = ";")
    run_contrast <- function(selection, offset) {
      tryCatch(cssem::contrast(association, spec, reps = inner_reps,
        level = level,
        seed = as.integer((as.numeric(seed) + as.numeric(offset)) %% .Machine$integer.max),
        selection = selection), error = identity)
    }
    fixed <- run_contrast("fixed", 1L)
    repeated <- run_contrast("repeat", 2L)
    extract <- function(object, mode, values) {
      if (inherits(object, "error")) {
        return(data.frame(estimand = paste0(names(values), "_", mode),
          estimate = ifelse(is.finite(values), unname(values), NA_real_),
          estimate_status = ifelse(is.finite(values), "available", "unavailable"),
          lower = NA_real_, upper = NA_real_, interval_status = "unavailable",
          status_reason = paste("contrast failed:", conditionMessage(object)),
          stringsAsFactors = FALSE))
      }
      intervals <- object$intervals
      success_count <- sum(object$replicates$status == "success")
      minimum_success <- ceiling(min_success_rate * inner_reps)
      enough <- success_count >= minimum_success
      rows <- lapply(names(values), function(name) {
        interval <- intervals[intervals$contrast == name, , drop = FALSE]
        valid_estimate <- is.finite(values[[name]])
        available_interval <- valid_estimate && enough && nrow(interval) == 1L &&
          is.finite(interval$ci_low[[1L]]) && is.finite(interval$ci_high[[1L]])
        data.frame(estimand = paste0(name, "_", mode),
          estimate = if (valid_estimate) unname(values[[name]]) else NA_real_,
          estimate_status = if (valid_estimate) "available" else "unavailable",
          lower = if (available_interval) interval$ci_low[[1L]] else NA_real_,
          upper = if (available_interval) interval$ci_high[[1L]] else NA_real_,
          interval_status = if (available_interval) "available" else "unavailable",
          status_reason = if (available_interval) "" else sprintf(
            "inner bootstrap interval unavailable: %d/%d successful draws (minimum %d)",
            success_count, inner_reps, minimum_success),
          stringsAsFactors = FALSE)
      })
      list(estimates = do.call(rbind, rows), successful = success_count,
        selection_changes = if (mode == "repeat") object$selection_changes else 0L)
    }
    fixed_rows <- extract(fixed, "fixed", point)
    repeat_rows <- extract(repeated, "repeat", point)
    estimates <- rbind(
      if (is.list(fixed_rows) && !is.data.frame(fixed_rows)) fixed_rows$estimates else fixed_rows,
      if (is.list(repeat_rows) && !is.data.frame(repeat_rows)) repeat_rows$estimates else repeat_rows
    )
    list(estimates = estimates, audit = list(
      selected_shapes = selected_shape,
      fixed_successful = if (is.list(fixed_rows) && !is.data.frame(fixed_rows)) fixed_rows$successful else 0L,
      repeat_successful = if (is.list(repeat_rows) && !is.data.frame(repeat_rows)) repeat_rows$successful else 0L,
      repeat_selection_changes = if (is.list(repeat_rows) && !is.data.frame(repeat_rows)) repeat_rows$selection_changes else NA_integer_
    ))
  }

  result <- tryCatch({
    measurement <- cssem::specify_measurement(
      X = cssem::manifest("x_observed", reliability = scenario$reliability_x,
        standardize = FALSE),
      Z = cssem::manifest("z_observed", reliability = 1, standardize = FALSE),
      Y = cssem::manifest("y_observed", reliability = 1, standardize = FALSE),
      folds = 3L)
    fit <- cssem::fit_states(measurement, data, seed = seed, diagnostics = FALSE)
    structure <- cssem::specify_structure(Y ~ linear(X) + Z, order = c("X", "Z", "Y"))
    association <- cssem::associate(fit, structure, structural_repeats = 2L,
      seed = seed, spline_df = 3L, shadow_scope = "unrestricted")
    make_estimate_rows(association)
  }, error = identity)

  if (inherits(result, "error")) {
    estimands <- c("latent_path_fixed", "latent_path_repeat",
      "observed_score_path_fixed", "observed_score_path_repeat")
    estimates <- data.frame(estimand = estimands, estimate = NA_real_,
      estimate_status = "unavailable", lower = NA_real_, upper = NA_real_,
      interval_status = "unavailable", status_reason = conditionMessage(result),
      stringsAsFactors = FALSE)
    audit <- list(selected_shapes = NA_character_, fixed_successful = 0L,
      repeat_successful = 0L, repeat_selection_changes = NA_integer_)
    status <- "failed"
    failure_reason <- conditionMessage(result)
    converged <- FALSE
  } else {
    estimates <- result$estimates
    audit <- result$audit
    interval_unavailable <- estimates$interval_status == "unavailable"
    status <- if (any(interval_unavailable)) "partial" else "completed"
    failure_reason <- if (any(interval_unavailable))
      paste(unique(estimates$status_reason[interval_unavailable]), collapse = " | ") else ""
    converged <- TRUE
  }

  audit_row <- data.frame(
    scenario = scenario$scenario, n = nrow(data), analyze_seed = seed,
    selected_shapes = audit$selected_shapes,
    fixed_successful = audit$fixed_successful,
    repeat_successful = audit$repeat_successful,
    repeat_selection_changes = audit$repeat_selection_changes,
    stringsAsFactors = FALSE)
  audit_dir <- Sys.getenv("CSSEM_G15_OUTDIR",
    unset = file.path(getwd(), "tests", "internal", "validation_results"))
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  run_id <- Sys.getenv("CSSEM_G15_RUN_ID", unset = "unknown")
  audit_path <- file.path(audit_dir, sprintf("g15-inference-audit-%s-%d.csv",
    run_id, Sys.getpid()))
  utils::write.table(audit_row, audit_path, sep = ",", row.names = FALSE,
    col.names = !file.exists(audit_path), append = file.exists(audit_path))
  list(status = status, converged = converged, failure_reason = failure_reason,
    estimates = estimates)
  }
})

.g15_run <- function() {
  scenarios <- .g15_scenarios
  spec <- cssem::study_spec(scenarios, sample_sizes = 240L,
    generate = .g15_generate, truth = .g15_truth, analyze = .g15_analyze,
    metadata = list(
      description = "Independent bootstrap calibration for reliability and structural shape selection",
      analysis_scope = paste("Manifest single-item constructs with declared reliability; the focal X -> Y effect is linear,",
        "Z has a linear or monotone-curved nuisance effect, and the interval conditions on the observed measurement scale."),
      target = "Analytic latent X -> Y path and its observed-score linear-projection counterpart",
      seed = .g15_seed, confidence_level = .g15_level,
      absolute_bias_threshold = .g15_thresholds$absolute_bias,
      coverage_nominal = .g15_thresholds$coverage_nominal,
      coverage_tolerance = .g15_thresholds$coverage_tolerance,
      unconditional_failure_max = .g15_thresholds$unconditional_failure_max,
      minimum_inner_bootstrap_success = .g15_thresholds$minimum_inner_bootstrap_success))
  if (.g15_config$shards > 1L) {
    simulate_shard <- getFromNamespace(".study_simulate_shard", "cssem")
    simulation <- simulate_shard(spec, reps = .g15_config$reps,
      seed = .g15_seed, workers = .g15_config$workers,
      shard = .g15_config$shard, shards = .g15_config$shards)
    audit_paths <- list.files(.g15_output_dir, pattern = sprintf(
      "^g15-inference-audit-%s-[0-9]+\\.csv$", .g15_run_id), full.names = TRUE)
    audit_table <- if (length(audit_paths)) do.call(rbind, lapply(audit_paths,
      utils::read.csv, stringsAsFactors = FALSE)) else data.frame()
    if (length(audit_paths)) unlink(audit_paths)
    shard_path <- file.path(.g15_output_dir, sprintf(
      "g15-inference-confirmation-shard-%02d.rds", .g15_config$shard))
    provenance <- list(
      R_version = R.version.string,
      cssem_version = as.character(utils::packageVersion("cssem")),
      RNGkind = RNGkind(),
      operating_system = unname(Sys.info()[["sysname"]]),
      study_tier = .g15_config$tier,
      outer_reps = .g15_config$reps,
      inner_reps = .g15_config$inner_reps,
      study_seed = .g15_seed,
      confidence_level = .g15_level,
      minimum_inner_bootstrap_success = .g15_thresholds$minimum_inner_bootstrap_success,
      workers_requested = .g15_config$workers
    )
    saveRDS(list(simulation = simulation, diagnostics = audit_table,
      provenance = provenance), shard_path)
    message(sprintf("Saved confirmation shard %d/%d: %d outer replications per scenario, %d inner draws, %d workers.",
      .g15_config$shard, .g15_config$shards, .g15_config$reps,
      .g15_config$inner_reps, .g15_config$workers))
    message(sprintf("Shard artifact: %s", shard_path))
    return(invisible(shard_path))
  }

  simulation <- cssem::simulate_study(spec, reps = .g15_config$reps,
    seed = .g15_seed, workers = .g15_config$workers)
  summary <- cssem::summarize_study(simulation)
  metrics <- summary$metrics[summary$metrics$metric %in% c("bias", "rmse",
    "coverage", "failure", "partial", "convergence", "interval_availability"), , drop = FALSE]
  metrics$tier <- .g15_config$tier
  metrics$outer_reps <- .g15_config$reps
  metrics$inner_reps <- .g15_config$inner_reps
  metrics$seed <- .g15_seed
  metrics$absolute_bias_max <- .g15_thresholds$absolute_bias
  metrics$coverage_nominal <- .g15_thresholds$coverage_nominal
  metrics$coverage_tolerance <- .g15_thresholds$coverage_tolerance
  metrics$unconditional_failure_max <- .g15_thresholds$unconditional_failure_max

  output_dir <- .g15_output_dir
  stem <- paste0("g15-inference-", .g15_config$tier)
  audit_paths <- list.files(output_dir, pattern = sprintf(
    "^g15-inference-audit-%s-[0-9]+\\.csv$", .g15_run_id), full.names = TRUE)
  worker_audits <- if (length(audit_paths)) do.call(rbind, lapply(audit_paths,
    utils::read.csv, stringsAsFactors = FALSE)) else data.frame()
  audit_table <- worker_audits
  if (length(audit_paths)) unlink(audit_paths)
  saveRDS(list(replications = simulation$replications,
    scenarios = simulation$scenarios, sample_sizes = simulation$sample_sizes,
    metadata = simulation$metadata, seed = simulation$seed,
    reps = simulation$reps, workers = simulation$workers),
    file.path(output_dir, paste0(stem, "-replications.rds")))
  saveRDS(list(metrics = metrics, diagnostics = audit_table),
    file.path(output_dir, paste0(stem, "-summary.rds")))
  aggregate_path <- file.path(output_dir, "g15-inference-coverage.csv")
  prior <- if (file.exists(aggregate_path)) utils::read.csv(aggregate_path,
    stringsAsFactors = FALSE) else data.frame()
  if (nrow(prior)) prior <- prior[prior$tier != .g15_config$tier, , drop = FALSE]
  utils::write.csv(rbind(prior, metrics), aggregate_path, row.names = FALSE,
    na = "")
  message(sprintf("Saved %s tier: %d outer replications per scenario, %d inner draws, %d workers.",
    .g15_config$tier, .g15_config$reps, .g15_config$inner_reps,
    .g15_config$workers))
  message(sprintf("Raw results: %s", file.path(output_dir, paste0(stem, "-replications.rds"))))
  message(sprintf("Aggregate results: %s", aggregate_path))
}

.g15_run()
