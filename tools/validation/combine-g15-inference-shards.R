# Combine the deterministic G15 confirmation shards downloaded from Actions.
# The study summary is computed only after the complete replication grid has
# been checked and reassembled.

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the cssem package root.", call. = FALSE)
}
if (!requireNamespace("cssem", quietly = TRUE)) {
  stop("Install the current cssem checkout before combining G15 artifacts.", call. = FALSE)
}

env <- function(key, default) {
  value <- Sys.getenv(key, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

input_dir <- env("CSSEM_G15_SHARD_INDIR", "g15-artifacts")
output_dir <- env("CSSEM_G15_OUTDIR", "g15-combined")
expected_shards <- as.integer(env("CSSEM_G15_SHARDS", "20"))
expected_reps <- 500L
expected_inner_reps <- 199L
expected_seed <- 150415L
if (is.na(expected_shards) || expected_shards < 1L) {
  stop("CSSEM_G15_SHARDS must be a positive integer.", call. = FALSE)
}

artifact_paths <- list.files(input_dir,
  pattern = "^g15-inference-confirmation-shard-[0-9]+\\.rds$",
  recursive = TRUE, full.names = TRUE)
artifact_ids <- as.integer(sub(
  "^.*g15-inference-confirmation-shard-([0-9]+)\\.rds$", "\\1", artifact_paths))
if (length(artifact_paths) != expected_shards || anyNA(artifact_ids) ||
    anyDuplicated(artifact_ids) || !setequal(artifact_ids, seq_len(expected_shards))) {
  stop(sprintf("Expected exactly shards 1 through %d; found: %s.",
    expected_shards, paste(sort(artifact_ids), collapse = ", ")), call. = FALSE)
}

parts <- lapply(artifact_paths, readRDS)
valid_part <- vapply(parts, function(part) {
  is.list(part) && inherits(part$simulation, "cssem_study_shard") &&
    is.data.frame(part$diagnostics) && is.list(part$provenance) &&
    is.character(part$provenance$R_version) &&
    is.character(part$provenance$cssem_version) &&
    is.character(part$provenance$RNGkind) &&
    is.character(part$provenance$operating_system) &&
    is.character(part$provenance$study_tier) &&
    is.numeric(part$provenance$outer_reps) &&
    is.numeric(part$provenance$inner_reps) &&
    is.numeric(part$provenance$study_seed) &&
    is.numeric(part$provenance$confidence_level) &&
    is.numeric(part$provenance$minimum_inner_bootstrap_success) &&
    is.numeric(part$provenance$workers_requested)
}, logical(1))
if (!all(valid_part)) stop("A shard artifact has an invalid structure.", call. = FALSE)
provenance <- parts[[1L]]$provenance
if (!all(vapply(parts, function(part) identical(part$provenance, provenance),
    logical(1)))) {
  stop("Shard runtime provenance differs; do not combine a mixed-environment study.",
    call. = FALSE)
}
recorded_ids <- vapply(parts, function(part) part$simulation$shard, integer(1))
if (!identical(artifact_ids, recorded_ids)) {
  stop("Artifact names and recorded shard identifiers do not match.", call. = FALSE)
}

simulation <- getFromNamespace(".study_combine_shards", "cssem")(
  lapply(parts, `[[`, "simulation"))
if (simulation$reps != expected_reps || simulation$seed != expected_seed ||
    !identical(as.character(simulation$scenarios$scenario),
      c("rho0.40_linear", "rho0.80_linear", "rho0.40_curved", "rho0.80_curved")) ||
    !identical(simulation$sample_sizes, 240L) ||
    provenance$cssem_version != as.character(utils::packageVersion("cssem")) ||
    provenance$study_tier != "confirmation" ||
    provenance$outer_reps != expected_reps ||
    provenance$inner_reps != expected_inner_reps ||
    provenance$study_seed != expected_seed ||
    provenance$confidence_level != .95 ||
    provenance$minimum_inner_bootstrap_success != .90 ||
    provenance$workers_requested != 2L ||
    any(vapply(parts, function(part) {
      part$simulation$workers != part$provenance$workers_requested
    }, logical(1))) ||
    simulation$metadata$confidence_level != provenance$confidence_level ||
    simulation$metadata$seed != expected_seed ||
    simulation$metadata$minimum_inner_bootstrap_success !=
      provenance$minimum_inner_bootstrap_success ||
    simulation$metadata$absolute_bias_threshold != .10 ||
    simulation$metadata$coverage_nominal != .95 ||
    simulation$metadata$coverage_tolerance != .04 ||
    simulation$metadata$unconditional_failure_max != .05) {
  stop("The combined artifacts do not match the registered confirmation design.",
    call. = FALSE)
}

diagnostic_parts <- lapply(parts, `[[`, "diagnostics")
diagnostics <- if (all(vapply(diagnostic_parts, nrow, integer(1)) == 0L)) {
  data.frame()
} else {
  do.call(rbind, diagnostic_parts)
}
if (nrow(diagnostics)) {
  required_diagnostics <- c("scenario", "analyze_seed", "selected_shapes",
    "fixed_successful", "repeat_successful", "repeat_selection_changes")
  if (!all(required_diagnostics %in% names(diagnostics)) ||
      anyDuplicated(diagnostics$analyze_seed)) {
    stop("The combined diagnostic audit is missing required fields or has duplicate seeds.",
      call. = FALSE)
  }
  replication_audit <- unique(simulation$replications[c("scenario", "analyze_seed")])
  diagnostic_audit <- unique(diagnostics[c("scenario", "analyze_seed")])
  if (!all(diagnostic_audit$analyze_seed %in% replication_audit$analyze_seed) ||
      !all(diagnostic_audit$scenario ==
        replication_audit$scenario[match(diagnostic_audit$analyze_seed,
          replication_audit$analyze_seed)])) {
    stop("A diagnostic row does not map to a scheduled replication.", call. = FALSE)
  }
}
rownames(diagnostics) <- NULL

summary <- cssem::summarize_study(simulation)
metrics <- summary$metrics[summary$metrics$metric %in% c("bias", "rmse",
  "coverage", "failure", "partial", "convergence", "interval_availability"), , drop = FALSE]
metrics$tier <- "confirmation"
metrics$outer_reps <- simulation$reps
metrics$inner_reps <- provenance$inner_reps
metrics$seed <- simulation$seed
metrics$absolute_bias_max <- simulation$metadata$absolute_bias_threshold
metrics$coverage_nominal <- simulation$metadata$coverage_nominal
metrics$coverage_tolerance <- simulation$metadata$coverage_tolerance
metrics$unconditional_failure_max <- simulation$metadata$unconditional_failure_max

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stem <- "g15-inference-confirmation"
saveRDS(list(replications = simulation$replications,
  scenarios = simulation$scenarios, sample_sizes = simulation$sample_sizes,
  metadata = simulation$metadata, seed = simulation$seed,
  reps = simulation$reps, workers = simulation$workers),
  file.path(output_dir, paste0(stem, "-replications.rds")))
saveRDS(list(metrics = metrics, diagnostics = diagnostics,
  provenance = provenance),
  file.path(output_dir, paste0(stem, "-summary.rds")))

aggregate_path <- file.path(output_dir, "g15-inference-coverage.csv")
prior_path <- file.path("tests", "internal", "validation_results",
  "g15-inference-coverage.csv")
prior <- if (file.exists(prior_path)) utils::read.csv(prior_path,
  stringsAsFactors = FALSE) else data.frame()
if (nrow(prior)) prior <- prior[prior$tier != "confirmation", , drop = FALSE]
utils::write.csv(rbind(prior, metrics), aggregate_path, row.names = FALSE, na = "")
message(sprintf("Combined confirmation tier: %d outer replications per scenario, %d inner draws, %d shards.",
  simulation$reps, expected_inner_reps, expected_shards))
message(sprintf("Raw results: %s", file.path(output_dir,
  paste0(stem, "-replications.rds"))))
message(sprintf("Aggregate results: %s", aggregate_path))
