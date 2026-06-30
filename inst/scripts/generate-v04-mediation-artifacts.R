source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

output_dir <- file.path("tests", "internal", "validation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- cssem_mediation_validation_manifest("screening")
workers <- max(1L, parallel::detectCores() - 1L)
started <- Sys.time()
results <- cssem_run_mediation_validation(manifest, reps = 5, seed = 4026, eiv_bootstrap = 300, workers = workers)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

utils::write.csv(results, file.path(output_dir, "mediation_validation.csv"), row.names = FALSE)

groups <- split(results, list(results$scenario, results$loading), drop = TRUE)
summary <- do.call(rbind, lapply(groups, function(group) {
  data.frame(
    scenario = group$scenario[[1L]],
    loading = group$loading[[1L]],
    n_rows = nrow(group),
    mean_true_indirect = mean(group$true_indirect),
    mean_naive_indirect = mean(group$naive_indirect),
    mean_disattenuated_indirect = mean(group$disattenuated_indirect, na.rm = TRUE),
    mean_naive_abs_bias = mean(group$naive_abs_bias),
    mean_disattenuated_abs_bias = mean(group$disattenuated_abs_bias, na.rm = TRUE),
    disattenuated_available_rate = mean(!is.na(group$disattenuated_indirect)),
    disattenuated_coverage = mean(group$disattenuated_covers_truth, na.rm = TRUE),
    mean_runtime_seconds = mean(group$runtime_seconds),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(summary, file.path(output_dir, "mediation_validation_summary.csv"), row.names = FALSE)

metadata <- data.frame(
  run = "mediation_validation",
  workers_requested = workers,
  jobs = nrow(manifest) * 5L,
  rows_written = nrow(results),
  elapsed_seconds = round(elapsed, 2),
  stringsAsFactors = FALSE
)
utils::write.csv(metadata, file.path(output_dir, "mediation_validation_run_metadata.csv"), row.names = FALSE)

print(summary)
