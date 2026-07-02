source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

output_dir <- file.path("tests", "internal", "validation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- cssem_moderated_mediation_validation_manifest("screening")
workers <- max(1L, parallel::detectCores() - 1L)
started <- Sys.time()
results <- cssem_run_moderated_mediation_validation(manifest, reps = 5, seed = 8026, eiv_bootstrap = 300, workers = workers)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

utils::write.csv(results, file.path(output_dir, "moderated_mediation_validation.csv"), row.names = FALSE)

groups <- split(results, list(results$scenario, results$loading), drop = TRUE)
summary <- do.call(rbind, lapply(groups, function(group) data.frame(
  scenario = group$scenario[[1L]], loading = group$loading[[1L]], n_rows = nrow(group),
  mean_true_index = mean(group$true_index),
  mean_naive_index = mean(group$naive_index),
  mean_disattenuated_index = mean(group$disattenuated_index, na.rm = TRUE),
  mean_naive_abs_bias = mean(group$naive_abs_bias),
  mean_disattenuated_abs_bias = mean(group$disattenuated_abs_bias, na.rm = TRUE),
  index_coverage = mean(group$index_covers_truth, na.rm = TRUE),
  mean_runtime_seconds = mean(group$runtime_seconds),
  stringsAsFactors = FALSE
)))
utils::write.csv(summary, file.path(output_dir, "moderated_mediation_validation_summary.csv"), row.names = FALSE)

metadata <- data.frame(
  run = "moderated_mediation_validation", workers_requested = workers,
  jobs = nrow(manifest) * 5L, rows_written = nrow(results), elapsed_seconds = round(elapsed, 2),
  stringsAsFactors = FALSE
)
utils::write.csv(metadata, file.path(output_dir, "moderated_mediation_validation_run_metadata.csv"), row.names = FALSE)

print(summary)
