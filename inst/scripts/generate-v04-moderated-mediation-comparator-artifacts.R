source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

output_dir <- file.path("tests", "internal", "validation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- cssem_moderated_mediation_validation_manifest("full")
workers <- max(1L, parallel::detectCores() - 1L)
started <- Sys.time()
results <- cssem_run_moderated_mediation_comparator_validation(
  manifest, reps = 10, seed = 9026, eiv_bootstrap = 200, seminr_bootstrap = 200, workers = workers
)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

utils::write.csv(results, file.path(output_dir, "moderated_mediation_comparator_validation.csv"), row.names = FALSE)

groups <- split(results, list(results$engine, results$scenario, results$loading), drop = TRUE)
summary <- do.call(rbind, lapply(groups, function(group) data.frame(
  engine = group$engine[[1L]], scenario = group$scenario[[1L]], loading = group$loading[[1L]],
  n_rows = nrow(group), available_rate = mean(!is.na(group$index)),
  mean_true_index = mean(group$true_index),
  mean_index = mean(group$index, na.rm = TRUE),
  mean_abs_bias = mean(group$abs_bias, na.rm = TRUE),
  coverage = mean(group$covers_truth, na.rm = TRUE),
  mean_runtime_seconds = mean(group$runtime_seconds, na.rm = TRUE),
  stringsAsFactors = FALSE
)))
utils::write.csv(summary, file.path(output_dir, "moderated_mediation_comparator_summary.csv"), row.names = FALSE)

engine_groups <- split(results, results$engine, drop = TRUE)
matrix <- do.call(rbind, lapply(engine_groups, function(group) data.frame(
  engine = group$engine[[1L]], available_rate = mean(!is.na(group$index)),
  mean_abs_bias = mean(group$abs_bias, na.rm = TRUE), coverage = mean(group$covers_truth, na.rm = TRUE),
  mean_runtime_seconds = mean(group$runtime_seconds, na.rm = TRUE), stringsAsFactors = FALSE
)))
matrix <- matrix[order(matrix$mean_abs_bias), ]
utils::write.csv(matrix, file.path(output_dir, "moderated_mediation_comparator_matrix.csv"), row.names = FALSE)

metadata <- data.frame(
  run = "moderated_mediation_comparator_validation", workers_requested = workers,
  jobs = nrow(manifest) * 10L, rows_written = nrow(results), elapsed_seconds = round(elapsed, 2),
  stringsAsFactors = FALSE
)
utils::write.csv(metadata, file.path(output_dir, "moderated_mediation_comparator_run_metadata.csv"), row.names = FALSE)

print(matrix)
print(summary)
