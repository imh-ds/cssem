# Exploratory diagnostic calibration: this does not promote residual
# correlations to automatic warnings.  It records their simulated rates.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
workers <- min(4L, max(1L, parallel::detectCores(logical = FALSE) - 1L))

started <- proc.time()[["elapsed"]]
diagnostic_results <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("diagnostic"), reps = 5, seed = 4026,
  diagnostics = TRUE, workers = workers
)
elapsed <- proc.time()[["elapsed"]] - started

utils::write.csv(diagnostic_results, file.path(results_dir, "measurement_diagnostic_screening.csv"), row.names = FALSE)
utils::write.csv(data.frame(
  run = "measurement_diagnostic_screening", workers_requested = workers,
  jobs = nrow(diagnostic_results), elapsed_seconds = elapsed,
  worker_pids = paste(sort(unique(diagnostic_results$worker_pid)), collapse = ";")
), file.path(results_dir, "measurement_diagnostic_run_metadata.csv"), row.names = FALSE)
print(diagnostic_results)
