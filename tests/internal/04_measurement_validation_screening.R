# Compact v0.2 measurement validation run.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
workers <- min(4L, max(1L, parallel::detectCores(logical = FALSE) - 1L))

started <- proc.time()[["elapsed"]]
measurement_results <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"), reps = 3, seed = 2026, workers = workers
)
elapsed <- proc.time()[["elapsed"]] - started
utils::write.csv(measurement_results, file.path(results_dir, "measurement_screening.csv"), row.names = FALSE)
utils::write.csv(data.frame(
  run = "measurement_screening", workers_requested = workers,
  jobs = nrow(measurement_results), elapsed_seconds = elapsed,
  worker_pids = paste(sort(unique(measurement_results$worker_pid)), collapse = ";")
), file.path(results_dir, "measurement_screening_run_metadata.csv"), row.names = FALSE)
print(measurement_results)
