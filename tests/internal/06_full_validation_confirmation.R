# Run only after reviewing the compact screening outputs from scripts 04 and 05.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
workers <- min(4L, max(1L, parallel::detectCores(logical = FALSE) - 1L))

measurement_started <- proc.time()[["elapsed"]]
measurement_results <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"), reps = 5, seed = 2026, workers = workers
)
measurement_elapsed <- proc.time()[["elapsed"]] - measurement_started
structural_started <- proc.time()[["elapsed"]]
structural_results <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"), reps = 5, seed = 3026, workers = workers
)
structural_elapsed <- proc.time()[["elapsed"]] - structural_started
report <- cssem_validation_report(measurement_results, structural_results)

utils::write.csv(measurement_results, file.path(results_dir, "measurement_screening.csv"), row.names = FALSE)
utils::write.csv(structural_results, file.path(results_dir, "structural_screening.csv"), row.names = FALSE)
utils::write.csv(report$gates, file.path(results_dir, "release_gates.csv"), row.names = FALSE)
utils::write.csv(rbind(
  data.frame(run = "measurement_confirmation", workers_requested = workers,
    jobs = nrow(measurement_results), elapsed_seconds = measurement_elapsed,
    worker_pids = paste(sort(unique(measurement_results$worker_pid)), collapse = ";")),
  data.frame(run = "structural_confirmation", workers_requested = workers,
    jobs = length(unique(paste(structural_results$scenario, structural_results$replication))), elapsed_seconds = structural_elapsed,
    worker_pids = paste(sort(unique(structural_results$worker_pid)), collapse = ";"))
), file.path(results_dir, "confirmation_run_metadata.csv"), row.names = FALSE)
print(report)
