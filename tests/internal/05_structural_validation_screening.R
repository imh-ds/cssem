# Compact v0.2 associational structural validation run.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
workers <- min(4L, max(1L, parallel::detectCores(logical = FALSE) - 1L))

started <- proc.time()[["elapsed"]]
structural_results <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"), reps = 3, seed = 2026, workers = workers
)
elapsed <- proc.time()[["elapsed"]] - started
utils::write.csv(structural_results, file.path(results_dir, "structural_screening.csv"), row.names = FALSE)
utils::write.csv(data.frame(
  run = "structural_screening", workers_requested = workers,
  jobs = length(unique(paste(structural_results$scenario, structural_results$replication))),
  elapsed_seconds = elapsed,
  worker_pids = paste(sort(unique(structural_results$worker_pid)), collapse = ";")
), file.path(results_dir, "structural_screening_run_metadata.csv"), row.names = FALSE)
print(structural_results)
