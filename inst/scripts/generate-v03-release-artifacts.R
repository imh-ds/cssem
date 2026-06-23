workspace_lib <- "local_r_lib"
if (dir.exists(workspace_lib)) .libPaths(c(workspace_lib, .libPaths()))

library(cssem)

output_dir <- file.path("tests", "internal", "validation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

measurement_manifest <- cssem_measurement_validation_manifest("screening")
structural_manifest <- cssem_structural_validation_manifest("screening")

measurement_started <- Sys.time()
measurement_confirmation <- cssem_run_measurement_validation(
  measurement_manifest,
  reps = 10,
  seed = 2026,
  workers = 4
)
measurement_elapsed <- as.numeric(difftime(Sys.time(), measurement_started, units = "secs"))
utils::write.csv(
  measurement_confirmation,
  file.path(output_dir, "measurement_confirmation.csv"),
  row.names = FALSE
)

measurement_worker_ids <- paste(sort(unique(measurement_confirmation$worker_pid)), collapse = ";")
measurement_metadata <- data.frame(
  run = "measurement_confirmation",
  workers_requested = 4L,
  jobs = nrow(measurement_confirmation),
  elapsed_seconds = round(measurement_elapsed, 2),
  worker_pids = measurement_worker_ids,
  stringsAsFactors = FALSE
)

structural_started <- Sys.time()
structural_confirmation <- cssem_run_structural_validation(
  structural_manifest,
  reps = 10,
  seed = 3026,
  workers = 4
)
structural_elapsed <- as.numeric(difftime(Sys.time(), structural_started, units = "secs"))
utils::write.csv(
  structural_confirmation,
  file.path(output_dir, "structural_confirmation.csv"),
  row.names = FALSE
)

structural_worker_ids <- paste(sort(unique(structural_confirmation$worker_pid)), collapse = ";")
structural_metadata <- data.frame(
  run = "structural_confirmation",
  workers_requested = 4L,
  jobs = nrow(structural_confirmation),
  elapsed_seconds = round(structural_elapsed, 2),
  worker_pids = structural_worker_ids,
  stringsAsFactors = FALSE
)

confirmation_metadata <- rbind(measurement_metadata, structural_metadata)
utils::write.csv(
  confirmation_metadata,
  file.path(output_dir, "confirmation_run_metadata.csv"),
  row.names = FALSE
)

release_report <- cssem_validation_report(
  measurement_confirmation,
  structural_confirmation
)
utils::write.csv(
  release_report$gates,
  file.path(output_dir, "release_gates.csv"),
  row.names = FALSE
)

print(release_report$gates)
cat("All v0.3 release gates passed:", release_report$passed, "\n")
