source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

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

envelope <- cssem_supported_envelope()
inside <- measurement_confirmation$n >= envelope$minimum_n &
  measurement_confirmation$loading >= envelope$minimum_loading &
  measurement_confirmation$missing <= envelope$maximum_missing &
  measurement_confirmation$cross_loading == 0 &
  measurement_confirmation$local_dependence == 0 &
  !measurement_confirmation$sparse &
  measurement_confirmation$overlap < .80

supported_envelope <- cbind(
  envelope,
  data.frame(
    confirmation_measurement_jobs = nrow(measurement_confirmation),
    confirmation_structural_jobs = nrow(structural_confirmation),
    confirmation_jobs_inside_envelope = sum(inside),
    inside_envelope_convergence = mean(measurement_confirmation$converged[inside]),
    release_gates_passed = release_report$passed,
    exploratory_conditions = "cross_loadings,strong_overlap,sparse_categories,local_dependence",
    stringsAsFactors = FALSE
  )
)
utils::write.csv(
  supported_envelope,
  file.path(output_dir, "supported_envelope.csv"),
  row.names = FALSE
)

print(release_report$gates)
cat("All v0.3 release gates passed:", release_report$passed, "\n")
