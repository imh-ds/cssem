source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

output_dir <- file.path("tests", "internal", "validation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- cssem_measurement_validation_manifest("screening")
started <- Sys.time()
comparators <- cssem_run_comparator_validation(
  manifest,
  reps = 3,
  seed = 4026,
  folds = 3,
  iterations = 8,
  workers = 1
)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

utils::write.csv(
  comparators,
  file.path(output_dir, "comparator_validation.csv"),
  row.names = FALSE
)

summary_groups <- split(comparators, list(comparators$engine, comparators$status, comparators$available), drop = TRUE)
summary <- do.call(rbind, lapply(summary_groups, function(group) {
  engine_rows <- comparators[comparators$engine == group$engine[[1L]] &
    comparators$available == group$available[[1L]], , drop = FALSE]
  success_rows <- engine_rows[engine_rows$status == "success", , drop = FALSE]
  data.frame(
    engine = group$engine[[1L]],
    status = group$status[[1L]],
    available = group$available[[1L]],
    n_rows = nrow(group),
    n_engine_rows = nrow(engine_rows),
    n_success = sum(engine_rows$status == "success"),
    n_error = sum(engine_rows$status == "error"),
    success_rate = mean(engine_rows$status == "success"),
    recovery = if (all(is.na(group$recovery))) NA_real_ else mean(group$recovery, na.rm = TRUE),
    mean_recovery_success_only = if (!nrow(success_rows) || all(is.na(success_rows$recovery))) NA_real_ else mean(success_rows$recovery, na.rm = TRUE),
    downstream_rmse = if (all(is.na(group$downstream_rmse))) NA_real_ else mean(group$downstream_rmse, na.rm = TRUE),
    downstream_r_squared = if (all(is.na(group$downstream_r_squared))) NA_real_ else mean(group$downstream_r_squared, na.rm = TRUE),
    mean_downstream_rmse_success_only = if (!nrow(success_rows) || all(is.na(success_rows$downstream_rmse))) NA_real_ else mean(success_rows$downstream_rmse, na.rm = TRUE),
    mean_downstream_r_squared_success_only = if (!nrow(success_rows) || all(is.na(success_rows$downstream_r_squared))) NA_real_ else mean(success_rows$downstream_r_squared, na.rm = TRUE),
    runtime_seconds = if (all(is.na(group$runtime_seconds))) NA_real_ else mean(group$runtime_seconds, na.rm = TRUE),
    mean_runtime_success_only = if (!nrow(success_rows) || all(is.na(success_rows$runtime_seconds))) NA_real_ else mean(success_rows$runtime_seconds, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  summary,
  file.path(output_dir, "comparator_summary.csv"),
  row.names = FALSE
)

benchmark_groups <- split(comparators, list(comparators$engine, comparators$available), drop = TRUE)
benchmark_matrix <- do.call(rbind, lapply(benchmark_groups, function(group) {
  success_rows <- group[group$status == "success", , drop = FALSE]
  data.frame(
    engine = group$engine[[1L]],
    available = group$available[[1L]],
    n_rows = nrow(group),
    n_success = sum(group$status == "success"),
    n_error = sum(group$status == "error"),
    success_rate = mean(group$status == "success"),
    mean_recovery_success_only = if (!nrow(success_rows) || all(is.na(success_rows$recovery))) NA_real_ else mean(success_rows$recovery, na.rm = TRUE),
    mean_downstream_rmse_success_only = if (!nrow(success_rows) || all(is.na(success_rows$downstream_rmse))) NA_real_ else mean(success_rows$downstream_rmse, na.rm = TRUE),
    mean_downstream_r_squared_success_only = if (!nrow(success_rows) || all(is.na(success_rows$downstream_r_squared))) NA_real_ else mean(success_rows$downstream_r_squared, na.rm = TRUE),
    mean_runtime_success_only = if (!nrow(success_rows) || all(is.na(success_rows$runtime_seconds))) NA_real_ else mean(success_rows$runtime_seconds, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  benchmark_matrix,
  file.path(output_dir, "comparator_benchmark_matrix.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  run = "comparator_validation",
  workers_requested = 1L,
  jobs = nrow(manifest) * 3L,
  rows_written = nrow(comparators),
  elapsed_seconds = round(elapsed, 2),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "comparator_run_metadata.csv"),
  row.names = FALSE
)

print(summary)
print(benchmark_matrix)
