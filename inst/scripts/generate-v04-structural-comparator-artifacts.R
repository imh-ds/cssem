source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

output_dir <- file.path("tests", "internal", "validation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- cssem_structural_validation_manifest("screening")
started <- Sys.time()
comparators <- cssem_run_structural_comparator_validation(
  manifest,
  reps = 3,
  seed = 5026,
  folds = 3,
  iterations = 8,
  workers = 1
)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

utils::write.csv(
  comparators,
  file.path(output_dir, "structural_comparator_validation.csv"),
  row.names = FALSE
)

summary_groups <- split(comparators, list(comparators$engine, comparators$status, comparators$available), drop = TRUE)
summary <- do.call(rbind, lapply(summary_groups, function(group) {
  engine_rows <- comparators[comparators$engine == group$engine[[1L]] &
    comparators$available == group$available[[1L]], , drop = FALSE]
  success_rows <- engine_rows[engine_rows$status == "success", , drop = FALSE]
  target <- success_rows[success_rows$outcome == "Quality" & success_rows$predictor == "Trust", , drop = FALSE]
  linear_rows <- target[target$scenario == "linear", , drop = FALSE]
  inc_rows <- target[target$scenario == "monotone_increasing", , drop = FALSE]
  dec_rows <- target[target$scenario == "monotone_decreasing", , drop = FALSE]
  smooth_rows <- target[target$scenario == "smooth_strong", , drop = FALSE]
  null_rows <- target[target$scenario %in% c("linear", "null"), , drop = FALSE]
  omitted_rows <- target[target$scenario == "omitted", , drop = FALSE]
  downstream_rows <- target[target$scenario == "downstream", , drop = FALSE]
  data.frame(
    engine = group$engine[[1L]],
    status = group$status[[1L]],
    available = group$available[[1L]],
    n_rows = nrow(group),
    n_engine_rows = nrow(engine_rows),
    n_success = sum(engine_rows$status == "success"),
    n_error = sum(engine_rows$status == "error"),
    success_rate = mean(engine_rows$status == "success"),
    quality_trust_linear_selection = if (!nrow(linear_rows)) NA_real_ else mean(linear_rows$selected_shape == "linear"),
    quality_trust_monotone_increasing_selection = if (!nrow(inc_rows)) NA_real_ else mean(inc_rows$selected_shape == "monotone_increasing"),
    quality_trust_monotone_decreasing_selection = if (!nrow(dec_rows)) NA_real_ else mean(dec_rows$selected_shape == "monotone_decreasing"),
    quality_trust_strong_smooth_selection = if (!nrow(smooth_rows)) NA_real_ else mean(grepl("^smooth", smooth_rows$selected_shape)),
    quality_trust_false_nonlinear_rate = if (!nrow(null_rows)) NA_real_ else mean(null_rows$selected_shape != "linear"),
    quality_trust_omitted_gap_flag = if (!nrow(omitted_rows)) NA_real_ else mean(omitted_rows$temporal_gap < -.03),
    quality_trust_downstream_gap_flag = if (!nrow(downstream_rows)) NA_real_ else mean(downstream_rows$unrestricted_minus_temporal < -.03),
    mean_structural_bias = if (!nrow(target) || all(is.na(target$structural_bias))) NA_real_ else mean(target$structural_bias, na.rm = TRUE),
    structural_ci_coverage = if (!nrow(target) || all(is.na(target$ci_covers_truth))) NA_real_ else mean(target$ci_covers_truth, na.rm = TRUE),
    shape_recovery_rate = if (!nrow(target) || all(is.na(target$shape_correct))) NA_real_ else mean(target$shape_correct, na.rm = TRUE),
    mean_theory_r_squared_success_only = if (!nrow(success_rows) || all(is.na(success_rows$theory_r_squared))) NA_real_ else mean(success_rows$theory_r_squared, na.rm = TRUE),
    coverage_adjusted_theory_r_squared_success_only = if (!nrow(success_rows) || all(is.na(success_rows$theory_r_squared)) || all(is.na(success_rows$score_coverage))) NA_real_ else mean(success_rows$theory_r_squared * success_rows$score_coverage, na.rm = TRUE),
    quality_trust_coverage_adjusted_theory_r_squared = if (!nrow(target) || all(is.na(target$theory_r_squared)) || all(is.na(target$score_coverage))) NA_real_ else mean(target$theory_r_squared * target$score_coverage, na.rm = TRUE),
    mean_selection_stability_success_only = if (!nrow(success_rows) || all(is.na(success_rows$selection_stability))) NA_real_ else mean(success_rows$selection_stability, na.rm = TRUE),
    mean_score_coverage_success_only = if (!nrow(success_rows) || all(is.na(success_rows$score_coverage))) NA_real_ else mean(success_rows$score_coverage, na.rm = TRUE),
    mean_total_runtime_success_only = if (!nrow(success_rows) || all(is.na(success_rows$total_runtime_seconds))) NA_real_ else mean(success_rows$total_runtime_seconds, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  summary,
  file.path(output_dir, "structural_comparator_summary.csv"),
  row.names = FALSE
)

benchmark_groups <- split(comparators, list(comparators$engine, comparators$available), drop = TRUE)
benchmark_matrix <- do.call(rbind, lapply(benchmark_groups, function(group) {
  success_rows <- group[group$status == "success", , drop = FALSE]
  target <- success_rows[success_rows$outcome == "Quality" & success_rows$predictor == "Trust", , drop = FALSE]
  linear_rows <- target[target$scenario == "linear", , drop = FALSE]
  inc_rows <- target[target$scenario == "monotone_increasing", , drop = FALSE]
  dec_rows <- target[target$scenario == "monotone_decreasing", , drop = FALSE]
  smooth_rows <- target[target$scenario == "smooth_strong", , drop = FALSE]
  null_rows <- target[target$scenario %in% c("linear", "null"), , drop = FALSE]
  omitted_rows <- target[target$scenario == "omitted", , drop = FALSE]
  downstream_rows <- target[target$scenario == "downstream", , drop = FALSE]
  data.frame(
    engine = group$engine[[1L]],
    available = group$available[[1L]],
    n_rows = nrow(group),
    n_success = sum(group$status == "success"),
    n_error = sum(group$status == "error"),
    success_rate = mean(group$status == "success"),
    quality_trust_linear_selection = if (!nrow(linear_rows)) NA_real_ else mean(linear_rows$selected_shape == "linear"),
    quality_trust_monotone_increasing_selection = if (!nrow(inc_rows)) NA_real_ else mean(inc_rows$selected_shape == "monotone_increasing"),
    quality_trust_monotone_decreasing_selection = if (!nrow(dec_rows)) NA_real_ else mean(dec_rows$selected_shape == "monotone_decreasing"),
    quality_trust_strong_smooth_selection = if (!nrow(smooth_rows)) NA_real_ else mean(grepl("^smooth", smooth_rows$selected_shape)),
    quality_trust_false_nonlinear_rate = if (!nrow(null_rows)) NA_real_ else mean(null_rows$selected_shape != "linear"),
    quality_trust_omitted_gap_flag = if (!nrow(omitted_rows)) NA_real_ else mean(omitted_rows$temporal_gap < -.03),
    quality_trust_downstream_gap_flag = if (!nrow(downstream_rows)) NA_real_ else mean(downstream_rows$unrestricted_minus_temporal < -.03),
    mean_structural_bias = if (!nrow(target) || all(is.na(target$structural_bias))) NA_real_ else mean(target$structural_bias, na.rm = TRUE),
    structural_ci_coverage = if (!nrow(target) || all(is.na(target$ci_covers_truth))) NA_real_ else mean(target$ci_covers_truth, na.rm = TRUE),
    shape_recovery_rate = if (!nrow(target) || all(is.na(target$shape_correct))) NA_real_ else mean(target$shape_correct, na.rm = TRUE),
    mean_theory_r_squared_success_only = if (!nrow(success_rows) || all(is.na(success_rows$theory_r_squared))) NA_real_ else mean(success_rows$theory_r_squared, na.rm = TRUE),
    coverage_adjusted_theory_r_squared_success_only = if (!nrow(success_rows) || all(is.na(success_rows$theory_r_squared)) || all(is.na(success_rows$score_coverage))) NA_real_ else mean(success_rows$theory_r_squared * success_rows$score_coverage, na.rm = TRUE),
    mean_selection_stability_success_only = if (!nrow(success_rows) || all(is.na(success_rows$selection_stability))) NA_real_ else mean(success_rows$selection_stability, na.rm = TRUE),
    mean_score_coverage_success_only = if (!nrow(success_rows) || all(is.na(success_rows$score_coverage))) NA_real_ else mean(success_rows$score_coverage, na.rm = TRUE),
    mean_total_runtime_success_only = if (!nrow(success_rows) || all(is.na(success_rows$total_runtime_seconds))) NA_real_ else mean(success_rows$total_runtime_seconds, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  benchmark_matrix,
  file.path(output_dir, "structural_comparator_benchmark_matrix.csv"),
  row.names = FALSE
)

coverage_adjusted_matrix <- benchmark_matrix[, c(
  "engine",
  "available",
  "n_rows",
  "n_success",
  "n_error",
  "success_rate",
  "mean_score_coverage_success_only",
  "mean_structural_bias",
  "structural_ci_coverage",
  "shape_recovery_rate",
  "mean_theory_r_squared_success_only",
  "coverage_adjusted_theory_r_squared_success_only",
  "mean_selection_stability_success_only",
  "quality_trust_linear_selection",
  "quality_trust_monotone_increasing_selection",
  "quality_trust_monotone_decreasing_selection",
  "quality_trust_strong_smooth_selection",
  "quality_trust_false_nonlinear_rate",
  "quality_trust_omitted_gap_flag",
  "quality_trust_downstream_gap_flag",
  "mean_total_runtime_success_only"
)]
utils::write.csv(
  coverage_adjusted_matrix,
  file.path(output_dir, "structural_comparator_coverage_adjusted_matrix.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  run = "structural_comparator_validation",
  workers_requested = 1L,
  jobs = nrow(manifest) * 3L,
  rows_written = nrow(comparators),
  elapsed_seconds = round(elapsed, 2),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "structural_comparator_run_metadata.csv"),
  row.names = FALSE
)

print(summary)
print(benchmark_matrix)
print(coverage_adjusted_matrix)
