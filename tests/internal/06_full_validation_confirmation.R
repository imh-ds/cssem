# Run only after reviewing the compact screening outputs from scripts 04 and 05.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

measurement_results <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"), reps = 5, seed = 2026
)
structural_results <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"), reps = 5, seed = 3026
)
report <- cssem_validation_report(measurement_results, structural_results)

utils::write.csv(measurement_results, file.path(results_dir, "measurement_screening.csv"), row.names = FALSE)
utils::write.csv(structural_results, file.path(results_dir, "structural_screening.csv"), row.names = FALSE)
utils::write.csv(report$gates, file.path(results_dir, "release_gates.csv"), row.names = FALSE)
print(report)
