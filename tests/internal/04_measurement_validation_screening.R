# Compact v0.2 measurement validation run.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

measurement_results <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"), reps = 3, seed = 2026
)
utils::write.csv(measurement_results, file.path(results_dir, "measurement_screening.csv"), row.names = FALSE)
print(measurement_results)
