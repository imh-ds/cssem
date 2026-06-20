# Compact v0.2 associational structural validation run.
library(cssem)

results_dir <- "tests/internal/validation_results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

structural_results <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"), reps = 3, seed = 2026
)
utils::write.csv(structural_results, file.path(results_dir, "structural_screening.csv"), row.names = FALSE)
print(structural_results)
