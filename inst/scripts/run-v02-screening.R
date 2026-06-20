library(cssem)

measurement <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"), reps = 1, seed = 2026
)
structural <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"), reps = 1, seed = 3026
)
print(cssem_validation_report(measurement, structural))
