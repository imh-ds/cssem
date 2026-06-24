source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

# Local v0.3 developer screening: representative scenarios, one replication
# each, separate from the release confirmation workflow.
measurement <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"), reps = 1, seed = 2026
)
structural <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"), reps = 1, seed = 3026
)
print(cssem_validation_report(measurement, structural))
