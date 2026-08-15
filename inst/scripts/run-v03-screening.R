source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

# Local v0.3 developer screening: representative scenarios, one replication
# each, separate from the release confirmation workflow.
measurement <- validate_measurement(
  measurement_manifest("screening"), reps = 1, seed = 2026
)
structural <- validate_structure(
  structural_manifest("screening"), reps = 1, seed = 3026
)
print(validation_report(measurement, structural))
