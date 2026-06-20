library(cssem)

# CI verifies the v0.2 pipeline end to end without attempting a release-scale
# simulation. Full screening and confirmation remain manual workflows.
measurement_manifest <- cssem_measurement_validation_manifest("screening")[1, ]
structural_manifest <- cssem_structural_validation_manifest("screening")[1, ]

measurement <- cssem_run_measurement_validation(measurement_manifest, reps = 1, seed = 2026, folds = 2, iterations = 2, max_iterations = 2)
structural <- cssem_run_structural_validation(structural_manifest, reps = 1, seed = 3026, folds = 2, iterations = 2, max_iterations = 2)

stopifnot(nrow(measurement) == 1L, nrow(structural) == 2L)
print(measurement)
print(structural)
