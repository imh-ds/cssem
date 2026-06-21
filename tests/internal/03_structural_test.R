# Run after 02_measurement_test.R:
# Rscript tests/internal/03_structural_test.R
#
# This layer is associational. It uses locked scores, not the simulated truth,
# and does not make causal claims.

library(cssem)

script_file <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_file[grepl("^--file=", script_file)])
script_dir <- if (length(script_file)) dirname(normalizePath(script_file)) else "tests/internal"
fit_file <- file.path(script_dir, "measurement_fit.rds")

if (!file.exists(fit_file)) {
  stop("Run tests/internal/02_measurement_test.R first.", call. = FALSE)
}

fit <- readRDS(fit_file)

# Declare theory-specified associational paths.
structure <- cssem_structure(list(
  Quality = "Trust",
  Loyalty = c("Trust", "Quality")
), order = c("Trust", "Quality", "Loyalty"))

# Compare declared linear, monotone, and low-complexity smooth effects using
# structural-fold CV. At most one nonlinear edge is retained per outcome.
# The shadow model is an adequacy benchmark, not an alternative theory.
association <- cssem_associate(fit, structure)

print(association)
cat("\nCandidate model metrics:\n")
print(association$candidate_metrics)

cat("\nQuality effect card:\n")
print(cssem_effect_card(association, "Quality"))
cat("\nLoyalty effect card:\n")
print(cssem_effect_card(association, "Loyalty"))
cat("\nEffect evidence ledger:\n")
print(cssem_effect_ledger(association))

cat("\nShadow-model specification gaps (positive favors the declared model):\n")
print(cssem_specification_gap(association))

# Optional visual checks in an interactive R session:
# plot(fit, type = "scores")
# plot(fit, type = "redundancy")

saveRDS(association, file.path(script_dir, "associational_structure.rds"))
cat("\nSaved associational_structure.rds in", script_dir, "\n")
