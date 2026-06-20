# Run after 01_data_preparation.R:
# Rscript tests/internal/02_measurement_test.R

library(cssem)

script_file <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_file[grepl("^--file=", script_file)])
script_dir <- if (length(script_file)) dirname(normalizePath(script_file)) else "tests/internal"
survey_file <- file.path(script_dir, "sample_likert_survey.rds")
truth_file <- file.path(script_dir, "sample_likert_truth.rds")

if (!file.exists(survey_file) || !file.exists(truth_file)) {
  stop("Run tests/internal/01_data_preparation.R first.", call. = FALSE)
}

survey <- readRDS(survey_file)
truth <- readRDS(truth_file)

# Declare the measurement model.
model <- cssem_model(
  constructs = list(
    Trust = list(indicators = paste0("trust", 1:4), scales = "ordinal", keys = c(1, 1, 1, 1)),
    Quality = list(indicators = paste0("quality", 1:4), scales = "ordinal", keys = c(1, 1, 1, 1)),
    Loyalty = list(indicators = paste0("loyalty", 1:4), scales = "ordinal", keys = c(1, 1, 1, 1))
  ),
  folds = 5
)

# Estimate locked, out-of-fold construct states.
fit <- cssem_fit(model, survey, seed = 2026, draws = 10)
locked_scores <- fit$locked_scores

print(fit)
print(utils::head(locked_scores))

# Simulation-only recovery check: diagonal values compare the estimated score
# against the latent state that generated its indicators.
cat("\nConstruct recovery against simulated truth:\n")
print(cor(locked_scores, truth))

cat("\nTrust construct card:\n")
print(cssem_construct_card(fit, "Trust"))
cat("\nMeasurement evidence ledger:\n")
print(cssem_evidence_ledger(fit))
cat("\nAutomatic measurement warnings:\n")
print(fit$warnings)
cat("\nExploratory residual-dependence diagnostics:\n")
print(cssem_residual_diagnostics(fit))

# Save the fitted measurement layer for 03_structural_test.R.
saveRDS(fit, file.path(script_dir, "measurement_fit.rds"))
saveRDS(locked_scores, file.path(script_dir, "locked_scores.rds"))
cat("\nSaved measurement_fit.rds and locked_scores.rds in", script_dir, "\n")
