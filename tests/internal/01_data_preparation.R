# Run this script first from the repository root:
# Rscript tests/internal/01_data_preparation.R
#
# It creates reusable sample survey data and the known latent truth used only
# to validate this simulation. The .rds files are saved beside this script.

script_file <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_file[grepl("^--file=", script_file)])
script_dir <- if (length(script_file)) dirname(normalizePath(script_file)) else "tests/internal"

set.seed(2026)
n <- 500

# Simulated construct states. These are unavailable in a real study and are
# saved here only so that measurement recovery can be demonstrated.
trust_true <- rnorm(n)
quality_true <- 0.55 * trust_true + rnorm(n, sd = 0.85)
loyalty_true <- 0.35 * trust_true + 0.60 * quality_true + rnorm(n, sd = 0.75)

make_likert_items <- function(z, prefix, n_items = 4, loading = 0.80) {
  out <- lapply(seq_len(n_items), function(j) {
    response <- loading * z + rnorm(length(z), sd = sqrt(1 - loading^2))
    cut(response, breaks = c(-Inf, -0.85, -0.20, 0.40, 1.05, Inf), labels = 1:5)
  })
  out <- as.data.frame(out)
  names(out) <- paste0(prefix, seq_len(n_items))
  out
}

survey <- cbind(
  make_likert_items(trust_true, "trust"),
  make_likert_items(quality_true, "quality"),
  make_likert_items(loyalty_true, "loyalty")
)

# Add 3% item-level missingness, independently by item.
for (item in names(survey)) {
  missing_rows <- sample(seq_len(n), size = round(0.03 * n))
  survey[missing_rows, item] <- NA
}

truth <- data.frame(Trust = trust_true, Quality = quality_true, Loyalty = loyalty_true)
saveRDS(survey, file.path(script_dir, "sample_likert_survey.rds"))
saveRDS(truth, file.path(script_dir, "sample_likert_truth.rds"))

cat("Saved sample_likert_survey.rds and sample_likert_truth.rds in", script_dir, "\n")
print(utils::head(survey))
