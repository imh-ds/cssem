library(cssem)

set.seed(2026)

# 1. Set sample size
n <- 500

# 2. Simulate latent construct states (known only for validation)
trust_true <- rnorm(n)
quality_true <- 0.55 * trust_true + rnorm(n, sd = 0.85)
loyalty_true <- 0.35 * trust_true + 0.60 * quality_true + rnorm(n, sd = 0.75)

# 3. Turn each latent state into four noisy 5-point Likert indicators
make_likert_items <- function(z, prefix, n_items = 4, loading = 0.80) {
  out <- lapply(seq_len(n_items), function(j) {
    response <- loading * z + rnorm(length(z), sd = sqrt(1 - loading^2))

    cut(
      response,
      breaks = c(-Inf, -0.85, -0.20, 0.40, 1.05, Inf),
      labels = 1:5
    )
  })

  out <- as.data.frame(out)
  names(out) <- paste0(prefix, seq_len(n_items))
  out
}

# 4. Create an observed survey data set
survey <- cbind(
  make_likert_items(trust_true, "trust"),
  make_likert_items(quality_true, "quality"),
  make_likert_items(loyalty_true, "loyalty")
)

# 5. Optionally add 3% item-level missingness
for (item in names(survey)) {
  missing_rows <- sample(seq_len(n), size = round(0.03 * n))
  survey[missing_rows, item] <- NA
}

# 6. Declare the CS-SEM measurement model
model <- cssem_model(
  constructs = list(
    Trust = list(
      indicators = paste0("trust", 1:4),
      scales = "ordinal",
      keys = c(1, 1, 1, 1)
    ),
    Quality = list(
      indicators = paste0("quality", 1:4),
      scales = "ordinal",
      keys = c(1, 1, 1, 1)
    ),
    Loyalty = list(
      indicators = paste0("loyalty", 1:4),
      scales = "ordinal",
      keys = c(1, 1, 1, 1)
    )
  ),
  folds = 5
)

# 7. Estimate out-of-fold construct states
fit <- cssem_fit(
  model,
  survey,
  seed = 2026,
  draws = 10
)

# 8. Inspect the fitted object
print(fit)

# 9. Retrieve cross-fitted construct states
locked_scores <- fit$locked_scores
head(locked_scores)

# 10. Compare estimates with the known simulated truth
truth <- data.frame(
  Trust = trust_true,
  Quality = quality_true,
  Loyalty = loyalty_true
)

cor(locked_scores, truth)

# 11. Inspect one construct's recovery/stability card
cssem_construct_card(fit, "Trust")

# 12. View the measurement evidence ledger
cssem_evidence_ledger(fit)

# 13. Review warnings: sparse categories, reverse keys,
#     local dependence, leakage, or redundant constructs
fit$warnings

# 14. Score new compatible survey rows.
# Columns must be present in exactly this declared order.
new_scores <- cssem_score(
  fit,
  survey[c(
    paste0("trust", 1:4),
    paste0("quality", 1:4),
    paste0("loyalty", 1:4)
  )]
)

head(new_scores)
