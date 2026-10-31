# Independent validation of the average-derivative estimand (F04).
#
# Target: the population average derivative of E[Y | X, C] in X. With
# Y = 0.3 X + 0.2 X^2 + 0.7 C + e and E[X] = 0 it is exactly 0.3, derived from
# the generating equation rather than from any package code.
#
# Conditions:
#   gaussian        X | C Gaussian, homoskedastic; X observed without error
#   skewed          X | C a centred gamma (skewness 1.41); observed without error
#   gaussian_rel80  as gaussian, but the analysed X carries classical error with
#                   reliability 0.80, declared to the estimator, which
#                   disattenuates by the linear attenuation ratio
#   gaussian_rel80_adjuster80
#                   as gaussian_rel80, and the adjuster C is also measured with
#                   reliability 0.80 (declared): residual confounding remains
#                   unless the linear correction removes it
#
# For each condition the script reports bias, RMSE, interval coverage of 0.3,
# mean interval width, the share of replications whose Riesz diagnostic is
# "consistent", and the share labelled causal_under_assumptions, with Monte
# Carlo standard errors.
#
# Env: AME_REPS (default 500), AME_N (default 1000), AME_SEED (default 20261101),
#      AME_OUT (default "ame-validation.csv")

env <- function(key, default) { v <- Sys.getenv(key); if (nzchar(v)) v else default }
reps <- as.integer(env("AME_REPS", "500"))
n <- as.integer(env("AME_N", "1000"))
seed <- as.integer(env("AME_SEED", "20261101"))
out <- env("AME_OUT", "ame-validation.csv")
suppressPackageStartupMessages(library(cssem))

truth <- 0.3
simulate <- function(condition, n) {
  C <- stats::rnorm(n)
  u <- if (condition == "skewed") (stats::rgamma(n, 2, 1) - 2) / sqrt(2) else stats::rnorm(n)
  X <- 0.6 * C + 0.8 * u
  Y <- 0.3 * X + 0.2 * X^2 + 0.7 * C + stats::rnorm(n, sd = .5)
  measured <- function(v) v + stats::rnorm(n, sd = sqrt(stats::var(v) * (1 / .8 - 1)))
  error_x <- startsWith(condition, "gaussian_rel80")
  error_c <- condition == "gaussian_rel80_adjuster80"
  scores <- data.frame(X = if (error_x) measured(X) else X, C = if (error_c) measured(C) else C, Y = Y)
  structure(list(scores = scores, folds = sample(rep_len(1:5, n)),
    reliability = c(X = if (error_x) .8 else 1, C = if (error_c) .8 else 1, Y = 1), full_models = list(),
    structure = cssem_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))), class = "cssem_association")
}

rows <- list()
for (condition in c("gaussian", "skewed", "gaussian_rel80", "gaussian_rel80_adjuster80")) {
  for (r in seq_len(reps)) {
    set.seed(seed + 1000L * match(condition, c("gaussian", "skewed", "gaussian_rel80", "gaussian_rel80_adjuster80")) + r)
    fit <- suppressWarnings(causal_effect(simulate(condition, n), "X", "Y", adjust = "C",
      estimand = "adjusted_ame", temporal_order = c("C", "X", "Y")))
    rows[[length(rows) + 1L]] <- data.frame(condition = condition, replication = r, n = n,
      estimate = fit$adjusted_effect, ci_low = fit$ci_low, ci_high = fit$ci_high,
      riesz_status = fit$riesz_diagnostic$status, skewness = fit$riesz_diagnostic$skewness,
      disattenuated = fit$disattenuated, attenuation_ratio = fit$attenuation_ratio, label = fit$label)
  }
}
raw <- do.call(rbind, rows)
raw$covers <- raw$ci_low <= truth & raw$ci_high >= truth
utils::write.csv(raw, out, row.names = FALSE)

summary_rows <- do.call(rbind, lapply(split(raw, raw$condition), function(d) {
  dev <- d$estimate - truth; p <- mean(d$covers); k <- nrow(d)
  data.frame(condition = d$condition[[1L]], reps = k, mean_estimate = mean(d$estimate),
    bias = mean(dev), bias_mcse = stats::sd(dev) / sqrt(k), rmse = sqrt(mean(dev^2)),
    coverage = p, coverage_mcse = sqrt(p * (1 - p) / k), mean_width = mean(d$ci_high - d$ci_low),
    riesz_consistent = mean(d$riesz_status == "consistent"), disattenuated = mean(d$disattenuated),
    labelled_causal = mean(d$label == "causal_under_assumptions"))
}))
utils::write.csv(summary_rows, sub("\\.csv$", "-summary.csv", out), row.names = FALSE)
print(summary_rows, digits = 3, row.names = FALSE)
