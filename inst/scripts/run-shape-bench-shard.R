# CI shard runner for the shape-selection confirmation benchmark.
#
# Each replication fits the measurement model once and then saves BOTH:
#   * the shape associate() actually selects (end-to-end, the real rule), and
#   * the raw evidence that rule consumed: every candidate's per-fold CV losses,
#     the curvature-test p-value, and the same test on the true latent states.
#
# Saving the raw evidence is the point. Re-scoring a different acceptance rule,
# threshold, or shape label then costs seconds of arithmetic instead of another
# full run of measurement fits, which dominate the cost.
#
# Configured via environment variables (set by the workflow):
#   SHAPE_SHARD    1-based shard index
#   SHAPE_NSHARDS  total number of shards
#   SHAPE_SEED     base seed (default 771001)
#   SHAPE_OUTDIR   output directory (default "shape-out")
#   SHAPE_BLOCKS   optional comma-separated subset of block names

env <- function(key, default) {
  value <- Sys.getenv(key, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}
shard <- as.integer(env("SHAPE_SHARD", "1"))
nshards <- as.integer(env("SHAPE_NSHARDS", "1"))
seed0 <- as.integer(env("SHAPE_SEED", "771001"))
outdir <- env("SHAPE_OUTDIR", "shape-out")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(cssem))
ns <- asNamespace("cssem")
workers <- max(1L, parallel::detectCores())

shapes <- c("null", "linear", "monotone_increasing", "monotone_decreasing", "threshold",
            "diminishing", "plateau", "smooth_subtle")
cell <- function(scenario, n, items, reps, block, loading = .80, careless = 0, skew = 0) {
  data.frame(scenario = scenario, n = as.integer(n), items = as.integer(items),
    loading = loading, careless = careless, skew = skew, reps = as.integer(reps),
    block = block, stringsAsFactors = FALSE)
}
grid <- rbind(
  # The design the rule was developed on, at confirmation seeds.
  cell(c(shapes, "skewed"), 300, 4, reps = 40, block = "n300_items4"),
  # Larger sample: does the false-curve rate hold as power grows?
  cell(c("null", "linear", "skewed", "monotone_increasing", "diminishing", "plateau"),
       1000, 4, reps = 30, block = "n1000_items4"),
  # Measurement stress: weak loadings, careless responders, skewed indicators.
  cell(c("null", "linear", "monotone_increasing", "diminishing"), 500, 6, reps = 30,
       block = "low_reliability", loading = .55),
  cell(c("null", "linear", "monotone_increasing"), 500, 6, reps = 30,
       block = "careless", careless = .20),
  cell(c("null", "linear", "monotone_increasing"), 500, 6, reps = 30,
       block = "skew", skew = 1.20)
)
grid$skew[grid$scenario == "skewed"] <- 1.20
blocks <- env("SHAPE_BLOCKS", "")
if (nzchar(blocks)) grid <- grid[grid$block %in% trimws(strsplit(blocks, ",")[[1L]]), , drop = FALSE]

jobs <- list()
for (row in seq_len(nrow(grid))) for (replication in seq_len(grid$reps[[row]])) {
  setting <- grid[row, c("scenario", "n", "items", "loading", "careless", "skew"), drop = FALSE]
  jobs[[length(jobs) + 1L]] <- list(setting = setting, block = grid$block[[row]],
    replication = replication, seed = seed0 + length(jobs) + 1L)
}
# Stride rather than block so every shard gets a mix of cheap and expensive
# sample sizes; seeds are fixed per job, so sharding changes wall-clock only.
keep <- (((seq_along(jobs) - 1L) %% nshards) + 1L) == shard
jobs <- jobs[keep]
cat("cssem", as.character(utils::packageVersion("cssem")), "| shard", shard, "of", nshards,
    "|", length(jobs), "jobs | workers", workers, "\n")

# Self-contained by design: PSOCK workers receive this closure without the
# script's globals, so everything it needs is resolved inside it.
run_job <- function(job) tryCatch({
  ns <- asNamespace("cssem")
  generated <- do.call(ns$.structural_validation_data, ns$.structural_data_args(job$setting, job$seed))
  started <- proc.time()[["elapsed"]]
  fitted <- ns$.validation_fit(generated$model, generated$data, job$seed, 3L, 8L, 16L, FALSE)
  scores <- fitted$fit$locked_scores
  association <- associate(fitted$fit, generated$structure, structural_repeats = 5L,
    seed = job$seed, shadow_scope = "temporal")
  metrics <- association$candidate_metrics
  focal <- metrics[metrics$outcome == "Quality" & metrics$predictor == "Trust" & metrics$selected, , drop = FALSE]

  # Raw evidence for offline re-scoring of any future rule.
  fold_sets <- ns$.structural_fold_sets(fitted$fit$folds, 5L, job$seed)
  candidates <- c("linear", "monotone_increasing", "monotone_decreasing", "smooth_df3", "smooth_df4")
  losses <- sapply(candidates, function(shape)
    ns$.cv_shape_candidate(scores, "Quality", c(Trust = shape), fold_sets)$fold_mse)
  spline_p <- function(x, y) vapply(3:4, function(df) {
    stats::anova(stats::lm(y ~ x), stats::lm(y ~ splines::ns(x, df = df)))[2L, "Pr(>F)"]
  }, numeric(1))

  data.frame(
    block = job$block, scenario = job$setting$scenario, replication = job$replication,
    n = job$setting$n, items = job$setting$items, loading = job$setting$loading,
    careless = job$setting$careless, skew = job$setting$skew, seed = job$seed,
    selected_shape = focal$shape[[1L]], nonlinearity_p = focal$nonlinearity_p[[1L]],
    mean_mse_improvement = focal$mean_mse_improvement[[1L]],
    selection_frequency = focal$selection_frequency[[1L]],
    reliability_trust = unname(fitted$fit$reliability[["Trust"]]),
    reliability_quality = unname(fitted$fit$reliability[["Quality"]]),
    measurement_converged = fitted$converged,
    # Per-fold losses flattened one column per candidate-by-fold, plus the
    # classical spline test on scores and on the true latent states (the
    # detection ceiling measurement error leaves behind).
    t(stats::setNames(as.numeric(losses),
      paste0("loss.", rep(candidates, each = nrow(losses)), ".", seq_len(nrow(losses))))),
    f_scores_df3 = spline_p(scores$Trust, scores$Quality)[[1L]],
    f_scores_df4 = spline_p(scores$Trust, scores$Quality)[[2L]],
    f_truth_df3 = spline_p(generated$truth$Trust, generated$truth$Quality)[[1L]],
    f_truth_df4 = spline_p(generated$truth$Trust, generated$truth$Quality)[[2L]],
    seconds = proc.time()[["elapsed"]] - started, error = NA_character_, stringsAsFactors = FALSE
  )
}, error = function(err) data.frame(block = job$block, scenario = job$setting$scenario,
  replication = job$replication, n = job$setting$n, seed = job$seed,
  error = conditionMessage(err), stringsAsFactors = FALSE))

results <- ns$.validation_map(jobs, run_job, workers)
# A failed replication must not cost the shard its successful ones, so failures
# are carried as a row with an `error` message and padded to the full schema.
complete <- names(results[[which.max(vapply(results, ncol, integer(1)))]])
out <- do.call(rbind, lapply(results, function(row) {
  row[setdiff(complete, names(row))] <- NA
  row[complete]
}))
utils::write.csv(out, file.path(outdir, sprintf("shape_shard_%02d.csv", shard)), row.names = FALSE)
cat("shard done: rows", nrow(out), "| failures", sum(!is.na(out$error)), "\n")
