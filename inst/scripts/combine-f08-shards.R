# Combine F08 coverage-study shards (run-f08-shard.R) and evaluate the
# registered gates. Env: SIM_INDIR (sim-artifacts), SIM_OUTDIR (sim-combined).

indir <- Sys.getenv("SIM_INDIR", "sim-artifacts"); outdir <- Sys.getenv("SIM_OUTDIR", "sim-combined")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
files <- list.files(indir, pattern = "^f08_shard_.*\\.csv$", recursive = TRUE, full.names = TRUE)
if (!length(files)) stop("No F08 shard files found.", call. = FALSE)
raw <- do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))
utils::write.csv(raw, file.path(outdir, "f08-raw.csv"), row.names = FALSE)
logs <- unlist(lapply(list.files(indir, pattern = "^f08_shard_.*\\.log$", recursive = TRUE, full.names = TRUE), readLines))

dup <- duplicated(raw[c("condition", "replication", "coefficient")])
integrity <- data.frame(rows = nrow(raw), duplicate_keys = sum(dup),
  replications = nrow(unique(raw[c("condition", "replication")])),
  failed = sum(raw$status != "success") / 2,
  commits = paste(unique(stats::na.omit(raw$source_commit)), collapse = ";"),
  r_versions = paste(unique(raw$r_version), collapse = ";"),
  cssem_versions = paste(unique(raw$package_version), collapse = ";"),
  crashed_jobs = sum(grepl("crashed", logs) & !grepl(" 0 crashed", logs)))
utils::write.csv(integrity, file.path(outdir, "f08-integrity.csv"), row.names = FALSE)
print(integrity, row.names = FALSE)
if (any(dup)) stop("Duplicate keys; refusing to summarise.", call. = FALSE)

wilson <- function(k, m, z = 1.96) {
  if (!m) return(c(NA_real_, NA_real_))
  p <- k / m; centre <- (p + z^2 / (2 * m)) / (1 + z^2 / m)
  half <- z * sqrt(p * (1 - p) / m + z^2 / (4 * m^2)) / (1 + z^2 / m)
  c(centre - half, centre + half)
}
cells <- split(raw, list(raw$condition, raw$coefficient), drop = TRUE)
summary <- do.call(rbind, lapply(cells, function(d) {
  ok <- d$status == "success" & is.finite(d$estimate)
  dev <- d$estimate[ok] - d$target[ok]
  method <- function(prefix) {
    lo <- d[[paste0(prefix, "_low")]]; hi <- d[[paste0(prefix, "_high")]]
    iv <- ok & is.finite(lo) & is.finite(hi)
    k <- sum(lo[iv] <= d$target[iv] & hi[iv] >= d$target[iv]); m <- sum(iv); w <- wilson(k, m)
    c(intervals = m, coverage = if (m) k / m else NA_real_, wilson_low = w[[1]], wilson_high = w[[2]],
      mean_width = if (m) mean(hi[iv] - lo[iv]) else NA_real_)
  }
  locked <- method("locked"); pipeline <- method("pipeline")
  gate_bias <- length(dev) > 0 && abs(mean(dev)) < .05
  gate <- function(x) is.finite(x[["wilson_low"]]) && x[["wilson_low"]] <= .99 && x[["wilson_high"]] >= .91
  data.frame(condition = d$condition[[1L]], rho = d$rho[[1L]], loading_x1 = d$loading_x1[[1L]],
    loading_x2 = d$loading_x2[[1L]], coefficient = d$coefficient[[1L]], target = d$target[[1L]],
    replications = nrow(d), estimates = sum(ok), mean_estimate = mean(d$estimate[ok]),
    bias = mean(dev), bias_mcse = stats::sd(dev) / sqrt(length(dev)), rmse = sqrt(mean(dev^2)),
    mean_corrected = mean(d$corrected[ok]), mean_reliability = mean(d$reliability[ok]),
    mean_outcome_reliability = mean(d$outcome_reliability[ok]),
    limiter_engaged = mean(!(d$eiv_stable[ok] %in% TRUE)), linear_selected = mean(d$shape[ok] == "linear"),
    converged = mean(d$converged %in% TRUE), refits = sum(d$fit_attempts > 1L, na.rm = TRUE),
    locked_intervals = locked[["intervals"]], locked_coverage = locked[["coverage"]],
    locked_wilson_low = locked[["wilson_low"]], locked_wilson_high = locked[["wilson_high"]],
    locked_width = locked[["mean_width"]],
    pipeline_intervals = pipeline[["intervals"]], pipeline_coverage = pipeline[["coverage"]],
    pipeline_wilson_low = pipeline[["wilson_low"]], pipeline_wilson_high = pipeline[["wilson_high"]],
    pipeline_width = pipeline[["mean_width"]],
    gate_bias = gate_bias, gate_locked = gate(locked), gate_pipeline = gate(pipeline))
}))
summary <- summary[order(summary$condition, summary$coefficient), ]
utils::write.csv(summary, file.path(outdir, "f08-summary.csv"), row.names = FALSE)
print(summary[, c("rho", "loading_x1", "loading_x2", "coefficient", "target", "bias", "rmse",
  "locked_coverage", "pipeline_coverage", "locked_width", "pipeline_width", "limiter_engaged",
  "gate_bias", "gate_locked", "gate_pipeline")], digits = 3, row.names = FALSE)
cat("\nAll gates passed:", all(summary$gate_bias & summary$gate_locked & summary$gate_pipeline), "\n")
