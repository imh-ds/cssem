# Combine mediation and moderated-mediation shard artifacts (from
# run-mediation-shard.R) into raw CSVs and per-cell summaries. Run by the CI
# combine job after all shards finish.
#
# The primary target is the population counterfactual quantity implied by the
# generating equations (`true_indirect`, `true_index`). Deviations are also
# summarised against the independent sample-level target for reference.
# Every summary carries its denominator and Monte Carlo standard errors.
#
# Env:  SIM_INDIR  directory of downloaded artifacts (default "sim-artifacts")
#       SIM_OUTDIR output directory (default "sim-combined")

indir  <- Sys.getenv("SIM_INDIR", "sim-artifacts")
outdir <- Sys.getenv("SIM_OUTDIR", "sim-combined")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

read_all <- function(pattern) {
  files <- list.files(indir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(NULL)
  do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))
}
write_tab <- function(x, name) utils::write.csv(x, file.path(outdir, name), row.names = FALSE)
mcse_mean <- function(x) if (length(x) > 1L) stats::sd(x) / sqrt(length(x)) else NA_real_

failures <- unlist(lapply(list.files(indir, pattern = "^failed_.*\\.txt$", recursive = TRUE, full.names = TRUE), readLines))

summarise <- function(raw, estimate, truth, sample_target, label) {
  key <- c("engine", "scenario", "n", "loading", "replication")
  dup <- duplicated(raw[key])
  integrity <- data.frame(study = label, rows = nrow(raw), duplicate_keys = sum(dup),
    commits = paste(unique(stats::na.omit(raw$source_commit)), collapse = ";"),
    r_versions = paste(unique(stats::na.omit(raw$r_version)), collapse = ";"),
    cssem_versions = paste(unique(stats::na.omit(raw$package_version)), collapse = ";"),
    cssem_converged = mean(raw$measurement_converged[raw$engine == "cssem_disattenuated"] %in% TRUE),
    cssem_refits = sum(raw$fit_attempts[raw$engine == "cssem_disattenuated"] > 1L, na.rm = TRUE))
  if (any(dup)) {
    write_tab(integrity, sprintf("%s-integrity.csv", label))
    stop("Duplicate simulation keys found in ", label, "; refusing to summarise.", call. = FALSE)
  }
  status <- as.data.frame(table(engine = raw$engine, scenario = raw$scenario, status = raw$status), stringsAsFactors = FALSE)
  write_tab(status[status$Freq > 0, ], sprintf("%s-engine-status.csv", label))

  cells <- split(raw, list(raw$engine, raw$scenario, raw$loading, raw$n), drop = TRUE)
  out <- do.call(rbind, lapply(cells, function(d) {
    ok <- d$status == "success" & is.finite(d[[estimate]])
    dev <- d[[estimate]][ok] - d[[truth]][ok]
    sdev <- d[[estimate]][ok] - d[[sample_target]][ok]
    iv <- ok & is.finite(d$ci_low) & is.finite(d$ci_high)
    k <- sum(d$covers_truth[iv] %in% TRUE); m <- sum(iv)
    data.frame(engine = d$engine[[1L]], scenario = d$scenario[[1L]], loading = d$loading[[1L]], n = d$n[[1L]],
      rows = nrow(d), estimates = sum(ok), population_target = mean(d[[truth]]),
      mean_estimate = if (any(ok)) mean(d[[estimate]][ok]) else NA_real_,
      mean_deviation = if (length(dev)) mean(dev) else NA_real_,
      mean_abs_deviation = if (length(dev)) mean(abs(dev)) else NA_real_,
      mad_mcse = mcse_mean(abs(dev)), rmsd = if (length(dev)) sqrt(mean(dev^2)) else NA_real_,
      sample_target_mean_abs_deviation = if (length(sdev)) mean(abs(sdev)) else NA_real_,
      intervals = m, containment = if (m) k / m else NA_real_,
      containment_mcse = if (m) sqrt((k / m) * (1 - k / m) / m) else NA_real_,
      mean_width = if (m) mean(d$ci_high[iv] - d$ci_low[iv]) else NA_real_)
  }))
  out <- out[order(out$scenario, out$loading, out$n, out$engine), ]
  write_tab(out, sprintf("%s-summary-by-cell.csv", label))
  list(integrity = integrity, summary = out)
}

integrity <- list()
med <- read_all("^mediation_shard_.*\\.csv$")
if (!is.null(med)) {
  write_tab(med, "mediation-raw.csv")
  s <- summarise(med, "indirect", "true_indirect", "sample_target_indirect", "mediation")
  integrity[[length(integrity) + 1L]] <- s$integrity
  cat("\n-- mediation: mean |deviation| from the population indirect effect --\n")
  print(stats::xtabs(mean_abs_deviation ~ paste(scenario, loading) + engine, s$summary), digits = 3)
}
mod <- read_all("^moderated_shard_.*\\.csv$")
if (!is.null(mod)) {
  write_tab(mod, "moderated-raw.csv")
  s <- summarise(mod, "index", "true_index", "sample_target_index", "moderated")
  integrity[[length(integrity) + 1L]] <- s$integrity
  cat("\n-- moderated mediation: mean |deviation| from the population index --\n")
  print(stats::xtabs(mean_abs_deviation ~ paste(scenario, loading) + engine, s$summary), digits = 3)
}
if (length(integrity)) {
  integrity <- do.call(rbind, integrity)
  integrity$failed_jobs <- length(failures)
  write_tab(integrity, "integrity.csv")
  cat("\n-- integrity --\n"); print(integrity, row.names = FALSE)
}
if (length(failures)) writeLines(failures, file.path(outdir, "failed-jobs.txt"))
cat("\nCombined outputs written to", outdir, "\n")
