# Combine full-factorial simulation shard artifacts (from run-sim-shard.R) into
# combined raw CSVs plus the S1/S2/S3 summary tables. Run by the CI combine job
# after all shards finish.
#
# Summaries are reported per scenario, never pooled across linear and nonlinear
# truths: a linear interval scored against the linear projection of a curved
# truth is a different estimand, and CS-SEM reports no interval when it selects
# a smooth shape, so pooled coverage compares different estimands on different
# subsets. Every table carries its denominator.
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
mcse_mean <- function(x) stats::sd(x) / sqrt(length(x))

# Focal-edge truth classes. `interaction` generates its product in the Loyalty
# equation only, so the focal Trust -> Quality edge is linear there (a negative
# control, not interaction detection).
linear_truth <- c("linear", "downstream", "low_reliability", "omitted", "careless",
  "skewed", "null", "interaction")
# Shapes for which more than one nonlinear family is a reasonable approximation;
# their rate is nonlinearity detection, not family recovery.
any_nonlinear <- c("plateau", "threshold", "diminishing")

meas  <- read_all("^measurement\\.csv$")
struc <- read_all("^structural_shard_.*\\.csv$")
dropped <- unlist(lapply(list.files(indir, pattern = "^structural_dropped_.*\\.txt$",
  recursive = TRUE, full.names = TRUE), readLines))

if (!is.null(meas)) {
  write_tab(meas, "s1-measurement-raw.csv")
  ok <- meas[meas$status == "success", ]
  s1 <- do.call(rbind, lapply(split(ok, ok$engine), function(d) data.frame(
    engine = d$engine[[1L]], fits = sum(meas$engine == d$engine[[1L]]), completed = nrow(d),
    converged = sum(d$converged %in% TRUE),
    recovery = mean(d$recovery, na.rm = TRUE), recovery_mcse = mcse_mean(d$recovery[is.finite(d$recovery)]),
    recovery_converged = if (any(d$converged %in% TRUE)) mean(d$recovery[d$converged %in% TRUE], na.rm = TRUE) else NA_real_,
    downstream_rmse = mean(d$downstream_rmse, na.rm = TRUE))))
  s1 <- s1[order(-s1$recovery), ]
  write_tab(s1, "s1-recovery-summary.csv")
  cat("\n-- S1 recovery by engine --\n"); print(s1, digits = 3, row.names = FALSE)
}

if (!is.null(struc)) {
  write_tab(struc, "s2s3-structural-raw.csv")

  # Integrity: one row per engine x scenario cell x replication x edge.
  key <- c("engine", "scenario", "n", "items", "loading", "careless", "skew",
    "replication", "outcome", "predictor")
  key <- intersect(key, names(struc))
  dup <- duplicated(struc[key])
  integrity <- data.frame(
    rows = nrow(struc), duplicate_keys = sum(dup),
    commits = paste(unique(stats::na.omit(struc$source_commit)), collapse = ";"),
    r_versions = paste(unique(stats::na.omit(struc$r_version)), collapse = ";"),
    cssem_versions = paste(unique(struc$package_version[struc$engine == "cssem_locked"]), collapse = ";"),
    dropped_replications = paste(dropped, collapse = " | "))
  write_tab(integrity, "integrity.csv")
  cat("\n-- integrity --\n"); print(integrity, row.names = FALSE)
  if (any(dup)) stop("Duplicate simulation keys found; refusing to summarise.", call. = FALSE)

  # Per-engine availability, including failed and unavailable rows.
  status_tab <- as.data.frame(table(engine = struc$engine, status = struc$status), stringsAsFactors = FALSE)
  write_tab(status_tab[status_tab$Freq > 0, ], "engine-status.csv")

  ok  <- struc[struc$status == "success", ]
  foc <- ok[ok$outcome == "Quality" & ok$predictor == "Trust", ]
  cell <- function(d) paste(d$scenario, d$n, sep = "|")

  # S2: focal-edge deviation on linear-truth scenarios, per scenario x N x engine.
  lin <- foc[foc$scenario %in% linear_truth, ]
  lin$dev <- lin$headline_estimate - lin$truth_slope
  s2 <- do.call(rbind, lapply(split(lin, list(lin$engine, lin$scenario, lin$n), drop = TRUE), function(d) {
    e <- d$dev[is.finite(d$dev)]
    data.frame(engine = d$engine[[1L]], scenario = d$scenario[[1L]], n = d$n[[1L]],
      rows = nrow(d), estimates = length(e),
      mean_deviation = if (length(e)) mean(e) else NA_real_,
      mean_abs_deviation = if (length(e)) mean(abs(e)) else NA_real_,
      mad_mcse = if (length(e) > 1L) mcse_mean(abs(e)) else NA_real_,
      rmsd = if (length(e)) sqrt(mean(e^2)) else NA_real_,
      corrected_share = if (d$engine[[1L]] == "cssem_locked") mean(d$eiv_applicable %in% TRUE) else NA_real_)
  }))
  write_tab(s2[order(s2$scenario, s2$n, s2$mean_abs_deviation), ], "s2-deviation-by-scenario-n.csv")

  # S2b: interval containment and width on linear-truth scenarios only.
  iv <- lin[is.finite(lin$corrected_ci_low) & is.finite(lin$corrected_ci_high), ]
  iv$width <- iv$corrected_ci_high - iv$corrected_ci_low
  s2b <- do.call(rbind, lapply(split(iv, list(iv$engine, iv$scenario, iv$n), drop = TRUE), function(d) {
    k <- sum(d$ci_covers_truth %in% TRUE); m <- nrow(d)
    data.frame(engine = d$engine[[1L]], scenario = d$scenario[[1L]], n = d$n[[1L]],
      intervals = m, containment = k / m, containment_mcse = sqrt((k / m) * (1 - k / m) / m),
      mean_width = mean(d$width))
  }))
  write_tab(s2b[order(s2b$scenario, s2b$n, s2b$engine), ], "s2-interval-containment.csv")
  pooled <- do.call(rbind, lapply(split(iv, iv$engine), function(d) data.frame(
    engine = d$engine[[1L]], intervals = nrow(d), containment = mean(d$ci_covers_truth %in% TRUE),
    mean_width = mean(d$width))))
  write_tab(pooled, "s2-interval-containment-linear-truth-pooled.csv")
  cat("\n-- S2b containment, linear-truth scenarios only --\n"); print(pooled, digits = 3, row.names = FALSE)

  # S3: selected family by scenario x N for every engine that runs the CS-SEM
  # selector (score-swap engines), so score sources are compared under an
  # identical selector. Native engines are linear by construction and omitted.
  swap <- foc[!is.na(foc$selected_shape) &
    foc$engine %in% c("cssem_locked", "lavaan_dwls", "composite_proxy", "ordinal_factor_proxy", "seminr_pls"), ]
  swap$family <- ifelse(grepl("^smooth", swap$selected_shape), "smooth", swap$selected_shape)
  fam <- as.data.frame(table(engine = swap$engine, scenario = swap$scenario, n = swap$n,
    family = swap$family), stringsAsFactors = FALSE)
  tot <- stats::aggregate(Freq ~ engine + scenario + n, data = fam, FUN = sum)
  names(tot)[4] <- "rows"
  fam <- merge(fam, tot)
  fam <- fam[fam$rows > 0, ]
  fam$share <- fam$Freq / fam$rows
  write_tab(fam[order(fam$engine, fam$scenario, fam$n, fam$family), ], "s3-family-by-engine-scenario-n.csv")

  sc <- swap[!is.na(swap$shape_correct), ]
  s3 <- do.call(rbind, lapply(split(sc, list(sc$engine, sc$scenario, sc$n), drop = TRUE), function(d) {
    p <- mean(d$shape_correct); m <- nrow(d)
    data.frame(engine = d$engine[[1L]], scenario = d$scenario[[1L]], n = d$n[[1L]], rows = m,
      criterion = if (d$scenario[[1L]] %in% linear_truth) "selects_linear"
        else if (d$scenario[[1L]] %in% any_nonlinear) "any_nonlinear_family" else "exact_family",
      rate = p, rate_mcse = sqrt(p * (1 - p) / m))
  }))
  write_tab(s3[order(s3$scenario, s3$n, s3$engine), ], "s3-shape-by-engine-scenario-n.csv")
  cat("\n-- S3 CS-SEM shape rates --\n")
  print(s3[s3$engine == "cssem_locked", c("scenario", "n", "rows", "criterion", "rate")], digits = 2, row.names = FALSE)
}

cat("\nCombined outputs written to", outdir, "\n")
