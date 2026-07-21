# Combine full-factorial simulation shard artifacts (from run-sim-shard.R) into
# combined raw CSVs plus the S1/S2/S3 summary tables. Run by the CI combine job
# after all shards finish.
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

meas  <- read_all("^measurement\\.csv$")
struc <- read_all("^structural_shard_.*\\.csv$")

if (!is.null(meas)) {
  utils::write.csv(meas, file.path(outdir, "s1-measurement-raw.csv"), row.names = FALSE)
  s1 <- aggregate(cbind(recovery, downstream_rmse) ~ engine,
    data = meas[meas$status == "success", ], FUN = function(x) mean(x, na.rm = TRUE))
  s1 <- s1[order(-s1$recovery), ]
  utils::write.csv(s1, file.path(outdir, "s1-recovery-summary.csv"), row.names = FALSE)
  cat("\n-- S1 recovery by engine --\n"); print(s1, digits = 3, row.names = FALSE)
}

if (!is.null(struc)) {
  utils::write.csv(struc, file.path(outdir, "s2s3-structural-raw.csv"), row.names = FALSE)
  ok  <- struc[struc$status == "success" & is.finite(struc$structural_bias), ]
  foc <- ok[ok$outcome == "Quality" & ok$predictor == "Trust", ]

  bias_tab <- aggregate(structural_bias ~ engine + scenario + n, data = foc, FUN = mean)
  utils::write.csv(bias_tab, file.path(outdir, "s2-bias-by-scenario-n.csv"), row.names = FALSE)

  cov_tab <- aggregate(ci_covers_truth ~ engine, data = foc[!is.na(foc$ci_covers_truth), ],
    FUN = function(x) mean(x, na.rm = TRUE))
  utils::write.csv(cov_tab, file.path(outdir, "s2-ci-coverage.csv"), row.names = FALSE)
  cat("\n-- S2 CI coverage by engine --\n"); print(cov_tab, digits = 3, row.names = FALSE)

  # S3 uses ALL success rows (not the finite-bias subset): smooth-selected edges
  # carry NA structural_bias, so filtering on finite bias would drop the correct
  # nonlinear selections.
  s3src <- struc[struc$status == "success" & struc$engine == "cssem_locked" &
    struc$outcome == "Quality" & struc$predictor == "Trust", ]
  s3 <- aggregate(shape_correct ~ scenario, data = s3src[!is.na(s3src$shape_correct), ],
    FUN = mean)
  names(s3)[2] <- "shape_recovery_rate"
  utils::write.csv(s3, file.path(outdir, "s3-shape-recovery.csv"), row.names = FALSE)
  cat("\n-- S3 shape recovery by scenario --\n"); print(s3, digits = 2, row.names = FALSE)
}

cat("\nCombined outputs written to", outdir, "\n")
