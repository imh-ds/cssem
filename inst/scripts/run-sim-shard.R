# CI shard runner for the full-factorial validation simulation (S1 recovery,
# S2 disattenuation bias, S3 shape recovery). One matrix job per shard; each
# writes a CSV artifact that combine-sim-shards.R concatenates. The simulation
# is seed-deterministic, so sharding changes only wall-clock, not results.
#
# Configured via environment variables (set by the workflow):
#   SIM_PART     "structural" (default) or "measurement"
#   SIM_SHARD    1-based shard index (structural only)
#   SIM_NSHARDS  total number of structural shards
#   SIM_REPS     replications per scenario (default 25)
#   SIM_SEED     base seed (default 20260720)
#   SIM_OUTDIR   output directory (default "sim-out")

env <- function(key, default) {
  v <- Sys.getenv(key, unset = NA_character_)
  if (is.na(v) || !nzchar(v)) default else v
}
part    <- env("SIM_PART", "structural")
shard   <- as.integer(env("SIM_SHARD", "1"))
nshards <- as.integer(env("SIM_NSHARDS", "1"))
reps    <- as.integer(env("SIM_REPS", "25"))
seed    <- as.integer(env("SIM_SEED", "20260720"))
outdir  <- env("SIM_OUTDIR", "sim-out")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(cssem))
workers <- max(1L, parallel::detectCores())
cat("cssem", as.character(utils::packageVersion("cssem")), "| part =", part,
  "| reps =", reps, "| workers =", workers, "\n")

if (identical(part, "measurement")) {
  # S1 latent recovery across the screening measurement conditions.
  res <- cssem_run_comparator_validation(
    cssem_measurement_validation_manifest("screening"),
    reps = reps, seed = seed, folds = 3L, iterations = 8L, workers = workers)
  utils::write.csv(res, file.path(outdir, "measurement.csv"), row.names = FALSE)
  cat("measurement done: rows", nrow(res), "\n")
} else {
  # S2/S3 structural on the full grid capped at N <= 500, strided across shards
  # for load balance. Each shard gets a unique seed offset so draws never
  # collide across shards while staying fully reproducible.
  manifest <- cssem_structural_validation_manifest("full")
  manifest <- manifest[manifest$n <= 500L, , drop = FALSE]
  keep <- (((seq_len(nrow(manifest)) - 1L) %% nshards) + 1L) == shard
  shard_manifest <- manifest[keep, , drop = FALSE]
  cat("structural shard", shard, "of", nshards, ":", nrow(shard_manifest), "scenarios\n")
  if (nrow(shard_manifest) == 0L) {
    cat("empty shard; nothing to do\n")
  } else {
    res <- cssem_run_structural_comparator_validation(
      shard_manifest, reps = reps, seed = seed + (shard - 1L) * 10000L,
      folds = 3L, iterations = 8L, workers = workers)
    utils::write.csv(res, file.path(outdir, sprintf("structural_shard_%02d.csv", shard)),
      row.names = FALSE)
    cat("structural shard done: rows", nrow(res), "\n")
  }
}
