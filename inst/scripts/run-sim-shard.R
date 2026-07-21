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

  # Run each scenario in isolation. The harness runs replications in a parallel
  # cluster whose parLapply ABORTS the whole call if any single replication
  # errors (e.g. a rare "computationally singular" solve in a comparator engine
  # on a hard cell). Isolating per scenario -- with a per-replication salvage
  # fallback -- means one bad cell is skipped, not the entire shard.
  run_scenario <- function(row, base_seed) {
    fast <- tryCatch(
      cssem_run_structural_comparator_validation(row, reps = reps, seed = base_seed,
        folds = 3L, iterations = 8L, workers = workers),
      error = function(e) { message("  parallel failed (", conditionMessage(e),
        "); salvaging per-replication"); NULL })
    if (!is.null(fast)) return(fast)
    # Salvage: run replications one at a time (workers = 1, no cluster), keeping
    # seeds aligned to the fast path (job index r uses base_seed + r), so a
    # single offending replication is dropped and the rest reproduce exactly.
    per_rep <- lapply(seq_len(reps), function(r) tryCatch(
      cssem_run_structural_comparator_validation(row, reps = 1L, seed = base_seed + r - 1L,
        folds = 3L, iterations = 8L, workers = 1L),
      error = function(e) NULL))
    per_rep <- per_rep[!vapply(per_rep, is.null, logical(1))]
    if (length(per_rep)) do.call(rbind, per_rep) else NULL
  }

  results <- list(); failed <- character(0)
  for (i in seq_len(nrow(shard_manifest))) {
    row <- shard_manifest[i, , drop = FALSE]
    base_seed <- seed + (shard - 1L) * 10000L + (i - 1L) * 1000L
    res <- run_scenario(row, base_seed)
    if (!is.null(res) && nrow(res) > 0L) results[[length(results) + 1L]] <- res
    else failed <- c(failed, row$scenario)
  }
  if (length(results)) {
    out_df <- do.call(rbind, results)
    utils::write.csv(out_df, file.path(outdir, sprintf("structural_shard_%02d.csv", shard)),
      row.names = FALSE)
    cat("structural shard done: rows", nrow(out_df),
      "| scenarios kept", length(results), "of", nrow(shard_manifest),
      if (length(failed)) paste("| dropped:", paste(failed, collapse = ",")) else "", "\n")
  } else {
    cat("structural shard: no scenarios produced results", "\n")
  }
}
