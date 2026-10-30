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
#   SIM_ITERATIONS, SIM_MAX_ITERATIONS  encoder budget and retry (15, 30)
#   GITHUB_SHA   source commit, recorded on every row (set by Actions)

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
# Encoder budget: the submission default (15) with a retry at 30 for structural
# fits that do not converge. The 0.4.0 run used 8 with no retry.
iterations     <- as.integer(env("SIM_ITERATIONS", "15"))
max_iterations <- as.integer(env("SIM_MAX_ITERATIONS", "30"))
commit  <- env("GITHUB_SHA", NA_character_)
r_version <- paste(R.version$major, R.version$minor, sep = ".")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(cssem))
workers <- max(1L, parallel::detectCores())
cat("cssem", as.character(utils::packageVersion("cssem")), "| part =", part,
  "| reps =", reps, "| workers =", workers, "\n")

if (identical(part, "measurement")) {
  # S1 latent recovery across the screening measurement conditions.
  res <- validate_comparator(
    measurement_manifest("screening"),
    reps = reps, seed = seed, folds = 3L, iterations = iterations, workers = workers)
  res$source_commit <- commit
  res$r_version <- r_version
  utils::write.csv(res, file.path(outdir, "measurement.csv"), row.names = FALSE)
  cat("measurement done: rows", nrow(res), "\n")
} else {
  # S2/S3 structural on the full grid capped at N <= 500, strided across shards
  # for load balance.
  #
  # Each scenario's seed is derived from its position in the UNSHARDED manifest,
  # and the scenario is run on its own, so the draws a scenario receives do not
  # depend on how many shards the run used or on which shard picked it up.
  # Offsetting by the shard index instead (the previous scheme) meant the same
  # scenario drew different data at a different NSHARDS, so a run reproduced
  # only at a fixed shard count.
  manifest <- structural_manifest("full")
  manifest <- manifest[manifest$n <= 500L, , drop = FALSE]
  index <- seq_len(nrow(manifest))
  rows <- index[(((index - 1L) %% nshards) + 1L) == shard]
  cat("structural shard", shard, "of", nshards, ":", length(rows), "scenarios\n")
  if (reps >= 1000L) stop("reps must stay below the 1000-seed spacing between scenarios.")

  # Run one scenario. The harness's parallel map aborts the whole call if any
  # replication errors (e.g. a rare singular solve in a comparator engine), so
  # on failure the scenario is rerun one replication at a time. With a
  # one-row manifest, replication r of a call with seed s uses seed s + r, so
  # the salvage call for r uses seed base + r - 1 and draws identical data. Its
  # replication label is reset to r; leaving the per-call label of 1 is what
  # produced duplicate scenario/replication keys in the 0.4.0 artifacts.
  run_scenario <- function(row, base_seed) {
    run <- function(reps_now, seed_now, workers_now) validate_structure_comparator(
      row, reps = reps_now, seed = seed_now, folds = 3L, iterations = iterations,
      max_iterations = max_iterations, workers = workers_now)
    fast <- tryCatch(run(reps, base_seed, workers), error = function(e) {
      message("  parallel run failed (", conditionMessage(e), "); salvaging per replication")
      NULL
    })
    if (!is.null(fast)) return(list(result = fast, dropped = integer()))
    per_rep <- lapply(seq_len(reps), function(r) tryCatch({
      one <- run(1L, base_seed + r - 1L, 1L)
      one$replication <- r
      one
    }, error = function(e) NULL))
    dropped <- which(vapply(per_rep, is.null, logical(1)))
    kept <- per_rep[!vapply(per_rep, is.null, logical(1))]
    list(result = if (length(kept)) do.call(rbind, kept) else NULL, dropped = dropped)
  }

  if (!length(rows)) {
    cat("empty shard; nothing to do\n")
  } else {
    parts <- list(); dropped_log <- character()
    for (i in rows) {
      out <- run_scenario(manifest[i, , drop = FALSE], seed + (i - 1L) * 1000L)
      if (length(out$dropped)) dropped_log <- c(dropped_log, sprintf("%d:%s:reps=%s", i,
        manifest$scenario[[i]], paste(out$dropped, collapse = ",")))
      if (!is.null(out$result) && nrow(out$result)) {
        out$result$scenario_index <- i
        out$result$scenario_seed <- seed + (i - 1L) * 1000L
        parts[[length(parts) + 1L]] <- out$result
      }
    }
    if (length(parts)) {
      res <- do.call(rbind, parts)
      res$source_commit <- commit
      res$r_version <- r_version
      utils::write.csv(res, file.path(outdir, sprintf("structural_shard_%02d.csv", shard)),
        row.names = FALSE)
      cat("structural shard done: rows", nrow(res), "\n")
    }
    if (length(dropped_log)) {
      writeLines(dropped_log, file.path(outdir, sprintf("structural_dropped_%02d.txt", shard)))
      cat("dropped replications:", paste(dropped_log, collapse = "; "), "\n")
    }
  }
}
