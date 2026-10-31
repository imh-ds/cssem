# CI shard runner for the mediation (Study 5) and moderated-mediation (Study 6)
# comparator simulations. One matrix job per shard; each writes CSV artifacts
# that combine-mediation-shards.R concatenates.
#
# Work is sharded by replication rather than by cell, because a moderated
# replication costs roughly twenty times a mediation one. Each replication's
# seed is derived from its cell's position in the UNSHARDED manifest and its
# replication number, so results do not depend on the shard count: replication
# r of cell i uses seed base + (i - 1) * 1000 + r, exactly as a single call
# with `reps` replications at seed base + (i - 1) * 1000 would.
#
# Configured via environment variables (set by the workflow):
#   SIM_SHARD    1-based shard index
#   SIM_NSHARDS  total number of shards
#   SIM_REPS     replications per cell (default 50)
#   SIM_SEED     base seed (default 20261101); moderated cells add 500000
#   SIM_OUTDIR   output directory (default "sim-out")
#   SIM_ITERATIONS, SIM_MAX_ITERATIONS  encoder budget and retry (15, 30)
#   SIM_BOOTSTRAP  bootstrap resamples for every interval (default 200)
#   GITHUB_SHA   source commit, recorded on every row (set by Actions)

env <- function(key, default) {
  v <- Sys.getenv(key, unset = NA_character_)
  if (is.na(v) || !nzchar(v)) default else v
}
shard   <- as.integer(env("SIM_SHARD", "1"))
nshards <- as.integer(env("SIM_NSHARDS", "1"))
reps    <- as.integer(env("SIM_REPS", "50"))
seed    <- as.integer(env("SIM_SEED", "20261101"))
outdir  <- env("SIM_OUTDIR", "sim-out")
iterations     <- as.integer(env("SIM_ITERATIONS", "15"))
max_iterations <- as.integer(env("SIM_MAX_ITERATIONS", "30"))
bootstrap <- as.integer(env("SIM_BOOTSTRAP", "200"))
commit  <- env("GITHUB_SHA", NA_character_)
r_version <- paste(R.version$major, R.version$minor, sep = ".")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
if (reps >= 1000L) stop("reps must stay below the 1000-seed spacing between cells.")

suppressPackageStartupMessages(library(cssem))
# mclapply cannot fork on Windows; local smoke runs there go serially.
workers <- if (.Platform$OS.type == "windows") 1L else max(1L, parallel::detectCores())

studies <- list(
  mediation = list(manifest = indirect_effect_manifest("benchmark"), seed = seed,
    run = function(row, s) validate_indirect_effect_comparator(row, reps = 1L, seed = s, folds = 3L,
      iterations = iterations, max_iterations = max_iterations, structural_repeats = 3L,
      eiv_bootstrap = bootstrap, seminr_bootstrap = bootstrap)),
  moderated = list(manifest = conditional_indirect_effect_manifest("full"), seed = seed + 500000L,
    run = function(row, s) validate_conditional_indirect_effect_comparator(row, reps = 1L, seed = s, folds = 3L,
      iterations = iterations, max_iterations = max_iterations, structural_repeats = 3L,
      eiv_bootstrap = bootstrap, seminr_bootstrap = bootstrap))
)

jobs <- do.call(rbind, lapply(names(studies), function(study) {
  cells <- seq_len(nrow(studies[[study]]$manifest))
  expand.grid(replication = seq_len(reps), cell = cells, study = study, stringsAsFactors = FALSE)[, c("study", "cell", "replication")]
}))
jobs <- jobs[(((seq_len(nrow(jobs)) - 1L) %% nshards) + 1L) == shard, , drop = FALSE]
cat("cssem", as.character(utils::packageVersion("cssem")), "| shard", shard, "of", nshards,
  "| jobs =", nrow(jobs), "| reps =", reps, "| workers =", workers, "\n")

run_job <- function(k) {
  job <- jobs[k, ]
  study <- studies[[job$study]]
  cell_seed <- study$seed + (job$cell - 1L) * 1000L
  # A one-row manifest with reps = 1 runs at seed s + 1, so s = cell_seed + r - 1.
  out <- tryCatch(study$run(study$manifest[job$cell, , drop = FALSE], cell_seed + job$replication - 1L),
    error = function(e) conditionMessage(e))
  if (is.character(out)) return(list(study = job$study, failure = sprintf("%s:cell=%d:rep=%d:%s",
    job$study, job$cell, job$replication, gsub("[\r\n]+", " ", out))))
  out$replication <- job$replication
  out$cell_index <- job$cell
  out$cell_seed <- cell_seed
  list(study = job$study, result = out)
}

results <- if (nrow(jobs)) parallel::mclapply(seq_len(nrow(jobs)), run_job,
  mc.cores = workers, mc.preschedule = FALSE) else list()
# A forked worker that dies returns a try-error rather than a list.
crashed <- vapply(results, function(x) !is.list(x), logical(1))
failures <- c(vapply(results[!crashed], function(x) if (is.null(x$failure)) NA_character_ else x$failure, character(1)),
  sprintf("worker crash:%s", vapply(results[crashed], as.character, character(1))))
failures <- failures[!is.na(failures)]

for (study in names(studies)) {
  parts <- lapply(Filter(function(x) is.list(x) && identical(x$study, study) && !is.null(x$result), results), `[[`, "result")
  if (!length(parts)) next
  res <- do.call(rbind, parts)
  res$package_version <- as.character(utils::packageVersion("cssem"))
  res$source_commit <- commit
  res$r_version <- r_version
  utils::write.csv(res, file.path(outdir, sprintf("%s_shard_%02d.csv", study, shard)), row.names = FALSE)
  cat(study, "rows:", nrow(res), "\n")
}
if (length(failures)) {
  writeLines(failures, file.path(outdir, sprintf("failed_%02d.txt", shard)))
  cat("failed jobs:", length(failures), "\n")
}
# Upload needs at least one file even when a shard has nothing to report.
writeLines(sprintf("shard %d: %d jobs, %d failed", shard, nrow(jobs), length(failures)),
  file.path(outdir, sprintf("shard_%02d.log", shard)))
