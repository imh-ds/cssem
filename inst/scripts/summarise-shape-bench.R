# Summarise the sharded shape-selection confirmation benchmark.
#
#   SHAPE_INDIR   directory holding shard CSVs (or artifact subdirectories)
#   SHAPE_OUTDIR  where the combined CSV is written
#
# Prints, per block, the share of replications reporting each shape family, and
# the detection ceiling the same curvature test reaches on the true latent
# states. Every input column is preserved in the combined CSV so a different
# acceptance rule can be scored from it without refitting anything.

env <- function(key, default) {
  value <- Sys.getenv(key, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}
indir <- env("SHAPE_INDIR", "shape-artifacts")
outdir <- env("SHAPE_OUTDIR", "shape-combined")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(indir, pattern = "^shape_shard_.*\\.csv$", full.names = TRUE, recursive = TRUE)
if (!length(files)) stop("No shard CSVs found under ", indir, call. = FALSE)
parts <- lapply(files, utils::read.csv, stringsAsFactors = FALSE)
columns <- names(parts[[which.max(vapply(parts, ncol, integer(1)))]])
res <- do.call(rbind, lapply(parts, function(part) {
  part[setdiff(columns, names(part))] <- NA
  part[columns]
}))
res <- res[order(res$block, res$scenario, res$replication), ]
utils::write.csv(res, file.path(outdir, "shape-confirmation.csv"), row.names = FALSE)

failed <- !is.na(res$error)
cat("replications:", nrow(res), "| failures:", sum(failed),
    "| measurement converged:", round(mean(res$measurement_converged[!failed]), 3), "\n")
if (any(failed)) print(utils::head(unique(res$error[failed]), 5L))

ok <- res[!failed, , drop = FALSE]
ok$family <- ifelse(ok$selected_shape == "linear", "linear",
  ifelse(grepl("^smooth", ok$selected_shape), "smooth", ok$selected_shape))
levels_family <- c("linear", "monotone_increasing", "monotone_decreasing", "smooth")
for (block in unique(ok$block)) {
  rows <- ok[ok$block == block, , drop = FALSE]
  cat("\n=====", block, "| reps", length(unique(rows$replication)), "=====\n")
  print(round(prop.table(table(rows$scenario, factor(rows$family, levels = levels_family)), 1), 2))
  # What the same test achieves on the true states: the gap is measurement
  # error, not the selection rule.
  ceiling <- tapply(pmin(rows$f_truth_df3, rows$f_truth_df4) * 2 < .05, rows$scenario, mean)
  cat("detection ceiling on true states:", paste(names(ceiling), round(ceiling, 2), sep = "=", collapse = "  "), "\n")
}

# Exact counts (F09): k of n replications per block, scenario, and family, for
# the selector on scores and the identical selector on the true latent states,
# with convergence and refit counts, so every reported rate can be reconciled.
if ("latent_selected_shape" %in% names(ok)) {
  ok$latent_family <- ifelse(ok$latent_selected_shape == "linear", "linear",
    ifelse(grepl("^smooth", ok$latent_selected_shape), "smooth", ok$latent_selected_shape))
}
cells <- split(ok, list(ok$block, ok$scenario), drop = TRUE)
counts <- do.call(rbind, lapply(cells, function(d) {
  n <- nrow(d)
  row <- data.frame(block = d$block[[1L]], scenario = d$scenario[[1L]], n = n,
    converged = sum(d$measurement_converged %in% TRUE),
    refits = if ("fit_attempts" %in% names(d)) sum(d$fit_attempts > 1L, na.rm = TRUE) else NA_integer_,
    scores_curve = sum(d$family != "linear"))
  for (f in levels_family) row[[paste0("scores_", f)]] <- sum(d$family == f)
  if ("latent_family" %in% names(d)) {
    row$latent_curve <- sum(d$latent_family != "linear")
    for (f in levels_family) row[[paste0("latent_", f)]] <- sum(d$latent_family == f)
  }
  row
}))
counts <- counts[order(counts$block, counts$scenario), ]
utils::write.csv(counts, file.path(outdir, "shape-confirmation-counts.csv"), row.names = FALSE)
cat("\n----- exact counts (curve = any nonlinear family) -----\n")
print(counts[, intersect(c("block", "scenario", "n", "converged", "refits", "scores_curve", "latent_curve"),
  names(counts))], row.names = FALSE)
cat("\nnull/linear/skewed replications:", sum(ok$scenario %in% c("null", "linear", "skewed")),
  "| null+linear:", sum(ok$scenario %in% c("null", "linear")), "\n")
