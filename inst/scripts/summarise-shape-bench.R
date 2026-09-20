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
