# Regenerate the numbers in the JSS paper's simulation tables from the combined
# CI artifacts, so every reported value traces to a script and an artifact.
#
# Usage: Rscript inst/scripts/make-jss-tables.R <structural_dir> <mediation_dir>
#        [<f08_dir>] [<shape_dir>] [<out_dir>]
#   structural_dir  combined output of full-factorial-sim.yaml (Studies 1-4)
#   mediation_dir   combined output of mediation-sim.yaml (Studies 5-6)
#   f08_dir         combined output of f08-coverage.yaml (Section 7.4.2)
#   shape_dir       combined output of shape-selection-bench.yaml (Table 6)
# Writes one CSV per table to out_dir (default "jss-tables"), plus a SHA-256
# manifest of every input file it read.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Usage: make-jss-tables.R <structural_dir> <mediation_dir> [f08_dir] [shape_dir] [out_dir]")
structural_dir <- args[[1]]; mediation_dir <- args[[2]]
f08_dir <- if (length(args) >= 3L && nzchar(args[[3]])) args[[3]] else NA
shape_dir <- if (length(args) >= 4L && nzchar(args[[4]])) args[[4]] else NA
out_dir <- if (length(args) >= 5L) args[[5]] else "jss-tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
inputs <- character()
read <- function(dir, file) {
  path <- file.path(dir, file); inputs <<- c(inputs, path)
  utils::read.csv(path, stringsAsFactors = FALSE)
}
write <- function(x, name) utils::write.csv(x, file.path(out_dir, name), row.names = FALSE)

# Table 1: construct-score recovery.
write(read(structural_dir, "s1-recovery-summary.csv"), "table1-recovery.csv")

# Table 2: RMSD by linear-truth scenario and N; Table 3: naive vs corrected.
s2 <- read(structural_dir, "s2-deviation-by-scenario-n.csv")
engines <- c("cssem_locked", "lavaan_sam", "lavaan_sam_ordinal", "lavaan_native", "lavaan_dwls", "seminr_pls", "composite_proxy")
t2 <- stats::reshape(s2[s2$engine %in% engines, c("engine", "scenario", "n", "rmsd")],
  idvar = c("scenario", "n"), timevar = "engine", direction = "wide")
write(t2[order(t2$scenario, t2$n), ], "table2-rmsd.csv")
raw <- read(structural_dir, "s2s3-structural-raw.csv")
focal <- raw[raw$status == "success" & raw$outcome == "Quality" & raw$predictor == "Trust", ]
linear_truth <- c("linear", "downstream", "low_reliability", "omitted", "careless", "skewed", "null", "interaction")
cs <- focal[focal$engine == "cssem_locked" & focal$scenario %in% linear_truth, ]
write(stats::aggregate(cbind(truth_slope, naive_estimate, corrected_estimate, predictor_reliability) ~ scenario + n,
  cs, mean), "table3-naive-corrected.csv")

# Table 4: containment and width, linear-truth scenarios.
write(read(structural_dir, "s2-interval-containment.csv"), "table4-containment.csv")
write(read(structural_dir, "s2-interval-containment-linear-truth-pooled.csv"), "table4-containment-pooled.csv")

# Table 5: CS-SEM family distribution; Table 5b: rates by score source.
cssem <- focal[focal$engine == "cssem_locked", ]
cssem$family <- ifelse(grepl("^smooth", cssem$selected_shape), "smooth", cssem$selected_shape)
write(as.data.frame.matrix(round(prop.table(table(paste(cssem$scenario, cssem$n), cssem$family), 1), 3)), "table5-families.csv")
s3 <- read(structural_dir, "s3-shape-by-engine-scenario-n.csv")
s3$hits <- s3$rate * s3$rows
pooled <- stats::aggregate(cbind(hits, rows) ~ engine + scenario + criterion, s3, sum)
pooled$rate <- pooled$hits / pooled$rows
write(pooled[order(pooled$criterion, pooled$scenario, pooled$engine), ], "table5b-score-source.csv")

# Tables 7-8: mediation and moderated mediation against population targets.
summarise_cells <- function(raw, estimate, truth) {
  raw <- raw[raw$status == "success" & is.finite(raw[[estimate]]), ]
  raw$dev <- raw[[estimate]] - raw[[truth]]
  raw$iv <- is.finite(raw$ci_low) & is.finite(raw$ci_high)
  do.call(rbind, lapply(split(raw, list(raw$engine, raw$scenario, raw$loading), drop = TRUE), function(x) data.frame(
    engine = x$engine[1], scenario = x$scenario[1], loading = x$loading[1], estimates = nrow(x),
    target = mean(x[[truth]]), mean_estimate = mean(x[[estimate]]), mean_abs_deviation = mean(abs(x$dev)),
    mad_mcse = stats::sd(abs(x$dev)) / sqrt(nrow(x)), containment = if (any(x$iv)) mean(x$covers_truth[x$iv] %in% TRUE) else NA,
    intervals = sum(x$iv))))
}
write(summarise_cells(read(mediation_dir, "mediation-raw.csv"), "indirect", "true_indirect"), "table7-mediation.csv")
write(summarise_cells(read(mediation_dir, "moderated-raw.csv"), "index", "true_index"), "table8-moderated.csv")

# Table 9: release gates, evaluated on the Studies 1-4 cssem rows.
if (requireNamespace("cssem", quietly = TRUE)) {
  m <- read(structural_dir, "s1-measurement-raw.csv")
  keys <- c("scenario", "replication", "n", "loading", "missing", "local_dependence", "cross_loading", "overlap", "sparse")
  base <- unique(m[m$engine == "cssem_locked", c(keys, "converged")])
  recovery <- function(e) { x <- m[m$engine == e, c(keys, "recovery")]; names(x)[ncol(x)] <- paste0(e, "_recovery"); x }
  w <- Reduce(function(a, b) merge(a, b, by = keys), list(base, recovery("cssem_locked"),
    recovery("ordinal_factor_proxy"), recovery("composite_proxy")))
  names(w)[names(w) == "cssem_locked_recovery"] <- "cssem_recovery"
  write(cssem::validation_report(w, focal[focal$engine == "cssem_locked", ])$gates, "table9-release-gates.csv")
}

# Section 7.4.2 (F08) and Table 6 (shape confirmation), when supplied.
if (!is.na(f08_dir)) write(read(f08_dir, "f08-summary.csv"), "table-f08-coverage.csv")
if (!is.na(shape_dir)) write(read(shape_dir, "shape-confirmation-counts.csv"), "table6-shape-counts.csv")

hash <- vapply(unique(inputs), function(p) {
  out <- tryCatch(system2("sha256sum", shQuote(p), stdout = TRUE), error = function(e) NA_character_, warning = function(w) NA_character_)
  if (length(out) && !is.na(out[[1]])) strsplit(out[[1]], " ")[[1]][[1]] else tools::md5sum(p)[[1]]
}, character(1))
write(data.frame(file = unique(inputs), digest = unname(hash), bytes = file.size(unique(inputs))), "input-manifest.csv")
cat("Wrote", length(list.files(out_dir)), "files to", out_dir, "\n")
