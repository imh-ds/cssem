# F08 registered coverage study: structural edges against population targets,
# with multi-item ordinal measurement, independently varied predictor
# reliabilities, correlated predictors, and two searched predictors.
#
# Design (fixed before any run):
#   Latent X1, X2 standard normal with correlation rho in {0, 0.4}.
#   Y = std_P(0.40 X1 + 0.30 X2 + N(0, 0.7^2)), standardized with population
#   constants, so the targets are the population standardized coefficients
#   b1 = 0.40 / sd(Y_raw) and b2 = 0.30 / sd(Y_raw), derived analytically.
#   Each construct has four five-category ordinal items with 2% MCAR
#   missingness. Item loadings: X1 in {0.55, 0.80} crossed with X2 in
#   {0.55, 0.80}; Y fixed at 0.80. Eight conditions, N = 400.
#   Structure: Y ~ X1 + X2 with both shapes searched (auto).
#
# Estimates: the latent-standardized coefficients of X1 and X2 (the corrected
# coefficient rescaled by sqrt(rho_p / rho_Y)), whose targets are b1 and b2.
# The corrected coefficient itself, whose target is b_p sqrt(rho_Y / rho_p), is
# recorded alongside.
# Intervals, both 95% percentile:
#   locked    contrast() on locked scores with selection repeated in every
#             resample (199 resamples), rescaled by the fitted reliabilities
#   pipeline  bootstrap_model(refit = "measurement"): measurement encoders,
#             reliabilities, and shape selection refitted in every resample
#             (59 resamples, first 100 replications of each condition)
#
# Gates (registered): for each condition x coefficient x interval method, the
# Wilson 95% interval for coverage must intersect [0.91, 0.99] and the absolute
# bias of the latent-standardized estimate must be below 0.05. Reported regardless of the gates: bias with Monte
# Carlo standard error, RMSE, coverage with its Wilson interval, mean width,
# the share of replications with a corrected estimate, the share in which the
# correction limiter engaged, selected-shape shares, and measurement
# convergence.
#
# Env: SIM_SHARD, SIM_NSHARDS, SIM_REPS (default 200), SIM_SEED (20261102),
#      SIM_OUTDIR (sim-out), SIM_LOCKED_B (199), SIM_PIPELINE_B (59),
#      SIM_PIPELINE_REPS (100), GITHUB_SHA.

env <- function(key, default) { v <- Sys.getenv(key); if (nzchar(v)) v else default }
shard <- as.integer(env("SIM_SHARD", "1")); nshards <- as.integer(env("SIM_NSHARDS", "1"))
reps <- as.integer(env("SIM_REPS", "200")); seed <- as.integer(env("SIM_SEED", "20261102"))
outdir <- env("SIM_OUTDIR", "sim-out")
locked_b <- as.integer(env("SIM_LOCKED_B", "199")); pipeline_b <- as.integer(env("SIM_PIPELINE_B", "59"))
pipeline_reps <- as.integer(env("SIM_PIPELINE_REPS", "100"))
commit <- env("GITHUB_SHA", NA_character_)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
if (reps >= 1000L) stop("reps must stay below the 1000-seed spacing between conditions.")
suppressPackageStartupMessages(library(cssem))
workers <- if (.Platform$OS.type == "windows") 1L else max(1L, parallel::detectCores())

conditions <- expand.grid(rho = c(0, .4), loading_x1 = c(.55, .80), loading_x2 = c(.55, .80),
  KEEP.OUT.ATTRS = FALSE)
conditions$n <- 400L
conditions$sd_y <- sqrt(.40^2 + .30^2 + 2 * .40 * .30 * conditions$rho + .7^2)
conditions$target_x1 <- .40 / conditions$sd_y
conditions$target_x2 <- .30 / conditions$sd_y

simulate <- function(cond) {
  n <- cond$n
  X1 <- stats::rnorm(n)
  X2 <- cond$rho * X1 + sqrt(1 - cond$rho^2) * stats::rnorm(n)
  Y <- (.40 * X1 + .30 * X2 + stats::rnorm(n, sd = .7)) / cond$sd_y
  items <- cbind(cssem:::.validation_items(X1, "a", cond$loading_x1, .02, items = 4L),
    cssem:::.validation_items(X2, "b", cond$loading_x2, .02, items = 4L),
    cssem:::.validation_items(Y, "c", .80, .02, items = 4L))
  items
}

measurement <- specify_measurement(X1 = ordinal(paste0("a", 1:4)), X2 = ordinal(paste0("b", 1:4)),
  Y = ordinal(paste0("c", 1:4)), folds = 3L)
structure_spec <- specify_structure(Y ~ X1 + X2, order = c("X1", "X2", "Y"))
spec <- contrast_spec(list(x1 = "edge:Y~X1:corrected", x2 = "edge:Y~X2:corrected"))

edge_values <- function(association) {
  table <- effect_ledger(association)
  pick <- function(p, column) {
    row <- table[table$outcome == "Y" & table$predictor == p, , drop = FALSE]
    if (nrow(row) == 1L && column %in% names(row)) row[[column]][[1L]] else NA
  }
  both <- function(column) c(x1 = pick("X1", column), x2 = pick("X2", column))
  list(latent = both("latent_standardized_estimate"), corrected = both("corrected_estimate"),
    naive = both("naive_estimate"), stable = both("eiv_stable"),
    scale = sqrt(both("predictor_reliability") / both("outcome_reliability")),
    reliability = both("predictor_reliability"), outcome_reliability = pick("X1", "outcome_reliability"),
    shape = c(x1 = unname(association$full_models$Y$shapes[["X1"]]), x2 = unname(association$full_models$Y$shapes[["X2"]])))
}

run_one <- function(job) {
  cond <- conditions[job$condition, ]
  rep_seed <- seed + (job$condition - 1L) * 1000L + job$replication
  set.seed(rep_seed)
  data <- simulate(cond)
  started <- proc.time()[["elapsed"]]
  out <- tryCatch({
    vf <- cssem:::.validation_fit(measurement, data, rep_seed, 3L, 15L, 30L, FALSE)
    fit <- vf$fit
    association <- associate(fit, structure_spec, structural_repeats = 3L, seed = rep_seed, shadow_scope = "temporal")
    point <- edge_values(association)
    locked <- tryCatch(contrast(association, spec, reps = locked_b, seed = rep_seed, selection = "repeat"),
      error = identity)
    pipeline <- if (job$replication <= pipeline_reps) tryCatch(bootstrap_model(fit, function(ctx) {
      a <- associate(ctx$fit, structure_spec, structural_repeats = 3L, seed = rep_seed, shadow_scope = "temporal")
      edge_values(a)$latent
    }, reps = pipeline_b, seed = rep_seed, refit = "measurement"), error = identity) else NULL
    interval <- function(obj, name) {
      if (is.null(obj) || inherits(obj, "error")) return(c(NA_real_, NA_real_))
      if (inherits(obj, "cssem_bootstrap")) {
        d <- obj$draws[, name]; d <- d[is.finite(d)]
        if (length(d) < .8 * pipeline_b) return(c(NA_real_, NA_real_))
        return(stats::quantile(d, c(.025, .975), names = FALSE))
      }
      row <- obj$intervals[obj$intervals$contrast == name, , drop = FALSE]
      if (nrow(row) == 1L) c(row$ci_low[[1L]], row$ci_high[[1L]]) else c(NA_real_, NA_real_)
    }
    do.call(rbind, lapply(c("x1", "x2"), function(k) {
      li <- interval(locked, k) * point$scale[[k]]; pi <- interval(pipeline, k)
      data.frame(coefficient = k, target = cond[[paste0("target_", k)]], estimate = point$latent[[k]],
        corrected = point$corrected[[k]], naive = point$naive[[k]], eiv_stable = point$stable[[k]],
        reliability = point$reliability[[k]], outcome_reliability = point$outcome_reliability, shape = point$shape[[k]],
        locked_low = li[[1]], locked_high = li[[2]], pipeline_low = pi[[1]], pipeline_high = pi[[2]],
        locked_error = if (inherits(locked, "error")) conditionMessage(locked) else "",
        pipeline_error = if (inherits(pipeline, "error")) conditionMessage(pipeline) else "",
        pipeline_success = if (is.null(pipeline) || inherits(pipeline, "error")) 0L else sum(pipeline$replicates$status == "success"),
        converged = vf$converged, fit_attempts = vf$attempts, status = "success", stringsAsFactors = FALSE)
    }))
  }, error = function(e) data.frame(coefficient = c("x1", "x2"), target = c(cond$target_x1, cond$target_x2),
    estimate = NA_real_, corrected = NA_real_, naive = NA_real_, eiv_stable = NA, reliability = NA_real_,
    outcome_reliability = NA_real_, shape = NA_character_, locked_low = NA_real_,
    locked_high = NA_real_, pipeline_low = NA_real_, pipeline_high = NA_real_, locked_error = "",
    pipeline_error = "", pipeline_success = 0L, converged = NA, fit_attempts = NA_integer_,
    status = paste("error:", conditionMessage(e)), stringsAsFactors = FALSE))
  cbind(condition = job$condition, replication = job$replication, seed = rep_seed, cond[, c("rho", "loading_x1", "loading_x2", "n")],
    out, runtime_seconds = proc.time()[["elapsed"]] - started, row.names = NULL)
}

jobs <- expand.grid(replication = seq_len(reps), condition = seq_len(nrow(conditions)))
jobs <- jobs[(((seq_len(nrow(jobs)) - 1L) %% nshards) + 1L) == shard, , drop = FALSE]
cat("cssem", as.character(utils::packageVersion("cssem")), "| F08 shard", shard, "of", nshards,
  "| jobs", nrow(jobs), "| workers", workers, "\n")
results <- parallel::mclapply(split(jobs, seq_len(nrow(jobs))), run_one, mc.cores = workers, mc.preschedule = FALSE)
crashed <- vapply(results, function(x) !is.data.frame(x), logical(1))
res <- do.call(rbind, results[!crashed])
if (!is.null(res)) {
  res$package_version <- as.character(utils::packageVersion("cssem"))
  res$source_commit <- commit
  res$r_version <- paste(R.version$major, R.version$minor, sep = ".")
  utils::write.csv(res, file.path(outdir, sprintf("f08_shard_%02d.csv", shard)), row.names = FALSE)
}
writeLines(c(sprintf("shard %d: %d jobs, %d crashed", shard, nrow(jobs), sum(crashed)),
  vapply(results[crashed], function(x) paste(as.character(x), collapse = " "), character(1))),
  file.path(outdir, sprintf("f08_shard_%02d.log", shard)))
