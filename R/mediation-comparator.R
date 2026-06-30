# Rigorous mediation benchmark: score the CS-SEM disattenuated indirect effect
# against native CB-SEM (lavaan latent SEM) and PLS-SEM (seminr) mediation on a
# shared task with a known latent-scale truth. CB-SEM disattenuates and should be
# near-unbiased; PLS-SEM carries measurement error into composites and attenuates;
# the CS-SEM naive estimate is included to show the same attenuation without the
# correction.

# Generic lavaan mediation syntax: a measurement model per construct, labelled
# structural paths, and a defined total indirect effect summing the product of
# labels along every directed path from x to y.
.lavaan_mediation_syntax <- function(model, structure, x, y) {
  measurement <- vapply(names(model$constructs), function(construct)
    paste(construct, "=~", paste(model$constructs[[construct]]$indicators, collapse = " + ")), character(1))
  label <- function(predictor, outcome) paste0("b_", predictor, "_", outcome)
  structural <- vapply(names(structure$effects), function(outcome) {
    predictors <- names(structure$effects[[outcome]])
    paste(outcome, "~", paste(paste0(label(predictors, outcome), "*", predictors), collapse = " + "))
  }, character(1))
  indirect_paths <- Filter(function(path) length(path) > 2L, .structure_paths(structure, x, y))
  terms <- vapply(indirect_paths, function(path)
    paste(mapply(label, path[-length(path)], path[-1L]), collapse = "*"), character(1))
  paste(c(measurement, structural, paste("ind :=", paste(terms, collapse = " + "))), collapse = "\n")
}

# Native CB-SEM indirect effect: an ordinal (WLSMV) latent SEM with a defined
# indirect parameter, read from the standardized solution with its delta-method
# interval. The standardized estimate is comparable to the standardized latent
# truth; the raw `est` (in latent-units) is not.
.mediation_lavaan <- function(generated, x, y) {
  if (!requireNamespace("lavaan", quietly = TRUE))
    return(list(available = FALSE, status = "skipped_not_installed", estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, runtime = NA_real_))
  syntax <- .lavaan_mediation_syntax(generated$model, generated$structure, x, y)
  started <- proc.time()[["elapsed"]]
  tryCatch({
    fit <- lavaan::sem(syntax, data = generated$data, ordered = names(generated$data), estimator = "WLSMV", std.lv = TRUE)
    standardized <- lavaan::standardizedSolution(fit)
    row <- standardized[standardized$op == ":=" & standardized$lhs == "ind", , drop = FALSE]
    list(available = TRUE, status = "success", estimate = row$est.std[[1L]],
      ci_low = row$ci.lower[[1L]], ci_high = row$ci.upper[[1L]], runtime = proc.time()[["elapsed"]] - started)
  }, error = function(err) list(available = TRUE, status = "error", estimate = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, runtime = proc.time()[["elapsed"]] - started))
}

# Native PLS-SEM indirect effect: the sum of path-coefficient products along the
# directed paths from x to y, with a percentile interval from a manual row
# bootstrap (estimate_pls is fast, so this avoids seminr's bootstrap API and is
# uniform across single, parallel, and serial mediation).
.mediation_seminr <- function(generated, x, y, bootstrap, seed) {
  if (!requireNamespace("seminr", quietly = TRUE))
    return(list(available = FALSE, status = "skipped_not_installed", estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, runtime = NA_real_))
  measurement_model <- do.call(seminr::constructs, lapply(names(generated$model$constructs),
    function(construct) seminr::composite(construct, generated$model$constructs[[construct]]$indicators)))
  edges <- list()
  for (outcome in names(generated$structure$effects)) for (predictor in names(generated$structure$effects[[outcome]]))
    edges[[length(edges) + 1L]] <- seminr::paths(from = predictor, to = outcome)
  structural_model <- do.call(seminr::relationships, edges)
  indirect_paths <- Filter(function(path) length(path) > 2L, .structure_paths(generated$structure, x, y))
  data <- .mean_impute_data(generated$data); n <- nrow(data)
  indirect_of <- function(rows) {
    fit <- seminr::estimate_pls(data = rows, measurement_model = measurement_model, structural_model = structural_model)
    coefficients <- fit$path_coef
    sum(vapply(indirect_paths, function(path)
      prod(mapply(function(from, to) coefficients[from, to], path[-length(path)], path[-1L])), numeric(1)))
  }
  started <- proc.time()[["elapsed"]]
  tryCatch({
    point <- suppressMessages(indirect_of(data))
    interval <- c(NA_real_, NA_real_)
    if (bootstrap > 0L) {
      set.seed(seed + 909L)
      draws <- suppressMessages(vapply(seq_len(bootstrap), function(b)
        indirect_of(data[sample.int(n, n, replace = TRUE), , drop = FALSE]), numeric(1)))
      draws <- draws[is.finite(draws)]
      if (length(draws)) interval <- stats::quantile(draws, c(.025, .975), names = FALSE)
    }
    list(available = TRUE, status = "success", estimate = point, ci_low = interval[[1L]],
      ci_high = interval[[2L]], runtime = proc.time()[["elapsed"]] - started)
  }, error = function(err) list(available = TRUE, status = "error", estimate = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, runtime = proc.time()[["elapsed"]] - started))
}

.mediation_comparator_row <- function(engine, setting, replication, truth, estimate, ci_low, ci_high, runtime, status = "success") {
  covers <- if (!is.finite(ci_low) || !is.finite(ci_high) || !is.finite(truth)) NA else truth >= ci_low && truth <= ci_high
  data.frame(engine = engine, scenario = setting$scenario, replication = replication, n = setting$n,
    loading = setting$loading, status = status, true_indirect = truth, indirect = estimate,
    abs_bias = abs(estimate - truth), ci_low = ci_low, ci_high = ci_high, covers_truth = covers,
    runtime_seconds = unname(runtime), worker_pid = Sys.getpid(), stringsAsFactors = FALSE)
}

.mediation_comparator_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  items <- if ("items" %in% names(setting)) setting$items else 4L
  edge_shape <- if ("edge_shape" %in% names(setting)) setting$edge_shape else "linear"
  generated <- .mediation_validation_data(setting$scenario, setting$n, setting$loading, job$seed, items = items, edge_shape = edge_shape)
  constructs <- names(generated$states); x <- constructs[[1L]]; y <- constructs[[length(constructs)]]
  truth <- generated$truth[["indirect_total"]]
  rows <- list()

  cssem_elapsed <- system.time({
    model <- generated$model; model$folds <- job$folds
    fit <- cssem_fit(model, generated$data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
    association <- cssem_associate(fit, generated$structure, structural_repeats = job$structural_repeats,
      seed = job$seed, shadow_scope = "temporal")
    mediation <- cssem_mediation(association, x, y, eiv_bootstrap = job$eiv_bootstrap, seed = job$seed)
  })["elapsed"]
  indirect <- mediation$summary[mediation$summary$component == "indirect_total", , drop = FALSE]
  rows[[1L]] <- .mediation_comparator_row("cssem_disattenuated", setting, job$replication, truth,
    indirect$disattenuated_effect, indirect$disattenuated_ci_low, indirect$disattenuated_ci_high, cssem_elapsed)
  rows[[2L]] <- .mediation_comparator_row("cssem_naive", setting, job$replication, truth,
    indirect$naive_effect, indirect$naive_ci_low, indirect$naive_ci_high, cssem_elapsed)

  lavaan_result <- .mediation_lavaan(generated, x, y)
  rows[[3L]] <- .mediation_comparator_row("lavaan_native", setting, job$replication, truth,
    lavaan_result$estimate, lavaan_result$ci_low, lavaan_result$ci_high, lavaan_result$runtime, lavaan_result$status)

  seminr_result <- .mediation_seminr(generated, x, y, job$seminr_bootstrap, job$seed)
  rows[[4L]] <- .mediation_comparator_row("seminr_native", setting, job$replication, truth,
    seminr_result$estimate, seminr_result$ci_low, seminr_result$ci_high, seminr_result$runtime, seminr_result$status)

  cbind(do.call(rbind, rows), items = items, edge_shape = edge_shape, row.names = NULL)
}

#' Run the mediation benchmark against native CB-SEM and PLS-SEM
#'
#' Scores the CS-SEM disattenuated indirect effect against native CB-SEM
#' (`lavaan` latent SEM, delta-method interval) and PLS-SEM (`seminr` path
#' products, bootstrap interval) on a shared mediation task with a known
#' latent-scale truth, reporting absolute bias and interval coverage. The naive
#' CS-SEM indirect effect is included to show the attenuation the correction
#' removes. Optional comparators are skipped when their packages are absent.
#'
#' @param manifest A manifest from
#'   [cssem_mediation_validation_manifest()] (use the `"benchmark"` tier).
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Cross-fitting folds.
#' @param iterations Measurement iterations.
#' @param structural_repeats Repeated structural CV assignments for selection.
#' @param eiv_bootstrap Bootstrap resamples for the CS-SEM mediation intervals.
#' @param seminr_bootstrap Bootstrap resamples for the PLS-SEM interval.
#' @param workers Independent jobs to run concurrently.
#' @return A data frame with one row per engine, scenario, and replication
#'   carrying the indirect estimate, absolute bias against truth, interval, and
#'   coverage.
#' @examples
#' results <- cssem_run_mediation_comparator_validation(
#'   cssem_mediation_validation_manifest("benchmark")[1, ], reps = 1,
#'   eiv_bootstrap = 50, seminr_bootstrap = 50
#' )
#' unique(results$engine)
#' @export
cssem_run_mediation_comparator_validation <- function(manifest, reps = 3L, seed = 1L, folds = 3L,
                                                      iterations = 8L, structural_repeats = 3L,
                                                      eiv_bootstrap = 200L, seminr_bootstrap = 200L,
                                                      workers = 1L) {
  if (!is.data.frame(manifest) || !all(c("scenario", "n", "loading") %in% names(manifest)))
    stop("manifest must contain scenario, n, and loading.", call. = FALSE)
  jobs <- vector("list", nrow(manifest) * reps); index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L
    jobs[[index]] <- list(setting = as.list(manifest[scenario_index, , drop = FALSE]), replication = replication,
      seed = seed + index, folds = folds, iterations = iterations, structural_repeats = structural_repeats,
      eiv_bootstrap = eiv_bootstrap, seminr_bootstrap = seminr_bootstrap)
  }
  do.call(rbind, .validation_map(jobs, .mediation_comparator_one, workers))
}
