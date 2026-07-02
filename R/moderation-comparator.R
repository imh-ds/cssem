# Rigorous moderated-mediation benchmark: score the CS-SEM disattenuated index of
# moderated mediation against native PLS-SEM (seminr composite + two-stage
# interaction) and native CB-SEM (seminr reflective + product-indicator latent
# interaction, via lavaan) on a shared task with a known latent-scale truth. The
# CS-SEM naive index is included to show the attenuation the correction removes.

# seminr measurement and structural models for a scenario, using composites for
# PLS-SEM and reflective constructs for CB-SEM. The moderated path differs by
# scenario: the b-path (M -> Y) or the a-path (X -> M).
.moderated_seminr_models <- function(items, scenario, reflective) {
  constructor <- if (reflective) seminr::reflective else seminr::composite
  method <- if (reflective) seminr::product_indicator else seminr::two_stage
  base <- list(
    constructor("X", seminr::multi_items("a", seq_len(items))),
    constructor("W", seminr::multi_items("b", seq_len(items))),
    constructor("M", seminr::multi_items("c", seq_len(items))),
    constructor("Y", seminr::multi_items("d", seq_len(items)))
  )
  if (scenario == "b_path") {
    interaction <- seminr::interaction_term(iv = "M", moderator = "W", method = method)
    mm <- do.call(seminr::constructs, c(base, list(interaction)))
    sm <- seminr::relationships(seminr::paths(from = "X", to = "M"),
      seminr::paths(from = c("X", "M", "W", "M*W"), to = "Y"))
  } else {
    interaction <- seminr::interaction_term(iv = "X", moderator = "W", method = method)
    mm <- do.call(seminr::constructs, c(base, list(interaction)))
    sm <- seminr::relationships(seminr::paths(from = c("X", "W", "X*W"), to = "M"),
      seminr::paths(from = c("X", "M"), to = "Y"))
  }
  list(measurement_model = mm, structural_model = sm)
}

# Index of moderated mediation from a fitted path-coefficient matrix: the product
# of the two coefficients whose product carries the indirect effect's dependence
# on the moderator (a * b_MW for b-path moderation; b_XW * b for a-path).
.moderated_index_from_paths <- function(path_coef, scenario) {
  if (scenario == "b_path") path_coef["X", "M"] * path_coef["M*W", "Y"]
  else path_coef["X*W", "M"] * path_coef["M", "Y"]
}

.moderated_mediation_seminr <- function(generated, scenario, items, bootstrap, seed) {
  if (!requireNamespace("seminr", quietly = TRUE))
    return(list(available = FALSE, status = "skipped_not_installed", estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, runtime = NA_real_))
  models <- .moderated_seminr_models(items, scenario, reflective = FALSE)
  data <- .mean_impute_data(generated$data); n <- nrow(data)
  index_of <- function(rows) .moderated_index_from_paths(
    seminr::estimate_pls(rows, models$measurement_model, models$structural_model)$path_coef, scenario)
  started <- proc.time()[["elapsed"]]
  tryCatch({
    point <- suppressMessages(index_of(data))
    interval <- c(NA_real_, NA_real_)
    if (bootstrap > 0L) {
      set.seed(seed + 909L)
      draws <- suppressMessages(vapply(seq_len(bootstrap), function(b)
        index_of(data[sample.int(n, n, replace = TRUE), , drop = FALSE]), numeric(1)))
      draws <- draws[is.finite(draws)]
      if (length(draws)) interval <- stats::quantile(draws, c(.025, .975), names = FALSE)
    }
    list(available = TRUE, status = "success", estimate = point, ci_low = interval[[1L]], ci_high = interval[[2L]], runtime = proc.time()[["elapsed"]] - started)
  }, error = function(err) list(available = TRUE, status = "error", estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, runtime = proc.time()[["elapsed"]] - started))
}

# Native CB-SEM index via a product-indicator latent interaction. The point
# estimate is reported (product-indicator refitting makes a bootstrap interval
# impractical for a benchmark grid).
.moderated_mediation_lavaan <- function(generated, scenario, items) {
  if (!requireNamespace("seminr", quietly = TRUE) || !requireNamespace("lavaan", quietly = TRUE))
    return(list(available = FALSE, status = "skipped_not_installed", estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, runtime = NA_real_))
  models <- .moderated_seminr_models(items, scenario, reflective = TRUE)
  started <- proc.time()[["elapsed"]]
  tryCatch({
    fit <- suppressMessages(seminr::estimate_cbsem(.mean_impute_data(generated$data), models$measurement_model, models$structural_model))
    list(available = TRUE, status = "success", estimate = .moderated_index_from_paths(fit$path_coef, scenario),
      ci_low = NA_real_, ci_high = NA_real_, runtime = proc.time()[["elapsed"]] - started)
  }, error = function(err) list(available = TRUE, status = "error", estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, runtime = proc.time()[["elapsed"]] - started))
}

.moderated_comparator_row <- function(engine, setting, replication, truth, estimate, ci_low, ci_high, runtime, status = "success") {
  covers <- if (!is.finite(ci_low) || !is.finite(ci_high) || !is.finite(truth)) NA else truth >= ci_low && truth <= ci_high
  data.frame(engine = engine, scenario = setting$scenario, replication = replication, n = setting$n,
    loading = setting$loading, status = status, true_index = truth, index = estimate,
    abs_bias = abs(estimate - truth), ci_low = ci_low, ci_high = ci_high, covers_truth = covers,
    runtime_seconds = unname(runtime), worker_pid = Sys.getpid(), stringsAsFactors = FALSE)
}

.moderated_mediation_comparator_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  items <- if ("items" %in% names(setting)) setting$items else 6L
  generated <- .moderated_mediation_validation_data(setting$scenario, setting$n, setting$loading, job$seed, items = items)
  truth <- generated$truth$index
  rows <- list()

  cssem_elapsed <- system.time({
    model <- generated$model; model$folds <- job$folds
    fit <- cssem_fit(model, generated$data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
    association <- cssem_associate(fit, generated$structure, structural_repeats = job$structural_repeats,
      seed = job$seed, shadow_scope = "temporal")
    disattenuated <- cssem_moderated_mediation(association, "X", "Y", "W", eiv_bootstrap = job$eiv_bootstrap, seed = job$seed, disattenuate = TRUE)
    naive <- cssem_moderated_mediation(association, "X", "Y", "W", eiv_bootstrap = 0L, disattenuate = FALSE)
  })["elapsed"]
  rows[[1L]] <- .moderated_comparator_row("cssem_disattenuated", setting, job$replication, truth,
    disattenuated$index$estimate, disattenuated$index$ci[[1L]], disattenuated$index$ci[[2L]], cssem_elapsed)
  rows[[2L]] <- .moderated_comparator_row("cssem_naive", setting, job$replication, truth,
    naive$index$estimate, NA_real_, NA_real_, cssem_elapsed)

  seminr_result <- .moderated_mediation_seminr(generated, setting$scenario, items, job$seminr_bootstrap, job$seed)
  rows[[3L]] <- .moderated_comparator_row("seminr_native", setting, job$replication, truth,
    seminr_result$estimate, seminr_result$ci_low, seminr_result$ci_high, seminr_result$runtime, seminr_result$status)

  lavaan_result <- .moderated_mediation_lavaan(generated, setting$scenario, items)
  rows[[4L]] <- .moderated_comparator_row("lavaan_native", setting, job$replication, truth,
    lavaan_result$estimate, lavaan_result$ci_low, lavaan_result$ci_high, lavaan_result$runtime, lavaan_result$status)

  cbind(do.call(rbind, rows), items = items, row.names = NULL)
}

#' Run the moderated mediation benchmark against native CB-SEM and PLS-SEM
#'
#' Scores the CS-SEM disattenuated index of moderated mediation against native
#' PLS-SEM (seminr, composite with a two-stage interaction) and native CB-SEM
#' (seminr reflective with a product-indicator latent interaction) on a shared
#' task with a known latent-scale truth, reporting absolute bias and interval
#' coverage. The CS-SEM naive index is included to show the attenuation the
#' correction removes. Optional comparators are skipped when their packages are
#' absent.
#'
#' @param manifest A manifest from
#'   [cssem_moderated_mediation_validation_manifest()].
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Cross-fitting folds.
#' @param iterations Measurement iterations.
#' @param structural_repeats Repeated structural CV assignments for selection.
#' @param eiv_bootstrap Bootstrap resamples for the CS-SEM index interval.
#' @param seminr_bootstrap Bootstrap resamples for the PLS-SEM interval.
#' @param workers Independent jobs to run concurrently.
#' @return A data frame with one row per engine, scenario, and replication
#'   carrying the index estimate, absolute bias against truth, interval, and
#'   coverage.
#' @examples
#' results <- cssem_run_moderated_mediation_comparator_validation(
#'   cssem_moderated_mediation_validation_manifest("screening")[1, ], reps = 1,
#'   eiv_bootstrap = 50, seminr_bootstrap = 50
#' )
#' unique(results$engine)
#' @export
cssem_run_moderated_mediation_comparator_validation <- function(manifest, reps = 3L, seed = 1L, folds = 3L,
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
  do.call(rbind, .validation_map(jobs, .moderated_mediation_comparator_one, workers))
}
