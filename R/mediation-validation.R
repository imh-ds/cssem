# Deterministic mediation stress-test scenarios. Latent construct states follow a
# declared linear single, parallel, or serial mediation structure; ordinal
# indicator blocks add measurement error whose severity is set by `loading`. The
# known latent-scale decomposition is the target the disattenuated estimate must
# recover. This is the lightweight harness; the publication-ready benchmark
# (including native PLS-SEM and CB-SEM mediation comparators) builds on it.

.mediation_validation_data <- function(scenario, n, loading = .80, seed = 1L,
                                        items = 4L, missing = .02) {
  set.seed(seed)
  standardize <- function(v) as.numeric(scale(v))
  X <- standardize(stats::rnorm(n))
  states <- switch(scenario,
    single = {
      M <- standardize(.50 * X + stats::rnorm(n, sd = .55))
      Y <- standardize(.30 * X + .45 * M + stats::rnorm(n, sd = .55))
      list(X = X, M = M, Y = Y)
    },
    parallel = {
      M1 <- standardize(.50 * X + stats::rnorm(n, sd = .55))
      M2 <- standardize(-.35 * X + stats::rnorm(n, sd = .55))
      Y <- standardize(.25 * X + .40 * M1 + .50 * M2 + stats::rnorm(n, sd = .55))
      list(X = X, M1 = M1, M2 = M2, Y = Y)
    },
    serial = {
      M1 <- standardize(.50 * X + stats::rnorm(n, sd = .55))
      M2 <- standardize(.30 * X + .55 * M1 + stats::rnorm(n, sd = .55))
      Y <- standardize(.20 * X + .25 * M1 + .50 * M2 + stats::rnorm(n, sd = .55))
      list(X = X, M1 = M1, M2 = M2, Y = Y)
    },
    stop("Unknown mediation scenario: ", scenario, call. = FALSE)
  )
  structure <- switch(scenario,
    single = cssem_structure(list(M = "X", Y = c("X", "M")), order = c("X", "M", "Y")),
    parallel = cssem_structure(list(M1 = "X", M2 = "X", Y = c("X", "M1", "M2")), order = c("X", "M1", "M2", "Y")),
    serial = cssem_structure(list(M1 = "X", M2 = c("X", "M1"), Y = c("X", "M1", "M2")), order = c("X", "M1", "M2", "Y"))
  )
  prefixes <- stats::setNames(letters[seq_along(states)], names(states))
  data <- do.call(cbind, unname(Map(function(state, prefix)
    .validation_items(state, prefix, loading, missing, items = items), states, prefixes)))
  specifications <- stats::setNames(lapply(prefixes, function(prefix)
    list(indicators = paste0(prefix, seq_len(items)), scales = "ordinal")), names(states))
  list(data = data, model = cssem_model(specifications), structure = structure,
       states = as.data.frame(states), truth = .mediation_truth(as.data.frame(states), structure))
}

# Latent-scale decomposition obtained by running the engine on the error-free
# construct states. This is the estimand the observed-score estimate targets.
.mediation_truth <- function(latent, structure) {
  models <- stats::setNames(vector("list", length(latent)), names(latent))
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    models[[outcome]] <- .fit_shape_model(latent, outcome, stats::setNames(rep("linear", length(predictors)), predictors))
  }
  names_in_order <- names(latent)
  core <- .cssem_mediation_core(models, latent, structure, names_in_order[[1L]], names_in_order[[length(names_in_order)]])
  summary <- core$summary
  stats::setNames(summary$naive_effect, summary$component)[c("total", "direct", "indirect_total")]
}

.mediation_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  items <- if ("items" %in% names(setting)) setting$items else 4L
  generated <- .mediation_validation_data(setting$scenario, setting$n, setting$loading, job$seed, items = items)
  model <- generated$model; model$folds <- job$folds
  constructs <- names(generated$states)
  elapsed <- system.time({
    fit <- cssem_fit(model, generated$data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
    association <- cssem_associate(fit, generated$structure, structural_repeats = job$structural_repeats,
      seed = job$seed, shadow_scope = "temporal")
    mediation <- cssem_mediation(association, constructs[[1L]], constructs[[length(constructs)]],
      eiv_bootstrap = job$eiv_bootstrap, seed = job$seed)
  })["elapsed"]
  summary <- mediation$summary
  indirect <- summary[summary$component == "indirect_total", , drop = FALSE]
  truth <- generated$truth
  # NA when disattenuation was unavailable (a path traverses a selected smooth
  # edge), so coverage is summarized only over reps where it could be assessed.
  covers <- if (!is.finite(indirect$disattenuated_ci_low) || !is.finite(indirect$disattenuated_ci_high)) NA else
    truth[["indirect_total"]] >= indirect$disattenuated_ci_low &&
      truth[["indirect_total"]] <= indirect$disattenuated_ci_high
  data.frame(
    scenario = setting$scenario, replication = job$replication, n = setting$n, loading = setting$loading,
    true_indirect = truth[["indirect_total"]], naive_indirect = indirect$naive_effect,
    disattenuated_indirect = indirect$disattenuated_effect,
    naive_abs_bias = abs(indirect$naive_effect - truth[["indirect_total"]]),
    disattenuated_abs_bias = abs(indirect$disattenuated_effect - truth[["indirect_total"]]),
    disattenuated_ci_low = indirect$disattenuated_ci_low, disattenuated_ci_high = indirect$disattenuated_ci_high,
    disattenuated_covers_truth = covers,
    true_direct = truth[["direct"]],
    disattenuated_direct = summary$disattenuated_effect[summary$component == "direct"],
    true_total = truth[["total"]],
    disattenuated_total = summary$disattenuated_effect[summary$component == "total"],
    runtime_seconds = unname(elapsed), worker_pid = Sys.getpid(), stringsAsFactors = FALSE
  )
}

#' Create a deterministic mediation validation manifest
#'
#' @param tier `"screening"` for a compact local suite or `"full"` for a larger
#'   factorial grid.
#' @return A scenario data frame for [cssem_run_mediation_validation()].
#' @examples
#' cssem_mediation_validation_manifest("screening")
#' @export
cssem_mediation_validation_manifest <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  if (tier == "screening") return(data.frame(
    scenario = c("single", "single", "parallel", "serial"),
    n = c(400L, 600L, 500L, 600L),
    loading = c(.80, .60, .80, .80),
    items = c(4L, 6L, 4L, 4L),
    stringsAsFactors = FALSE
  ))
  grid <- expand.grid(scenario = c("single", "parallel", "serial"), n = c(400L, 800L),
    loading = c(.60, .80), items = 4L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid[, c("scenario", "n", "loading", "items")]
}

#' Run deterministic mediation validation simulations
#'
#' Fits CS-SEM, decomposes the mediation effect, and compares the naive and
#' disattenuated indirect effects against the known latent-scale truth, with
#' bootstrap-interval coverage. Establishes the workflow that the publication
#' benchmark (with native PLS-SEM and CB-SEM mediation) will extend.
#'
#' @param manifest A manifest from [cssem_mediation_validation_manifest()].
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Cross-fitting folds.
#' @param iterations Measurement iterations.
#' @param structural_repeats Repeated structural CV assignments for selection.
#' @param eiv_bootstrap Bootstrap resamples for mediation intervals.
#' @param workers Independent jobs to run concurrently.
#' @return A data frame with one row per scenario and replication carrying the
#'   true, naive, and disattenuated indirect effects, absolute biases, and
#'   interval coverage.
#' @examples
#' results <- cssem_run_mediation_validation(
#'   cssem_mediation_validation_manifest("screening")[1, ], reps = 1, eiv_bootstrap = 50
#' )
#' @export
cssem_run_mediation_validation <- function(manifest, reps = 3L, seed = 1L, folds = 3L,
                                           iterations = 8L, structural_repeats = 3L,
                                           eiv_bootstrap = 200L, workers = 1L) {
  if (!is.data.frame(manifest) || !all(c("scenario", "n", "loading") %in% names(manifest)))
    stop("manifest must contain scenario, n, and loading.", call. = FALSE)
  jobs <- vector("list", nrow(manifest) * reps); index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L
    jobs[[index]] <- list(setting = as.list(manifest[scenario_index, , drop = FALSE]), replication = replication,
      seed = seed + index, folds = folds, iterations = iterations, structural_repeats = structural_repeats,
      eiv_bootstrap = eiv_bootstrap)
  }
  do.call(rbind, .validation_map(jobs, .mediation_validation_one, workers))
}
