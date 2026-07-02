# Deterministic moderated-mediation stress-test scenarios. A moderator W acts on
# either the b-path (M -> Y) or the a-path (X -> M) via a declared product
# interaction; ordinal indicator blocks add measurement error set by `loading`.
# The known latent-scale index of moderated mediation is the target the
# disattenuated estimate must recover.

.moderated_mediation_structure <- function(scenario) {
  linear <- function(predictors) stats::setNames(lapply(predictors, function(p) cssem_effect("linear")), predictors)
  switch(scenario,
    b_path = cssem_structure(list(M = linear("X"), Y = linear(c("X", "M", "W", "M:W"))), order = c("X", "W", "M", "Y")),
    a_path = cssem_structure(list(M = linear(c("X", "W", "X:W")), Y = linear(c("X", "M"))), order = c("X", "W", "M", "Y")),
    stop("Unknown moderated mediation scenario: ", scenario, call. = FALSE)
  )
}

# Latent-scale conditional indirect effects and index of moderated mediation,
# computed by the propagation engine on the error-free construct states.
.moderated_mediation_truth <- function(latent, structure, moderator, levels) {
  models <- stats::setNames(vector("list", length(latent)), names(latent))
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    shapes <- stats::setNames(vapply(predictors, function(p) if (.is_interaction(p)) "product" else "linear", character(1)), predictors)
    models[[outcome]] <- .fit_shape_model(latent, outcome, shapes)
  }
  order <- .resolve_temporal_order(structure, names(latent)); names_in_order <- names(latent)
  x <- names_in_order[[1L]]; y <- names_in_order[[length(names_in_order)]]
  paths <- .structure_paths(structure, x, y)
  conditional <- .moderated_conditional(models, latent, order, x, y, paths, moderator, levels, 1)
  list(conditional = conditional, index = .moderated_index(conditional, levels))
}

.moderated_mediation_validation_data <- function(scenario, n, loading = .80, seed = 1L, items = 4L, missing = .02) {
  set.seed(seed); standardize <- function(v) as.numeric(scale(v))
  X <- standardize(stats::rnorm(n)); W <- standardize(stats::rnorm(n))
  states <- switch(scenario,
    b_path = {
      M <- standardize(.5 * X + stats::rnorm(n, sd = .5))
      Y <- standardize(.2 * X + .4 * M + .1 * W + .4 * (M * W) + stats::rnorm(n, sd = .5))
      list(X = X, W = W, M = M, Y = Y)
    },
    a_path = {
      M <- standardize(.5 * X + .1 * W + .4 * (X * W) + stats::rnorm(n, sd = .5))
      Y <- standardize(.2 * X + .5 * M + stats::rnorm(n, sd = .5))
      list(X = X, W = W, M = M, Y = Y)
    },
    stop("Unknown moderated mediation scenario: ", scenario, call. = FALSE)
  )
  structure <- .moderated_mediation_structure(scenario)
  prefixes <- stats::setNames(letters[seq_along(states)], names(states))
  data <- do.call(cbind, unname(Map(function(state, prefix)
    .validation_items(state, prefix, loading, missing, items = items), states, prefixes)))
  specifications <- stats::setNames(lapply(prefixes, function(prefix)
    list(indicators = paste0(prefix, seq_len(items)), scales = "ordinal")), names(states))
  list(data = data, model = cssem_model(specifications), structure = structure, states = as.data.frame(states),
       truth = .moderated_mediation_truth(as.data.frame(states), structure, "W", c(-1, 0, 1)))
}

.moderated_mediation_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  items <- if ("items" %in% names(setting)) setting$items else 4L
  generated <- .moderated_mediation_validation_data(setting$scenario, setting$n, setting$loading, job$seed, items = items)
  model <- generated$model; model$folds <- job$folds
  constructs <- names(generated$states); x <- constructs[[1L]]; y <- constructs[[length(constructs)]]
  elapsed <- system.time({
    fit <- cssem_fit(model, generated$data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
    association <- cssem_associate(fit, generated$structure, structural_repeats = job$structural_repeats,
      seed = job$seed, shadow_scope = "temporal")
    disattenuated <- cssem_moderated_mediation(association, x, y, "W", eiv_bootstrap = job$eiv_bootstrap, seed = job$seed, disattenuate = TRUE)
    naive <- cssem_moderated_mediation(association, x, y, "W", eiv_bootstrap = 0L, disattenuate = FALSE)
  })["elapsed"]
  true_index <- generated$truth$index
  ci <- disattenuated$index$ci
  covers <- if (!is.finite(ci[[1L]]) || !is.finite(ci[[2L]])) NA else true_index >= ci[[1L]] && true_index <= ci[[2L]]
  data.frame(scenario = setting$scenario, replication = job$replication, n = setting$n, loading = setting$loading,
    true_index = true_index, naive_index = naive$index$estimate, disattenuated_index = disattenuated$index$estimate,
    naive_abs_bias = abs(naive$index$estimate - true_index), disattenuated_abs_bias = abs(disattenuated$index$estimate - true_index),
    index_ci_low = ci[[1L]], index_ci_high = ci[[2L]], index_covers_truth = covers,
    runtime_seconds = unname(elapsed), worker_pid = Sys.getpid(), stringsAsFactors = FALSE)
}

#' Create a deterministic moderated mediation validation manifest
#'
#' @param tier `"screening"` for a compact suite or `"full"` for a larger grid.
#' @return A scenario data frame for [cssem_run_moderated_mediation_validation()].
#' @examples
#' cssem_moderated_mediation_validation_manifest("screening")
#' @export
cssem_moderated_mediation_validation_manifest <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  if (tier == "screening") return(data.frame(
    scenario = c("b_path", "a_path", "b_path"),
    n = c(500L, 500L, 600L), loading = c(.80, .80, .60), items = c(6L, 6L, 6L),
    stringsAsFactors = FALSE
  ))
  grid <- expand.grid(scenario = c("b_path", "a_path"), n = c(500L, 800L), loading = c(.60, .80),
    items = 6L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid[, c("scenario", "n", "loading", "items")]
}

#' Run deterministic moderated mediation validation simulations
#'
#' Fits CS-SEM and compares the naive and disattenuated index of moderated
#' mediation against the known latent-scale truth, with bootstrap-interval
#' coverage. The moderator acts on the b-path or a-path via a declared
#' interaction.
#'
#' @param manifest A manifest from
#'   [cssem_moderated_mediation_validation_manifest()].
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Cross-fitting folds.
#' @param iterations Measurement iterations.
#' @param structural_repeats Repeated structural CV assignments for selection.
#' @param eiv_bootstrap Bootstrap resamples for the index interval.
#' @param workers Independent jobs to run concurrently.
#' @return A data frame with one row per scenario and replication carrying the
#'   true, naive, and disattenuated index of moderated mediation, absolute
#'   biases, and interval coverage.
#' @examples
#' results <- cssem_run_moderated_mediation_validation(
#'   cssem_moderated_mediation_validation_manifest("screening")[1, ], reps = 1, eiv_bootstrap = 50
#' )
#' @export
cssem_run_moderated_mediation_validation <- function(manifest, reps = 3L, seed = 1L, folds = 3L,
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
  do.call(rbind, .validation_map(jobs, .moderated_mediation_validation_one, workers))
}
