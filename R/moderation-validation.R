# Deterministic moderated-mediation stress-test scenarios. A moderator W acts on
# the b-path (M -> Y), the a-path (X -> M), or both, via a declared product
# interaction; ordinal indicator blocks add measurement error set by `loading`.
# The target is the population index of moderated mediation computed from the
# true equations (latent-generators.R); the independent sample-level target
# and the package's own sample oracle are reported alongside it.

.moderated_mediation_generator <- function(scenario) {
  nodes <- switch(scenario,
    b_path = list(M = function(s) .5 * s$X,
      Y = function(s) .2 * s$X + .4 * s$M + .1 * s$W + .4 * s$M * s$W),
    a_path = list(M = function(s) .5 * s$X + .1 * s$W + .4 * s$X * s$W,
      Y = function(s) .2 * s$X + .5 * s$M),
    both_paths = list(M = function(s) .5 * s$X + .1 * s$W + .3 * s$X * s$W,
      Y = function(s) .2 * s$X + .4 * s$M + .1 * s$W + .3 * s$M * s$W),
    stop("Unknown moderated mediation scenario: ", scenario, call. = FALSE)
  )
  list(exogenous = c("X", "W"), nodes = nodes, noise_sd = .5)
}

# Population conditional indirect effects at fixed moderator levels (W is
# standard normal in the population, so levels are in its SD units) and the
# index of moderated mediation, cached per scenario.
.moderated_mediation_population <- function(scenario, levels = c(-1, 0, 1), delta = 1) {
  .generator_cached(paste("moderated", scenario, paste(levels, collapse = ","), delta), function() {
    generator <- .moderated_mediation_generator(scenario)
    draw <- .generator_draw(generator, .population_draws)
    constants <- .generator_constants(generator, draw)
    conditional <- vapply(levels, function(level) .generator_effects(generator, draw, constants, "X", "Y",
      delta, fixed = list(W = level))[["indirect_total"]], numeric(1))
    list(constants = constants,
      truth = list(conditional = conditional, index = .moderated_index(conditional, levels, 1)))
  })
}

.moderated_mediation_structure <- function(scenario) {
  linear <- function(predictors) stats::setNames(lapply(predictors, function(p) .build_effect("linear")), predictors)
  switch(scenario,
    b_path = .build_structure(list(M = linear("X"), Y = linear(c("X", "M", "W", "M:W"))), order = c("X", "W", "M", "Y")),
    a_path = .build_structure(list(M = linear(c("X", "W", "X:W")), Y = linear(c("X", "M"))), order = c("X", "W", "M", "Y")),
    both_paths = .build_structure(list(M = linear(c("X", "W", "X:W")), Y = linear(c("X", "M", "W", "M:W"))),
      order = c("X", "W", "M", "Y")),
    stop("Unknown moderated mediation scenario: ", scenario, call. = FALSE)
  )
}

# Independent analytic target for the linear interaction data-generating models.
# The conditional indirect effect is a product of separately fitted path
# coefficients, with the interaction coefficient changing either the a- or
# b-path. No moderated propagation helper is used here.
.moderated_mediation_truth <- function(latent, structure, moderator, levels) {
  names_in_order <- names(latent); x <- names_in_order[[1L]]; y <- names_in_order[[length(names_in_order)]]
  paths <- Filter(function(path) length(path) > 2L, .structure_paths(structure, x, y))
  if (length(paths) != 1L) stop("Independent moderated truth requires one mediating path.", call. = FALSE)
  mediator <- paths[[1L]][[2L]]
  mediator_fit <- stats::lm(stats::reformulate(names(structure$effects[[mediator]]), mediator), latent)
  outcome_fit <- stats::lm(stats::reformulate(names(structure$effects[[y]]), y), latent)
  mediator_coefficients <- stats::coef(mediator_fit); outcome_coefficients <- stats::coef(outcome_fit)
  interaction <- function(predictors, left, right) {
    hits <- predictors[vapply(predictors, function(predictor)
      .is_interaction(predictor) && setequal(.interaction_terms(predictor), c(left, right)), logical(1))]
    if (length(hits)) hits[[1L]] else NA_character_
  }
  b_interaction <- interaction(names(structure$effects[[y]]), mediator, moderator)
  a_interaction <- interaction(names(structure$effects[[mediator]]), x, moderator)
  a <- unname(mediator_coefficients[[x]]); b <- unname(outcome_coefficients[[mediator]])
  values <- .moderator_values(latent, moderator, levels)
  if (is.na(a_interaction) && is.na(b_interaction))
    stop("No declared interaction changes the mediated path.", call. = FALSE)
  # Either path, or both, may be moderated; the conditional indirect effect is
  # the product of the two conditional path slopes.
  a_conditional <- a + if (is.na(a_interaction)) 0 else unname(mediator_coefficients[[a_interaction]]) * values
  b_conditional <- b + if (is.na(b_interaction)) 0 else unname(outcome_coefficients[[b_interaction]]) * values
  conditional <- a_conditional * b_conditional
  out <- list(conditional = conditional,
    index = .moderated_index(conditional, levels, .safe_scale(latent[[moderator]])))
  attr(out, "method") <- "analytic_interaction"
  out
}

# Retain the propagation-based target as an explicitly named sample oracle for
# comparison. It is not used as the independent population truth.
.moderated_mediation_sample_oracle <- function(latent, structure, moderator, levels) {
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
  list(conditional = conditional, index = .moderated_index(conditional, levels, .safe_scale(latent[[moderator]])))
}

.moderated_mediation_validation_data <- function(scenario, n, loading = .80, seed = 1L, items = 4L, missing = .02) {
  generator <- .moderated_mediation_generator(scenario)
  population <- .moderated_mediation_population(scenario)
  set.seed(seed)
  latent <- .generator_states(generator, n, population$constants)
  structure <- .moderated_mediation_structure(scenario)
  prefixes <- stats::setNames(letters[seq_along(latent)], names(latent))
  data <- do.call(cbind, unname(Map(function(state, prefix)
    .validation_items(state, prefix, loading, missing, items = items), latent, prefixes)))
  specifications <- stats::setNames(lapply(prefixes, function(prefix)
    list(indicators = paste0(prefix, seq_len(items)), scales = "ordinal")), names(latent))
  sample_target <- .moderated_mediation_truth(latent, structure, "W", c(-1, 0, 1))
  list(data = data, model = .build_measurement(specifications, folds = 5L), structure = structure, states = latent,
       truth = population$truth, truth_method = "population_counterfactual",
       sample_target = sample_target, sample_target_method = attr(sample_target, "method"),
       sample_oracle = .moderated_mediation_sample_oracle(latent, structure, "W", c(-1, 0, 1)))
}

.moderated_mediation_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  items <- if ("items" %in% names(setting)) setting$items else 4L
  generated <- .moderated_mediation_validation_data(setting$scenario, setting$n, setting$loading, job$seed, items = items)
  model <- generated$model; model$folds <- job$folds
  constructs <- names(generated$states); x <- constructs[[1L]]; y <- constructs[[length(constructs)]]
  elapsed <- system.time({
    fit <- .fit_states_quiet(model, generated$data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
    association <- associate(fit, generated$structure, structural_repeats = job$structural_repeats,
      seed = job$seed, shadow_scope = "temporal")
    disattenuated <- conditional_indirect_effect(association, x, y, "W", eiv_bootstrap = job$eiv_bootstrap, seed = job$seed, disattenuate = TRUE)
    naive <- conditional_indirect_effect(association, x, y, "W", eiv_bootstrap = 0L, disattenuate = FALSE)
  })["elapsed"]
  true_index <- generated$truth$index
  sample_oracle_index <- generated$sample_oracle$index
  ci <- disattenuated$index$ci
  covers <- if (!is.finite(ci[[1L]]) || !is.finite(ci[[2L]])) NA else true_index >= ci[[1L]] && true_index <= ci[[2L]]
  data.frame(scenario = setting$scenario, replication = job$replication, n = setting$n, loading = setting$loading,
    truth_method = generated$truth_method, sample_target_method = generated$sample_target_method,
    sample_target_index = generated$sample_target$index, sample_oracle_index = sample_oracle_index,
    true_index = true_index, naive_index = naive$index$estimate, disattenuated_index = disattenuated$index$estimate,
    naive_abs_bias = abs(naive$index$estimate - true_index), disattenuated_abs_bias = abs(disattenuated$index$estimate - true_index),
    naive_sample_oracle_abs_error = abs(naive$index$estimate - sample_oracle_index),
    disattenuated_sample_oracle_abs_error = abs(disattenuated$index$estimate - sample_oracle_index),
    index_ci_low = ci[[1L]], index_ci_high = ci[[2L]], index_covers_truth = covers,
    runtime_seconds = unname(elapsed), worker_pid = Sys.getpid(), stringsAsFactors = FALSE)
}

#' Create a deterministic moderated mediation validation manifest
#'
#' @param tier `"screening"` for a compact suite or `"full"` for a larger grid.
#' @return A scenario data frame for [validate_conditional_indirect_effect()].
#' @examples
#' conditional_indirect_effect_manifest("screening")
#' @export
conditional_indirect_effect_manifest <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  if (tier == "screening") return(data.frame(
    scenario = c("b_path", "a_path", "b_path"),
    n = c(500L, 500L, 600L), loading = c(.80, .80, .60), items = c(6L, 6L, 6L),
    stringsAsFactors = FALSE
  ))
  grid <- expand.grid(scenario = c("b_path", "a_path", "both_paths"), n = c(500L, 800L), loading = c(.60, .80),
    items = 6L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid[, c("scenario", "n", "loading", "items")]
}

#' Run deterministic moderated mediation validation simulations
#'
#' Fits CS-SEM and compares the naive and disattenuated index of moderated
#' mediation against the population index implied by the generating equations,
#' with bootstrap-interval coverage. The moderator acts on the b-path, the
#' a-path, or both via declared interactions.
#'
#' @param manifest A manifest from
#'   [conditional_indirect_effect_manifest()].
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
#' results <- validate_conditional_indirect_effect(
#'   conditional_indirect_effect_manifest("screening")[1, ], reps = 1, eiv_bootstrap = 50
#' )
#' @export
validate_conditional_indirect_effect <- function(manifest, reps = 3L, seed = 1L, folds = 3L,
                                                     iterations = 8L, structural_repeats = 3L,
                                                     eiv_bootstrap = 200L, workers = 1L) {
  .preserve_seed()
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
