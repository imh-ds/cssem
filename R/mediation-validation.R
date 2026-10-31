# Deterministic mediation stress-test scenarios. Latent construct states follow a
# known single, parallel, or serial mediation structure, linear or with a
# saturating a- or b-path; ordinal indicator blocks add measurement error whose
# severity is set by `loading`. The target is the population counterfactual
# decomposition computed from the true equations (latent-generators.R). The
# independent sample-level target and the package's own sample oracle are
# reported alongside it. This is the lightweight harness; the publication
# benchmark (with native PLS-SEM and CB-SEM mediation comparators) builds on it.

.saturating_mediation <- c("saturating_a_path", "saturating_b_path")

# True structural equations for each scenario. The saturating scenarios share
# the single-mediator graph and replace one path with a tanh.
.mediation_generator <- function(scenario) {
  nodes <- switch(scenario,
    single = list(M = function(s) .50 * s$X, Y = function(s) .30 * s$X + .45 * s$M),
    parallel = list(M1 = function(s) .50 * s$X, M2 = function(s) -.35 * s$X,
      Y = function(s) .25 * s$X + .40 * s$M1 + .50 * s$M2),
    serial = list(M1 = function(s) .50 * s$X, M2 = function(s) .30 * s$X + .55 * s$M1,
      Y = function(s) .20 * s$X + .25 * s$M1 + .50 * s$M2),
    saturating_a_path = list(M = function(s) .90 * tanh(1.2 * s$X), Y = function(s) .30 * s$X + .45 * s$M),
    saturating_b_path = list(M = function(s) .50 * s$X, Y = function(s) .30 * s$X + .90 * tanh(1.2 * s$M)),
    stop("Unknown mediation scenario: ", scenario, call. = FALSE)
  )
  list(exogenous = "X", nodes = nodes, noise_sd = .55)
}

# Population constants and counterfactual decomposition, cached per scenario.
.mediation_population <- function(scenario, delta = 1) {
  .generator_cached(paste("mediation", scenario, delta), function() {
    generator <- .mediation_generator(scenario)
    draw <- .generator_draw(generator, .population_draws)
    constants <- .generator_constants(generator, draw)
    y <- names(generator$nodes)[[length(generator$nodes)]]
    list(constants = constants, truth = .generator_effects(generator, draw, constants, "X", y, delta))
  })
}

# Build the declared mediation structure, optionally fixing every edge to a
# linear shape policy. Declaring linear edges reflects confirmatory mediation
# practice and keeps whole-chain disattenuation available (no edge can be
# auto-selected as smooth), which also speeds the bootstrap refits. A smooth
# policy declares every edge smooth, which the nonlinear validation target uses.
.mediation_structure <- function(scenario, edge_shape = "auto") {
  edge <- function(predictors) {
    if (edge_shape %in% c("linear", "smooth")) stats::setNames(lapply(predictors, function(p) .build_effect(edge_shape)), predictors) else predictors
  }
  if (scenario %in% .saturating_mediation) scenario <- "single"
  switch(scenario,
    single = .build_structure(list(M = edge("X"), Y = edge(c("X", "M"))), order = c("X", "M", "Y")),
    parallel = .build_structure(list(M1 = edge("X"), M2 = edge("X"), Y = edge(c("X", "M1", "M2"))), order = c("X", "M1", "M2", "Y")),
    serial = .build_structure(list(M1 = edge("X"), M2 = edge(c("X", "M1")), Y = edge(c("X", "M1", "M2"))), order = c("X", "M1", "M2", "Y")),
    stop("Unknown mediation scenario: ", scenario, call. = FALSE)
  )
}

.mediation_validation_data <- function(scenario, n, loading = .80, seed = 1L,
                                        items = 4L, missing = .02, edge_shape = "auto") {
  generator <- .mediation_generator(scenario)
  population <- .mediation_population(scenario)
  set.seed(seed)
  latent <- .generator_states(generator, n, population$constants)
  structure <- .mediation_structure(scenario, edge_shape)
  prefixes <- stats::setNames(letters[seq_along(latent)], names(latent))
  data <- do.call(cbind, unname(Map(function(state, prefix)
    .validation_items(state, prefix, loading, missing, items = items), latent, prefixes)))
  specifications <- stats::setNames(lapply(prefixes, function(prefix)
    list(indicators = paste0(prefix, seq_len(items)), scales = "ordinal")), names(latent))
  # The sample-level target integrates fitted smooth edges whenever the truth
  # or the declared policy is nonlinear; otherwise it is the analytic product.
  nonlinear <- scenario %in% .saturating_mediation || identical(edge_shape, "smooth")
  sample_target <- if (nonlinear)
    .mediation_truth(latent, .mediation_structure(scenario, "smooth"), method = "intervention_integral") else
    .mediation_truth(latent, structure, method = "analytic_linear")
  sample_oracle <- .mediation_sample_oracle(latent, structure)
  list(data = data, model = .build_measurement(specifications, folds = 5L), structure = structure,
       states = latent, truth = population$truth, truth_method = "population_counterfactual",
       sample_target = sample_target, sample_target_method = attr(sample_target, "method"),
       sample_oracle = sample_oracle)
}

# Independent analytic target for a linear mediation DAG. Each directed-path
# effect is a product of separately fitted edge coefficients; no propagation or
# mediation-core code is used, so a shared baseline error cannot cancel out.
.linear_mediation_truth <- function(latent, structure, x, y) {
  coefficients <- vector("list", length(structure$effects)); names(coefficients) <- names(structure$effects)
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    fit <- stats::lm(stats::reformulate(predictors, outcome), latent)
    coefficients[[outcome]] <- stats::setNames(
      vapply(predictors, function(predictor) unname(stats::coef(fit)[[predictor]]), numeric(1)), predictors)
  }
  paths <- .structure_paths(structure, x, y)
  path_effect <- function(path) {
    if (length(path) < 2L) return(NA_real_)
    prod(vapply(seq_len(length(path) - 1L), function(i)
      coefficients[[path[[i + 1L]]]][[path[[i]]]], numeric(1)))
  }
  effects <- vapply(paths, path_effect, numeric(1))
  direct_path <- which(vapply(paths, length, integer(1)) == 2L)
  direct <- if (length(direct_path)) unname(coefficients[[y]][[x]]) else 0
  out <- stats::setNames(c(total = sum(effects), direct = direct, indirect_total = sum(effects) - direct),
    c("total", "direct", "indirect_total"))
  attr(out, "method") <- "analytic_linear"
  out
}

# The prior validation target is retained as an explicitly named sample oracle.
# It measures agreement with the package's own fitted-state propagation, not
# independent correctness of that propagation.
.mediation_sample_oracle <- function(latent, structure,
                                     x = names(latent)[[1L]],
                                     y = names(latent)[[length(names(latent))]]) {
  models <- stats::setNames(vector("list", length(latent)), names(latent))
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    models[[outcome]] <- .fit_shape_model(latent, outcome, stats::setNames(rep("linear", length(predictors)), predictors))
  }
  core <- .cssem_mediation_core(models, latent, structure, x, y)
  summary <- core$summary
  stats::setNames(summary$naive_effect, summary$component)[c("total", "direct", "indirect_total")]
}

# Independently integrate a nonlinear structural model under a shift in x. Each
# unit keeps its own structural residual (latent value minus fitted value), so
# a shifted node is its fitted value at the shifted inputs plus that residual:
# the unit-level counterfactual of an additive-noise model, averaged over the
# empirical residual distribution. The package engine instead propagates
# fitted conditional means, which differs whenever a downstream model is
# nonlinear in a mediator, so the two cannot share that approximation. Nodes
# with no active incoming edge keep their latent value, and the zero-shift arm
# reproduces the latent records exactly. This deliberately does not call
# .propagate_y(), .mediation_effect(), or .cssem_mediation_core().
.intervention_mediation_truth <- function(latent, structure, models, x, y, delta = 1) {
  order <- .resolve_temporal_order(structure, names(latent))
  all_edges <- .model_edges(models)
  model_inputs <- function(model) unique(unlist(lapply(names(model$shapes), .predictor_constructs), use.names = FALSE))
  residuals <- lapply(stats::setNames(names(models), names(models)), function(node) {
    model <- models[[node]]
    if (is.null(model)) return(NULL)
    inputs <- latent[, model_inputs(model), drop = FALSE]
    latent[[node]] - .predict_shape_model(model, inputs)
  })
  propagate <- function(shift, active) {
    frame <- latent
    frame[[x]] <- latent[[x]] + shift
    for (node in order) {
      model <- models[[node]]
      if (is.null(model) || identical(node, x)) next
      constructs <- model_inputs(model)
      incoming <- vapply(constructs, function(construct) .edge(construct, node) %in% active, logical(1))
      if (!any(incoming)) next
      inputs <- as.data.frame(stats::setNames(lapply(constructs, function(construct)
        if (.edge(construct, node) %in% active) frame[[construct]] else latent[[construct]]), constructs),
        stringsAsFactors = FALSE)
      frame[[node]] <- .predict_shape_model(model, inputs) + residuals[[node]]
    }
    frame[[y]]
  }
  effect <- function(active) mean(propagate(delta, active) - propagate(0, active), na.rm = TRUE) / delta
  total <- effect(all_edges)
  direct <- if (.edge(x, y) %in% all_edges) effect(.edge(x, y)) else 0
  out <- stats::setNames(c(total = total, direct = direct, indirect_total = total - direct),
    c("total", "direct", "indirect_total"))
  attr(out, "method") <- "intervention_integral"
  out
}

# Build fixed-shape models only for the independent nonlinear validation path.
.mediation_truth <- function(latent, structure, x = names(latent)[[1L]],
                             y = names(latent)[[length(names(latent))]], method = "analytic_linear",
                             delta = 1) {
  if (!identical(method, "intervention_integral"))
    return(.linear_mediation_truth(latent, structure, x, y))
  models <- stats::setNames(vector("list", length(latent)), names(latent))
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    shapes <- stats::setNames(vapply(predictors, function(predictor) {
      policy <- structure$effects[[outcome]][[predictor]]$shape
      if (.is_interaction(predictor)) "product" else if (identical(policy, "smooth")) "smooth_df3" else "linear"
    }, character(1)), predictors)
    models[[outcome]] <- .fit_shape_model(latent, outcome, shapes)
  }
  .intervention_mediation_truth(latent, structure, models, x, y, delta)
}

.mediation_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  items <- if ("items" %in% names(setting)) setting$items else 4L
  edge_shape <- if ("edge_shape" %in% names(setting)) setting$edge_shape else "auto"
  generated <- .mediation_validation_data(setting$scenario, setting$n, setting$loading, job$seed, items = items, edge_shape = edge_shape)
  model <- generated$model; model$folds <- job$folds
  constructs <- names(generated$states)
  elapsed <- system.time({
    fit <- .fit_states_quiet(model, generated$data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
    association <- associate(fit, generated$structure, structural_repeats = job$structural_repeats,
      seed = job$seed, shadow_scope = "temporal")
    mediation <- indirect_effect(association, constructs[[1L]], constructs[[length(constructs)]],
      eiv_bootstrap = job$eiv_bootstrap, seed = job$seed)
  })["elapsed"]
  summary <- mediation$summary
  indirect <- summary[summary$component == "indirect_total", , drop = FALSE]
  truth <- generated$truth
  sample_target <- generated$sample_target
  sample_oracle <- generated$sample_oracle
  # NA when disattenuation was unavailable (a path traverses a selected smooth
  # edge), so coverage is summarized only over reps where it could be assessed.
  covers <- if (!is.finite(indirect$disattenuated_ci_low) || !is.finite(indirect$disattenuated_ci_high)) NA else
    truth[["indirect_total"]] >= indirect$disattenuated_ci_low &&
      truth[["indirect_total"]] <= indirect$disattenuated_ci_high
  data.frame(
    scenario = setting$scenario, replication = job$replication, n = setting$n, loading = setting$loading,
    truth_method = generated$truth_method, sample_target_method = generated$sample_target_method,
    sample_target_indirect = sample_target[["indirect_total"]],
    sample_oracle_indirect = sample_oracle[["indirect_total"]],
    true_indirect = truth[["indirect_total"]], naive_indirect = indirect$naive_effect,
    disattenuated_indirect = indirect$disattenuated_effect,
    naive_abs_bias = abs(indirect$naive_effect - truth[["indirect_total"]]),
    disattenuated_abs_bias = abs(indirect$disattenuated_effect - truth[["indirect_total"]]),
    naive_sample_oracle_abs_error = abs(indirect$naive_effect - sample_oracle[["indirect_total"]]),
    disattenuated_sample_oracle_abs_error = abs(indirect$disattenuated_effect - sample_oracle[["indirect_total"]]),
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
#' @return A scenario data frame for [validate_indirect_effect()].
#' @examples
#' indirect_effect_manifest("screening")
#' @export
indirect_effect_manifest <- function(tier = c("screening", "full", "benchmark")) {
  tier <- match.arg(tier)
  if (tier == "screening") return(data.frame(
    scenario = c("single", "single", "parallel", "serial"),
    n = c(400L, 600L, 500L, 600L),
    loading = c(.80, .60, .80, .80),
    items = c(4L, 6L, 4L, 4L),
    edge_shape = "auto",
    stringsAsFactors = FALSE
  ))
  if (tier == "benchmark") {
    # Native-comparator benchmark. Linear scenarios declare linear edges, so
    # every engine estimates the same linear mediation model. Saturating
    # scenarios let CS-SEM select each edge's shape, while the native
    # comparators fit their linear models; all are scored against the same
    # population counterfactual.
    grid <- expand.grid(scenario = c("single", "parallel", "serial", .saturating_mediation), n = c(400L, 800L),
      loading = c(.60, .80), items = 4L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    grid$edge_shape <- ifelse(grid$scenario %in% .saturating_mediation, "auto", "linear")
    return(grid[, c("scenario", "n", "loading", "items", "edge_shape")])
  }
  grid <- expand.grid(scenario = c("single", "parallel", "serial"), n = c(400L, 800L),
    loading = c(.60, .80), items = 4L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$edge_shape <- "auto"
  grid[, c("scenario", "n", "loading", "items", "edge_shape")]
}

#' Run deterministic mediation validation simulations
#'
#' Fits CS-SEM, decomposes the mediation effect, and compares the naive and
#' disattenuated indirect effects against the population counterfactual
#' indirect effect implied by the generating equations, with bootstrap-interval
#' coverage. The independent sample-level target and the package's own sample
#' oracle are reported alongside it. Establishes the workflow that the publication
#' benchmark (with native PLS-SEM and CB-SEM mediation) will extend.
#'
#' @param manifest A manifest from [indirect_effect_manifest()].
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
#' results <- validate_indirect_effect(
#'   indirect_effect_manifest("screening")[1, ], reps = 1, eiv_bootstrap = 50
#' )
#' @export
validate_indirect_effect <- function(manifest, reps = 3L, seed = 1L, folds = 3L,
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
  do.call(rbind, .validation_map(jobs, .mediation_validation_one, workers))
}
