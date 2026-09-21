# Deterministic mediation stress-test scenarios. Latent construct states follow a
# declared linear single, parallel, or serial mediation structure; ordinal
# indicator blocks add measurement error whose severity is set by `loading`. The
# known latent-scale decomposition is the target the disattenuated estimate must
# recover. This is the lightweight harness; the publication-ready benchmark
# (including native PLS-SEM and CB-SEM mediation comparators) builds on it.

# Build the declared mediation structure, optionally fixing every edge to a
# linear shape policy. Declaring linear edges reflects confirmatory mediation
# practice and keeps whole-chain disattenuation available (no edge can be
# auto-selected as smooth), which also speeds the bootstrap refits.
.mediation_structure <- function(scenario, edge_shape = "auto") {
  edge <- function(predictors) {
    if (identical(edge_shape, "linear")) stats::setNames(lapply(predictors, function(p) .build_effect("linear")), predictors) else predictors
  }
  switch(scenario,
    single = .build_structure(list(M = edge("X"), Y = edge(c("X", "M"))), order = c("X", "M", "Y")),
    parallel = .build_structure(list(M1 = edge("X"), M2 = edge("X"), Y = edge(c("X", "M1", "M2"))), order = c("X", "M1", "M2", "Y")),
    serial = .build_structure(list(M1 = edge("X"), M2 = edge(c("X", "M1")), Y = edge(c("X", "M1", "M2"))), order = c("X", "M1", "M2", "Y")),
    stop("Unknown mediation scenario: ", scenario, call. = FALSE)
  )
}

.mediation_validation_data <- function(scenario, n, loading = .80, seed = 1L,
                                        items = 4L, missing = .02, edge_shape = "auto") {
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
  structure <- .mediation_structure(scenario, edge_shape)
  prefixes <- stats::setNames(letters[seq_along(states)], names(states))
  data <- do.call(cbind, unname(Map(function(state, prefix)
    .validation_items(state, prefix, loading, missing, items = items), states, prefixes)))
  specifications <- stats::setNames(lapply(prefixes, function(prefix)
    list(indicators = paste0(prefix, seq_len(items)), scales = "ordinal")), names(states))
  latent <- as.data.frame(states)
  truth_method <- if (identical(edge_shape, "smooth")) "intervention_integral" else "analytic_linear"
  truth <- .mediation_truth(latent, structure, method = truth_method)
  sample_oracle <- .mediation_sample_oracle(latent, structure)
  list(data = data, model = .build_measurement(specifications, folds = 5L), structure = structure,
       states = latent, truth = truth, sample_oracle = sample_oracle,
       truth_method = attr(truth, "method"))
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

# Independently integrate a nonlinear structural model under a shift in x. The
# active-edge traversal is local to this validation helper: nodes with no
# active incoming edge retain their observed latent value, and both intervention
# arms are built from the same latent records. This deliberately does not call
# .propagate_y(), .mediation_effect(), or .cssem_mediation_core().
.intervention_mediation_truth <- function(latent, structure, models, x, y, delta = 1) {
  order <- .resolve_temporal_order(structure, names(latent))
  all_edges <- .model_edges(models)
  propagate <- function(shift, active) {
    frame <- latent
    frame[[x]] <- latent[[x]] + shift
    for (node in order) {
      model <- models[[node]]
      if (is.null(model) || identical(node, x)) next
      constructs <- unique(unlist(lapply(names(model$shapes), .predictor_constructs), use.names = FALSE))
      incoming <- vapply(constructs, function(construct) .edge(construct, node) %in% active, logical(1))
      if (!any(incoming)) next
      inputs <- as.data.frame(stats::setNames(lapply(constructs, function(construct)
        if (.edge(construct, node) %in% active) frame[[construct]] else latent[[construct]]), constructs),
        stringsAsFactors = FALSE)
      frame[[node]] <- .predict_shape_model(model, inputs)
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
                             y = names(latent)[[length(names(latent))]], method = "analytic_linear") {
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
  .intervention_mediation_truth(latent, structure, models, x, y)
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
  sample_oracle <- generated$sample_oracle
  # NA when disattenuation was unavailable (a path traverses a selected smooth
  # edge), so coverage is summarized only over reps where it could be assessed.
  covers <- if (!is.finite(indirect$disattenuated_ci_low) || !is.finite(indirect$disattenuated_ci_high)) NA else
    truth[["indirect_total"]] >= indirect$disattenuated_ci_low &&
      truth[["indirect_total"]] <= indirect$disattenuated_ci_high
  data.frame(
    scenario = setting$scenario, replication = job$replication, n = setting$n, loading = setting$loading,
    truth_method = generated$truth_method, sample_oracle_indirect = sample_oracle[["indirect_total"]],
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
    # Confirmatory mediation grid for the native-comparator benchmark: edges are
    # declared linear, so every engine estimates the same linear mediation model.
    grid <- expand.grid(scenario = c("single", "parallel", "serial"), n = c(400L, 800L),
      loading = c(.60, .80), items = 4L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    grid$edge_shape <- "linear"
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
#' disattenuated indirect effects against the known latent-scale truth, with
#' bootstrap-interval coverage. Establishes the workflow that the publication
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
