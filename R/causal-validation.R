# Independent score-level causal validation. These scenarios target known
# structural effects and are deliberately separate from the estimator outputs.

.causal_validation_assumptions <- function() {
  list(consistency = "assumed", no_unmeasured_confounding = "assumed",
    positivity = "assumed", measurement_validity = "assumed",
    temporal_order = "assumed",
    no_exposure_induced_mediator_outcome_confounding = "assumed")
}

.causal_validation_design <- function(scenario) {
  edges <- switch(scenario,
    confounding = data.frame(from = c("C", "C", "U", "U", "X"),
      to = c("X", "Y", "X", "Y", "Y")),
    weak_overlap = data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")),
    nuisance_misspecification = data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")),
    measurement_error = data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")),
    nonlinear_treatment = data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")),
    mediation = data.frame(from = c("C", "C", "C", "X", "M", "X"),
      to = c("X", "M", "Y", "M", "Y", "Y")),
    stop("Unknown causal validation scenario: ", scenario, call. = FALSE)
  )
  causal_design(edges, treatment = "X", outcome = "Y", adjust = "C",
    assumptions = .causal_validation_assumptions())
}

.causal_validation_sample <- function(scenario, n, seed) {
  set.seed(seed)
  reliability <- c(C = 1, X = 1, Y = 1)
  folds <- sample(rep_len(seq_len(5L), n))
  truth_method <- "known_structural_coefficient"
  estimand <- "adjusted_linear"
  mediation <- FALSE

  if (scenario == "confounding") {
    C <- stats::rnorm(n); U <- stats::rnorm(n)
    X <- .7 * C + .7 * U + stats::rnorm(n, sd = .7)
    Y <- .4 * X + .5 * C + .8 * U + stats::rnorm(n, sd = .7)
    scores <- data.frame(C = C, X = X, Y = Y)
  } else if (scenario == "weak_overlap") {
    C <- stats::rnorm(n)
    X <- .995 * C + .025 * stats::rnorm(n)
    Y <- .4 * X + .6 * C + stats::rnorm(n, sd = .7)
    scores <- data.frame(C = C, X = X, Y = Y)
  } else if (scenario == "nuisance_misspecification") {
    C <- stats::runif(n, -2, 2)
    X <- sin(1.3 * C) + stats::rnorm(n, sd = .7)
    Y <- .3 * X + sin(1.3 * C) + stats::rnorm(n, sd = .7)
    scores <- data.frame(C = C, X = X, Y = Y)
    estimand <- "adjusted_dml"
  } else if (scenario == "measurement_error") {
    C_true <- stats::rnorm(n)
    X_true <- .7 * C_true + stats::rnorm(n, sd = .7)
    Y_true <- .4 * X_true + .6 * C_true + stats::rnorm(n, sd = .7)
    reliability <- c(C = .80, X = .75, Y = .85)
    add_error <- function(value, rho) value + stats::rnorm(n,
      sd = sqrt((1 - rho) / rho) * stats::sd(value))
    scores <- data.frame(C = add_error(C_true, reliability[["C"]]),
      X = add_error(X_true, reliability[["X"]]),
      Y = add_error(Y_true, reliability[["Y"]]))
  } else if (scenario == "nonlinear_treatment") {
    C <- stats::rnorm(n)
    X <- .6 * C + stats::rnorm(n, sd = .8)
    Y <- .2 * X + .25 * X^2 + .3 * C + stats::rnorm(n, sd = .7)
    scores <- data.frame(C = C, X = X, Y = Y)
    truth_method <- "population_average_derivative"
    estimand <- "adjusted_ame"
  } else if (scenario == "mediation") {
    C <- stats::rnorm(n)
    X <- .5 * C + stats::rnorm(n)
    M <- .5 * X + .4 * C + stats::rnorm(n, sd = .8)
    Y <- .2 * X + .45 * M + .35 * C + stats::rnorm(n, sd = .7)
    scores <- data.frame(C = C, X = X, M = M, Y = Y)
    reliability <- c(C = 1, X = 1, M = 1, Y = 1)
    truth_method <- "product_of_structural_paths"
    estimand <- "interventional_mediation"
    mediation <- TRUE
  } else stop("Unknown causal validation scenario: ", scenario, call. = FALSE)

  structure_spec <- if (mediation) {
    .build_structure(list(M = c("X", "C"), Y = c("X", "M", "C")),
      order = c("C", "X", "M", "Y"))
  } else {
    .build_structure(list(Y = c("X", "C")), order = c("C", "X", "Y"))
  }
  full_models <- if (mediation) list(
    M = .fit_shape_model(scores, "M", c(X = "linear", C = "linear")),
    Y = .fit_shape_model(scores, "Y", c(X = "linear", M = "linear", C = "linear"))
  ) else list()
  association <- list(scores = scores, reliability = reliability, folds = folds,
    structure = structure_spec, full_models = full_models)
  class(association) <- "cssem_association"
  list(association = association, design = .causal_validation_design(scenario),
    estimand = estimand, truth_method = truth_method)
}

.causal_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  scenario <- as.character(setting$scenario[[1L]])
  n <- as.integer(setting$n[[1L]])
  truth <- as.numeric(setting$target[[1L]])
  generated <- .causal_validation_sample(scenario, n, job$seed)
  elapsed <- system.time({
    result <- tryCatch({
      if (isTRUE(generated$estimand == "interventional_mediation")) {
        causal_indirect_effect(generated$association, "X", "Y", adjust = "C",
          temporal_order = generated$design$topological_order,
          eiv_bootstrap = job$bootstrap, seed = job$seed, design = generated$design)
      } else {
        causal_effect(generated$association, "X", "Y", adjust = "C",
          estimand = generated$estimand,
          temporal_order = generated$design$topological_order,
          disattenuate = identical(scenario, "measurement_error"),
          eiv_bootstrap = job$bootstrap, seed = job$seed, design = generated$design)
      }
    }, error = function(error) error)
  })[["elapsed"]]

  if (inherits(result, "error")) return(data.frame(
    scenario = scenario, replication = job$replication, n = n,
    estimand = generated$estimand, truth_method = generated$truth_method,
    truth = truth, estimate = NA_real_, bias = NA_real_, absolute_bias = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, ci_covers_truth = NA,
    label = "not_estimated", graph_valid = NA, adjustment_valid = NA,
    causal_admissible = NA, identification_strength = NA_real_,
    overlap_status = NA_character_, nuisance_method = NA_character_,
    nuisance_treatment_rmse = NA_real_, nuisance_outcome_rmse = NA_real_,
    nuisance_stage_r2 = NA_real_, runtime_seconds = unname(elapsed),
    status = "error", error = conditionMessage(result), stringsAsFactors = FALSE
  ))

  if (identical(generated$estimand, "interventional_mediation")) {
    component <- result$summary[result$summary$component == "indirect_total", , drop = FALSE]
    estimate <- component$disattenuated_effect[[1L]]
    low <- component$disattenuated_ci_low[[1L]]
    high <- component$disattenuated_ci_high[[1L]]
  } else {
    estimate <- result$adjusted_effect
    low <- result$ci_low
    high <- result$ci_high
  }
  if (length(low) != 1L || is.null(low)) low <- NA_real_
  if (length(high) != 1L || is.null(high)) high <- NA_real_
  has_interval <- length(low) == 1L && length(high) == 1L &&
    is.finite(low) && is.finite(high)
  ci_covers_truth <- if (has_interval) truth >= low && truth <= high else NA
  audit <- result$design_audit
  nuisance <- result$nuisance_diagnostics
  data.frame(
    scenario = scenario, replication = job$replication, n = n,
    estimand = generated$estimand, truth_method = generated$truth_method,
    truth = truth, estimate = estimate, bias = estimate - truth,
    absolute_bias = abs(estimate - truth), ci_low = low, ci_high = high,
    ci_covers_truth = ci_covers_truth, label = result$label,
    graph_valid = if (is.null(audit)) NA else audit$valid,
    adjustment_valid = if (is.null(audit)) NA else audit$adjustment_valid,
    causal_admissible = if (is.null(audit)) NA else audit$causal_admissible,
    identification_strength = result$identification_strength,
    overlap_status = result$overlap_diagnostic$status,
    nuisance_method = nuisance$method,
    nuisance_treatment_rmse = if (is.null(nuisance$treatment_rmse)) NA_real_ else nuisance$treatment_rmse,
    nuisance_outcome_rmse = if (is.null(nuisance$outcome_rmse)) NA_real_ else nuisance$outcome_rmse,
    nuisance_stage_r2 = if (is.null(nuisance$mediator_r2_min)) NA_real_ else nuisance$mediator_r2_min,
    runtime_seconds = unname(elapsed), status = "ok", error = "",
    stringsAsFactors = FALSE
  )
}

#' Create independent causal-estimand validation scenarios
#'
#' The manifest includes confounding omitted from adjustment, weak treatment
#' support, nonlinear nuisance misspecification, construct-score measurement
#' error, a nonlinear dose response, and interventional mediation. Targets are
#' known structural coefficients, a population average derivative, or a product
#' of declared mediation paths; they are not copied from CS-SEM estimates.
#'
#' @param tier Either "screening" for six bounded smoke-test scenarios or
#'   "full" for each scenario at three sample sizes.
#' @return A deterministic data frame with scenario, sample size, estimand,
#'   independent target, and truth-method columns.
#' @examples
#' causal_validation_manifest("screening")
#' @export
causal_validation_manifest <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  scenarios <- c("confounding", "weak_overlap", "nuisance_misspecification",
    "measurement_error", "nonlinear_treatment", "mediation")
  estimands <- c("adjusted_linear", "adjusted_linear", "adjusted_dml",
    "adjusted_linear", "adjusted_ame", "interventional_mediation")
  target <- c(.4, .4, .3, .4, .2, .225)
  truth_method <- c("known_structural_coefficient", "known_structural_coefficient",
    "known_structural_coefficient", "known_structural_coefficient",
    "population_average_derivative", "product_of_structural_paths")
  sizes <- if (tier == "screening") c(800L, 800L, 800L, 800L, 1000L, 800L) else
    rep(c(500L, 1000L, 2000L), each = length(scenarios))
  scenario <- if (tier == "screening") scenarios else rep(scenarios, times = 3L)
  index <- match(scenario, scenarios)
  data.frame(scenario = scenario, n = as.integer(sizes),
    estimand = estimands[index], target = target[index],
    truth_method = truth_method[index], stringsAsFactors = FALSE)
}

#' Run independent causal-estimand validation simulations
#'
#' Evaluates causal-effect estimates against fixed analytic or structural
#' targets, retaining bias, interval coverage when intervals are available,
#' graph admissibility, an estimand-specific treatment-support proxy, nuisance
#' diagnostics, failures, and runtime. Runs on locked construct scores: the
#' measurement-error scenario injects known score error and supplies its known
#' reliability; it does not refit an item measurement model.
#'
#' @param manifest A scenario data frame returned by
#'   [causal_validation_manifest()].
#' @param reps Positive integer replications per manifest row.
#' @param seed Integer base seed; each replication receives a deterministic
#'   offset seed.
#' @param bootstrap Non-negative integer percentile bootstrap resamples for
#'   linear and mediation intervals. Flexible effect estimands use their
#'   influence-function intervals.
#' @return One row per replication with independent truth, estimate, signed and
#'   absolute bias, interval coverage when available, causal label, graph and
#'   adjustment admissibility, overlap proxy, nuisance diagnostics, runtime,
#'   and any failure text. Unavailable intervals remain NA.
#' @examples
#' result <- validate_causal(causal_validation_manifest("screening"),
#'   reps = 1, bootstrap = 0)
#' @export
validate_causal <- function(manifest, reps = 3L, seed = 1L, bootstrap = 100L) {
  .preserve_seed()
  required <- c("scenario", "n", "estimand", "target", "truth_method")
  if (!is.data.frame(manifest) || !all(required %in% names(manifest)) || !nrow(manifest))
    stop("manifest must be a non-empty causal validation data frame with scenario, n, estimand, and target columns.", call. = FALSE)
  known <- c("confounding", "weak_overlap", "nuisance_misspecification",
    "measurement_error", "nonlinear_treatment", "mediation")
  if (!is.character(manifest$scenario) || anyNA(manifest$scenario) ||
      any(!manifest$scenario %in% known))
    stop("manifest contains an unknown causal validation scenario.", call. = FALSE)
  expected_estimand <- c(confounding = "adjusted_linear", weak_overlap = "adjusted_linear",
    nuisance_misspecification = "adjusted_dml", measurement_error = "adjusted_linear",
    nonlinear_treatment = "adjusted_ame", mediation = "interventional_mediation")
  if (!is.character(manifest$estimand) || anyNA(manifest$estimand) ||
      any(manifest$estimand != expected_estimand[manifest$scenario]))
    stop("manifest estimands must match their declared causal scenarios.", call. = FALSE)
  expected_truth_method <- c(confounding = "known_structural_coefficient",
    weak_overlap = "known_structural_coefficient",
    nuisance_misspecification = "known_structural_coefficient",
    measurement_error = "known_structural_coefficient",
    nonlinear_treatment = "population_average_derivative",
    mediation = "product_of_structural_paths")
  expected_target <- c(confounding = .4, weak_overlap = .4,
    nuisance_misspecification = .3, measurement_error = .4,
    nonlinear_treatment = .2, mediation = .225)
  if (!is.character(manifest$truth_method) || anyNA(manifest$truth_method) ||
      any(manifest$truth_method != expected_truth_method[manifest$scenario]) ||
      !is.numeric(manifest$target) || anyNA(manifest$target) ||
      any(!is.finite(manifest$target)) ||
      any(abs(manifest$target - expected_target[manifest$scenario]) > 1e-12))
    stop("manifest truth_method and target must match the independent scenario truth.", call. = FALSE)
  if (!is.numeric(manifest$n) || anyNA(manifest$n) || any(!is.finite(manifest$n)) ||
      any(manifest$n < 100 | manifest$n > .Machine$integer.max |
        manifest$n != as.integer(manifest$n)))
    stop("manifest n must contain integers of at least 100.", call. = FALSE)
  if (!is.numeric(manifest$target) || anyNA(manifest$target) || any(!is.finite(manifest$target)))
    stop("manifest target must contain finite numeric values.", call. = FALSE)
  if (length(reps) != 1L || !is.numeric(reps) || is.na(reps) || reps < 1L ||
      reps > .Machine$integer.max || reps != as.integer(reps))
    stop("reps must be a positive integer.", call. = FALSE)
  if (length(seed) != 1L || !is.numeric(seed) || is.na(seed) || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max - nrow(manifest) * reps ||
      seed != as.integer(seed))
    stop("seed must be a non-negative integer with room for every replication.", call. = FALSE)
  if (length(bootstrap) != 1L || !is.numeric(bootstrap) || is.na(bootstrap) ||
      bootstrap < 0L || bootstrap > .Machine$integer.max || bootstrap != as.integer(bootstrap))
    stop("bootstrap must be a non-negative integer.", call. = FALSE)

  jobs <- vector("list", nrow(manifest) * as.integer(reps))
  index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(as.integer(reps))) {
    index <- index + 1L
    jobs[[index]] <- list(setting = as.list(manifest[scenario_index, , drop = FALSE]),
      replication = replication, seed = as.integer(seed + index),
      bootstrap = as.integer(bootstrap))
  }
  do.call(rbind, lapply(jobs, .causal_validation_one))
}
