.validation_items <- function(z, prefix, loading, missing, sparse = FALSE,
                              cross_state = NULL, cross_loading = 0,
                              local_dependence = 0, items = 4L,
                              respondent_noise = NULL, skew = 0) {
  shared <- stats::rnorm(length(z))
  thresholds <- if (sparse) c(-Inf, -2.2, -0.2, .5, Inf) else c(-Inf, -1, -.2, .5, Inf)
  # Positive skew shifts the cutpoints upward so response mass piles into the
  # lowest categories: the floor effect common in frequency or symptom items.
  if (skew != 0) thresholds <- c(-Inf, -1, -.2, .5, Inf) + skew
  base_sd <- sqrt(max(.05, 1 - loading^2))
  # A per-respondent multiplier on the idiosyncratic item noise. Careless
  # responders carry a large multiplier, so their items are weakly informative
  # about the latent state and their posteriors are wide.
  noise_scale <- if (is.null(respondent_noise)) rep(1, length(z)) else respondent_noise
  block <- lapply(seq_len(items), function(j) {
    eta <- loading * z + stats::rnorm(length(z), sd = base_sd) * noise_scale
    if (!is.null(cross_state)) eta <- eta + cross_loading * cross_state
    if (j %in% c(1L, 2L)) eta <- eta + local_dependence * shared
    value <- cut(eta, breaks = thresholds, labels = FALSE)
    value[stats::runif(length(z)) < missing] <- NA_integer_
    value
  })
  names(block) <- paste0(prefix, seq_len(items))
  as.data.frame(block)
}

.measurement_validation_data <- function(n, loading, missing, local_dependence,
                                         cross_loading, overlap, sparse, seed) {
  set.seed(seed)
  a <- stats::rnorm(n)
  b <- overlap * a + sqrt(pmax(0, 1 - overlap^2)) * stats::rnorm(n)
  data <- cbind(
    .validation_items(a, "a", loading, missing, sparse, b, cross_loading, local_dependence),
    .validation_items(b, "b", loading, missing, sparse, a, cross_loading, local_dependence)
  )
  attr(data, "truth") <- cbind(A = a, B = b)
  data
}

.proxy_recovery <- function(data, truth) {
  blocks <- list(1:4, 5:8)
  pls <- mean(vapply(blocks, function(ix) {
    abs(stats::cor(rowMeans(data[ix], na.rm = TRUE), truth[, if (min(ix) == 1) 1 else 2], use = "complete.obs"))
  }, numeric(1)))
  factor_proxy <- mean(vapply(blocks, function(ix) {
    x <- scale(data[ix]); x[is.na(x)] <- 0
    score <- x %*% stats::prcomp(x)$rotation[, 1]
    abs(stats::cor(score, truth[, if (min(ix) == 1) 1 else 2]))
  }, numeric(1)))
  c(ordinal_factor_proxy = factor_proxy, composite_proxy = pls)
}

.validation_fit <- function(model, data, seed, folds, iterations, max_iterations, diagnostics) {
  model$folds <- folds
  fit <- cssem_fit(model, data, seed = seed, iterations = iterations, diagnostics = diagnostics)
  converged <- all(vapply(fit$measurement_engine, function(x) isTRUE(x$converged), logical(1)))
  attempts <- 1L
  if (!converged && max_iterations > iterations) {
    fit <- cssem_fit(model, data, seed = seed, iterations = max_iterations, diagnostics = diagnostics)
    converged <- all(vapply(fit$measurement_engine, function(x) isTRUE(x$converged), logical(1)))
    attempts <- 2L
  }
  list(fit = fit, converged = converged, attempts = attempts)
}

.validation_map <- function(jobs, fun, workers) {
  workers <- as.integer(workers)
  if (is.na(workers) || workers < 1L) stop("workers must be a positive integer.", call. = FALSE)
  if (workers == 1L || length(jobs) == 1L) return(lapply(jobs, fun))
  workers <- min(workers, length(jobs))
  lib_paths <- .libPaths()
  cluster <- parallel::makePSOCKcluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterCall(cluster, function(paths) .libPaths(paths), lib_paths)
  parallel::clusterEvalQ(cluster, suppressPackageStartupMessages(library(cssem)))
  parallel::parLapply(cluster, jobs, fun)
}

.measurement_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  data <- .measurement_validation_data(setting$n, setting$loading, setting$missing,
    setting$local_dependence, setting$cross_loading, setting$overlap, setting$sparse, job$seed)
  truth <- attr(data, "truth")
  model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
    B = list(indicators = paste0("b", 1:4), scales = "ordinal")), folds = job$folds)
  elapsed <- system.time(validation_fit <- .validation_fit(model, data, job$seed, job$folds, job$iterations, job$max_iterations, job$diagnostics))["elapsed"]
  fit <- validation_fit$fit
  proxy <- .proxy_recovery(data, truth)
  residual_max <- if (nrow(fit$residual_dependence)) max(abs(fit$residual_dependence$residual_correlation), na.rm = TRUE) else NA_real_
  cbind(data.frame(scenario = setting$scenario, replication = job$replication), setting[, setdiff(names(setting), "scenario"), drop = FALSE],
    data.frame(cssem_recovery = mean(abs(diag(stats::cor(fit$locked_scores, truth)))),
      ordinal_factor_proxy_recovery = proxy[["ordinal_factor_proxy"]], composite_proxy_recovery = proxy[["composite_proxy"]],
      held_out_loss = mean(fit$item_metrics$value), stability = mean(fit$stability),
      converged = validation_fit$converged, fit_attempts = validation_fit$attempts,
      runtime_seconds = unname(elapsed), automatic_warning_count = nrow(fit$warnings),
      sparse_warning_detected = any(fit$warnings$type == "sparse_category"), residual_dependence_max = residual_max,
      diagnostic_true_positive = isTRUE(setting$sparse) && any(fit$warnings$type == "sparse_category"),
      diagnostic_false_positive = !isTRUE(setting$sparse) && nrow(fit$warnings) > 0L,
      residual_dependence_signal = is.finite(residual_max) && residual_max >= .25,
      residual_diagnostic_true_positive = setting$local_dependence > 0 && is.finite(residual_max) && residual_max >= .25,
      residual_diagnostic_false_positive = setting$local_dependence == 0 && is.finite(residual_max) && residual_max >= .25,
      diagnostic_status = if (job$diagnostics) "exploratory" else "not_run",
      worker_pid = Sys.getpid(), stringsAsFactors = FALSE))
}

#' Return the v0.3 supported operating envelope
#'
#' @return A one-row data frame describing the initial supported conditions.
#' @examples
#' cssem_supported_envelope()
#' @export
cssem_supported_envelope <- function() {
  data.frame(
    constructs = "one-dimensional ordinal manifestation blocks",
    minimum_indicators = 4L, minimum_n = 200L, minimum_loading = .70,
    maximum_missing = .10, notes = "Cross-loadings, strong overlap, sparse categories, and local dependence are exploratory.",
    stringsAsFactors = FALSE
  )
}

#' Create a deterministic v0.3 measurement validation manifest
#'
#' @param tier `"screening"` for a compact local suite or `"full"` for the
#'   factorial confirmation manifest.
#' @return A scenario data frame.
#' @examples
#' cssem_measurement_validation_manifest("screening")
#' @export
cssem_measurement_validation_manifest <- function(tier = c("screening", "diagnostic", "full")) {
  tier <- match.arg(tier)
  if (tier == "screening") return(data.frame(
    scenario = c("clean", "moderate_missing", "weak_signal", "cross_loading", "local_dependence", "sparse_categories", "high_overlap"),
    n = c(200L, 500L, 200L, 500L, 500L, 300L, 500L),
    loading = c(.80, .80, .55, .80, .80, .80, .80),
    missing = c(0, .10, .05, .05, .05, .05, .05),
    local_dependence = c(0, 0, 0, 0, .35, 0, 0),
    cross_loading = c(0, 0, 0, .20, 0, 0, 0),
    overlap = c(0, 0, 0, 0, 0, 0, .80),
    sparse = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ))
  if (tier == "diagnostic") return(data.frame(
    scenario = c("diagnostic_clean", "diagnostic_local_dependence", "diagnostic_sparse"),
    n = c(500L, 500L, 500L), loading = c(.80, .80, .80), missing = c(.05, .05, .05),
    local_dependence = c(0, .35, 0), cross_loading = c(0, 0, 0), overlap = c(0, 0, 0),
    sparse = c(FALSE, FALSE, TRUE), stringsAsFactors = FALSE
  ))
  grid <- expand.grid(n = c(200L, 500L), loading = c(.55, .80), missing = c(0, .10),
    local_dependence = c(0, .35), cross_loading = c(0, .20), overlap = c(0, .80), sparse = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$scenario <- paste0("full_", seq_len(nrow(grid)))
  grid[, c("scenario", setdiff(names(grid), "scenario"))]
}

#' Run deterministic measurement validation simulations
#'
#' @param manifest A measurement manifest from [cssem_measurement_validation_manifest()].
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Measurement cross-fitting folds.
#' @param iterations Marginal-IRT iterations per fit.
#' @param max_iterations Retry budget used if the initial fit has not converged.
#' @param diagnostics Whether to calculate exploratory diagnostics.
#' @param workers Number of independent replications to run concurrently. Use
#'   `1` for sequential execution; PSOCK workers are supported on Windows.
#' @return A machine-readable data frame with recovery, loss, stability,
#'   convergence, runtime, and diagnostic metrics.
#' @examples
#' results <- cssem_run_measurement_validation(
#'   cssem_measurement_validation_manifest("screening")[1, ], reps = 1
#' )
#' @export
cssem_run_measurement_validation <- function(manifest, reps = 3L, seed = 1L,
                                             folds = 3L, iterations = 8L,
                                             max_iterations = 16L, diagnostics = FALSE,
                                             workers = 1L) {
  required <- c("scenario", "n", "loading", "missing", "local_dependence", "cross_loading", "overlap", "sparse")
  if (!is.data.frame(manifest) || !all(required %in% names(manifest))) stop("manifest is missing required measurement scenario columns.", call. = FALSE)
  jobs <- vector("list", nrow(manifest) * reps); index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L
    jobs[[index]] <- list(setting = as.list(manifest[scenario_index, , drop = FALSE]), replication = replication,
      seed = seed + index, folds = folds, iterations = iterations, max_iterations = max_iterations, diagnostics = diagnostics)
  }
  do.call(rbind, .validation_map(jobs, .measurement_validation_one, workers))
}

.structural_validation_data <- function(type, n, seed, loading = .80, missing = .05, items = 4L,
                                        careless = 0, skew = 0) {
  set.seed(seed)
  trust <- stats::rnorm(n)
  context <- stats::rnorm(n)
  quality <- switch(type,
    monotone_increasing = as.numeric(scale(.55 * trust + .95 * pmax(trust, 0) + stats::rnorm(n, sd = .35))),
    monotone_decreasing = as.numeric(scale(-.55 * trust - .95 * pmax(trust, 0) + stats::rnorm(n, sd = .35))),
    smooth_subtle = as.numeric(scale(.45 * (trust^2 - 1) + stats::rnorm(n, sd = .65))),
    smooth_strong = as.numeric(scale(2.10 * (trust^2 - 1) + stats::rnorm(n, sd = .10))),
    # Behaviorally realistic monotone-nonlinear effects: a saturating plateau, a
    # threshold that activates past a point, and concave diminishing returns.
    plateau = as.numeric(scale(1.70 * tanh(1.40 * trust) + stats::rnorm(n, sd = .40))),
    threshold = as.numeric(scale(1.50 * pmax(trust - .50, 0) + stats::rnorm(n, sd = .40))),
    diminishing = as.numeric(scale(1.30 * log(trust + 4) + stats::rnorm(n, sd = .40))),
    null = stats::rnorm(n),
    omitted = .35 * trust + .65 * context + stats::rnorm(n, sd = .65),
    .65 * trust + stats::rnorm(n, sd = .70)
  )
  loyalty <- switch(type,
    interaction = .20 * trust + .25 * quality + .50 * trust * quality + stats::rnorm(n, sd = .70),
    downstream = .15 * trust + .95 * quality + stats::rnorm(n, sd = .45),
    .30 * trust + .60 * quality + stats::rnorm(n, sd = .70)
  )
  states <- list(Trust = trust, Quality = quality, Loyalty = loyalty)
  if (type == "omitted") states <- list(Trust = trust, Context = context, Quality = quality, Loyalty = loyalty)
  # One careless-responding multiplier shared across every item a respondent
  # answers, producing heteroskedastic measurement information across people.
  respondent_noise <- rep(1, n)
  if (careless > 0) respondent_noise[sample.int(n, floor(careless * n))] <- 6
  data <- do.call(cbind, unname(Map(function(z, name) .validation_items(z, tolower(name), loading, missing,
    items = items, respondent_noise = respondent_noise, skew = skew), states, names(states))))
  structure_spec <- if (type == "omitted") {
    cssem_structure(list(Quality = "Trust", Loyalty = c("Trust", "Quality")), order = c("Trust", "Context", "Quality", "Loyalty"))
  } else {
    cssem_structure(list(Quality = "Trust", Loyalty = c("Trust", "Quality")), order = c("Trust", "Quality", "Loyalty"))
  }
  specifications <- lapply(names(states), function(name) list(indicators = paste0(tolower(name), seq_len(items)), scales = "ordinal"))
  names(specifications) <- names(states)
  model <- cssem_model(specifications)
  list(data = data, truth = as.data.frame(states), model = model, structure = structure_spec)
}

# Assemble the .structural_validation_data() argument list from a manifest row,
# forwarding the optional measurement-stress columns when a manifest supplies
# them and otherwise relying on the generator defaults.
.structural_data_args <- function(setting, seed) {
  args <- list(setting$scenario, setting$n, seed,
    items = if ("items" %in% names(setting)) setting$items else 4L)
  for (param in c("loading", "missing", "careless", "skew"))
    if (param %in% names(setting)) args[[param]] <- setting[[param]]
  args
}

#' Create a deterministic v0.3 structural validation manifest
#'
#' @param tier `"screening"` or `"full"`.
#' @return A structural scenario data frame.
#' @examples
#' cssem_structural_validation_manifest()
#' @export
cssem_structural_validation_manifest <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  types <- c("linear", "monotone_increasing", "monotone_decreasing", "smooth_subtle", "smooth_strong", "null", "interaction", "omitted", "downstream")
  if (tier == "screening") {
    base <- data.frame(
      scenario = types,
      n = c(220L, 500L, 500L, 300L, 700L, 300L, 300L, 300L, 260L),
      items = c(4L, 6L, 6L, 4L, 6L, 4L, 4L, 4L, 4L),
      loading = .80, careless = 0, skew = 0, stringsAsFactors = FALSE
    )
    # Behaviorally realistic structural shapes and measurement-stress scenarios
    # added in v0.4. Existing release gates filter by scenario name and ignore
    # these; they exercise the disattenuation and respondent-weighting machinery.
    extra <- data.frame(
      scenario = c("plateau", "threshold", "diminishing", "low_reliability", "careless", "skewed"),
      n = c(500L, 500L, 500L, 600L, 600L, 500L),
      items = c(6L, 6L, 6L, 6L, 6L, 6L),
      loading = c(.80, .80, .80, .55, .80, .80),
      careless = c(0, 0, 0, 0, .20, 0),
      skew = c(0, 0, 0, 0, 0, 1.20),
      stringsAsFactors = FALSE
    )
    return(rbind(base, extra))
  }
  shapes <- expand.grid(scenario = c(types, "plateau", "threshold", "diminishing"),
    n = c(220L, 500L, 1000L), items = c(4L, 6L), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  shapes$loading <- .80; shapes$careless <- 0; shapes$skew <- 0
  stress <- data.frame(
    scenario = rep(c("low_reliability", "careless", "skewed"), each = 2L),
    n = rep(c(500L, 1000L), times = 3L), items = 6L,
    loading = rep(c(.55, .80, .80), each = 2L),
    careless = rep(c(0, .20, 0), each = 2L),
    skew = rep(c(0, 0, 1.20), each = 2L), stringsAsFactors = FALSE
  )
  rbind(shapes, stress)
}

.structural_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  generated <- do.call(.structural_validation_data, .structural_data_args(setting, job$seed))
  elapsed <- system.time({
    validation_fit <- .validation_fit(generated$model, generated$data, job$seed, job$folds, job$iterations, job$max_iterations, FALSE)
    association <- cssem_associate(validation_fit$fit, generated$structure,
      structural_repeats = job$structural_repeats, seed = job$seed, shadow_scope = "both")
  })["elapsed"]
  gaps <- association$specification_gap
  candidates <- association$candidate_metrics
  ledger <- cssem_effect_ledger(association)
  rows <- lapply(seq_len(nrow(ledger)), function(index) {
    selected <- ledger[index, , drop = FALSE]; outcome <- selected$outcome[[1L]]; predictor <- selected$predictor[[1L]]
    linear <- candidates[candidates$outcome == outcome & candidates$predictor == predictor & candidates$shape == "linear", , drop = FALSE]
    smooth_candidates <- candidates[candidates$outcome == outcome & candidates$predictor == predictor & grepl("^smooth", candidates$shape), , drop = FALSE]
    best_smooth <- if (nrow(smooth_candidates)) smooth_candidates[which.max(smooth_candidates$mean_mse_improvement), , drop = FALSE] else data.frame(r_squared = NA_real_)
    data.frame(
      scenario = setting$scenario, replication = job$replication, n = setting$n, outcome = outcome, predictor = predictor,
      selected_shape = selected$shape, selected_spline_df = if (grepl("^smooth_df", selected$shape)) as.integer(sub("smooth_df", "", selected$shape)) else NA_integer_,
      linear_r_squared = linear$r_squared[[1L]], smooth_r_squared = best_smooth$r_squared[[1L]],
      mean_mse_improvement = selected$mean_mse_improvement, mse_improvement_se = selected$mse_improvement_se,
      selection_stability = selected$selection_frequency, edge_drop_mse_increase = selected$edge_drop_mse_increase,
      theory_r_squared = selected$theory_r_squared, temporal_gap = selected$temporal_gap, unrestricted_gap = selected$unrestricted_gap,
      unrestricted_minus_temporal = selected$unrestricted_gap - selected$temporal_gap,
      measurement_converged = validation_fit$converged, fit_attempts = validation_fit$attempts,
      runtime_seconds = unname(elapsed), worker_pid = Sys.getpid(), stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Run deterministic v0.3 associational structural validation simulations
#'
#' @param manifest A manifest from [cssem_structural_validation_manifest()].
#' @param reps Replications per structural scenario.
#' @param seed Base seed.
#' @param folds Cross-fitting folds for measurement and structure.
#' @param iterations Measurement iterations.
#' @param max_iterations Retry budget used if the initial measurement fit has
#'   not converged.
#' @param structural_repeats Number of repeated structural CV assignments used
#'   for edge-level shape selection.
#' @param workers Number of independent replications to run concurrently. Use
#'   `1` for sequential execution; PSOCK workers are supported on Windows.
#' @return A machine-readable data frame containing model selection, R-squared,
#'   temporal and unrestricted gaps, and runtime for every outcome.
#' @examples
#' results <- cssem_run_structural_validation(
#'   cssem_structural_validation_manifest("screening")[1, ], reps = 1
#' )
#' @export
cssem_run_structural_validation <- function(manifest, reps = 3L, seed = 1L,
                                            folds = 3L, iterations = 8L,
                                            max_iterations = 16L,
                                            structural_repeats = 5L, workers = 1L) {
  if (!is.data.frame(manifest) || !all(c("scenario", "n") %in% names(manifest))) stop("manifest must contain scenario and n.", call. = FALSE)
  jobs <- vector("list", nrow(manifest) * reps); index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L
    jobs[[index]] <- list(setting = as.list(manifest[scenario_index, , drop = FALSE]), replication = replication,
      seed = seed + index, folds = folds, iterations = iterations, max_iterations = max_iterations,
      structural_repeats = structural_repeats)
  }
  do.call(rbind, .validation_map(jobs, .structural_validation_one, workers))
}

#' Evaluate v0.3 simulation release gates
#'
#' @param measurement_results Results from [cssem_run_measurement_validation()].
#' @param structural_results Results from [cssem_run_structural_validation()].
#' @return A list with gate-level evidence, pass/fail status, and the supported
#'   operating envelope.
#' @examples
#' # cssem_validation_report(measurement_results, structural_results)
#' @export
cssem_validation_report <- function(measurement_results, structural_results) {
  envelope <- cssem_supported_envelope()
  inside <- measurement_results$n >= envelope$minimum_n & measurement_results$loading >= envelope$minimum_loading &
    measurement_results$missing <= envelope$maximum_missing & measurement_results$cross_loading == 0 &
    measurement_results$local_dependence == 0 & !measurement_results$sparse & measurement_results$overlap < .80
  measurement_pass <- all(measurement_results$cssem_recovery[inside] >= measurement_results$ordinal_factor_proxy_recovery[inside] - .02) &&
    all(measurement_results$cssem_recovery[inside] >= measurement_results$composite_proxy_recovery[inside] - .02)
  convergence_rate <- if ("converged" %in% names(measurement_results) && any(inside)) mean(measurement_results$converged[inside]) else NA_real_
  edge <- if ("predictor" %in% names(structural_results)) structural_results$predictor == "Trust" else TRUE
  linear <- structural_results[structural_results$scenario == "linear" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  increasing <- structural_results[structural_results$scenario == "monotone_increasing" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  decreasing <- structural_results[structural_results$scenario == "monotone_decreasing" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  smooth_strong <- structural_results[structural_results$scenario == "smooth_strong" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  null <- structural_results[structural_results$scenario == "null" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  omitted <- structural_results[structural_results$scenario == "omitted" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  downstream <- structural_results[structural_results$scenario == "downstream" & structural_results$outcome == "Quality" & edge, , drop = FALSE]
  gates <- data.frame(
    gate = c("measurement_noninferiority", "measurement_convergence", "linear_selection", "monotone_increasing_selection", "monotone_decreasing_selection", "strong_smooth_selection", "false_nonlinear_selection", "omitted_predictor_flag", "downstream_gap_divergence"),
    observed = c(if (any(inside)) min(pmin(measurement_results$cssem_recovery[inside] - measurement_results$ordinal_factor_proxy_recovery[inside], measurement_results$cssem_recovery[inside] - measurement_results$composite_proxy_recovery[inside])) else NA_real_,
      convergence_rate, mean(linear$selected_shape == "linear"), mean(increasing$selected_shape == "monotone_increasing"),
      mean(decreasing$selected_shape == "monotone_decreasing"), mean(grepl("^smooth", smooth_strong$selected_shape)),
      mean(c(linear$selected_shape != "linear", null$selected_shape != "linear")), mean(omitted$temporal_gap < -.03), mean(downstream$unrestricted_minus_temporal < -.03)),
    threshold = c(-.02, .95, .80, .70, .70, .70, .20, .70, .70),
    comparison = c(">=", ">=", ">=", ">=", ">=", ">=", "<=", ">=", ">="), stringsAsFactors = FALSE
  )
  gates$passed <- !is.na(gates$observed) & ifelse(gates$comparison == ">=", gates$observed >= gates$threshold, gates$observed <= gates$threshold)
  list(envelope = envelope, gates = gates, passed = all(gates$passed),
    interpretation = "A negative gap diagnoses predictive incompleteness under its stated information set; it never establishes reverse causal direction.")
}
