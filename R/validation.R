.validation_items <- function(z, prefix, loading, missing, sparse = FALSE,
                              cross_state = NULL, cross_loading = 0,
                              local_dependence = 0, items = 4L) {
  shared <- stats::rnorm(length(z))
  thresholds <- if (sparse) c(-Inf, -2.2, -0.2, .5, Inf) else c(-Inf, -1, -.2, .5, Inf)
  block <- lapply(seq_len(items), function(j) {
    eta <- loading * z + stats::rnorm(length(z), sd = sqrt(max(.05, 1 - loading^2)))
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

#' Return the v0.2 supported operating envelope
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

#' Create a deterministic v0.2 measurement validation manifest
#'
#' @param tier `"screening"` for a compact local suite or `"full"` for the
#'   factorial confirmation manifest.
#' @return A scenario data frame.
#' @examples
#' cssem_measurement_validation_manifest("screening")
#' @export
cssem_measurement_validation_manifest <- function(tier = c("screening", "full")) {
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
#' @return A machine-readable data frame with recovery, loss, stability,
#'   convergence, runtime, and diagnostic metrics.
#' @examples
#' results <- cssem_run_measurement_validation(
#'   cssem_measurement_validation_manifest("screening")[1, ], reps = 1
#' )
#' @export
cssem_run_measurement_validation <- function(manifest, reps = 3L, seed = 1L,
                                             folds = 3L, iterations = 8L,
                                             max_iterations = 16L, diagnostics = FALSE) {
  required <- c("scenario", "n", "loading", "missing", "local_dependence", "cross_loading", "overlap", "sparse")
  if (!is.data.frame(manifest) || !all(required %in% names(manifest))) stop("manifest is missing required measurement scenario columns.", call. = FALSE)
  rows <- vector("list", nrow(manifest) * reps); index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L; setting <- manifest[scenario_index, ]
    data <- .measurement_validation_data(setting$n, setting$loading, setting$missing,
      setting$local_dependence, setting$cross_loading, setting$overlap, setting$sparse, seed + index)
    truth <- attr(data, "truth")
    model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
      B = list(indicators = paste0("b", 1:4), scales = "ordinal")), folds = folds)
    elapsed <- system.time(validation_fit <- .validation_fit(model, data, seed + index, folds, iterations, max_iterations, diagnostics))["elapsed"]
    fit <- validation_fit$fit
    proxy <- .proxy_recovery(data, truth)
    residual_max <- if (nrow(fit$residual_dependence)) max(abs(fit$residual_dependence$residual_correlation), na.rm = TRUE) else NA_real_
    rows[[index]] <- cbind(data.frame(scenario = setting$scenario, replication = replication), setting[, setdiff(names(setting), "scenario"), drop = FALSE],
      data.frame(cssem_recovery = mean(abs(diag(stats::cor(fit$locked_scores, truth)))),
        ordinal_factor_proxy_recovery = proxy[["ordinal_factor_proxy"]], composite_proxy_recovery = proxy[["composite_proxy"]],
        held_out_loss = mean(fit$item_metrics$value), stability = mean(fit$stability),
        converged = validation_fit$converged, fit_attempts = validation_fit$attempts,
        runtime_seconds = unname(elapsed), automatic_warning_count = nrow(fit$warnings),
        sparse_warning_detected = any(fit$warnings$type == "sparse_category"), residual_dependence_max = residual_max,
        diagnostic_true_positive = isTRUE(setting$sparse) && any(fit$warnings$type == "sparse_category"),
        diagnostic_false_positive = !isTRUE(setting$sparse) && nrow(fit$warnings) > 0L,
        diagnostic_status = if (diagnostics) "exploratory" else "not_run", stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.structural_validation_data <- function(type, n, seed, loading = .80, missing = .05) {
  set.seed(seed)
  trust <- stats::rnorm(n)
  context <- stats::rnorm(n)
  quality <- switch(type,
    smooth = as.numeric(scale(.55 * trust + .30 * trust^3 + stats::rnorm(n, sd = .65))),
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
  data <- do.call(cbind, unname(Map(function(z, name) .validation_items(z, tolower(name), loading, missing), states, names(states))))
  structure_spec <- if (type == "omitted") {
    cssem_structure(list(Quality = "Trust", Loyalty = c("Trust", "Quality")), order = c("Trust", "Context", "Quality", "Loyalty"))
  } else {
    cssem_structure(list(Quality = "Trust", Loyalty = c("Trust", "Quality")), order = c("Trust", "Quality", "Loyalty"))
  }
  specifications <- lapply(names(states), function(name) list(indicators = paste0(tolower(name), 1:4), scales = "ordinal"))
  names(specifications) <- names(states)
  model <- cssem_model(specifications)
  list(data = data, truth = as.data.frame(states), model = model, structure = structure_spec)
}

#' Create a deterministic v0.2 structural validation manifest
#'
#' @param tier `"screening"` or `"full"`.
#' @return A structural scenario data frame.
#' @examples
#' cssem_structural_validation_manifest()
#' @export
cssem_structural_validation_manifest <- function(tier = c("screening", "full")) {
  tier <- match.arg(tier)
  types <- c("linear", "smooth", "interaction", "omitted", "downstream")
  if (tier == "screening") return(data.frame(scenario = types, n = c(220L, 260L, 300L, 300L, 260L), stringsAsFactors = FALSE))
  expand.grid(scenario = types, n = c(220L, 500L), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

#' Run deterministic associational structural validation simulations
#'
#' @param manifest A manifest from [cssem_structural_validation_manifest()].
#' @param reps Replications per structural scenario.
#' @param seed Base seed.
#' @param folds Cross-fitting folds for measurement and structure.
#' @param iterations Measurement iterations.
#' @param max_iterations Retry budget used if the initial measurement fit has
#'   not converged.
#' @return A machine-readable data frame containing model selection, R-squared,
#'   temporal and unrestricted gaps, and runtime for every outcome.
#' @examples
#' results <- cssem_run_structural_validation(
#'   cssem_structural_validation_manifest("screening")[1, ], reps = 1
#' )
#' @export
cssem_run_structural_validation <- function(manifest, reps = 3L, seed = 1L,
                                            folds = 3L, iterations = 8L,
                                            max_iterations = 16L) {
  if (!is.data.frame(manifest) || !all(c("scenario", "n") %in% names(manifest))) stop("manifest must contain scenario and n.", call. = FALSE)
  rows <- list(); index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L; setting <- manifest[scenario_index, ]
    generated <- .structural_validation_data(setting$scenario, setting$n, seed + index)
    elapsed <- system.time({
      validation_fit <- .validation_fit(generated$model, generated$data, seed + index, folds, iterations, max_iterations, FALSE)
      fit <- validation_fit$fit
      association <- cssem_associate(fit, generated$structure, shadow_scope = "both")
    })["elapsed"]
    gaps <- association$specification_gap
    candidates <- association$candidate_metrics
    for (outcome in names(association$full_models)) {
      selected <- candidates[candidates$outcome == outcome & candidates$selected, , drop = FALSE]
      temporal <- gaps[gaps$outcome == outcome & gaps$shadow_scope == "temporal", , drop = FALSE]
      unrestricted <- gaps[gaps$outcome == outcome & gaps$shadow_scope == "unrestricted", , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        scenario = setting$scenario, replication = replication, n = setting$n, outcome = outcome,
        selected_shape = selected$candidate, linear_r_squared = candidates$r_squared[candidates$outcome == outcome & candidates$candidate == "linear"],
        smooth_r_squared = candidates$r_squared[candidates$outcome == outcome & candidates$candidate == "smooth"],
        theory_r_squared = temporal$theory_r_squared, temporal_gap = temporal$specification_gap,
        unrestricted_gap = unrestricted$specification_gap,
        unrestricted_minus_temporal = unrestricted$specification_gap - temporal$specification_gap,
        measurement_converged = validation_fit$converged, fit_attempts = validation_fit$attempts,
        runtime_seconds = unname(elapsed), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' Evaluate v0.2 simulation release gates
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
  linear <- structural_results[structural_results$scenario == "linear", , drop = FALSE]
  smooth <- structural_results[structural_results$scenario == "smooth" & structural_results$outcome == "Quality", , drop = FALSE]
  omitted <- structural_results[structural_results$scenario == "omitted" & structural_results$outcome == "Quality", , drop = FALSE]
  downstream <- structural_results[structural_results$scenario == "downstream" & structural_results$outcome == "Quality", , drop = FALSE]
  gates <- data.frame(
    gate = c("measurement_noninferiority", "measurement_convergence", "linear_selection", "smooth_selection", "omitted_predictor_flag", "downstream_gap_divergence"),
    observed = c(if (any(inside)) min(pmin(measurement_results$cssem_recovery[inside] - measurement_results$ordinal_factor_proxy_recovery[inside], measurement_results$cssem_recovery[inside] - measurement_results$composite_proxy_recovery[inside])) else NA_real_,
      convergence_rate, mean(linear$selected_shape == "linear"), mean(smooth$selected_shape == "smooth"),
      mean(omitted$temporal_gap < -.03), mean(downstream$unrestricted_minus_temporal < -.03)),
    threshold = c(-.02, .95, .80, .70, .70, .70),
    comparison = c(">=", ">=", ">=", ">=", ">=", ">="), stringsAsFactors = FALSE
  )
  gates$passed <- !is.na(gates$observed) & gates$observed >= gates$threshold
  list(envelope = envelope, gates = gates, passed = all(gates$passed),
    interpretation = "A negative gap diagnoses predictive incompleteness under its stated information set; it never establishes reverse causal direction.")
}
