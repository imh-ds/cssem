#' Run optional comparator validation for the CS-SEM measurement suite
#'
#' Runs the deterministic measurement scenarios against the current CS-SEM
#' locked-score encoder, the built-in ordinal-factor and composite proxies, and
#' optional external comparators when their packages are installed.
#'
#' `lavaan` is used for a DWLS-style CFA comparator when available, and
#' `seminr` is used for a supported PLS-family composite comparator when
#' available. Optional engines are skipped gracefully when the packages are not
#' installed.
#'
#' @param manifest A measurement manifest from
#'   [cssem_measurement_validation_manifest()].
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Measurement cross-fitting folds used for the CS-SEM encoder.
#' @param iterations Measurement iterations used for the CS-SEM encoder.
#' @param workers Number of independent scenario-replication jobs to run
#'   concurrently.
#'
#' @return A data frame with one row per engine, scenario, and replication.
#'   Columns include `engine`, `package_name`, `package_version`, `available`,
#'   `status`, `converged`, `runtime_seconds`, `recovery`,
#'   `downstream_rmse`, and `downstream_r_squared`.
#' @examples
#' comparators <- cssem_run_comparator_validation(
#'   cssem_measurement_validation_manifest("screening")[1, ],
#'   reps = 1,
#'   seed = 1,
#'   workers = 1
#' )
#' unique(comparators$engine)
#' @export
cssem_run_comparator_validation <- function(manifest, reps = 3L, seed = 1L,
                                            folds = 3L, iterations = 8L,
                                            workers = 1L) {
  required <- c("scenario", "n", "loading", "missing", "local_dependence", "cross_loading", "overlap", "sparse")
  if (!is.data.frame(manifest) || !all(required %in% names(manifest)))
    stop("manifest is missing required measurement scenario columns.", call. = FALSE)
  jobs <- vector("list", nrow(manifest) * reps)
  index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L
    jobs[[index]] <- list(
      setting = as.list(manifest[scenario_index, , drop = FALSE]),
      replication = replication,
      seed = seed + index,
      folds = folds,
      iterations = iterations
    )
  }
  do.call(rbind, .validation_map(jobs, .comparator_validation_one, workers))
}

#' Run optional structural comparator validation on a shared associational task
#'
#' Runs the structural validation scenarios with a fixed CS-SEM associational
#' selector while swapping only the construct-score engine. This is intended to
#' show where cross-fitted locked scores help with shape selection, shadow-gap
#' diagnostics, and structural robustness, rather than only latent recovery.
#'
#' @param manifest A structural manifest from
#'   [cssem_structural_validation_manifest()].
#' @param reps Replications per scenario.
#' @param seed Base seed.
#' @param folds Measurement cross-fitting folds used for the CS-SEM encoder.
#' @param iterations Measurement iterations used for the CS-SEM encoder.
#' @param max_iterations Retry budget used if the initial measurement fit has
#'   not converged.
#' @param structural_repeats Number of repeated structural CV assignments used
#'   for edge-level shape selection.
#' @param eiv_bootstrap Percentile-bootstrap resamples for the corrected
#'   structural-coefficient interval in each engine's associational fit.
#' @param workers Number of independent scenario-replication jobs to run
#'   concurrently.
#'
#' @return A data frame with one row per engine, scenario, outcome, and
#'   replication. Columns include `engine`, `status`, `selected_shape`,
#'   `theory_r_squared`, `temporal_gap`, `unrestricted_gap`,
#'   `selection_stability`, `score_coverage`, and runtime metadata.
#' @examples
#' structural_comparators <- cssem_run_structural_comparator_validation(
#'   cssem_structural_validation_manifest("screening")[1, ],
#'   reps = 1,
#'   seed = 1,
#'   workers = 1
#' )
#' unique(structural_comparators$engine)
#' @export
cssem_run_structural_comparator_validation <- function(manifest, reps = 3L,
                                                       seed = 1L, folds = 3L,
                                                       iterations = 8L,
                                                       max_iterations = 16L,
                                                       structural_repeats = 5L,
                                                       eiv_bootstrap = 200L,
                                                       workers = 1L) {
  if (!is.data.frame(manifest) || !all(c("scenario", "n") %in% names(manifest))) {
    stop("manifest must contain scenario and n.", call. = FALSE)
  }
  jobs <- vector("list", nrow(manifest) * reps)
  index <- 0L
  for (scenario_index in seq_len(nrow(manifest))) for (replication in seq_len(reps)) {
    index <- index + 1L
    jobs[[index]] <- list(
      setting = as.list(manifest[scenario_index, , drop = FALSE]),
      replication = replication,
      seed = seed + index,
      folds = folds,
      iterations = iterations,
      max_iterations = max_iterations,
      structural_repeats = structural_repeats,
      eiv_bootstrap = eiv_bootstrap
    )
  }
  do.call(rbind, .validation_map(jobs, .structural_comparator_one, workers))
}

.comparator_validation_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  data <- .measurement_validation_data(
    setting$n,
    setting$loading,
    setting$missing,
    setting$local_dependence,
    setting$cross_loading,
    setting$overlap,
    setting$sparse,
    job$seed
  )
  truth <- attr(data, "truth")
  model <- cssem_model(list(
    A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
    B = list(indicators = paste0("b", 1:4), scales = "ordinal")
  ), folds = job$folds)

  rows <- list()
  index <- 0L

  cssem_elapsed <- system.time(
    fit <- cssem_fit(model, data, seed = job$seed, iterations = job$iterations, diagnostics = FALSE)
  )["elapsed"]
  proxy <- .proxy_scores(data)
  cssem_version <- as.character(utils::packageVersion("cssem"))

  index <- index + 1L
  rows[[index]] <- .comparator_row(
    engine = "cssem_locked",
    package_name = "cssem",
    package_version = cssem_version,
    available = TRUE,
    status = "success",
    converged = all(vapply(fit$measurement_engine, function(x) isTRUE(x$converged), logical(1))),
    runtime_seconds = unname(cssem_elapsed),
    recovery = .score_recovery(fit$locked_scores, truth),
    downstream = .downstream_metrics(fit$locked_scores, truth, seed = job$seed),
    setting = setting,
    replication = job$replication
  )

  index <- index + 1L
  rows[[index]] <- .comparator_row(
    engine = "ordinal_factor_proxy",
    package_name = "cssem",
    package_version = cssem_version,
    available = TRUE,
    status = "success",
    converged = TRUE,
    runtime_seconds = 0,
    recovery = .score_recovery(proxy[["ordinal_factor_proxy"]], truth),
    downstream = .downstream_metrics(proxy[["ordinal_factor_proxy"]], truth, seed = job$seed),
    setting = setting,
    replication = job$replication
  )

  index <- index + 1L
  rows[[index]] <- .comparator_row(
    engine = "composite_proxy",
    package_name = "cssem",
    package_version = cssem_version,
    available = TRUE,
    status = "success",
    converged = TRUE,
    runtime_seconds = 0,
    recovery = .score_recovery(proxy[["composite_proxy"]], truth),
    downstream = .downstream_metrics(proxy[["composite_proxy"]], truth, seed = job$seed),
    setting = setting,
    replication = job$replication
  )

  index <- index + 1L
  rows[[index]] <- .lavaan_comparator_row(data, truth, setting, job$replication)

  index <- index + 1L
  rows[[index]] <- .seminr_comparator_row(data, truth, setting, job$replication)

  do.call(rbind, rows)
}

.comparator_row <- function(engine, package_name, package_version, available,
                            status, converged, runtime_seconds, recovery,
                            downstream,
                            setting, replication, error_message = NA_character_) {
  cbind(
    data.frame(
      engine = engine,
      package_name = package_name,
      package_version = package_version,
      available = available,
      status = status,
      converged = converged,
      runtime_seconds = runtime_seconds,
      recovery = recovery,
      downstream_rmse = downstream[["rmse"]],
      downstream_r_squared = downstream[["r_squared"]],
      error_message = error_message,
      replication = replication,
      stringsAsFactors = FALSE
    ),
    setting,
    data.frame(worker_pid = Sys.getpid(), stringsAsFactors = FALSE)
  )
}

.score_recovery <- function(scores, truth) {
  scores <- as.data.frame(scores)
  truth <- as.data.frame(truth)
  matched <- intersect(names(truth), names(scores))
  if (!length(matched)) return(NA_real_)
  mean(abs(diag(stats::cor(scores[, matched, drop = FALSE], truth[, matched, drop = FALSE], use = "complete.obs"))))
}

.proxy_scores <- function(data) {
  composite <- data.frame(
    A = rowMeans(data[1:4], na.rm = TRUE),
    B = rowMeans(data[5:8], na.rm = TRUE)
  )
  factor_proxy <- lapply(list(data[1:4], data[5:8]), function(block) {
    x <- scale(block)
    x[is.na(x)] <- 0
    as.numeric(x %*% stats::prcomp(x)$rotation[, 1])
  })
  ordinal_factor <- data.frame(A = factor_proxy[[1L]], B = factor_proxy[[2L]])
  list(
    ordinal_factor_proxy = ordinal_factor,
    composite_proxy = composite
  )
}

.construct_proxy_scores <- function(data, model) {
  specs <- model$constructs
  composite <- lapply(specs, function(spec) rowMeans(data[, spec$indicators, drop = FALSE], na.rm = TRUE))
  factor_proxy <- lapply(specs, function(spec) {
    block <- scale(data[, spec$indicators, drop = FALSE])
    block[is.na(block)] <- 0
    as.numeric(block %*% stats::prcomp(block)$rotation[, 1])
  })
  list(
    ordinal_factor_proxy = as.data.frame(factor_proxy, optional = TRUE),
    composite_proxy = as.data.frame(composite, optional = TRUE)
  )
}

.fill_score_frame <- function(scores, all_names, n, case_idx = NULL) {
  scores <- as.data.frame(scores)
  out <- as.data.frame(matrix(NA_real_, nrow = n, ncol = length(all_names)))
  names(out) <- all_names
  matched <- intersect(all_names, names(scores))
  if (!length(matched)) return(out)
  if (is.null(case_idx)) {
    rows <- seq_len(min(nrow(scores), n))
  } else {
    rows <- as.integer(case_idx)
  }
  out[rows, matched] <- scores[, matched, drop = FALSE]
  out
}

.impute_score_frame <- function(scores) {
  out <- as.data.frame(scores)
  for (name in names(out)) {
    missing <- is.na(out[[name]])
    if (any(missing)) {
      fill <- mean(out[[name]], na.rm = TRUE)
      if (!is.finite(fill)) fill <- 0
      out[[name]][missing] <- fill
    }
  }
  out
}

.score_coverage <- function(scores) {
  scores <- as.data.frame(scores)
  if (!nrow(scores) || !ncol(scores)) return(NA_real_)
  mean(stats::complete.cases(scores))
}

.make_pseudo_fit <- function(scores, folds, reliability = NULL, score_posterior_sd = NULL) {
  structure(list(locked_scores = as.data.frame(scores), folds = folds, reliability = reliability,
    score_posterior_sd = score_posterior_sd), class = "cssem_fit")
}

.lavaan_measurement_syntax <- function(model) {
  paste(vapply(names(model$constructs), function(name) {
    paste(name, "=~", paste(model$constructs[[name]]$indicators, collapse = " + "))
  }, character(1)), collapse = "\n")
}

.seminr_measurement_model <- function(model) {
  constructs <- lapply(names(model$constructs), function(name) {
    spec <- model$constructs[[name]]
    indicators <- spec$indicators
    prefix <- sub("[0-9]+$", "", indicators[[1L]])
    numbers <- as.integer(sub("^.*?([0-9]+)$", "\\1", indicators))
    seminr::composite(name, seminr::multi_items(prefix, numbers))
  })
  do.call(seminr::constructs, constructs)
}

.seminr_structural_model <- function(all_names) {
  if (length(all_names) < 2L) return(do.call(seminr::relationships, list()))
  paths <- lapply(seq_len(length(all_names) - 1L), function(i) {
    seminr::paths(from = all_names[[i]], to = all_names[[i + 1L]])
  })
  do.call(seminr::relationships, paths)
}

.scoring_engine_result <- function(engine, generated, validation_fit, iterations,
                                   seed, replication) {
  model <- generated$model
  data <- generated$data
  all_names <- names(model$constructs)
  if (engine == "cssem_locked") {
    return(list(
      engine = engine,
      package_name = "cssem",
      package_version = as.character(utils::packageVersion("cssem")),
      available = TRUE,
      status = "success",
      converged = validation_fit$converged,
      score_runtime_seconds = 0,
      score_coverage = 1,
      scores = validation_fit$fit$locked_scores,
      reliability = validation_fit$fit$reliability,
      score_posterior_sd = validation_fit$fit$score_posterior_sd,
      error_message = NA_character_
    ))
  }
  if (engine %in% c("ordinal_factor_proxy", "composite_proxy")) {
    scores <- .construct_proxy_scores(data, model)[[engine]]
    return(list(
      engine = engine,
      package_name = "cssem",
      package_version = as.character(utils::packageVersion("cssem")),
      available = TRUE,
      status = "success",
      converged = TRUE,
      score_runtime_seconds = 0,
      score_coverage = .score_coverage(scores),
      scores = .impute_score_frame(scores),
      error_message = NA_character_
    ))
  }
  if (engine == "lavaan_dwls") {
    if (!requireNamespace("lavaan", quietly = TRUE)) {
      return(list(
        engine = engine,
        package_name = "lavaan",
        package_version = NA_character_,
        available = FALSE,
        status = "skipped_not_installed",
        converged = NA,
        score_runtime_seconds = NA_real_,
        score_coverage = NA_real_,
        scores = NULL,
        error_message = NA_character_
      ))
    }
    started <- proc.time()[["elapsed"]]
    result <- tryCatch({
      fit <- lavaan::cfa(
        model = .lavaan_measurement_syntax(model),
        data = data,
        ordered = names(data),
        estimator = "WLSMV",
        std.lv = TRUE
      )
      raw_scores <- lavaan::lavPredict(fit)
      filled <- .fill_score_frame(raw_scores, all_names, nrow(data), lavaan::lavInspect(fit, "case.idx"))
      list(
        status = "success",
        converged = isTRUE(lavaan::lavInspect(fit, "converged")),
        score_coverage = .score_coverage(filled),
        scores = .impute_score_frame(filled),
        error_message = NA_character_
      )
    }, error = function(err) {
      list(
        status = "error",
        converged = FALSE,
        score_coverage = NA_real_,
        scores = NULL,
        error_message = conditionMessage(err)
      )
    })
    return(c(result, list(
      engine = engine,
      package_name = "lavaan",
      package_version = as.character(utils::packageVersion("lavaan")),
      available = TRUE,
      score_runtime_seconds = proc.time()[["elapsed"]] - started
    )))
  }
  if (engine == "seminr_pls") {
    if (!requireNamespace("seminr", quietly = TRUE)) {
      return(list(
        engine = engine,
        package_name = "seminr",
        package_version = NA_character_,
        available = FALSE,
        status = "skipped_not_installed",
        converged = NA,
        score_runtime_seconds = NA_real_,
        score_coverage = NA_real_,
        scores = NULL,
        error_message = NA_character_
      ))
    }
    started <- proc.time()[["elapsed"]]
    result <- tryCatch({
      fit <- seminr::estimate_pls(
        data = .mean_impute_data(data),
        measurement_model = .seminr_measurement_model(model),
        structural_model = .seminr_structural_model(all_names)
      )
      scores <- fit$construct_scores
      filled <- .fill_score_frame(scores, all_names, nrow(data))
      list(
        status = "success",
        converged = TRUE,
        score_coverage = .score_coverage(filled),
        scores = .impute_score_frame(filled),
        error_message = NA_character_
      )
    }, error = function(err) {
      list(
        status = "error",
        converged = FALSE,
        score_coverage = NA_real_,
        scores = NULL,
        error_message = conditionMessage(err)
      )
    })
    return(c(result, list(
      engine = engine,
      package_name = "seminr",
      package_version = as.character(utils::packageVersion("seminr")),
      available = TRUE,
      score_runtime_seconds = proc.time()[["elapsed"]] - started
    )))
  }
  stop("Unknown engine: ", engine, call. = FALSE)
}

.structural_constructs <- function(structure) {
  unique(c(names(structure$effects), unlist(lapply(structure$effects, names), use.names = FALSE)))
}

# Native CB-SEM comparator: a full ordinal (WLSMV) latent SEM that estimates the
# declared structural paths jointly with the measurement model. Unlike scoring a
# CFA and regressing the scores, this disattenuates the structural coefficients,
# which is CB-SEM's genuine strength and the fair baseline for the correction.
.lavaan_native_structural <- function(data, model, structure) {
  if (!requireNamespace("lavaan", quietly = TRUE))
    return(list(available = FALSE, status = "skipped_not_installed", converged = NA, paths = NULL,
      runtime = NA_real_, version = NA_character_, error_message = NA_character_))
  constructs <- .structural_constructs(structure)
  measurement <- paste(vapply(constructs, function(nm)
    paste(nm, "=~", paste(model$constructs[[nm]]$indicators, collapse = " + ")), character(1)), collapse = "\n")
  structural <- paste(vapply(names(structure$effects), function(outcome)
    paste(outcome, "~", paste(names(structure$effects[[outcome]]), collapse = " + ")), character(1)), collapse = "\n")
  indicators <- unlist(lapply(constructs, function(nm) model$constructs[[nm]]$indicators), use.names = FALSE)
  started <- proc.time()[["elapsed"]]
  tryCatch({
    fit <- lavaan::sem(paste(measurement, structural, sep = "\n"), data = data,
      ordered = indicators, estimator = "WLSMV", std.lv = TRUE)
    reg <- lavaan::standardizedSolution(fit)
    reg <- reg[reg$op == "~", , drop = FALSE]
    paths <- data.frame(outcome = reg$lhs, predictor = reg$rhs, estimate = reg$est.std,
      se = reg$se, stringsAsFactors = FALSE)
    list(available = TRUE, status = "success", converged = isTRUE(lavaan::lavInspect(fit, "converged")),
      paths = paths, runtime = proc.time()[["elapsed"]] - started,
      version = as.character(utils::packageVersion("lavaan")), error_message = NA_character_)
  }, error = function(err) list(available = TRUE, status = "error", converged = FALSE, paths = NULL,
    runtime = proc.time()[["elapsed"]] - started, version = as.character(utils::packageVersion("lavaan")),
    error_message = conditionMessage(err)))
}

# Native PLS-SEM comparator: estimate the declared path model directly in seminr
# and read its standardized path coefficients. PLS composites carry measurement
# error into the structural model, so these paths are expected to attenuate.
.seminr_native_structural <- function(data, model, structure) {
  if (!requireNamespace("seminr", quietly = TRUE))
    return(list(available = FALSE, status = "skipped_not_installed", converged = NA, paths = NULL,
      runtime = NA_real_, version = NA_character_, error_message = NA_character_))
  constructs <- .structural_constructs(structure)
  measurement_model <- do.call(seminr::constructs, lapply(constructs, function(nm) {
    indicators <- model$constructs[[nm]]$indicators
    prefix <- sub("[0-9]+$", "", indicators[[1L]])
    numbers <- as.integer(sub("^.*?([0-9]+)$", "\\1", indicators))
    seminr::composite(nm, seminr::multi_items(prefix, numbers))
  }))
  edges <- list()
  for (outcome in names(structure$effects)) for (predictor in names(structure$effects[[outcome]]))
    edges[[length(edges) + 1L]] <- seminr::paths(from = predictor, to = outcome)
  structural_model <- do.call(seminr::relationships, edges)
  started <- proc.time()[["elapsed"]]
  tryCatch({
    fit <- seminr::estimate_pls(data = .mean_impute_data(data),
      measurement_model = measurement_model, structural_model = structural_model)
    pc <- fit$path_coef
    rows <- list()
    for (outcome in names(structure$effects)) for (predictor in names(structure$effects[[outcome]]))
      rows[[length(rows) + 1L]] <- data.frame(outcome = outcome, predictor = predictor,
        estimate = pc[predictor, outcome], se = NA_real_, stringsAsFactors = FALSE)
    list(available = TRUE, status = "success", converged = TRUE, paths = do.call(rbind, rows),
      runtime = proc.time()[["elapsed"]] - started,
      version = as.character(utils::packageVersion("seminr")), error_message = NA_character_)
  }, error = function(err) list(available = TRUE, status = "error", converged = FALSE, paths = NULL,
    runtime = proc.time()[["elapsed"]] - started, version = as.character(utils::packageVersion("seminr")),
    error_message = conditionMessage(err)))
}

# Build comparator rows for a native structural engine, matching the score-swap
# row schema. The native path is the engine's headline estimate; lavaan supplies
# an analytic interval from its standard error, seminr leaves the interval empty.
.native_structural_rows <- function(engine, native, package_name, truth_slopes, scenario,
                                     setting, replication, validation_fit) {
  build <- function(outcome, predictor, estimate, se, status, error_message) {
    truth <- truth_slopes$truth_slope[truth_slopes$outcome == outcome & truth_slopes$predictor == predictor]
    truth <- if (length(truth)) truth[[1L]] else NA_real_
    ci_low <- if (is.finite(se)) estimate - 1.96 * se else NA_real_
    ci_high <- if (is.finite(se)) estimate + 1.96 * se else NA_real_
    covers <- if (is.finite(ci_low) && is.finite(ci_high) && is.finite(truth)) truth >= ci_low && truth <= ci_high else NA
    shape_row <- data.frame(outcome = if (is.na(outcome)) "" else outcome,
      predictor = if (is.na(predictor)) "" else predictor, shape = "linear", stringsAsFactors = FALSE)
    cbind(data.frame(
      engine = engine, package_name = package_name, package_version = native$version,
      available = native$available, status = status, converged = isTRUE(native$converged),
      replication = replication, score_runtime_seconds = NA_real_,
      association_runtime_seconds = native$runtime, total_runtime_seconds = native$runtime,
      score_coverage = NA_real_, outcome = outcome, predictor = predictor, selected_shape = "linear",
      theory_r_squared = NA_real_, temporal_gap = NA_real_, unrestricted_gap = NA_real_,
      unrestricted_minus_temporal = NA_real_, selection_stability = NA_real_, edge_drop_mse_increase = NA_real_,
      truth_slope = truth, naive_estimate = estimate, corrected_estimate = NA_real_,
      predictor_reliability = NA_real_, corrected_ci_low = ci_low, corrected_ci_high = ci_high,
      eiv_applicable = FALSE, headline_estimate = estimate, structural_bias = abs(estimate - truth),
      ci_covers_truth = covers, shape_correct = .shape_correct(scenario, shape_row),
      measurement_converged = validation_fit$converged, fit_attempts = validation_fit$attempts,
      error_message = error_message, worker_pid = Sys.getpid(), stringsAsFactors = FALSE), setting)
  }
  if (!identical(native$status, "success") || is.null(native$paths)) {
    return(build(NA_character_, NA_character_, NA_real_, NA_real_, native$status, native$error_message))
  }
  do.call(rbind, lapply(seq_len(nrow(native$paths)), function(i) {
    p <- native$paths[i, , drop = FALSE]
    build(p$outcome[[1L]], p$predictor[[1L]], p$estimate[[1L]], p$se[[1L]], "success", NA_character_)
  }))
}

# True partial structural slopes implied by the data-generating latent states.
# For each declared outcome the slope of every predictor is the standardized
# multiple-regression coefficient on the known states; this is the unbiased
# target the errors-in-variables correction tries to recover.
.truth_slopes <- function(truth, structure) {
  states <- as.data.frame(scale(as.data.frame(truth)))
  rows <- list()
  for (outcome in names(structure$effects)) {
    predictors <- names(structure$effects[[outcome]])
    if (!all(c(outcome, predictors) %in% names(states))) next
    coefs <- stats::coef(stats::lm(stats::reformulate(predictors, outcome), data = states))
    for (predictor in predictors) {
      rows[[length(rows) + 1L]] <- data.frame(outcome = outcome, predictor = predictor,
        truth_slope = unname(coefs[[predictor]]), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) data.frame(outcome = character(), predictor = character(), truth_slope = numeric()) else do.call(rbind, rows)
}

# Engine headline structural slope: the disattenuated estimate when an
# errors-in-variables correction is available, otherwise the naive slope that a
# composite or PLS pipeline would report.
.headline_estimate <- function(selected) {
  corrected <- selected$corrected_estimate[[1L]]
  if (isTRUE(selected$eiv_applicable[[1L]]) && is.finite(corrected)) corrected else selected$naive_estimate[[1L]]
}

.ci_covers <- function(selected) {
  lo <- selected$corrected_ci_low[[1L]]; hi <- selected$corrected_ci_high[[1L]]; truth <- selected$truth_slope[[1L]]
  if (!is.finite(lo) || !is.finite(hi) || !is.finite(truth)) NA else (truth >= lo && truth <= hi)
}

# Whether the selected shape family matches the data-generating shape on the
# focal Trust -> Quality edge. Linear-only engines cannot score the nonlinear
# scenarios, which is the point of the comparison.
.shape_correct <- function(scenario, selected) {
  if (selected$outcome[[1L]] != "Quality" || selected$predictor[[1L]] != "Trust") return(NA)
  family <- if (grepl("^smooth", selected$shape[[1L]])) "smooth" else selected$shape[[1L]]
  switch(scenario,
    monotone_increasing = family == "monotone_increasing",
    monotone_decreasing = family == "monotone_decreasing",
    smooth_subtle = family == "smooth",
    smooth_strong = family == "smooth",
    # Monotone-nonlinear behavioral shapes: any non-linear detection counts,
    # since the point is that linear-only CB-SEM/PLS-SEM cannot represent them.
    plateau = family != "linear",
    threshold = family != "linear",
    diminishing = family != "linear",
    family == "linear")
}

.structural_comparator_one <- function(job) {
  setting <- as.data.frame(job$setting, stringsAsFactors = FALSE)
  generated <- do.call(.structural_validation_data, .structural_data_args(setting, job$seed))
  validation_fit <- .validation_fit(
    generated$model,
    generated$data,
    job$seed,
    job$folds,
    job$iterations,
    job$max_iterations,
    FALSE
  )
  truth_slopes <- .truth_slopes(generated$truth, generated$structure)
  engines <- c("cssem_locked", "ordinal_factor_proxy", "composite_proxy", "lavaan_dwls", "seminr_pls")
  rows <- list()
  index <- 0L
  for (engine in engines) {
    scored <- .scoring_engine_result(
      engine, generated, validation_fit, job$iterations, job$seed, job$replication
    )
    if (!identical(scored$status, "success")) {
      index <- index + 1L
      rows[[index]] <- cbind(
        data.frame(
          engine = scored$engine,
          package_name = scored$package_name,
          package_version = scored$package_version,
          available = scored$available,
          status = scored$status,
          converged = scored$converged,
          replication = job$replication,
          score_runtime_seconds = scored$score_runtime_seconds,
          association_runtime_seconds = NA_real_,
          total_runtime_seconds = scored$score_runtime_seconds,
          score_coverage = scored$score_coverage,
          outcome = NA_character_,
          predictor = NA_character_,
          selected_shape = NA_character_,
          theory_r_squared = NA_real_,
          temporal_gap = NA_real_,
          unrestricted_gap = NA_real_,
          unrestricted_minus_temporal = NA_real_,
          selection_stability = NA_real_,
          edge_drop_mse_increase = NA_real_,
          truth_slope = NA_real_,
          naive_estimate = NA_real_,
          corrected_estimate = NA_real_,
          predictor_reliability = NA_real_,
          corrected_ci_low = NA_real_,
          corrected_ci_high = NA_real_,
          eiv_applicable = NA,
          headline_estimate = NA_real_,
          structural_bias = NA_real_,
          ci_covers_truth = NA,
          shape_correct = NA,
          measurement_converged = validation_fit$converged,
          fit_attempts = validation_fit$attempts,
          error_message = scored$error_message,
          worker_pid = Sys.getpid(),
          stringsAsFactors = FALSE
        ),
        setting
      )
      next
    }
    association_elapsed <- system.time({
      pseudo_fit <- .make_pseudo_fit(scored$scores, validation_fit$fit$folds, scored$reliability, scored$score_posterior_sd)
      association <- cssem_associate(
        pseudo_fit,
        generated$structure,
        structural_repeats = job$structural_repeats,
        seed = job$seed,
        shadow_scope = "both",
        eiv_bootstrap = job$eiv_bootstrap,
        respondent_weighting = "none"
      )
    })["elapsed"]
    ledger <- cssem_effect_ledger(association)
    ledger <- merge(ledger, truth_slopes, by = c("outcome", "predictor"), all.x = TRUE, sort = FALSE)
    for (row_index in seq_len(nrow(ledger))) {
      selected <- ledger[row_index, , drop = FALSE]
      index <- index + 1L
      rows[[index]] <- cbind(
        data.frame(
          engine = scored$engine,
          package_name = scored$package_name,
          package_version = scored$package_version,
          available = scored$available,
          status = scored$status,
          converged = scored$converged,
          replication = job$replication,
          score_runtime_seconds = scored$score_runtime_seconds,
          association_runtime_seconds = unname(association_elapsed),
          total_runtime_seconds = scored$score_runtime_seconds + unname(association_elapsed),
          score_coverage = scored$score_coverage,
          outcome = selected$outcome[[1L]],
          predictor = selected$predictor[[1L]],
          selected_shape = selected$shape[[1L]],
          theory_r_squared = selected$theory_r_squared[[1L]],
          temporal_gap = selected$temporal_gap[[1L]],
          unrestricted_gap = selected$unrestricted_gap[[1L]],
          unrestricted_minus_temporal = selected$unrestricted_gap[[1L]] - selected$temporal_gap[[1L]],
          selection_stability = selected$selection_frequency[[1L]],
          edge_drop_mse_increase = selected$edge_drop_mse_increase[[1L]],
          truth_slope = selected$truth_slope[[1L]],
          naive_estimate = selected$naive_estimate[[1L]],
          corrected_estimate = selected$corrected_estimate[[1L]],
          predictor_reliability = selected$predictor_reliability[[1L]],
          corrected_ci_low = selected$corrected_ci_low[[1L]],
          corrected_ci_high = selected$corrected_ci_high[[1L]],
          eiv_applicable = selected$eiv_applicable[[1L]],
          headline_estimate = .headline_estimate(selected),
          structural_bias = abs(.headline_estimate(selected) - selected$truth_slope[[1L]]),
          ci_covers_truth = .ci_covers(selected),
          shape_correct = .shape_correct(setting$scenario, selected),
          measurement_converged = validation_fit$converged,
          fit_attempts = validation_fit$attempts,
          error_message = NA_character_,
          worker_pid = Sys.getpid(),
          stringsAsFactors = FALSE
        ),
        setting
      )
    }
  }
  # Native CB-SEM and PLS-SEM structural estimates: their own joint/path
  # coefficients, scored on the same truth-referenced bias and coverage metrics.
  lavaan_native <- .lavaan_native_structural(generated$data, generated$model, generated$structure)
  index <- index + 1L
  rows[[index]] <- .native_structural_rows("lavaan_native", lavaan_native, "lavaan", truth_slopes,
    setting$scenario, setting, job$replication, validation_fit)
  seminr_native <- .seminr_native_structural(generated$data, generated$model, generated$structure)
  index <- index + 1L
  rows[[index]] <- .native_structural_rows("seminr_native", seminr_native, "seminr", truth_slopes,
    setting$scenario, setting, job$replication, validation_fit)
  do.call(rbind, rows)
}

.cv_linear_metrics <- function(predictor, outcome, folds) {
  keep <- is.finite(predictor) & is.finite(outcome)
  predictor <- predictor[keep]
  outcome <- outcome[keep]
  folds <- folds[keep]
  if (length(outcome) < 8L || length(unique(folds)) < 2L) {
    return(c(rmse = NA_real_, r_squared = NA_real_))
  }
  prediction <- rep(NA_real_, length(outcome))
  for (fold in sort(unique(folds))) {
    train <- folds != fold
    test <- folds == fold
    if (sum(train) < 4L || sum(test) < 1L) next
    train_data <- data.frame(outcome = outcome[train], predictor = predictor[train])
    fit <- stats::lm(outcome ~ predictor, data = train_data)
    prediction[test] <- stats::predict(fit, newdata = data.frame(predictor = predictor[test]))
  }
  keep_pred <- is.finite(prediction) & is.finite(outcome)
  if (!any(keep_pred)) return(c(rmse = NA_real_, r_squared = NA_real_))
  sse <- sum((outcome[keep_pred] - prediction[keep_pred])^2)
  sst <- sum((outcome[keep_pred] - mean(outcome[keep_pred]))^2)
  c(
    rmse = sqrt(mean((outcome[keep_pred] - prediction[keep_pred])^2)),
    r_squared = if (sst > 0) 1 - sse / sst else NA_real_
  )
}

.downstream_metrics <- function(scores, truth, seed, folds = 5L) {
  scores <- as.data.frame(scores)
  truth <- as.data.frame(truth)
  matched <- intersect(c("A", "B"), intersect(names(scores), names(truth)))
  if (length(matched) < 2L) return(c(rmse = NA_real_, r_squared = NA_real_))
  set.seed(seed + 5000L)
  fold_id <- sample(rep(seq_len(min(as.integer(folds), nrow(scores))), length.out = nrow(scores)))
  ab <- .cv_linear_metrics(scores[["A"]], truth[["B"]], fold_id)
  ba <- .cv_linear_metrics(scores[["B"]], truth[["A"]], fold_id)
  rmse_values <- c(ab[["rmse"]], ba[["rmse"]])
  r2_values <- c(ab[["r_squared"]], ba[["r_squared"]])
  c(
    rmse = if (all(is.na(rmse_values))) NA_real_ else mean(rmse_values, na.rm = TRUE),
    r_squared = if (all(is.na(r2_values))) NA_real_ else mean(r2_values, na.rm = TRUE)
  )
}

.lavaan_scored_truth <- function(fit, truth) {
  case_idx <- lavaan::lavInspect(fit, "case.idx")
  truth <- as.data.frame(truth)
  if (is.null(case_idx) || !length(case_idx)) return(truth)
  truth[case_idx, , drop = FALSE]
}

.lavaan_comparator_row <- function(data, truth, setting, replication) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    return(.comparator_row(
      engine = "lavaan_dwls",
      package_name = "lavaan",
      package_version = NA_character_,
      available = FALSE,
      status = "skipped_not_installed",
      converged = NA,
      runtime_seconds = NA_real_,
      recovery = NA_real_,
      downstream = c(rmse = NA_real_, r_squared = NA_real_),
      setting = setting,
      replication = replication
    ))
  }
  syntax <- paste(
    "A =~", paste(paste0("a", 1:4), collapse = " + "),
    "\nB =~", paste(paste0("b", 1:4), collapse = " + ")
  )
  started <- proc.time()[["elapsed"]]
  result <- tryCatch({
    fit <- lavaan::cfa(
      model = syntax,
      data = data,
      ordered = names(data),
      estimator = "WLSMV",
      std.lv = TRUE
    )
    scores <- lavaan::lavPredict(fit)
    scored_truth <- .lavaan_scored_truth(fit, truth)
    list(
      status = "success",
      converged = isTRUE(lavaan::lavInspect(fit, "converged")),
      recovery = .score_recovery(scores, scored_truth),
      downstream = .downstream_metrics(scores, scored_truth, seed = replication + 6000L),
      error_message = NA_character_
    )
  }, error = function(err) {
    list(
      status = "error",
      converged = FALSE,
      recovery = NA_real_,
      downstream = c(rmse = NA_real_, r_squared = NA_real_),
      error_message = conditionMessage(err)
    )
  })
  .comparator_row(
    engine = "lavaan_dwls",
    package_name = "lavaan",
    package_version = as.character(utils::packageVersion("lavaan")),
    available = TRUE,
    status = result$status,
    converged = result$converged,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    recovery = result$recovery,
    downstream = result$downstream,
    error_message = result$error_message,
    setting = setting,
    replication = replication
  )
}

.mean_impute_data <- function(data) {
  out <- as.data.frame(data)
  for (name in names(out)) {
    missing <- is.na(out[[name]])
    if (any(missing)) out[[name]][missing] <- mean(out[[name]], na.rm = TRUE)
  }
  out
}

.seminr_comparator_row <- function(data, truth, setting, replication) {
  if (!requireNamespace("seminr", quietly = TRUE)) {
    return(.comparator_row(
      engine = "seminr_pls",
      package_name = "seminr",
      package_version = NA_character_,
      available = FALSE,
      status = "skipped_not_installed",
      converged = NA,
      runtime_seconds = NA_real_,
      recovery = NA_real_,
      downstream = c(rmse = NA_real_, r_squared = NA_real_),
      setting = setting,
      replication = replication
    ))
  }
  started <- proc.time()[["elapsed"]]
  result <- tryCatch({
    imputed <- .mean_impute_data(data)
    measurement_model <- seminr::constructs(
      seminr::composite("A", seminr::multi_items("a", 1:4)),
      seminr::composite("B", seminr::multi_items("b", 1:4))
    )
    # seminr 2.5.0 does not accept an empty relationships() specification for
    # measurement-only scoring, so use the smallest valid path model to obtain
    # comparator construct scores.
    structural_model <- seminr::relationships(
      seminr::paths(from = "A", to = "B")
    )
    fit <- seminr::estimate_pls(
      data = imputed,
      measurement_model = measurement_model,
      structural_model = structural_model
    )
    scores <- fit$construct_scores
    list(
      status = "success",
      converged = TRUE,
      recovery = .score_recovery(scores, truth),
      downstream = .downstream_metrics(scores, truth, seed = replication + 7000L),
      error_message = NA_character_
    )
  }, error = function(err) {
    list(
      status = "error",
      converged = FALSE,
      recovery = NA_real_,
      downstream = c(rmse = NA_real_, r_squared = NA_real_),
      error_message = conditionMessage(err)
    )
  })
  .comparator_row(
    engine = "seminr_pls",
    package_name = "seminr",
    package_version = as.character(utils::packageVersion("seminr")),
    available = TRUE,
    status = result$status,
    converged = result$converged,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    recovery = result$recovery,
    downstream = result$downstream,
    error_message = result$error_message,
    setting = setting,
    replication = replication
  )
}
