.study_rate_row <- function(metric, scenario, n, estimand, scope, numerator,
                            denominator, planned_reps, level, null_value = NA_real_) {
  available <- denominator > 0L
  estimate <- if (available) numerator / denominator else NA_real_
  mcse <- if (available) sqrt(estimate * (1 - estimate) / denominator) else NA_real_
  bounds <- c(NA_real_, NA_real_)
  if (available) {
    z <- stats::qnorm(1 - (1 - level) / 2)
    scale <- 1 + z^2 / denominator
    center <- (estimate + z^2 / (2 * denominator)) / scale
    half <- z * sqrt(estimate * (1 - estimate) / denominator +
      z^2 / (4 * denominator^2)) / scale
    bounds <- c(center - half, center + half)
  }
  data.frame(metric = metric, scenario = scenario, n = n, estimand = estimand,
    scope = scope, estimate = estimate, numerator = as.integer(numerator),
    n_valid = as.integer(denominator), denominator = as.integer(denominator),
    planned_reps = as.integer(planned_reps), mcse = mcse,
    mc_lower = bounds[[1L]], mc_upper = bounds[[2L]],
    availability_status = if (available) "available" else "unavailable",
    null_value = null_value, stringsAsFactors = FALSE)
}

.study_unavailable_rate_row <- function(metric, scenario, n, estimand, scope,
                                        planned_reps) {
  data.frame(metric = metric, scenario = scenario, n = n, estimand = estimand,
    scope = scope, estimate = NA_real_, numerator = NA_integer_, n_valid = 0L,
    denominator = NA_integer_, planned_reps = as.integer(planned_reps),
    mcse = NA_real_, mc_lower = NA_real_, mc_upper = NA_real_,
    availability_status = "unavailable", null_value = NA_real_,
    stringsAsFactors = FALSE)
}

.study_mean_row <- function(metric, scenario, n, estimand, values, planned_reps, level) {
  values <- values[is.finite(values)]
  n_valid <- length(values)
  available <- n_valid > 0L
  estimate <- if (available) mean(values) else NA_real_
  mcse <- if (n_valid > 1L) stats::sd(values) / sqrt(n_valid) else NA_real_
  bounds <- c(NA_real_, NA_real_)
  if (n_valid > 1L) {
    half <- stats::qt(1 - (1 - level) / 2, df = n_valid - 1L) * mcse
    bounds <- estimate + c(-half, half)
  }
  data.frame(metric = metric, scenario = scenario, n = n, estimand = estimand,
    scope = "conditional", estimate = estimate, numerator = NA_integer_,
    n_valid = as.integer(n_valid), denominator = NA_integer_,
    planned_reps = as.integer(planned_reps), mcse = mcse,
    mc_lower = bounds[[1L]], mc_upper = bounds[[2L]],
    availability_status = if (available) "available" else "unavailable",
    null_value = NA_real_, stringsAsFactors = FALSE)
}

.study_rmse_row <- function(scenario, n, estimand, errors, planned_reps, level) {
  errors <- errors[is.finite(errors)]
  n_valid <- length(errors)
  available <- n_valid > 0L
  squared_errors <- errors^2
  mse <- if (available) mean(squared_errors) else NA_real_
  rmse <- if (available) sqrt(mse) else NA_real_
  mcse <- NA_real_
  bounds <- c(NA_real_, NA_real_)
  if (available && all(squared_errors == 0)) {
    mcse <- 0
    bounds <- c(0, 0)
  } else if (n_valid > 1L) {
    mse_se <- stats::sd(squared_errors) / sqrt(n_valid)
    mcse <- mse_se / (2 * sqrt(mse))
    half <- stats::qt(1 - (1 - level) / 2, df = n_valid - 1L) * mse_se
    bounds <- sqrt(pmax(0, mse + c(-half, half)))
  }
  data.frame(metric = "rmse", scenario = scenario, n = n, estimand = estimand,
    scope = "conditional", estimate = rmse, numerator = NA_integer_,
    n_valid = as.integer(n_valid), denominator = NA_integer_,
    planned_reps = as.integer(planned_reps), mcse = mcse,
    mc_lower = bounds[[1L]], mc_upper = bounds[[2L]],
    availability_status = if (available) "available" else "unavailable",
    null_value = NA_real_, stringsAsFactors = FALSE)
}

.study_validate_null_values <- function(null_values, estimands) {
  if (is.null(null_values)) return(setNames(numeric(), character()))
  valid <- is.numeric(null_values) && is.null(dim(null_values)) &&
    length(null_values) > 0L && !is.null(names(null_values)) &&
    !anyNA(names(null_values)) && all(nzchar(names(null_values))) &&
    !anyDuplicated(names(null_values)) && all(is.finite(null_values))
  if (!valid) {
    stop("null_values must be a finite named numeric vector with unique estimand names.",
      call. = FALSE)
  }
  if (any(!names(null_values) %in% estimands)) {
    stop("null_values names must match estimands in the simulation.", call. = FALSE)
  }
  null_values
}

.study_validate_simulation <- function(simulation) {
  if (!inherits(simulation, "cssem_simulation") || !is.data.frame(simulation$replications)) {
    stop("simulation must be a cssem_simulation object with a replications data frame.",
      call. = FALSE)
  }
  required <- c("scenario", "n", "replication", "estimand", "truth", "estimate",
    "estimate_status", "lower", "upper", "interval_status", "converged", "run_status")
  if (!all(required %in% names(simulation$replications)) ||
      !nrow(simulation$replications)) {
    stop("simulation$replications is empty or missing required columns.", call. = FALSE)
  }
  rows <- simulation$replications
  if (!is.character(rows$scenario) || !is.character(rows$estimand) ||
      !is.numeric(rows$n) || !is.numeric(rows$replication) || !is.numeric(rows$truth) ||
      !is.numeric(rows$estimate) || !is.numeric(rows$lower) || !is.numeric(rows$upper) ||
      !is.logical(rows$converged) ||
      anyNA(rows$scenario) || anyNA(rows$estimand) || anyNA(rows$n) ||
      anyNA(rows$replication) || any(!nzchar(rows$scenario)) || any(!nzchar(rows$estimand)) ||
      any(!is.finite(rows$n)) || any(rows$n < 1 | rows$n != floor(rows$n)) ||
      any(!is.finite(rows$replication)) || any(rows$replication < 1 | rows$replication != floor(rows$replication)) ||
      any(!is.finite(rows$truth))) {
    stop("simulation replications must have scenario, estimand, n, replication, and finite truth values.",
      call. = FALSE)
  }
  key <- rows[c("scenario", "n", "replication", "estimand")]
  if (anyDuplicated(key)) {
    stop("simulation replications must contain one row per scenario, n, replication, and estimand.",
      call. = FALSE)
  }
  if (!is.character(rows$estimate_status) || !is.character(rows$interval_status) ||
      !is.character(rows$run_status) || anyNA(rows$estimate_status) || anyNA(rows$interval_status) ||
      anyNA(rows$run_status) || any(!rows$estimate_status %in% c("available", "unavailable")) ||
      any(!rows$interval_status %in% c("available", "unavailable")) ||
      any(!rows$run_status %in% c("completed", "partial", "generation_failed", "analysis_failed"))) {
    stop("simulation replication statuses are invalid.", call. = FALSE)
  }
  estimate_available <- rows$estimate_status == "available"
  interval_available <- rows$interval_status == "available"
  if (any(!is.finite(rows$estimate[estimate_available])) ||
      any(!is.na(rows$estimate[!estimate_available])) ||
      any(!is.finite(rows$lower[interval_available])) ||
      any(!is.finite(rows$upper[interval_available])) ||
      any(rows$lower[interval_available] > rows$upper[interval_available]) ||
      any(!is.na(rows$lower[!interval_available])) ||
      any(!is.na(rows$upper[!interval_available])) ||
      any(interval_available & !estimate_available)) {
    stop("simulation estimate and interval values do not match their availability statuses.",
      call. = FALSE)
  }
  replication_keys <- unique(rows[c("scenario", "n", "replication")])
  for (i in seq_len(nrow(replication_keys))) {
    selected <- rows$scenario == replication_keys$scenario[[i]] &
      rows$n == replication_keys$n[[i]] & rows$replication == replication_keys$replication[[i]]
    if (length(unique(rows$run_status[selected])) != 1L ||
        length(unique(rows$converged[selected])) != 1L) {
      stop("run status and convergence must agree across estimands in a replication.",
        call. = FALSE)
    }
  }
  rows
}

.study_summarize_group <- function(rows, scenario, n, null_values, level) {
  replication_rows <- rows[!duplicated(rows[c("replication")]), , drop = FALSE]
  planned_reps <- nrow(replication_rows)
  estimands <- unique(rows$estimand)
  output <- list()
  for (estimand in estimands) {
    item <- rows[rows$estimand == estimand, , drop = FALSE]
    if (anyDuplicated(item$replication) || nrow(item) != planned_reps) {
      stop("each estimand must have exactly one row for every scheduled replication.",
        call. = FALSE)
    }
    valid_estimate <- item$estimate_status == "available" & is.finite(item$estimate)
    errors <- item$estimate[valid_estimate] - item$truth[valid_estimate]
    output[[length(output) + 1L]] <- .study_mean_row("bias", scenario, n, estimand,
      errors, planned_reps, level)
    output[[length(output) + 1L]] <- .study_rmse_row(scenario, n, estimand,
      errors, planned_reps, level)
    valid_interval <- item$interval_status == "available" &
      is.finite(item$lower) & is.finite(item$upper)
    widths <- item$upper[valid_interval] - item$lower[valid_interval]
    output[[length(output) + 1L]] <- .study_mean_row("mean_interval_width", scenario,
      n, estimand, widths, planned_reps, level)
    covered <- valid_interval & item$lower <= item$truth & item$upper >= item$truth
    failed_analysis <- item$run_status %in% c("generation_failed", "analysis_failed")
    conditional_denominator <- sum(valid_interval)
    for (scope in c("conditional", "unconditional")) {
      denominator <- if (scope == "conditional") conditional_denominator else planned_reps
      numerator <- if (scope == "conditional") sum(covered) else sum(covered & !failed_analysis)
      output[[length(output) + 1L]] <- .study_rate_row("coverage", scenario, n,
        estimand, scope, numerator, denominator, planned_reps, level)
    }
    has_null <- estimand %in% names(null_values)
    if (!has_null) {
      output[[length(output) + 1L]] <- .study_unavailable_rate_row("detection",
        scenario, n, estimand, "conditional", planned_reps)
      output[[length(output) + 1L]] <- .study_unavailable_rate_row("detection",
        scenario, n, estimand, "unconditional", planned_reps)
    } else {
      null_value <- unname(null_values[[estimand]])
      detected <- valid_interval & (item$lower > null_value | item$upper < null_value)
      for (scope in c("conditional", "unconditional")) {
        denominator <- if (scope == "conditional") conditional_denominator else planned_reps
        numerator <- if (scope == "conditional") sum(detected) else sum(detected & !failed_analysis)
        output[[length(output) + 1L]] <- .study_rate_row("detection", scenario, n,
          estimand, scope, numerator, denominator, planned_reps, level, null_value)
      }
    }
    output[[length(output) + 1L]] <- .study_rate_row("interval_availability", scenario,
      n, estimand, "unconditional", sum(valid_interval), planned_reps,
      planned_reps, level)
  }

  conditional_convergence <- !is.na(replication_rows$converged)
  conditional_denominator <- sum(conditional_convergence)
  for (scope in c("conditional", "unconditional")) {
    denominator <- if (scope == "conditional") conditional_denominator else planned_reps
    numerator <- if (scope == "conditional") {
      sum(replication_rows$converged %in% TRUE)
    } else {
      sum(replication_rows$converged %in% TRUE &
        !(replication_rows$run_status %in% c("generation_failed", "analysis_failed")))
    }
    output[[length(output) + 1L]] <- .study_rate_row("convergence", scenario, n,
      "*", scope, numerator, denominator, planned_reps, level)
  }
  failure <- replication_rows$run_status %in% c("generation_failed", "analysis_failed")
  output[[length(output) + 1L]] <- .study_rate_row("failure", scenario, n, "*",
    "unconditional", sum(failure), planned_reps, planned_reps, level)
  partial <- replication_rows$run_status == "partial"
  output[[length(output) + 1L]] <- .study_rate_row("partial", scenario, n, "*",
    "unconditional", sum(partial), planned_reps, planned_reps, level)
  do.call(rbind, output)
}

#' Summarize simulation-study operating characteristics
#'
#' @param simulation A `cssem_simulation` object returned by [simulate_study()].
#' @param null_values Optional finite named numeric vector giving a null value
#'   for each estimand whose detection rate should be summarized.
#' @param level Confidence level for Monte Carlo uncertainty intervals.
#'
#' @return A `cssem_study_summary` object with a `metrics` data frame containing
#'   point estimates, valid counts, planned denominators, Monte Carlo standard
#'   errors, uncertainty bounds, scopes, and metric availability.
#' @export
summarize_study <- function(simulation, null_values = NULL, level = .95) {
  valid_level <- is.numeric(level) && is.null(dim(level)) && length(level) == 1L &&
    is.finite(level) && level > 0 && level < 1
  if (!valid_level) {
    stop("level must be a finite confidence level strictly between zero and one.",
      call. = FALSE)
  }
  level <- as.numeric(level)
  rows <- .study_validate_simulation(simulation)
  estimands <- unique(rows$estimand)
  null_values <- .study_validate_null_values(null_values, estimands)
  groups <- unique(rows[c("scenario", "n")])
  metrics <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    selected <- rows[rows$scenario == groups$scenario[[i]] & rows$n == groups$n[[i]], , drop = FALSE]
    .study_summarize_group(selected, groups$scenario[[i]], groups$n[[i]], null_values, level)
  }))
  rownames(metrics) <- NULL
  structure(list(metrics = metrics, scenarios = simulation$scenarios,
    sample_sizes = simulation$sample_sizes, reps = simulation$reps,
    seed = simulation$seed, metadata = simulation$metadata,
    callback_labels = simulation$callback_labels, level = level),
    class = "cssem_study_summary")
}
