# Reusable, explicit resampling infrastructure.

.bootstrap_scalar_integer <- function(value, name, minimum = 1L) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
      value != as.integer(value) || value < minimum)
    stop(sprintf("%s must be a positive whole-number.", name), call. = FALSE)
  as.integer(value)
}

.bootstrap_statistic_vector <- function(value) {
  if (is.data.frame(value)) {
    if (nrow(value) != 1L || !ncol(value))
      stop("statistic must return a named numeric vector or a one-row data frame.", call. = FALSE)
    if (!all(vapply(value, is.numeric, logical(1))))
      stop("Every statistic column must be numeric.", call. = FALSE)
    value <- unlist(value[1L, , drop = FALSE], use.names = TRUE)
  }
  if (!is.numeric(value) || !length(value) || is.null(names(value)) ||
      any(!nzchar(names(value))) || anyDuplicated(names(value)))
    stop("statistic must return a non-empty named numeric vector.", call. = FALSE)
  stats::setNames(as.numeric(value), names(value))
}

.bootstrap_context <- function(fit, indices, refit, replicate, seed, settings) {
  raw <- fit$data[indices, , drop = FALSE]
  if (refit == "locked_scores") {
    scored_fit <- fit
    scores <- fit$locked_scores[indices, , drop = FALSE]
  } else {
    scored_fit <- do.call(.fit_states_quiet, c(list(
      model = fit$model, data = raw, seed = seed, draws = 0L,
      iterations = settings$iterations, tolerance = settings$tolerance,
      quadrature = settings$quadrature, diagnostics = FALSE,
      preset = settings$preset), list()))
    scores <- scored_fit$locked_scores
  }
  list(fit = scored_fit, scores = scores, data = raw, indices = indices,
    replicate = replicate, seed = seed, refit = refit)
}

.bootstrap_one <- function(job, fit, statistic, refit, settings) {
  set.seed(job$seed)
  indices <- sample.int(nrow(fit$data), nrow(fit$data), replace = TRUE)
  result <- tryCatch({
    context <- .bootstrap_context(fit, indices, refit, job$replicate, job$seed, settings)
    list(replicate = job$replicate, seed = job$seed, status = "success",
      failure_reason = "", values = .bootstrap_statistic_vector(statistic(context)))
  }, error = function(e) list(replicate = job$replicate, seed = job$seed,
    status = "failed", failure_reason = conditionMessage(e), values = NULL))
  result
}

.bootstrap_validate_level <- function(level) {
  if (length(level) != 1L || !is.numeric(level) || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be a number strictly between zero and one.", call. = FALSE)
  as.numeric(level)
}

.bootstrap_settings <- function(fit) {
  settings <- fit$fit_settings
  if (is.null(settings)) settings <- list()
  settings$iterations <- if (is.null(settings$iterations)) 15L else settings$iterations
  settings$tolerance <- if (is.null(settings$tolerance)) 1e-3 else settings$tolerance
  settings$quadrature <- if (is.null(settings$quadrature)) seq(-4, 4, length.out = 31L) else settings$quadrature
  settings$preset <- if (is.null(settings$preset)) "default" else settings$preset
  settings
}

.bootstrap_summary <- function(draws, point_estimate, replicates, level) {
  names <- colnames(draws)
  success <- replicates$status == "success"
  rows <- lapply(seq_along(names), function(j) {
    values <- draws[success, j]
    values <- values[is.finite(values)]
    interval <- if (length(values)) stats::quantile(values,
      probs = c((1 - level) / 2, (1 + level) / 2), names = FALSE, na.rm = TRUE,
      type = 7) else c(NA_real_, NA_real_)
    data.frame(parameter = names[[j]], estimate = unname(point_estimate[[j]]),
      ci_low = interval[[1L]], ci_high = interval[[2L]], level = level,
      method = "percentile", successful_replicates = length(values),
      failure_count = sum(!success), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Run an explicit, reusable bootstrap over a fitted CS-SEM object
#'
#' The statistic callback receives a context for each resampled data set and
#' must return a named numeric vector. The default `locked_scores` mode keeps
#' the fitted measurement encoders fixed and resamples their locked scores;
#' `measurement` refits the measurement encoder for every replicate. Shape
#' selection and any downstream estimand are therefore controlled by the
#' callback instead of being silently changed by this helper.
#'
#' @param fit A `fit_states` object, including the original `data`.
#' @param statistic A function taking one bootstrap context and returning a
#'   named numeric vector (or one-row numeric data frame).
#' @param reps Number of bootstrap replicates.
#' @param level Confidence level used by [confint()] and `summary()`.
#' @param seed Integer seed used to derive the deterministic replicate seeds.
#' @param refit Whether to resample `locked_scores` or refit the measurement
#'   encoder for each replicate.
#' @param workers Number of worker processes. Replicate seeds are deterministic
#'   across worker counts; values above one require the installed package to be
#'   available to the workers.
#' @param resume An earlier `cssem_bootstrap` object with the same `seed`,
#'   `level`, and `refit`, used to continue a partially completed run.
#' @param progress Reserved for a future progress display; accepted for API
#'   stability and does not alter results.
#' @return An object of class `cssem_bootstrap` containing replicate draws,
#'   statuses, failure reasons, summary intervals, and reproducibility metadata.
#' @export
bootstrap_model <- function(fit, statistic, reps = 200L, level = .95, seed = 1L,
                            refit = c("locked_scores", "measurement"), workers = 1L,
                            resume = NULL, progress = FALSE) {
  .preserve_seed()
  if (!inherits(fit, "fit_states")) stop("fit must be a fit_states object.", call. = FALSE)
  if (!is.function(statistic)) stop("statistic must be a function.", call. = FALSE)
  if (!is.data.frame(fit$data) || nrow(fit$data) < 2L)
    stop("fit must retain at least two rows of original data.", call. = FALSE)
  reps <- .bootstrap_scalar_integer(reps, "reps")
  level <- .bootstrap_validate_level(level)
  seed <- .bootstrap_scalar_integer(seed, "seed", minimum = 0L)
  workers <- .bootstrap_scalar_integer(workers, "workers")
  refit <- match.arg(refit)
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress))
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  settings <- .bootstrap_settings(fit)

  prior <- NULL
  if (!is.null(resume)) {
    if (!inherits(resume, "cssem_bootstrap")) stop("resume must be a cssem_bootstrap object.", call. = FALSE)
    if (!identical(as.integer(resume$seed), seed) || !identical(as.numeric(resume$level), level) ||
        !identical(resume$refit, refit))
      stop("resume must use the same seed, level, and refit mode.", call. = FALSE)
    if (reps < nrow(resume$replicates)) stop("reps cannot be smaller than the completed resume object.", call. = FALSE)
    prior <- resume
  }

  completed <- if (is.null(prior)) 0L else nrow(prior$replicates)
  point_context <- list(fit = fit, scores = fit$locked_scores, data = fit$data,
    indices = seq_len(nrow(fit$data)), replicate = 0L, seed = seed, refit = refit)
  point <- .bootstrap_statistic_vector(statistic(point_context))
  metric_names <- names(point)
  draws <- matrix(NA_real_, nrow = reps, ncol = length(point),
    dimnames = list(as.character(seq_len(reps)), metric_names))
  replicate_rows <- data.frame(replicate = seq_len(reps), seed = seed + seq_len(reps) - 1L,
    status = rep("pending", reps), failure_reason = rep(NA_character_, reps),
    stringsAsFactors = FALSE)
  if (!is.null(prior)) {
    if (!identical(colnames(prior$draws), metric_names))
      stop("resume statistic output does not match the original metric names.", call. = FALSE)
    draws[seq_len(completed), ] <- prior$draws
    replicate_rows[seq_len(completed), ] <- prior$replicates
  }
  if (completed < reps) {
    jobs <- lapply(seq.int(completed + 1L, reps), function(i)
      list(replicate = i, seed = seed + i - 1L))
    results <- if (workers == 1L) {
      lapply(jobs, .bootstrap_one, fit = fit, statistic = statistic,
        refit = refit, settings = settings)
    } else {
      cl <- parallel::makeCluster(workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, {
        if (!requireNamespace("cssem", quietly = TRUE))
          stop("workers > 1 requires the installed cssem package.")
        NULL
      })
      parallel::parLapply(cl, jobs, function(job, fit, statistic, refit, settings)
        cssem:::.bootstrap_one(job, fit, statistic, refit, settings),
        fit = fit, statistic = statistic, refit = refit, settings = settings)
    }
    for (result in results) {
      i <- result$replicate
      replicate_rows$status[[i]] <- result$status
      replicate_rows$failure_reason[[i]] <- if (result$status == "failed") result$failure_reason else ""
      if (identical(result$status, "success")) {
        if (!identical(names(result$values), metric_names)) {
          replicate_rows$status[[i]] <- "failed"
          replicate_rows$failure_reason[[i]] <- "statistic returned different metric names."
        } else draws[i, ] <- result$values
      }
    }
  }
  if (any(replicate_rows$status == "pending")) stop("bootstrap contains incomplete replicates.", call. = FALSE)
  failed <- replicate_rows[replicate_rows$status == "failed", , drop = FALSE]
  result <- list(draws = draws, point_estimate = point, summary = .bootstrap_summary(
    draws, point, replicate_rows, level), replicates = replicate_rows, failed = failed,
    successful_replicates = sum(replicate_rows$status == "success"),
    failure_count = sum(replicate_rows$status == "failed"), reps = reps, level = level,
    seed = seed, refit = refit, workers = workers,
    components = if (refit == "measurement") "measurement_encoder_refit" else "locked_scores_only",
    statistic = statistic, progress = progress)
  class(result) <- c("cssem_bootstrap", "list")
  result
}

#' @export
summary.cssem_bootstrap <- function(object, ...) {
  structure(list(summary = object$summary, successful_replicates = object$successful_replicates,
    failure_count = object$failure_count, failed = object$failed, level = object$level,
    refit = object$refit), class = c("summary.cssem_bootstrap", "summary"))
}

#' @export
print.cssem_bootstrap <- function(x, ...) {
  cat("CS-SEM bootstrap: ", x$successful_replicates, "/", x$reps,
    " successful replicates (", x$refit, ", level ", x$level, ")\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
print.summary.cssem_bootstrap <- function(x, ...) {
  cat("CS-SEM bootstrap summary: ", x$successful_replicates,
    " successful, ", x$failure_count, " failed; level ", x$level, "\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
confint.cssem_bootstrap <- function(object, parm, level = object$level, ...) {
  level <- .bootstrap_validate_level(level)
  if (missing(parm) || is.null(parm)) parm <- object$summary$parameter
  index <- if (is.numeric(parm)) as.integer(parm) else match(as.character(parm), object$summary$parameter)
  if (anyNA(index) || any(index < 1L) || any(index > nrow(object$summary)))
    stop("Unknown bootstrap parameter in `parm`.", call. = FALSE)
  draws <- object$draws[, index, drop = FALSE]
  summary <- .bootstrap_summary(draws, object$point_estimate[index], object$replicates, level)
  out <- cbind(summary$ci_low, summary$ci_high)
  colnames(out) <- c(paste0((1 - level) / 2 * 100, " %"), paste0((1 + level) / 2 * 100, " %"))
  rownames(out) <- summary$parameter
  out
}
