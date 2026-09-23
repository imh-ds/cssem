# Reusable, explicit resampling infrastructure.

.bootstrap_scalar_integer <- function(value, name, minimum = 1L) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
      value != as.integer(value) || value < minimum) {
    requirement <- if (minimum == 0L) "a non-negative whole-number" else "a positive whole-number"
    stop(sprintf("%s must be %s.", name, requirement), call. = FALSE)
  }
  as.integer(value)
}

.resolve_bootstrap_clusters <- function(fit, cluster = NULL) {
  input_data <- if (!is.null(fit$input_data)) fit$input_data else fit$data
  retained_ids <- if (!is.null(fit$row_ids)) as.integer(fit$row_ids) else seq_len(nrow(fit$data))
  if (is.null(cluster)) {
    ids <- fit$cluster_ids
    if (is.null(ids)) stop("resample = \"cluster\" requires cluster labels from `cluster` or the fitted object.", call. = FALSE)
    .validate_cluster_labels(ids, nrow(fit$data), "cluster")
    return(ids)
  }
  if (length(cluster) == 1L && is.character(cluster) && cluster %in% names(input_data)) {
    ids <- input_data[[cluster]][retained_ids]
  } else if (length(cluster) == nrow(input_data)) {
    ids <- cluster[retained_ids]
  } else if (length(cluster) == nrow(fit$data)) {
    ids <- cluster
  } else {
    stop("cluster must be a column name or a vector aligned to the fitted rows or original input rows.", call. = FALSE)
  }
  .validate_cluster_labels(ids, nrow(fit$data), "cluster")
  ids
}

.cluster_bootstrap_indices <- function(cluster_ids) {
  .validate_cluster_labels(cluster_ids)
  units <- unique(cluster_ids)
  if (length(units) < 2L) stop("Cluster bootstrap requires at least two independent units.", call. = FALSE)
  sampled <- units[sample.int(length(units), length(units), replace = TRUE)]
  draw_ids <- rep(paste0("draw_", seq_along(sampled)), vapply(sampled, function(unit) sum(cluster_ids == unit), integer(1)))
  indices <- unlist(lapply(sampled, function(unit) which(cluster_ids == unit)), use.names = FALSE)
  source_ids <- as.character(sampled)
  row_source_ids <- rep(source_ids, vapply(sampled, function(unit) sum(cluster_ids == unit), integer(1)))
  list(indices = as.integer(indices), draw_unit_ids = unique(draw_ids),
    row_draw_ids = draw_ids, source_unit_ids = source_ids,
    row_source_ids = row_source_ids)
}

.bootstrap_unit_metadata <- function(cluster_ids, indices, draw_unit_ids = NULL,
                                     source_unit_ids = NULL) {
  if (is.null(cluster_ids)) return(list(original_unit_n = NA_integer_,
    resampled_unit_n = NA_integer_, resampled_row_n = length(indices),
    resampled_draw_n = NA_integer_,
    rows_per_unit = integer(), draw_unit_ids = character(), source_unit_ids = character(),
    row_draw_ids = character(), row_source_ids = character()))
  if (is.null(draw_unit_ids)) draw_unit_ids <- as.character(cluster_ids[indices])
  if (is.null(source_unit_ids)) source_unit_ids <- as.character(cluster_ids[indices])
  counts <- table(factor(draw_unit_ids, levels = unique(draw_unit_ids)))
  list(original_unit_n = as.integer(length(unique(cluster_ids))),
    resampled_unit_n = as.integer(length(unique(source_unit_ids))),
    resampled_row_n = as.integer(length(indices)),
    resampled_draw_n = as.integer(length(unique(draw_unit_ids))),
    rows_per_unit = as.integer(counts), draw_unit_ids = as.character(unique(draw_unit_ids)),
    source_unit_ids = as.character(source_unit_ids), row_draw_ids = as.character(draw_unit_ids),
    row_source_ids = as.character(cluster_ids[indices]))
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
      any(!nzchar(names(value))) || anyDuplicated(names(value)) || any(!is.finite(value)))
    stop("statistic must return a non-empty named numeric vector.", call. = FALSE)
  stats::setNames(as.numeric(value), names(value))
}

.bootstrap_measurement_split <- function(fit, indices, settings) {
  if (is.null(settings$split)) return(list(model = fit$model, split = NULL))
  source <- fit$measurement_split
  assignment <- if (!is.null(source) && length(source$assignment) == nrow(fit$data)) {
    source$assignment
  } else if (inherits(settings$split, "cssem_splits") &&
             length(settings$split$assignment) == length(settings$split$row_ids)) {
    position <- match(fit$row_ids, settings$split$row_ids)
    if (anyNA(position)) stop("Stored split row IDs do not match the fitted data for bootstrap refitting.", call. = FALSE)
    settings$split$assignment[position]
  } else {
    settings$split
  }
  if (!is.numeric(assignment) || length(assignment) != nrow(fit$data) ||
      any(!is.finite(assignment)) || any(assignment != as.integer(assignment)))
    stop("Stored split must provide one finite integer assignment per fitted row for bootstrap refitting.", call. = FALSE)
  assignment <- as.integer(assignment[indices])
  levels <- sort(unique(assignment))
  if (length(levels) < 2L)
    stop("A bootstrap resample must retain at least two measurement folds for refitting.", call. = FALSE)
  assignment <- match(assignment, levels)
  model <- fit$model; model$folds <- length(levels)
  provenance <- if (!is.null(source$provenance)) source$provenance else data.frame(
    method = "bootstrap", folds = length(levels), seed = NA_real_,
    group = NA_character_, time = NA_character_, stringsAsFactors = FALSE)
  provenance$folds <- length(levels)
  split <- structure(list(method = if (is.null(source$method)) "bootstrap" else source$method,
    folds = length(levels), seed = if (is.null(source$seed)) NA_real_ else source$seed,
    assignment = as.integer(assignment), row_ids = seq_along(indices),
    provenance = provenance), class = c("cssem_splits", "list"))
  list(model = model, split = split)
}

.bootstrap_context <- function(fit, indices, refit, replicate, seed, settings,
                               cluster_ids = NULL, row_draw_ids = NULL,
                               source_unit_ids = NULL, row_source_ids = NULL,
                               resample = "row") {
  raw <- fit$data[indices, , drop = FALSE]
  unit_metadata <- .bootstrap_unit_metadata(cluster_ids, indices,
    draw_unit_ids = row_draw_ids, source_unit_ids = source_unit_ids)
  source_cluster_ids <- if (is.null(cluster_ids)) NULL else {
    if (is.null(row_source_ids)) as.character(cluster_ids[indices]) else as.character(row_source_ids)
  }
  if (refit == "locked_scores") {
    scored_fit <- fit
    scores <- fit$locked_scores[indices, , drop = FALSE]
  } else {
    refit_spec <- .bootstrap_measurement_split(fit, indices, settings)
    scored_fit <- do.call(.fit_states_quiet, c(list(
      model = refit_spec$model, data = raw, seed = seed, draws = 0L,
      iterations = settings$iterations, tolerance = settings$tolerance,
      quadrature = settings$quadrature, diagnostics = FALSE,
      preset = settings$preset, missing_policy = settings$missing_policy,
      split = refit_spec$split,
      cluster = source_cluster_ids, design = settings$design), list()))
    scores <- scored_fit$locked_scores
  }
  list(fit = scored_fit, scores = scores, data = raw, indices = indices,
    cluster_ids = source_cluster_ids, draw_cluster_ids = row_draw_ids,
    source_cluster_ids = source_unit_ids,
    unit_metadata = unit_metadata, replicate = replicate, seed = seed,
    refit = refit, resample = resample)
}

.bootstrap_one <- function(job, fit, statistic, refit, settings, resample = "row",
                           cluster_ids = NULL) {
  set.seed(job$seed)
  if (identical(resample, "cluster")) {
    sampled <- .cluster_bootstrap_indices(cluster_ids)
    indices <- sampled$indices
    row_draw_ids <- sampled$row_draw_ids
    source_unit_ids <- sampled$source_unit_ids
    row_source_ids <- sampled$row_source_ids
  } else {
    indices <- sample.int(nrow(fit$data), nrow(fit$data), replace = TRUE)
    row_draw_ids <- if (is.null(cluster_ids)) NULL else as.character(cluster_ids[indices])
    source_unit_ids <- if (is.null(cluster_ids)) NULL else as.character(cluster_ids[indices])
    row_source_ids <- source_unit_ids
  }
  result <- tryCatch({
    context <- .bootstrap_context(fit, indices, refit, job$replicate, job$seed, settings,
      cluster_ids = cluster_ids, row_draw_ids = row_draw_ids,
      source_unit_ids = source_unit_ids, row_source_ids = row_source_ids,
      resample = resample)
    raw_value <- statistic(context)
    list(replicate = job$replicate, seed = job$seed, status = "success",
      failure_reason = "", values = .bootstrap_statistic_vector(raw_value),
      metadata = attr(raw_value, "bootstrap_metadata"), unit_metadata = context$unit_metadata)
  }, error = function(e) list(replicate = job$replicate, seed = job$seed,
    status = "failed", failure_reason = conditionMessage(e), values = NULL, metadata = NULL,
    unit_metadata = .bootstrap_unit_metadata(cluster_ids, indices,
      draw_unit_ids = row_draw_ids, source_unit_ids = source_unit_ids)))
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
  settings$missing_policy <- if (is.null(settings$missing_policy)) "partial" else settings$missing_policy
  settings$split <- if (is.null(settings$split)) NULL else settings$split
  settings$design <- if (is.null(settings$design)) NULL else settings$design
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
#' @param resample Resampling unit. The default row mode preserves ordinary
#'   row bootstrap behavior. Cluster mode samples complete independent units
#'   with replacement and records draw-level unit provenance. This does not
#'   fit a multilevel SEM.
#' @param cluster Optional column name or cluster vector used when
#'   `resample = "cluster"`. If omitted, labels retained on `fit` are used.
#' @param workers Number of worker processes. Replicate seeds are deterministic
#'   across worker counts; values above one require the installed package to be
#'   available to the workers.
#' @param resume An earlier `cssem_bootstrap` object with the same `seed`,
#'   `level`, and `refit`, used to continue a partially completed run.
#' @param progress Whether to report completion of each replicate with a
#'   concise message. It does not alter the results.
#' @return An object of class `cssem_bootstrap` containing replicate draws,
#'   statuses, failure reasons, summary intervals, and reproducibility metadata.
#'   Cluster resampling additionally records the original-unit denominator,
#'   distinct source-unit and draw-occurrence counts, rows per draw, source and
#'   draw-level IDs, and a `cluster_resampling_only` limitation. In a measurement
#'   refit, `context$cluster_ids` uses source-unit labels so duplicated draws of
#'   one source unit stay in one measurement fold; `context$draw_cluster_ids`
#'   retains the draw-occurrence labels for provenance.
#' @export
bootstrap_model <- function(fit, statistic, reps = 200L, level = .95, seed = 1L,
                            refit = c("locked_scores", "measurement"), workers = 1L,
                            resume = NULL, progress = FALSE,
                            resample = c("row", "cluster"), cluster = NULL) {
  bootstrap_call <- match.call()
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
  resample <- match.arg(resample)
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress))
    stop("progress must be TRUE or FALSE.", call. = FALSE)
  settings <- .bootstrap_settings(fit)
  cluster_ids <- if (identical(resample, "cluster") || !is.null(cluster))
    .resolve_bootstrap_clusters(fit, cluster) else fit$cluster_ids

  prior <- NULL
  if (!is.null(resume)) {
    if (!inherits(resume, "cssem_bootstrap")) stop("resume must be a cssem_bootstrap object.", call. = FALSE)
    if (!identical(as.integer(resume$seed), seed) || !identical(as.numeric(resume$level), level) ||
        !identical(resume$refit, refit) || !identical(resume$resample, resample))
      stop("resume must use the same seed, level, refit, and resample mode.", call. = FALSE)
    if (reps < nrow(resume$replicates)) stop("reps cannot be smaller than the completed resume object.", call. = FALSE)
    prior <- resume
  }

  completed <- if (is.null(prior)) 0L else nrow(prior$replicates)
  point_context <- list(fit = fit, scores = fit$locked_scores, data = fit$data,
    indices = seq_len(nrow(fit$data)), cluster_ids = cluster_ids,
    source_cluster_ids = cluster_ids,
    unit_metadata = .bootstrap_unit_metadata(cluster_ids, seq_len(nrow(fit$data))),
    replicate = 0L, seed = seed, refit = refit, resample = resample)
  point <- .bootstrap_statistic_vector(statistic(point_context))
  metric_names <- names(point)
  draws <- matrix(NA_real_, nrow = reps, ncol = length(point),
    dimnames = list(as.character(seq_len(reps)), metric_names))
  replicate_rows <- data.frame(replicate = seq_len(reps), seed = seed + seq_len(reps) - 1L,
    status = rep("pending", reps), failure_reason = rep(NA_character_, reps),
    stringsAsFactors = FALSE)
  replicate_rows$original_unit_n <- if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids))
  replicate_rows$resampled_unit_n <- NA_integer_
  replicate_rows$resampled_draw_n <- NA_integer_
  replicate_rows$resampled_row_n <- NA_integer_
  replicate_rows$rows_per_unit <- I(vector("list", reps))
  replicate_rows$draw_unit_ids <- I(vector("list", reps))
  replicate_rows$source_unit_ids <- I(vector("list", reps))
  replicate_rows$row_draw_ids <- I(vector("list", reps))
  replicate_rows$metadata <- rep(list(NA), reps)
  if (!is.null(prior)) {
    if (!identical(colnames(prior$draws), metric_names))
      stop("resume statistic output does not match the original metric names.", call. = FALSE)
    draws[seq_len(completed), ] <- prior$draws
    common <- intersect(names(replicate_rows), names(prior$replicates))
    replicate_rows[seq_len(completed), common] <- prior$replicates[seq_len(completed), common]
  }
  if (completed < reps) {
    jobs <- lapply(seq.int(completed + 1L, reps), function(i)
      list(replicate = i, seed = seed + i - 1L))
    results <- if (workers == 1L) {
      lapply(jobs, .bootstrap_one, fit = fit, statistic = statistic,
        refit = refit, settings = settings, resample = resample,
        cluster_ids = cluster_ids)
    } else {
      cl <- parallel::makeCluster(workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, {
        if (!requireNamespace("cssem", quietly = TRUE))
          stop("workers > 1 requires the installed cssem package.")
        NULL
      })
      parallel::parLapply(cl, jobs, function(job, fit, statistic, refit, settings,
                                            resample, cluster_ids)
        cssem:::.bootstrap_one(job, fit, statistic, refit, settings,
          resample = resample, cluster_ids = cluster_ids),
        fit = fit, statistic = statistic, refit = refit, settings = settings,
        resample = resample, cluster_ids = cluster_ids)
    }
    for (result in results) {
      i <- result$replicate
      replicate_rows$status[[i]] <- result$status
      replicate_rows$failure_reason[[i]] <- if (result$status == "failed") result$failure_reason else ""
      replicate_rows$original_unit_n[[i]] <- result$unit_metadata$original_unit_n
      replicate_rows$resampled_unit_n[[i]] <- result$unit_metadata$resampled_unit_n
      replicate_rows$resampled_draw_n[[i]] <- result$unit_metadata$resampled_draw_n
      replicate_rows$resampled_row_n[[i]] <- result$unit_metadata$resampled_row_n
      replicate_rows$rows_per_unit[[i]] <- result$unit_metadata$rows_per_unit
      replicate_rows$draw_unit_ids[[i]] <- result$unit_metadata$draw_unit_ids
      replicate_rows$source_unit_ids[[i]] <- result$unit_metadata$source_unit_ids
      replicate_rows$row_draw_ids[[i]] <- result$unit_metadata$row_draw_ids
      replicate_rows$metadata[[i]] <- if (is.null(result$metadata)) NA else result$metadata
      if (identical(result$status, "success")) {
        if (!identical(names(result$values), metric_names)) {
          replicate_rows$status[[i]] <- "failed"
          replicate_rows$failure_reason[[i]] <- "statistic returned different metric names."
        } else draws[i, ] <- result$values
      }
      if (isTRUE(progress)) message(sprintf("Bootstrap replicate %d/%d: %s", i, reps, replicate_rows$status[[i]]))
    }
  }
  if (any(replicate_rows$status == "pending")) stop("bootstrap contains incomplete replicates.", call. = FALSE)
  failed <- replicate_rows[replicate_rows$status == "failed", , drop = FALSE]
  result <- list(draws = draws, point_estimate = point, summary = .bootstrap_summary(
    draws, point, replicate_rows, level), replicates = replicate_rows, failed = failed,
    successful_replicates = sum(replicate_rows$status == "success"),
    failure_count = sum(replicate_rows$status == "failed"), reps = reps, level = level,
    seed = seed, refit = refit, workers = workers, resample = resample,
    original_unit_n = if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids)),
    resampled_unit_n = if (is.null(cluster_ids)) NA_integer_ else unique(replicate_rows$resampled_unit_n),
    resampled_draw_n = if (is.null(cluster_ids)) NA_integer_ else unique(replicate_rows$resampled_draw_n),
    limitation = if (identical(resample, "cluster")) "cluster_resampling_only: grouped resampling does not estimate multilevel, longitudinal, growth, or survey-design parameters." else "row_resampling",
    components = if (refit == "measurement") "measurement_encoder_refit" else "locked_scores_only",
    statistic = statistic, progress = progress)
  fit_provenance <- fit$provenance_record
  input <- if (!is.null(fit_provenance)) fit_provenance$input else
    .cssem_provenance_input_summary(fit$data)
  result$provenance_record <- .cssem_provenance_record("bootstrap_model",
    .cssem_provenance_call(bootstrap_call, c("fit", "statistic", "resume", "cluster")),
    settings = list(reps = reps, level = level, seed = seed, refit = refit,
      workers = workers, resample = resample, progress = progress,
      statistic = paste(deparse(substitute(statistic)), collapse = " "),
      cluster = list(available_in_fit = !is.null(cluster_ids),
        resampling_requested = identical(resample, "cluster") || !is.null(cluster),
        column = if (length(cluster) == 1L && is.character(cluster)) cluster else NULL,
        unit_n = if (is.null(cluster_ids)) NA_integer_ else length(unique(cluster_ids)))),
    input = input, parent = if (is.null(fit_provenance)) list() else list(fit = fit_provenance),
    packages = c("MASS", "parallel"))
  class(result) <- c("cssem_bootstrap", "list")
  result
}

#' @export
summary.cssem_bootstrap <- function(object, ...) {
  structure(list(summary = object$summary, successful_replicates = object$successful_replicates,
    failure_count = object$failure_count, failed = object$failed, level = object$level,
    refit = object$refit, resample = object$resample,
    original_unit_n = object$original_unit_n,
    resampled_unit_n = object$resampled_unit_n,
    resampled_draw_n = object$resampled_draw_n,
    limitation = object$limitation), class = c("summary.cssem_bootstrap", "summary"))
}

#' @export
print.cssem_bootstrap <- function(x, ...) {
  cat("CS-SEM bootstrap: ", x$successful_replicates, "/", x$reps,
    " successful replicates (", x$refit, ", ", x$resample, ", level ", x$level, ")\n", sep = "")
  if (!is.null(x$limitation)) cat("  status: ", x$limitation, "\n", sep = "")
  if (!is.null(x$original_unit_n) && is.finite(x$original_unit_n))
    cat("  independent units: original ", x$original_unit_n, "; resampled draws retain unit provenance\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
print.summary.cssem_bootstrap <- function(x, ...) {
  cat("CS-SEM bootstrap summary: ", x$successful_replicates,
    " successful, ", x$failure_count, " failed; ", x$resample,
    "; level ", x$level, "\n", sep = "")
  if (!is.null(x$limitation)) cat("  status: ", x$limitation, "\n", sep = "")
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
