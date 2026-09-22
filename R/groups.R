# Common-anchor group comparison diagnostics.
#
# These functions deliberately operate on the pooled locked construct states.
# Fitting a separate measurement model per group would allow arbitrary latent
# location and scale changes, making a path difference uninterpretable.

.resolve_fit_groups <- function(fit, group) {
  if (!inherits(fit, "fit_states")) stop("fit must be a fit_states object.", call. = FALSE)
  input <- if (!is.null(fit$input_data)) fit$input_data else fit$data
  retained <- if (!is.null(fit$row_ids)) as.integer(fit$row_ids) else seq_len(nrow(input))
  if (is.character(group) && length(group) == 1L) {
    if (!group %in% names(input)) stop("group column was not found in the fit's input data.", call. = FALSE)
    values <- input[[group]]
  } else {
    values <- group
  }
  if (length(values) == nrow(input)) values <- values[retained]
  else if (length(values) != length(retained))
    stop("group must be a column name, a vector with one value per input row, or a vector with one value per retained row.", call. = FALSE)
  if (is.factor(values)) values <- as.character(values)
  if (is.numeric(values)) {
    if (any(!is.finite(values))) stop("group labels must not contain missing or non-finite values.", call. = FALSE)
    values <- as.character(values)
  } else {
    values <- as.character(values)
    if (anyNA(values) || any(!nzchar(trimws(values))))
      stop("group labels must not contain missing or empty values.", call. = FALSE)
  }
  labels <- unique(values)
  if (length(labels) < 2L) stop("group must identify at least two groups.", call. = FALSE)
  list(values = values, labels = labels, row_ids = retained)
}

.group_metadata <- function(values, labels, min_group_size) {
  n <- table(factor(values, levels = labels))
  small <- as.integer(n) < min_group_size
  if (any(small)) warning(sprintf("One or more groups are below min_group_size (%d); group contrasts may be unstable.",
    min_group_size), call. = FALSE)
  data.frame(group = labels, n = as.integer(n), small_group = small,
    stringsAsFactors = FALSE, row.names = NULL)
}

.group_validate_controls <- function(reference, min_group_size, level, tolerance) {
  if (length(min_group_size) != 1L || !is.numeric(min_group_size) || !is.finite(min_group_size) ||
      min_group_size < 2 || min_group_size != as.integer(min_group_size))
    stop("min_group_size must be a whole number of at least two.", call. = FALSE)
  if (length(level) != 1L || !is.numeric(level) || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be a numeric value between zero and one.", call. = FALSE)
  if (length(tolerance) != 1L || !is.numeric(tolerance) || !is.finite(tolerance) || tolerance < 0)
    stop("tolerance must be a non-negative numeric scalar.", call. = FALSE)
  if (!is.null(reference) && (length(reference) != 1L || is.na(reference) || !nzchar(as.character(reference))))
    stop("reference must name one group.", call. = FALSE)
  invisible(NULL)
}

.group_parameter_rows <- function(fit, groups) {
  rows <- list(); index <- 0L
  add <- function(construct, item, scale, group, parameter, estimate, observed_n,
                  available = is.finite(estimate), reason = if (available) "" else "Parameter fit unavailable.") {
    index <<- index + 1L
    rows[[index]] <<- data.frame(construct = construct, item = item, scale = scale,
      group = group, parameter = parameter, estimate = unname(estimate), observed_n = observed_n,
      available = available, availability_reason = reason, stringsAsFactors = FALSE)
  }
  for (nm in names(fit$full_encoders)) {
    encoder <- fit$full_encoders[[nm]]; spec <- fit$model$constructs[[nm]]
    z <- fit$locked_scores[[nm]]
    for (j in seq_along(spec$indicators)) {
      item <- spec$indicators[[j]]; scale <- spec$scales[[j]]
      if (identical(encoder$type, "manifest")) {
        for (g in groups$labels) add(nm, item, "manifest", g, "manifest_scale", NA_real_,
          sum(groups$values == g & is.finite(z)), FALSE,
          "Manifest constructs have no fitted item parameter.")
        next
      }
      prepared <- tryCatch(.prepare_item(fit$data[[item]], scale, encoder$keys[[j]], encoder$levels[[j]]),
        error = function(e) e)
      if (inherits(prepared, "error")) {
        for (g in groups$labels) add(nm, item, scale, g, "item_fit", NA_real_, 0L, FALSE, conditionMessage(prepared))
        next
      }
      y <- prepared$y
      for (g in groups$labels) {
        keep <- groups$values == g & is.finite(z) & !is.na(y)
        observed_n <- sum(keep)
        if (observed_n < 5L) {
          if (identical(scale, "ordinal")) {
            pars <- c(discrimination = NA_real_, stats::setNames(rep(NA_real_, length(encoder$encoders[[j]]$tau)),
              paste0("threshold_", seq_along(encoder$encoders[[j]]$tau))))
          } else pars <- c(intercept = NA_real_, slope = NA_real_, residual_sd = NA_real_)
          for (parameter in names(pars)) add(nm, item, scale, g, parameter, pars[[parameter]], observed_n,
            FALSE, "Fewer than five complete item-score observations in this group.")
          next
        }
        estimate <- tryCatch({
          item_fit <- if (identical(scale, "ordinal")) .fit_ordinal(z[keep], y[keep], k = length(encoder$levels[[j]]))
            else .fit_continuous(z[keep], y[keep])
          if (identical(scale, "ordinal")) {
            c(discrimination = item_fit$a, stats::setNames(item_fit$tau, paste0("threshold_", seq_along(item_fit$tau))))
          } else c(intercept = item_fit$intercept, slope = item_fit$slope, residual_sd = item_fit$sigma)
        }, error = function(e) structure(conditionMessage(e), class = "group_parameter_error"))
        if (inherits(estimate, "group_parameter_error")) {
          if (identical(scale, "ordinal")) pars <- c(discrimination = NA_real_,
            stats::setNames(rep(NA_real_, length(encoder$encoders[[j]]$tau)), paste0("threshold_", seq_along(encoder$encoders[[j]]$tau))))
          else pars <- c(intercept = NA_real_, slope = NA_real_, residual_sd = NA_real_)
          for (parameter in names(pars)) add(nm, item, scale, g, parameter, pars[[parameter]], observed_n,
            FALSE, as.character(estimate))
        } else for (parameter in names(estimate)) add(nm, item, scale, g, parameter, estimate[[parameter]], observed_n)
      }
    }
  }
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

.group_parameter_contrasts <- function(parameters, reference, tolerance) {
  if (!nrow(parameters)) return(data.frame())
  ref <- parameters[parameters$group == reference, , drop = FALSE]
  targets <- setdiff(unique(parameters$group), reference)
  rows <- list(); index <- 0L
  for (g in targets) {
    current <- parameters[parameters$group == g, , drop = FALSE]
    keys <- c("construct", "item", "scale", "parameter")
    merged <- merge(current, ref, by = keys, suffixes = c("", "_reference"), all = TRUE, sort = FALSE)
    for (i in seq_len(nrow(merged))) {
      available <- isTRUE(merged$available[[i]]) && isTRUE(merged$available_reference[[i]]) &&
        is.finite(merged$estimate[[i]]) && is.finite(merged$estimate_reference[[i]])
      difference <- if (available) merged$estimate[[i]] - merged$estimate_reference[[i]] else NA_real_
      index <- index + 1L
      rows[[index]] <- data.frame(construct = merged$construct[[i]], item = merged$item[[i]],
        scale = merged$scale[[i]], parameter = merged$parameter[[i]], group = g,
        reference = reference, estimate = merged$estimate[[i]], reference_estimate = merged$estimate_reference[[i]],
        difference = difference, absolute_difference = if (available) abs(difference) else NA_real_,
        flagged = isTRUE(available && abs(difference) > tolerance), available = available,
        availability_reason = if (available) "" else paste(unique(c(merged$availability_reason[[i]],
          merged$availability_reason_reference[[i]])), collapse = "; "), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

#' Diagnose common-anchor measurement invariance by group
#'
#' Group-specific item parameters are estimated conditional on the pooled
#' cross-fitted construct scores. This is a scale-safe diagnostic for item
#' functioning and score differences; it is not a joint equality-constrained
#' CFA or a conventional metric/scalar invariance test.
#'
#' @param fit A [fit_states()] object.
#' @param group A column name in the input data, or a group vector aligned to
#'   the input rows (or to retained fit rows).
#' @param reference Optional reference-group label. Defaults to the first
#'   observed group.
#' @param min_group_size Minimum group size used for the small-group warning.
#' @param level Confidence level recorded in the result metadata.
#' @param tolerance Absolute item-parameter difference that receives a flag.
#' @return A `cssem_measurement_invariance` object.
#' @export
measurement_invariance <- function(fit, group, reference = NULL, min_group_size = 20L,
                                    level = .95, tolerance = .10) {
  .preserve_seed()
  .measurement_check_fit(fit)
  .group_validate_controls(reference, min_group_size, level, tolerance)
  resolved <- .resolve_fit_groups(fit, group)
  if (is.null(reference)) reference <- resolved$labels[[1L]]
  if (!reference %in% resolved$labels) stop("reference must name one of the observed groups.", call. = FALSE)
  metadata <- .group_metadata(resolved$values, resolved$labels, as.integer(min_group_size))
  score_rows <- do.call(rbind, lapply(names(fit$locked_scores), function(construct) {
    score <- fit$locked_scores[[construct]]
    do.call(rbind, lapply(resolved$labels, function(g) {
      keep <- resolved$values == g & is.finite(score)
      data.frame(construct = construct, group = g, n = sum(keep),
        mean = if (any(keep)) mean(score[keep]) else NA_real_,
        sd = if (sum(keep) > 1L) stats::sd(score[keep]) else NA_real_, stringsAsFactors = FALSE)
    }))
  }))
  parameters <- .group_parameter_rows(fit, resolved)
  contrasts <- .group_parameter_contrasts(parameters, reference, tolerance)
  structure(list(groups = metadata, reference = reference, constructs = score_rows,
    item_parameters = parameters, item_contrasts = contrasts, level = level,
    tolerance = tolerance, status = "diagnostic_common_anchor",
    limitations = c("Parameters are conditional diagnostics on the pooled common-anchor score, not jointly estimated multi-group parameters.",
      "Threshold and discrimination flags are descriptive and are not calibrated ordinal DIF tests.",
      "Formal configural, metric, and scalar invariance labels require a future equality-constrained multi-group estimator.")),
    class = "cssem_measurement_invariance")
}

#' @export
print.cssem_measurement_invariance <- function(x, ...) {
  cat("CS-SEM common-anchor measurement comparison: ", length(unique(x$groups$group)), " groups; reference = ", x$reference, "\n", sep = "")
  print(x$groups, row.names = FALSE)
  invisible(x)
}

.group_association_labels <- function(association, group) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association object.", call. = FALSE)
  resolved <- .resolve_fit_groups(association$fit, group)
  fit_rows <- if (!is.null(association$fit$row_ids)) as.integer(association$fit$row_ids) else seq_len(length(resolved$values))
  ids <- if (!is.null(association$row_ids)) as.integer(association$row_ids) else fit_rows
  labels <- resolved$values[match(ids, fit_rows)]
  if (anyNA(labels)) stop("group labels could not be aligned to the association rows.", call. = FALSE)
  list(values = labels, labels = resolved$labels)
}

.group_selected_shapes <- function(association) {
  selected <- association$candidate_metrics[association$candidate_metrics$selected, , drop = FALSE]
  shapes <- lapply(names(association$full_models), function(outcome) {
    rows <- selected[selected$outcome == outcome, , drop = FALSE]
    stats::setNames(as.character(rows$shape), rows$predictor)
  })
  stats::setNames(shapes, names(association$full_models))
}

.group_shape_effects <- function(scores, labels, outcome, shapes, target_labels) {
  required <- unique(c(outcome, unlist(lapply(names(shapes), .predictor_constructs), use.names = FALSE)))
  rows <- list(); index <- 0L
  for (g in target_labels) {
    keep <- labels == g & apply(as.matrix(scores[, required, drop = FALSE]), 1L, function(x) all(is.finite(x)))
    n <- sum(keep); model <- NULL
    if (n >= length(shapes) + 3L) model <- tryCatch(.fit_shape_model(scores[keep, , drop = FALSE], outcome, shapes), error = function(e) NULL)
    for (predictor in names(shapes)) {
      scalar <- shapes[[predictor]] %in% c("linear", "product")
      available <- scalar && !is.null(model)
      estimate <- if (available) unname(model$coefficient[model$maps[[predictor]][[1L]]]) else NA_real_
      reason <- if (available) "" else if (!scalar) "Nonlinear edge has no single scalar path contrast." else if (n < length(shapes) + 3L) "Too few complete rows in this group." else "Group structural fit failed."
      index <- index + 1L
      rows[[index]] <- data.frame(outcome = outcome, predictor = predictor, shape = shapes[[predictor]],
        group = g, estimate = estimate, n = n, available = available, availability_reason = reason,
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.group_edge_difference <- function(scores, labels, outcome, predictor, shape, target, reference) {
  estimates <- .group_shape_effects(scores, labels, outcome, stats::setNames(shape, predictor), c(reference, target))
  left <- estimates[estimates$group == target, , drop = FALSE]; right <- estimates[estimates$group == reference, , drop = FALSE]
  available <- isTRUE(left$available[[1L]]) && isTRUE(right$available[[1L]])
  c(estimate = if (available) left$estimate[[1L]] - right$estimate[[1L]] else NA_real_, available = available)
}

#' Compare declared structural paths between groups
#'
#' Structural estimates use the association's pooled locked scores and selected
#' shapes. Linear and product edges receive scalar contrasts; nonlinear edges
#' remain explicitly unavailable because one scalar difference is not defined.
#' Optional label permutations preserve the observed group sizes.
#'
#' @param association A [associate()] result.
#' @param group A column name or group vector aligned to the association input.
#' @param reference Optional reference group label.
#' @param permutations Number of size-preserving label permutations. Zero
#'   returns raw contrasts without a p-value.
#' @param seed Integer permutation seed.
#' @param min_group_size Minimum group size for the small-group warning.
#' @param level Confidence level recorded in the result metadata.
#' @return A `cssem_group_comparison` object.
#' @export
group_comparison <- function(association, group, reference = NULL, permutations = 999L,
                              seed = 1L, min_group_size = 20L, level = .95) {
  .preserve_seed()
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association object.", call. = FALSE)
  if (length(permutations) != 1L || !is.numeric(permutations) || !is.finite(permutations) ||
      permutations < 0 || permutations != as.integer(permutations)) stop("permutations must be a non-negative whole number.", call. = FALSE)
  .group_validate_controls(reference, min_group_size, level, 0)
  resolved <- .group_association_labels(association, group)
  if (is.null(reference)) reference <- resolved$labels[[1L]]
  if (!reference %in% resolved$labels) stop("reference must name one of the observed groups.", call. = FALSE)
  metadata <- .group_metadata(resolved$values, resolved$labels, as.integer(min_group_size))
  targets <- setdiff(resolved$labels, reference)
  shapes <- .group_selected_shapes(association)
  group_effects <- do.call(rbind, lapply(names(shapes), function(outcome)
    .group_shape_effects(association$scores, resolved$values, outcome, shapes[[outcome]], resolved$labels)))
  rows <- list(); index <- 0L; set.seed(seed)
  for (outcome in names(shapes)) for (predictor in names(shapes[[outcome]])) for (target in targets) {
    shape <- shapes[[outcome]][[predictor]]
    pair <- resolved$values %in% c(reference, target)
    score_columns <- unique(c(outcome, unlist(lapply(names(shapes[[outcome]]), .predictor_constructs), use.names = FALSE)))
    complete <- pair & apply(as.matrix(association$scores[, score_columns, drop = FALSE]), 1L,
      function(x) all(is.finite(x)))
    labels_pair <- resolved$values[complete]; scores_pair <- association$scores[complete, , drop = FALSE]
    observed <- .group_edge_difference(scores_pair, labels_pair, outcome, predictor, shape, target, reference)
    null <- numeric(as.integer(permutations))
    if (isTRUE(observed[["available"]]) && permutations > 0L && length(unique(labels_pair)) == 2L) {
      for (b in seq_len(as.integer(permutations))) {
        permuted <- sample(labels_pair, length(labels_pair), replace = FALSE)
        null[[b]] <- .group_edge_difference(scores_pair, permuted, outcome, predictor, shape, target, reference)[["estimate"]]
      }
    }
    p <- if (permutations > 0L && isTRUE(observed[["available"]]) && any(is.finite(null)))
      (1 + sum(abs(null) >= abs(observed[["estimate"]]), na.rm = TRUE)) / (sum(is.finite(null)) + 1) else NA_real_
    null_interval <- if (any(is.finite(null)))
      stats::quantile(null, c((1 - level) / 2, (1 + level) / 2), na.rm = TRUE, names = FALSE) else c(NA_real_, NA_real_)
    target_info <- group_effects[group_effects$outcome == outcome & group_effects$predictor == predictor & group_effects$group == target, , drop = FALSE]
    reference_info <- group_effects[group_effects$outcome == outcome & group_effects$predictor == predictor & group_effects$group == reference, , drop = FALSE]
    index <- index + 1L
    rows[[index]] <- data.frame(outcome = outcome, predictor = predictor, shape = shape, group = target,
      reference = reference, estimate = if (isTRUE(observed[["available"]])) group_effects$estimate[group_effects$outcome == outcome & group_effects$predictor == predictor & group_effects$group == target][[1L]] else NA_real_,
      reference_estimate = if (isTRUE(observed[["available"]])) group_effects$estimate[group_effects$outcome == outcome & group_effects$predictor == predictor & group_effects$group == reference][[1L]] else NA_real_,
      difference = if (isTRUE(observed[["available"]])) observed[["estimate"]] else NA_real_,
      null_low = null_interval[[1L]], null_high = null_interval[[2L]], p_value = p,
      n_group = if (nrow(target_info)) target_info$n[[1L]] else 0L,
      n_reference = if (nrow(reference_info)) reference_info$n[[1L]] else 0L,
      permutations = as.integer(permutations), available = isTRUE(observed[["available"]]),
      availability_reason = if (isTRUE(observed[["available"]])) "" else {
        info <- group_effects[group_effects$outcome == outcome & group_effects$predictor == predictor & group_effects$group %in% c(target, reference), , drop = FALSE]
        paste(unique(info$availability_reason), collapse = "; ")
      }, stringsAsFactors = FALSE)
  }
  contrasts <- if (length(rows)) do.call(rbind, rows) else data.frame()
  structure(list(groups = metadata, reference = reference, group_effects = group_effects,
    contrasts = contrasts, permutation_settings = list(permutations = as.integer(permutations), seed = seed, level = level),
    status = "associational_group_contrast",
    limitations = c("Structural contrasts use pooled locked construct states and retain the association's selected shapes.",
      "Permutation p-values are label-randomization diagnostics, not causal interaction tests.",
      "Nonlinear edges have no single scalar path contrast and are reported as unavailable.")),
    class = "cssem_group_comparison")
}

#' @export
print.cssem_group_comparison <- function(x, ...) {
  cat("CS-SEM associational group comparison: ", length(unique(x$groups$group)), " groups; reference = ", x$reference, "\n", sep = "")
  print(x$contrasts, row.names = FALSE)
  invisible(x)
}
