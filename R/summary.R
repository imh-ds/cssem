# Stable public summaries and extractors for CS-SEM result objects.
#
# The package deliberately keeps the measurement and structural engines as
# separate layers.  These helpers provide a small, stable table contract over
# those layers without pretending that a marginal EAP fit or a selected
# nonlinear edge has covariance information that it does not retain.

.cssem_parameter_columns <- c(
  "parameter_id", "construct", "parameter", "item", "outcome", "predictor", "component", "path",
  "shape", "status",
  "estimate", "naive_estimate", "corrected_estimate", "estimate_basis", "basis",
  "units", "se", "ci_low", "ci_high", "uncertainty_method", "n", "available",
  "availability_reason"
)

.cssem_parameter_row <- function(...) {
  values <- list(...)
  out <- as.data.frame(values, stringsAsFactors = FALSE)
  for (name in setdiff(.cssem_parameter_columns, names(out))) out[[name]] <- NA
  out[.cssem_parameter_columns]
}

.cssem_parameter_key <- function(value) {
  value <- as.character(value)
  value[is.na(value) | !nzchar(value)] <- NA_character_
  value
}

.cssem_finalize_parameter_table <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  if (!"shape" %in% names(table)) table$shape <- NA_character_
  if (!"status" %in% names(table)) table$status <- ifelse(isTRUE(table$available), "available", "unavailable")
  status <- ifelse(!is.na(table$available) & table$available, "available", "unavailable")
  table$status <- as.character(status)
  ids <- rep(NA_character_, nrow(table))
  naive_ids <- rep(NA_character_, nrow(table))
  corrected_ids <- rep(NA_character_, nrow(table))
  aliases <- list()
  for (i in seq_len(nrow(table))) {
    outcome <- .cssem_parameter_key(table$outcome[[i]])
    predictor <- .cssem_parameter_key(table$predictor[[i]])
    component <- .cssem_parameter_key(table$component[[i]])
    path <- .cssem_parameter_key(table$path[[i]])
    construct <- .cssem_parameter_key(table$construct[[i]])
    parameter <- .cssem_parameter_key(table$parameter[[i]])
    item <- .cssem_parameter_key(table$item[[i]])
    basis <- as.character(table$estimate_basis[[i]])
    if (!is.na(outcome) && !is.na(predictor) && identical(component, "structural_edge")) {
      base <- paste0("edge:", outcome, "~", predictor)
      naive_ids[[i]] <- paste0(base, ":naive")
      corrected_ids[[i]] <- paste0(base, ":corrected")
      ids[[i]] <- if (identical(basis, "corrected_eiv")) corrected_ids[[i]] else naive_ids[[i]]
      aliases[[naive_ids[[i]]]] <- list(row = i, column = "naive_estimate",
        basis = "naive", available = is.finite(table$naive_estimate[[i]]),
        reason = if (is.finite(table$naive_estimate[[i]])) "" else table$availability_reason[[i]])
      aliases[[corrected_ids[[i]]]] <- list(row = i, column = "corrected_estimate",
        basis = "corrected_eiv", available = is.finite(table$corrected_estimate[[i]]),
        reason = if (is.finite(table$corrected_estimate[[i]])) "" else
          "Corrected estimate is unavailable because the required reliability or correction is unavailable.")
    } else if (!is.na(construct) && !is.na(parameter)) {
      item_key <- if (is.na(item)) "" else paste0(":", item)
      ids[[i]] <- paste0("measurement:", construct, ":", parameter, item_key)
    } else if (!is.na(component)) {
      target <- if (!is.na(path)) path else if (!is.na(outcome) && !is.na(predictor))
        paste0(outcome, "->", predictor) else component
      suffix <- if (nzchar(basis) && !identical(basis, "unavailable")) paste0(":", basis) else ""
      ids[[i]] <- paste0("effect:", component, ":", target, suffix)
    } else {
      ids[[i]] <- paste0("parameter:", i)
    }
  }
  if (anyDuplicated(ids)) {
    duplicates <- unique(ids[duplicated(ids)])
    stop(sprintf("Parameter table contains duplicate stable identities: %s.",
      paste(duplicates, collapse = ", ")), call. = FALSE)
  }
  table$parameter_id <- ids
  table$naive_parameter_id <- naive_ids
  table$corrected_parameter_id <- corrected_ids
  for (i in seq_len(nrow(table))) aliases[[ids[[i]]]] <- list(row = i, column = "estimate",
    basis = as.character(table$estimate_basis[[i]]), available = isTRUE(table$available[[i]]),
    reason = as.character(table$availability_reason[[i]]))
  attr(table, "parameter_aliases") <- aliases
  table
}

.cssem_unavailable_vcov <- function(object, parameter_names) {
  parameter_names <- as.character(parameter_names)
  out <- matrix(NA_real_, length(parameter_names), length(parameter_names),
    dimnames = list(parameter_names, parameter_names))
  attr(out, "available") <- FALSE
  attr(out, "reason") <- "No joint covariance object is retained by this CS-SEM fit; marginal intervals are not converted into covariance estimates."
  out
}

.cssem_effect_row <- function(component, estimate, basis, n, ci_low = NA_real_, ci_high = NA_real_,
                              path = NA_character_, outcome = NA_character_, predictor = NA_character_,
                              naive_estimate = NA_real_, corrected_estimate = NA_real_,
                              reason = NULL, units = "locked-score effect units", se = NA_real_,
                              uncertainty_method = NULL) {
  available <- is.finite(estimate)
  if (is.null(reason)) reason <- if (available) "" else "No scalar estimate is available for this result."
  uncertainty <- if (!is.null(uncertainty_method)) uncertainty_method else
    if (is.finite(ci_low) && is.finite(ci_high)) "percentile bootstrap" else
      "unavailable (no joint standard error or interval retained)"
  .cssem_parameter_row(
    outcome = outcome, predictor = predictor, component = component, path = path,
    estimate = estimate, naive_estimate = naive_estimate, corrected_estimate = corrected_estimate,
    estimate_basis = basis, basis = basis, units = units, se = se,
    ci_low = ci_low, ci_high = ci_high, uncertainty_method = uncertainty,
    n = as.integer(n), available = available, availability_reason = reason
  )
}

#' Extract a stable parameter/effect table
#'
#' Returns a public, tabular view of estimates and their reporting basis.  The
#' table always includes the estimate basis, units, sample size, uncertainty
#' method, and an explicit reason when a scalar estimate or uncertainty is not
#' available. Categorical structural coefficients include likelihood-based
#' standard errors and Wald intervals in family-appropriate units. Use
#' [effect_ledger()] and [construct_card()] for focused views.
#'
#' @param x A fitted CS-SEM result object.
#' @param ... Additional arguments reserved for methods.
#' @return A data frame with stable estimate and availability columns.
#' @export
parameter_table <- function(x, ...) UseMethod("parameter_table")

#' @export
parameter_table.default <- function(x, ...) {
  stop("parameter_table() is not defined for objects of class ", paste(class(x), collapse = "/"), ".", call. = FALSE)
}

#' @export
parameter_table.fit_states <- function(x, ...) {
  if (is.null(x$full_encoders)) return(data.frame())
  n <- if (!is.null(x$locked_scores)) nrow(x$locked_scores) else NA_integer_
  rows <- list(); index <- 0L
  add <- function(...) { index <<- index + 1L; rows[[index]] <<- .cssem_parameter_row(...) }
  for (construct in names(x$full_encoders)) {
    encoder <- x$full_encoders[[construct]]
    if (is.null(encoder)) next
    if (identical(encoder$type, "manifest")) {
      add(construct = construct, parameter = "manifest_scale", item = encoder$indicators,
        estimate = NA_real_, estimate_basis = "manifest passthrough", basis = "manifest passthrough",
        units = if (isTRUE(encoder$standardize)) "standardized manifest units" else "manifest units",
        uncertainty_method = "not applicable", n = n, available = FALSE,
        availability_reason = "Manifest constructs have no fitted measurement parameter.")
      next
    }
    items <- encoder$encoders
    for (j in seq_along(items)) {
      item_names <- names(items)
      item <- if (!is.null(item_names) && length(item_names) >= j && !is.null(item_names[[j]]))
        item_names[[j]] else encoder$indicators[[j]]
      item_fit <- items[[j]]
      if (identical(item_fit$type, "ordinal")) {
        add(construct = construct, parameter = "discrimination", item = item,
          estimate = unname(item_fit$a), estimate_basis = "marginal graded-response encoder",
          basis = "marginal graded-response encoder", units = "latent-node logit scale",
          uncertainty_method = "unavailable (encoder covariance not retained)", n = n,
          available = is.finite(item_fit$a), availability_reason = if (is.finite(item_fit$a)) "" else "Encoder did not return a finite discrimination.")
        for (k in seq_along(item_fit$tau)) add(construct = construct,
          parameter = paste0("threshold_", k), item = item,
          estimate = unname(item_fit$tau[[k]]), estimate_basis = "marginal graded-response encoder",
          basis = "marginal graded-response encoder", units = "latent-node logit scale",
          uncertainty_method = "unavailable (encoder covariance not retained)", n = n,
          available = is.finite(item_fit$tau[[k]]), availability_reason = if (is.finite(item_fit$tau[[k]])) "" else "Encoder did not return a finite threshold.")
      } else {
        for (parameter in c("intercept", "slope", "residual_sd")) {
          field <- if (parameter == "residual_sd") "sigma" else parameter
          value <- unname(item_fit[[field]])
          add(construct = construct, parameter = parameter, item = item,
            estimate = value, estimate_basis = "marginal Gaussian encoder",
            basis = "marginal Gaussian encoder", units = "observed item units",
            uncertainty_method = "unavailable (encoder covariance not retained)", n = n,
            available = is.finite(value), availability_reason = if (is.finite(value)) "" else "Encoder did not return a finite parameter.")
        }
      }
    }
  }
  if (!length(rows)) data.frame() else .cssem_finalize_parameter_table(do.call(rbind, rows))
}

.association_raw_estimate <- function(association, outcome, predictor) {
  effects <- association$effects[association$effects$outcome == outcome & association$effects$predictor == predictor, , drop = FALSE]
  if (!nrow(effects)) return(NA_real_)
  scalar <- effects[is.na(effects$x), , drop = FALSE]
  if (!nrow(scalar)) return(NA_real_)
  value <- scalar$estimate[[1L]]
  if (length(value) != 1L || !is.finite(value)) NA_real_ else unname(value)
}

#' @export
parameter_table.cssem_association <- function(x, ...) {
  ledger <- effect_ledger(x)
  if (!nrow(ledger)) return(data.frame())
  n <- if (is.null(x$scores)) NA_integer_ else nrow(x$scores)
  rows <- lapply(seq_len(nrow(ledger)), function(i) {
    row <- ledger[i, , drop = FALSE]
    raw <- .association_raw_estimate(x, row$outcome[[1L]], row$predictor[[1L]])
    corrected <- if ("corrected_estimate" %in% names(row)) row$corrected_estimate[[1L]] else NA_real_
    naive <- if ("naive_estimate" %in% names(row)) row$naive_estimate[[1L]] else raw
    if (!is.finite(naive)) naive <- raw
    use_corrected <- is.finite(corrected)
    estimate <- if (use_corrected) corrected else raw
    family <- .structural_family(x$response_families[[row$outcome[[1L]]]])
    categorical <- family$family != "gaussian"
    shape <- as.character(row$shape[[1L]])
    smooth_reason <- if (is.na(estimate) && shape %in% c("smooth", "spline", "monotone_increasing", "monotone_decreasing"))
      "This selected nonlinear edge is represented by a fitted curve; no single scalar coefficient is defined." else NULL
    reason <- if (categorical && is.finite(estimate))
      "Categorical EIV correction is unsupported; the maximum-likelihood coefficient is reported." else if (!is.null(smooth_reason)) smooth_reason else if (is.finite(estimate) && !use_corrected && !is.finite(corrected))
      "Naive estimate reported; corrected estimate is unavailable because the required reliability is unavailable." else if (is.finite(estimate))
      "" else "No scalar estimate is available for this selected edge."
    basis <- if (categorical && is.finite(estimate)) "maximum_likelihood" else if (use_corrected) "corrected_eiv" else if (is.finite(estimate)) "naive" else "unavailable"
    ci_low <- if ("corrected_ci_low" %in% names(row)) row$corrected_ci_low[[1L]] else NA_real_
    ci_high <- if ("corrected_ci_high" %in% names(row)) row$corrected_ci_high[[1L]] else NA_real_
    se <- NA_real_; uncertainty_method <- NULL
    if (categorical) {
      model <- x$full_models[[row$outcome[[1L]]]]
      coefficient_se <- model$coefficient_se
      predictor <- row$predictor[[1L]]
      se <- if (is.null(coefficient_se) || !predictor %in% names(coefficient_se)) NA_real_ else
        unname(coefficient_se[[predictor]])
      level <- if (is.null(x$level)) .95 else x$level
      critical <- stats::qnorm(1 - (1 - level) / 2)
      ci_low <- if (is.finite(se)) estimate - critical * se else NA_real_
      ci_high <- if (is.finite(se)) estimate + critical * se else NA_real_
      uncertainty_method <- if (is.finite(se))
        "model-based Wald interval from the fitted likelihood" else
          "unavailable (likelihood covariance could not be estimated)"
    }
    units <- if (categorical && family$family == "binomial")
      "log-odds per predictor locked-score unit" else if (categorical && family$family == "ordinal")
      "cumulative log-odds per predictor locked-score unit" else if (grepl(":", row$predictor[[1L]], fixed = TRUE))
      "outcome locked-score units per product of constituent locked-score units" else
      "outcome locked-score units per predictor locked-score unit"
    .cssem_effect_row("structural_edge", estimate, basis, n, ci_low, ci_high,
      outcome = row$outcome[[1L]], predictor = row$predictor[[1L]],
      naive_estimate = naive, corrected_estimate = corrected, reason = reason, units = units,
      se = se, uncertainty_method = uncertainty_method)
  })
  out <- do.call(rbind, rows)
  for (name in setdiff(names(ledger), names(out))) out[[name]] <- ledger[[name]]
  .cssem_finalize_parameter_table(out)
}

.mediation_parameter_table <- function(x, causal = FALSE) {
  reported <- .mediation_reported(x$summary, isTRUE(x$disattenuated))
  n <- if (!is.null(x$n)) x$n else NA_integer_
  rows <- lapply(seq_len(nrow(reported)), function(i) {
    row <- reported[i, , drop = FALSE]
    .cssem_effect_row(row$component[[1L]], row$reported_effect[[1L]], row$basis[[1L]], n,
      row$reported_ci_low[[1L]], row$reported_ci_high[[1L]],
      path = NA_character_, outcome = x$y, predictor = x$x,
      reason = if (is.finite(row$reported_effect[[1L]])) "" else "Mediation component is unavailable.",
      units = "outcome locked-score units per predictor contrast")
  })
  if (nrow(x$path_specific)) {
    paths <- .mediation_reported(x$path_specific, isTRUE(x$disattenuated))
    rows <- c(rows, lapply(seq_len(nrow(paths)), function(i) {
      row <- paths[i, , drop = FALSE]
      .cssem_effect_row("path_indirect", row$reported_effect[[1L]], row$basis[[1L]], n,
        row$reported_ci_low[[1L]], row$reported_ci_high[[1L]], path = row$path[[1L]],
        outcome = x$y, predictor = x$x,
        reason = if (is.finite(row$reported_effect[[1L]])) "" else "Path-specific effect is unavailable.",
        units = "outcome locked-score units per predictor contrast")
    }))
  }
  .cssem_finalize_parameter_table(do.call(rbind, rows))
}

#' @export
parameter_table.indirect_effect <- function(x, ...) .mediation_parameter_table(x)

#' @export
parameter_table.causal_indirect_effect <- function(x, ...) .mediation_parameter_table(x, causal = TRUE)

#' @export
parameter_table.causal_effect <- function(x, ...) {
  n <- x$n
  rows <- list(
    .cssem_effect_row("unadjusted", x$unadjusted, "naive", n, units = "outcome locked-score units per treatment locked-score unit"),
    .cssem_effect_row("adjusted_naive", x$adjusted_naive, "naive", n, units = "outcome locked-score units per treatment locked-score unit"),
    .cssem_effect_row("adjusted_effect", x$adjusted_effect,
      if (isTRUE(x$disattenuated)) "corrected_eiv" else if (x$estimand %in% c("adjusted_dml", "adjusted_ame")) "flexible_adjustment" else "naive",
      n, x$ci_low, x$ci_high, units = "outcome locked-score units per treatment locked-score unit")
  )
  out <- do.call(rbind, rows); out$outcome <- x$outcome; out$predictor <- x$treatment
  .cssem_finalize_parameter_table(out)
}

#' @export
parameter_table.conditional_slopes <- function(x, ...) {
  if (is.null(x$slopes) || !nrow(x$slopes)) return(data.frame())
  out <- do.call(rbind, lapply(seq_len(nrow(x$slopes)), function(i) {
    row <- x$slopes[i, , drop = FALSE]
    .cssem_effect_row("conditional_slope", row$slope[[1L]], if (isTRUE(x$disattenuated)) "corrected_eiv" else "naive", NA_integer_,
      if ("ci_low" %in% names(row)) row$ci_low[[1L]] else NA_real_, if ("ci_high" %in% names(row)) row$ci_high[[1L]] else NA_real_,
      outcome = x$outcome, predictor = x$predictor, path = row$level[[1L]], units = "outcome locked-score units per predictor unit")
  }))
  .cssem_finalize_parameter_table(out)
}

#' @export
parameter_table.conditional_indirect_effect <- function(x, ...) {
  if (is.null(x$conditional) || !nrow(x$conditional)) return(data.frame())
  .cssem_finalize_parameter_table(do.call(rbind, lapply(seq_len(nrow(x$conditional)), function(i) {
    row <- x$conditional[i, , drop = FALSE]
    .cssem_effect_row("conditional_indirect", row$indirect[[1L]], if (isTRUE(x$disattenuated)) "corrected_eiv" else "naive", x$n,
      if ("ci_low" %in% names(row)) row$ci_low[[1L]] else NA_real_, if ("ci_high" %in% names(row)) row$ci_high[[1L]] else NA_real_,
      outcome = x$y, predictor = x$x, path = row$level[[1L]], units = "outcome locked-score units per predictor contrast")
  })))
}

#' @export
parameter_table.cssem_routing <- function(x, ...) {
  table <- x$table
  if (is.null(table) || !nrow(table)) return(data.frame())
  .cssem_finalize_parameter_table(do.call(rbind, lapply(seq_len(nrow(table)), function(i) {
    row <- table[i, , drop = FALSE]
    .cssem_effect_row("routed_edge", row$effect[[1L]], row$status[[1L]], NA_integer_,
      row$ci_low[[1L]], row$ci_high[[1L]], path = row$path[[1L]],
      reason = if (is.finite(row$effect[[1L]])) "" else "Routed edge has no scalar estimate.",
      units = "outcome locked-score units per predictor locked-score unit")
  })))
}

#' @export
parameter_table.evidence_report <- function(x, ...) {
  effects <- x$effects
  if (is.null(effects) || !nrow(effects)) return(data.frame())
  .cssem_finalize_parameter_table(do.call(rbind, lapply(seq_len(nrow(effects)), function(i) {
    row <- effects[i, , drop = FALSE]
    path <- strsplit(row$path[[1L]], .PATH_ARROW, fixed = TRUE)[[1L]]
    .cssem_effect_row("evidence_edge", row$estimate[[1L]], row$causal_status[[1L]], NA_integer_,
      outcome = if (length(path) > 1L) path[[length(path)]] else NA_character_,
      predictor = if (length(path) > 1L) path[[1L]] else NA_character_, path = row$path[[1L]],
      reason = if (is.finite(row$estimate[[1L]])) "" else "Evidence report has no scalar estimate for this edge.",
      units = "outcome locked-score units per predictor locked-score unit")
  })))
}

#' @export
summary.fit_states <- function(object, ...) {
  constructs <- data.frame(construct = names(object$locked_scores),
    n = vapply(object$locked_scores, function(x) sum(is.finite(x)), integer(1)),
    reliability = if (is.null(object$reliability)) NA_real_ else unname(object$reliability[names(object$locked_scores)]),
    stability = if (is.null(object$stability)) NA_real_ else unname(object$stability[names(object$locked_scores)]),
    stringsAsFactors = FALSE)
  if (!is.null(object$measurement_engine)) {
    constructs$converged <- vapply(constructs$construct, function(nm) {
      value <- object$measurement_engine[[nm]]$converged
      if (length(value) != 1L || is.na(value)) NA else isTRUE(value)
    }, logical(1))
  } else constructs$converged <- NA
  constructs$held_out_loss <- vapply(constructs$construct, function(nm) {
    metrics <- if (is.null(object$item_metrics)) numeric() else object$item_metrics$value[object$item_metrics$construct == nm]
    if (!length(metrics) || !any(is.finite(metrics))) NA_real_ else mean(metrics, na.rm = TRUE)
  }, numeric(1))
  constructs$redundancy_max <- vapply(seq_len(nrow(constructs)), function(i) {
    if (is.null(object$redundancy) || ncol(object$redundancy) < 2L) return(0)
    other <- object$redundancy[i, -i, drop = TRUE]
    if (!length(other) || !any(is.finite(other))) NA_real_ else max(abs(other), na.rm = TRUE)
  }, numeric(1))
  constructs$warnings <- vapply(constructs$construct, function(nm) {
    if (is.null(object$warnings) || !nrow(object$warnings)) return(0L)
    indicators <- object$model$constructs[[nm]]$indicators
    sum(object$warnings$target == nm | object$warnings$target %in% indicators, na.rm = TRUE)
  }, integer(1))
  structure(list(constructs = constructs, parameters = parameter_table(object),
    items = if (is.null(object$item_metrics)) data.frame() else object$item_metrics,
    warnings = if (is.null(object$warnings)) data.frame() else object$warnings,
    n = nobs(object), folds = length(unique(object$folds))), class = c("summary.fit_states", "summary"))
}

#' @export
summary.cssem_association <- function(object, ...) {
  structure(list(effects = parameter_table(object), specification_gap = object$specification_gap,
    n = nobs(object), status = object$status, structural_repeats = object$structural_repeats),
    class = c("summary.cssem_association", "summary"))
}

#' @export
summary.indirect_effect <- function(object, ...) structure(list(effects = parameter_table(object), n = nobs(object), status = object$status), class = c("summary.indirect_effect", "summary"))

#' @export
summary.causal_indirect_effect <- function(object, ...) structure(list(effects = parameter_table(object), n = nobs(object), status = object$status), class = c("summary.causal_indirect_effect", "summary"))

#' @export
summary.causal_effect <- function(object, ...) structure(list(effects = parameter_table(object), n = nobs(object), status = object$status), class = c("summary.causal_effect", "summary"))

#' @export
summary.conditional_slopes <- function(object, ...) structure(list(effects = parameter_table(object), n = nobs(object), status = object$status), class = c("summary.conditional_slopes", "summary"))

#' @export
summary.conditional_indirect_effect <- function(object, ...) structure(list(effects = parameter_table(object), n = nobs(object), status = object$status), class = c("summary.conditional_indirect_effect", "summary"))

#' @export
summary.evidence_report <- function(object, ...) structure(list(effects = parameter_table(object), constructs = object$constructs, causal_claims = object$causal_claims, status = object$status), class = c("summary.evidence_report", "summary"))

#' @export
print.summary.fit_states <- function(x, ...) {
  cat("CS-SEM measurement summary: ", x$n, " observations, ", nrow(x$constructs), " construct(s)\n", sep = "")
  print(x$constructs, row.names = FALSE)
  invisible(x)
}

#' @export
print.summary.cssem_association <- function(x, ...) {
  cat("CS-SEM associational summary: ", x$n, " observations, ", nrow(x$effects), " declared edge(s)\n", sep = "")
  print(x$effects[, c("outcome", "predictor", "estimate", "estimate_basis", "uncertainty_method", "available"), drop = FALSE], row.names = FALSE)
  invisible(x)
}

.print_cssem_effect_summary <- function(x, label, ...) {
  cat("CS-SEM ", label, " summary: ", if (is.null(x$n)) "unknown" else x$n,
    " observations, ", nrow(x$effects), " effect row(s)\n", sep = "")
  print(x$effects[, intersect(c("outcome", "predictor", "component", "path", "estimate", "estimate_basis", "uncertainty_method", "available"), names(x$effects)), drop = FALSE], row.names = FALSE)
  invisible(x)
}

#' @export
print.summary.indirect_effect <- function(x, ...) .print_cssem_effect_summary(x, "mediation", ...)
#' @export
print.summary.causal_indirect_effect <- function(x, ...) .print_cssem_effect_summary(x, "causal mediation", ...)
#' @export
print.summary.causal_effect <- function(x, ...) .print_cssem_effect_summary(x, "causal effect", ...)
#' @export
print.summary.conditional_slopes <- function(x, ...) .print_cssem_effect_summary(x, "conditional slopes", ...)
#' @export
print.summary.conditional_indirect_effect <- function(x, ...) .print_cssem_effect_summary(x, "conditional mediation", ...)
#' @export
print.summary.evidence_report <- function(x, ...) {
  cat("CS-SEM evidence report summary: ", nrow(x$effects), " effect row(s)\n", sep = "")
  print(x$effects[, intersect(c("outcome", "predictor", "path", "estimate", "estimate_basis", "available"), names(x$effects)), drop = FALSE], row.names = FALSE)
  invisible(x)
}

#' @export
coef.fit_states <- function(object, ...) {
  table <- parameter_table(object)
  keep <- !is.na(table$available) & table$available
  out <- table$estimate[keep]; names(out) <- paste(table$construct[keep], table$parameter[keep], table$item[keep], sep = "::")
  attr(out, "parameter_table") <- table
  out
}

#' @export
coef.cssem_association <- function(object, ...) {
  table <- parameter_table(object)
  out <- table$estimate; names(out) <- paste(table$outcome, table$predictor, sep = "~")
  attr(out, "parameter_table") <- table
  out
}

#' @export
coef.indirect_effect <- function(object, ...) { table <- parameter_table(object); out <- table$estimate; names(out) <- table$component; attr(out, "parameter_table") <- table; out }
#' @export
coef.causal_indirect_effect <- coef.indirect_effect
#' @export
coef.causal_effect <- function(object, ...) { table <- parameter_table(object); out <- table$estimate; names(out) <- table$component; attr(out, "parameter_table") <- table; out }
#' @export
coef.conditional_slopes <- function(object, ...) { table <- parameter_table(object); out <- table$estimate; names(out) <- table$path; attr(out, "parameter_table") <- table; out }
#' @export
coef.conditional_indirect_effect <- function(object, ...) { table <- parameter_table(object); out <- table$estimate; names(out) <- table$path; attr(out, "parameter_table") <- table; out }

#' @export
vcov.fit_states <- function(object, ...) .cssem_unavailable_vcov(object, names(coef(object)))
#' @export
vcov.cssem_association <- function(object, ...) .cssem_unavailable_vcov(object, names(coef(object)))
#' @export
vcov.indirect_effect <- function(object, ...) .cssem_unavailable_vcov(object, names(coef(object)))
#' @export
vcov.causal_indirect_effect <- vcov.indirect_effect
#' @export
vcov.causal_effect <- function(object, ...) .cssem_unavailable_vcov(object, names(coef(object)))
#' @export
vcov.conditional_slopes <- function(object, ...) .cssem_unavailable_vcov(object, names(coef(object)))
#' @export
vcov.conditional_indirect_effect <- function(object, ...) .cssem_unavailable_vcov(object, names(coef(object)))

# Resolve the usual numeric or character `parm` forms without requiring
# callers to know the internal row order of a parameter table.
.cssem_parameter_indices <- function(table, parm) {
  if (missing(parm) || is.null(parm)) return(seq_len(nrow(table)))
  if (is.numeric(parm)) return(as.integer(parm))
  component_keys <- if ("component" %in% names(table)) as.character(table$component) else character()
  keys <- if (length(component_keys) && all(as.character(parm) %in% component_keys)) component_keys else
    if (all(!is.na(table$outcome)) && all(!is.na(table$predictor))) paste(table$outcome, table$predictor, sep = "~") else component_keys
  index <- match(as.character(parm), keys)
  if (anyNA(index)) stop("Unknown parameter name in `parm`.", call. = FALSE)
  index
}

#' @export
confint.cssem_association <- function(object, parm, level = .95, ...) {
  table <- parameter_table(object); parm <- .cssem_parameter_indices(table, if (missing(parm)) NULL else parm)
  out <- cbind(table$ci_low[parm], table$ci_high[parm]); colnames(out) <- c(paste0((1 - level) / 2 * 100, " %"), paste0((1 + level) / 2 * 100, " %")); rownames(out) <- paste(table$outcome[parm], table$predictor[parm], sep = "~"); out
}

.confint_from_table <- function(object, parm, level = .95) {
  table <- parameter_table(object); parm <- .cssem_parameter_indices(table, if (missing(parm)) NULL else parm)
  out <- cbind(table$ci_low[parm], table$ci_high[parm]); colnames(out) <- c(paste0((1 - level) / 2 * 100, " %"), paste0((1 + level) / 2 * 100, " %")); out
}
#' @export
confint.fit_states <- function(object, parm, level = .95, ...) .confint_from_table(object, parm, level)
#' @export
confint.indirect_effect <- function(object, parm, level = .95, ...) .confint_from_table(object, parm, level)
#' @export
confint.causal_indirect_effect <- confint.indirect_effect
#' @export
confint.causal_effect <- confint.indirect_effect
#' @export
confint.conditional_slopes <- confint.indirect_effect
#' @export
confint.conditional_indirect_effect <- confint.indirect_effect

#' @export
fitted.fit_states <- function(object, ...) {
  out <- as.data.frame(object$locked_scores); attr(out, "prediction_type") <- "out_of_fold_locked_scores"; out
}

#' @export
fitted.cssem_association <- function(object, type = c("out_of_fold", "in_sample"), ...) {
  type <- match.arg(type); scores <- object$scores
  out <- as.data.frame(lapply(names(object$full_models), function(outcome) {
    model <- object$full_models[[outcome]]
    if (type == "out_of_fold") object$predictions[[outcome]]$theory else .predict_shape_model(model, scores)
  }))
  names(out) <- names(object$full_models); attr(out, "prediction_type") <- type; out
}

#' @export
residuals.cssem_association <- function(object, type = c("out_of_fold", "in_sample"), ...) {
  type <- match.arg(type); fitted_values <- fitted(object, type = type); observed <- object$scores[names(fitted_values)]
  out <- as.data.frame(Map(function(y, f) y - f, observed, fitted_values)); names(out) <- names(fitted_values)
  attr(out, "prediction_type") <- type; out
}

#' @export
nobs.fit_states <- function(object, ...) nrow(object$locked_scores)
#' @export
nobs.cssem_association <- function(object, ...) nrow(object$scores)
#' @export
nobs.indirect_effect <- function(object, ...) as.integer(object$n)
#' @export
nobs.causal_indirect_effect <- nobs.indirect_effect
#' @export
nobs.causal_effect <- nobs.indirect_effect
#' @export
nobs.conditional_slopes <- nobs.indirect_effect
#' @export
nobs.conditional_indirect_effect <- nobs.indirect_effect

#' @export
update.fit_states <- function(object, model = NULL, data = NULL, ..., evaluate = TRUE) {
  if (is.null(model)) model <- object$model
  if (is.null(data)) data <- if (!is.null(object$input_data)) object$input_data else object$data
  if (is.null(model) || is.null(data)) stop("update.fit_states() needs the original model and data; this fit predates stored update inputs, so supply both.", call. = FALSE)
  settings <- if (is.null(object$fit_settings)) list() else object$fit_settings
  settings$model <- model; settings$data <- data
  settings <- utils::modifyList(settings, list(...))
  if (!isTRUE(evaluate)) return(as.call(c(list(as.name("fit_states")), settings)))
  do.call(fit_states, settings)
}

#' @export
update.cssem_association <- function(object, fit = NULL, structure = NULL, ..., evaluate = TRUE) {
  if (is.null(fit)) fit <- object$fit
  if (is.null(structure)) structure <- object$structure
  if (is.null(fit) || is.null(structure)) stop("update.cssem_association() needs the original fit and structure; supply both for an older association object.", call. = FALSE)
  settings <- if (is.null(object$association_settings)) list() else object$association_settings
  settings$fit <- fit; settings$structure <- structure
  settings <- utils::modifyList(settings, list(...))
  if (!isTRUE(evaluate)) return(as.call(c(list(as.name("associate")), settings)))
  do.call(associate, settings)
}
