# Stable numerical diagnostics for measurement fits and structural corrections.

.numerical_diagnostic_columns <- c(
  "component", "construct", "item", "outcome", "predictor", "scale", "n",
  "iterations", "objective_initial", "objective_final", "objective_change",
  "optimizer_status", "rank", "condition_number", "corrected_condition_number",
  "singular", "converged", "correction_shrink", "correction_strength",
  "reliability_floor_applied", "stable", "message"
)

.numerical_empty <- function() {
  out <- data.frame(stringsAsFactors = FALSE)
  for (name in .numerical_diagnostic_columns)
    out[[name]] <- if (name %in% c("n", "iterations", "rank")) integer() else
      if (name %in% c("singular", "converged", "reliability_floor_applied", "stable")) logical() else
      if (name %in% c("component", "construct", "item", "outcome", "predictor", "scale", "optimizer_status", "message")) character() else numeric()
  out[, .numerical_diagnostic_columns, drop = FALSE]
}

.numerical_row <- function(...) {
  values <- list(...)
  out <- as.data.frame(values, stringsAsFactors = FALSE)
  for (name in setdiff(.numerical_diagnostic_columns, names(out))) {
    out[[name]] <- if (name %in% c("n", "iterations", "rank")) NA_integer_ else
      if (name %in% c("singular", "converged", "reliability_floor_applied", "stable")) NA else
      if (name %in% c("component", "construct", "item", "outcome", "predictor", "scale", "optimizer_status", "message")) NA_character_ else NA_real_
  }
  out[, .numerical_diagnostic_columns, drop = FALSE]
}

.measurement_condition <- function(nodes) {
  design <- cbind(1, as.numeric(nodes))
  rank <- qr(design)$rank
  condition <- tryCatch(kappa(design), error = function(e) NA_real_)
  list(rank = as.integer(rank), condition_number = unname(condition), singular = rank < min(dim(design)))
}

.measurement_status_text <- function(status) {
  if (is.null(status) || !length(status)) return("unavailable")
  method <- if (is.null(status$method)) "unknown" else status$method
  convergence <- if (is.null(status$convergence)) "NA" else as.character(status$convergence)
  message <- if (is.null(status$message) || !nzchar(status$message)) "" else paste0(": ", status$message)
  paste0(method, " convergence=", convergence, message)
}

.measurement_numerical_diagnostics <- function(encoders, model, n = NA_integer_) {
  rows <- list(); index <- 0L
  add <- function(...) { index <<- index + 1L; rows[[index]] <<- .numerical_row(...) }
  for (construct in names(encoders)) {
    encoder <- encoders[[construct]]; spec <- model$constructs[[construct]]
    if (identical(encoder$type, "manifest")) {
      add(component = "measurement", construct = construct, item = encoder$indicators[[1L]],
        scale = "manifest", n = NA_integer_, iterations = 0L,
        optimizer_status = "not_applicable", singular = NA, converged = NA,
        message = "Manifest passthrough has no iterative measurement optimizer.")
      next
    }
    trace <- as.numeric(encoder$objective_trace)
    initial <- if (length(trace)) trace[[1L]] else NA_real_
    final <- if (length(trace)) trace[[length(trace)]] else NA_real_
    objective_change <- if (is.finite(initial) && is.finite(final)) final - initial else NA_real_
    basis <- .measurement_condition(encoder$nodes)
    statuses <- encoder$optimizer_status
    for (j in seq_along(encoder$indicators)) {
      status <- if (length(statuses) >= j) statuses[[j]] else NULL
      add(component = "measurement", construct = construct, item = encoder$indicators[[j]],
        scale = spec$scales[[j]], n = n,
        iterations = as.integer(encoder$iterations), objective_initial = initial,
        objective_final = final, objective_change = objective_change,
        optimizer_status = .measurement_status_text(status), rank = basis$rank,
        condition_number = basis$condition_number, singular = basis$singular,
        converged = isTRUE(encoder$converged),
        message = if (isTRUE(encoder$converged)) "" else
          "Increase iterations or inspect preflight support and objective trace.")
    }
  }
  if (!length(rows)) .numerical_empty() else do.call(rbind, rows)
}

.structural_numerical_diagnostics <- function(corrected) {
  if (is.null(corrected) || !nrow(corrected)) return(.numerical_empty())
  do.call(rbind, lapply(seq_len(nrow(corrected)), function(i) {
    row <- corrected[i, , drop = FALSE]
    .numerical_row(component = "structural_eiv", outcome = row$outcome[[1L]],
      predictor = row$predictor[[1L]], n = row$eiv_n[[1L]],
      rank = row$eiv_rank[[1L]], condition_number = row$eiv_condition_number[[1L]],
      corrected_condition_number = row$eiv_corrected_condition_number[[1L]],
      singular = row$eiv_singular[[1L]], correction_shrink = row$correction_shrink[[1L]],
      correction_strength = row$correction_strength[[1L]],
      reliability_floor_applied = row$reliability_floor_applied[[1L]],
      stable = row$eiv_stable[[1L]], message = row$eiv_diagnostic[[1L]])
  }))
}

#' Return numerical and stabilization diagnostics
#'
#' Measurement rows expose the marginal objective trace, final optimizer status,
#' convergence state, latent design rank, and condition number. If an
#' association is supplied, structural rows also expose the effective
#' errors-in-variables correction strength and covariance conditioning. These
#' are numerical diagnostics, not accuracy guarantees or inferential tests.
#'
#' @param fit A `fit_states` object.
#' @param association Optional `cssem_association` object built from `fit`.
#' @return A data frame of class `cssem_numerical_diagnostics`.
#' @export
numerical_diagnostics <- function(fit, association = NULL) {
  if (!inherits(fit, "fit_states")) stop("fit must be a fit_states.", call. = FALSE)
  if (!is.null(association) && !inherits(association, "cssem_association"))
    stop("association must be a cssem_association.", call. = FALSE)
  measurement <- if (is.null(fit$numerical_diagnostics)) .numerical_empty() else fit$numerical_diagnostics
  structural <- if (is.null(association$numerical_diagnostics)) .numerical_empty() else association$numerical_diagnostics
  out <- if (nrow(structural)) rbind(measurement, structural) else measurement
  class(out) <- c("cssem_numerical_diagnostics", "data.frame")
  out
}

#' @export
print.cssem_numerical_diagnostics <- function(x, ...) {
  cat("CS-SEM numerical diagnostics: ", nrow(x), " row(s)\n", sep = "")
  view <- x[, intersect(c("component", "construct", "item", "outcome", "predictor",
    "objective_change", "optimizer_status", "rank", "condition_number",
    "correction_strength", "stable", "message"), names(x)), drop = FALSE]
  print.data.frame(view, row.names = FALSE)
  invisible(x)
}
