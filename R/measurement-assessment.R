# Scale-aware measurement assessment.  This layer reports diagnostics that
# are defined for the CS-SEM encoder and labels conventional SEM quantities
# that the encoder does not identify.

.measurement_item_values <- function(fit, construct, item) {
  encoder <- fit$full_encoders[[construct]]
  j <- match(item, encoder$indicators)
  raw <- fit$data[[item]]
  if (identical(encoder$type, "manifest")) {
    values <- .as_numeric_values(raw, "manifest")
    if (encoder$key < 0) values <- -values
    return(list(values = values, scale = "manifest"))
  }
  scale <- encoder$scales[[j]]
  values <- if (identical(scale, "ordinal"))
    .prepare_item(raw, scale, encoder$keys[[j]], encoder$levels[[j]])$y else
    .as_numeric_values(raw, scale) * if (encoder$keys[[j]] < 0) -1 else 1
  list(values = values, scale = scale)
}

.measurement_item_cor <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 3L || length(unique(x[keep])) < 2L || length(unique(y[keep])) < 2L) return(NA_real_)
  suppressWarnings(stats::cor(x[keep], y[keep], method = "spearman"))
}

.measurement_item_correlations <- function(fit, constructs) {
  rows <- list(); index <- 0L
  add <- function(...) { index <<- index + 1L; rows[[index]] <<- data.frame(..., stringsAsFactors = FALSE) }
  for (construct in constructs) {
    encoder <- fit$full_encoders[[construct]]
    score <- fit$locked_scores[[construct]]
    for (item in encoder$indicators) {
      values <- .measurement_item_values(fit, construct, item)
      keep <- is.finite(values$values) & is.finite(score)
      add(construct = construct, item = item, scale = values$scale,
        correlation = .measurement_item_cor(values$values, score), n = sum(keep),
        method = "descriptive_spearman_item_score_correlation",
        interpretation = "Descriptive item-to-state association; not a CFA loading or cross-loading.")
    }
  }
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

.measurement_construct_items <- function(fit, construct) fit$full_encoders[[construct]]$indicators

.measurement_abs_pairwise <- function(values) {
  if (length(values) < 2L) return(NA_real_)
  correlations <- c()
  for (i in seq_len(length(values) - 1L)) for (j in (i + 1L):length(values))
    correlations <- c(correlations, abs(.measurement_item_cor(values[[i]], values[[j]])))
  if (!length(correlations) || !any(is.finite(correlations))) NA_real_ else mean(correlations, na.rm = TRUE)
}

.measurement_validity_pairs <- function(fit, constructs) {
  if (length(constructs) < 2L) return(data.frame())
  rows <- list(); index <- 0L
  for (i in seq_len(length(constructs) - 1L)) for (j in (i + 1L):length(constructs)) {
    a <- constructs[[i]]; b <- constructs[[j]]
    a_items <- lapply(.measurement_construct_items(fit, a), function(item) .measurement_item_values(fit, a, item)$values)
    b_items <- lapply(.measurement_construct_items(fit, b), function(item) .measurement_item_values(fit, b, item)$values)
    heterotrait <- c()
    for (x in a_items) for (y in b_items) heterotrait <- c(heterotrait, abs(.measurement_item_cor(x, y)))
    heterotrait <- if (length(heterotrait) && any(is.finite(heterotrait))) mean(heterotrait, na.rm = TRUE) else NA_real_
    monotrait_a <- .measurement_abs_pairwise(a_items); monotrait_b <- .measurement_abs_pairwise(b_items)
    htmt <- if (is.finite(heterotrait) && is.finite(monotrait_a) && is.finite(monotrait_b) && monotrait_a > 0 && monotrait_b > 0)
      heterotrait / sqrt(monotrait_a * monotrait_b) else NA_real_
    score_correlation <- .measurement_item_cor(fit$locked_scores[[a]], fit$locked_scores[[b]])
    index <- index + 1L
    rows[[index]] <- data.frame(construct_a = a, construct_b = b, htmt = htmt,
      heterotrait_correlation = heterotrait, monotrait_a = monotrait_a, monotrait_b = monotrait_b,
      score_correlation = score_correlation, method = "descriptive_spearman_item_correlations",
      defined = is.finite(htmt), interpretation = "Descriptive diagnostic; not a validated HTMT decision rule.",
      availability_reason = if (is.finite(htmt)) "" else "HTMT-like ratio is undefined when within-construct item correlations are unavailable or zero.",
      stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

.measurement_collinearity <- function(fit, constructs) {
  rows <- lapply(constructs, function(construct) {
    others <- setdiff(constructs, construct)
    if (!length(others)) return(data.frame(construct = construct, r_squared = NA_real_, vif = NA_real_, n = 0L,
      method = "locked_score_OLS", interpretation = "No other construct is available for a collinearity diagnostic.", stringsAsFactors = FALSE))
    frame <- fit$locked_scores[, c(construct, others), drop = FALSE]
    keep <- stats::complete.cases(frame); n <- sum(keep)
    if (n < 3L) return(data.frame(construct = construct, r_squared = NA_real_, vif = NA_real_, n = n,
      method = "locked_score_OLS", interpretation = "Insufficient complete rows for a collinearity diagnostic.", stringsAsFactors = FALSE))
    fit_vif <- stats::lm(stats::reformulate(others, construct), frame[keep, , drop = FALSE])
    r2 <- summary(fit_vif)$r.squared
    data.frame(construct = construct, r_squared = r2, vif = if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf,
      n = n, method = "locked_score_OLS", interpretation = "Descriptive VIF on locked construct states; not a survey-design or multilevel diagnostic.", stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Assess reliability, convergent diagnostics, and construct distinctiveness
#'
#' Returns quantities defined for the fitted CS-SEM encoder and explicitly
#' labels conventional quantities that are unavailable. EAP reliability is the
#' posterior signal/(signal + posterior variance) already used by the
#' structural errors-in-variables correction. The item-score R2 and HTMT-like
#' ratio are descriptive Spearman diagnostics; they are not CFA loadings, AVE,
#' or validated threshold decisions. No verdict thresholds are applied.
#'
#' @param fit A `fit_states` object with retained training data.
#' @param construct Optional construct name. `NULL` assesses all constructs.
#' @return An object of class `cssem_measurement_assessment` containing
#'   `constructs`, `validity`, `item_score_correlations`, `collinearity`, and
#'   a `methodology` description.
#' @export
measurement_assessment <- function(fit, construct = NULL) {
  assessment_call <- match.call()
  .measurement_check_fit(fit)
  .require_retained_data(fit, "measurement_assessment")
  constructs <- names(fit$full_encoders)
  if (!is.null(construct)) {
    if (length(construct) != 1L || !construct %in% constructs) stop("construct must name one fitted construct.", call. = FALSE)
    constructs <- construct
  }
  item_correlations <- .measurement_item_correlations(fit, constructs)
  rows <- lapply(constructs, function(nm) {
    own <- item_correlations[item_correlations$construct == nm, , drop = FALSE]
    values <- own$correlation^2
    values <- values[is.finite(values)]
    sd_values <- if (is.null(fit$score_posterior_sd)) numeric() else fit$score_posterior_sd[[nm]]
    median_sd <- if (length(sd_values) && any(is.finite(sd_values))) stats::median(sd_values, na.rm = TRUE) else NA_real_
    p90_sd <- if (length(sd_values) && any(is.finite(sd_values))) unname(stats::quantile(sd_values, .9, na.rm = TRUE)) else NA_real_
    data.frame(construct = nm, n_items = length(fit$full_encoders[[nm]]$indicators),
      scale_family = paste(unique(fit$full_encoders[[nm]]$scales), collapse = "; "),
      eap_reliability = if (is.null(fit$reliability)) NA_real_ else unname(fit$reliability[[nm]]),
      stability = if (is.null(fit$stability)) NA_real_ else unname(fit$stability[[nm]]),
      descriptive_convergent_r2 = if (length(values)) mean(values) else NA_real_,
      ave = NA_real_, ave_reason = "AVE is not defined: this encoder does not estimate CFA loadings or communalities.",
      posterior_sd_median = median_sd, posterior_sd_p90 = p90_sd,
      posterior_information = if (is.finite(median_sd)) 1 / (median_sd^2 + 1e-12) else NA_real_,
      stringsAsFactors = FALSE)
  })
  result <- structure(list(constructs = do.call(rbind, rows),
    validity = .measurement_validity_pairs(fit, constructs),
    item_score_correlations = item_correlations,
    collinearity = .measurement_collinearity(fit, constructs),
    methodology = list(
      reliability = "EAP posterior signal/(signal + posterior variance)",
      convergent = "Mean squared descriptive Spearman correlation between each item and its locked construct state",
      htmt = "Descriptive Spearman item-correlation ratio; no threshold verdict is applied",
      ave = "Unavailable because the CS-SEM encoder does not estimate CFA loadings or communalities",
      collinearity = "OLS VIF on locked construct states")),
    class = "cssem_measurement_assessment")
  fit_provenance <- fit$provenance_record
  input <- if (!is.null(fit_provenance)) fit_provenance$input else
    .cssem_provenance_input_summary(fit$data,
      retained_rows = if (is.null(fit$row_ids)) seq_len(nrow(fit$data)) else as.integer(fit$row_ids))
  result$provenance_record <- .cssem_provenance_record("measurement_assessment",
    .cssem_provenance_call(assessment_call, "fit"),
    settings = list(constructs = as.character(constructs)), input = input,
    parent = if (is.null(fit_provenance)) list() else list(fit = fit_provenance),
    packages = "stats")
  result
}

#' @export
summary.cssem_measurement_assessment <- function(object, ...) {
  structure(list(constructs = object$constructs, validity = object$validity,
    item_score_correlations = object$item_score_correlations,
    collinearity = object$collinearity, methodology = object$methodology),
    class = c("summary.cssem_measurement_assessment", "summary"))
}

#' @export
print.cssem_measurement_assessment <- function(x, ...) {
  cat("CS-SEM measurement assessment: ", nrow(x$constructs), " construct(s)\n", sep = "")
  print.data.frame(x$constructs[, intersect(c("construct", "scale_family", "eap_reliability", "descriptive_convergent_r2", "ave"), names(x$constructs)), drop = FALSE], row.names = FALSE)
  if (nrow(x$validity)) {
    cat("\nConstruct distinctiveness diagnostics\n")
    print.data.frame(x$validity[, intersect(c("construct_a", "construct_b", "htmt", "score_correlation", "defined"), names(x$validity)), drop = FALSE], row.names = FALSE)
  }
  invisible(x)
}

#' @export
print.summary.cssem_measurement_assessment <- function(x, ...) print.cssem_measurement_assessment(x, ...)
