# Public measurement parameter and item-response views.
#
# These functions expose the fitted encoder on its own scale.  Ordinal
# discrimination and thresholds are graded-response encoder parameters; they
# are not relabeled as CFA loadings.  Continuous intercepts, slopes, and
# residual scales remain in observed-item units.

.measurement_check_fit <- function(fit) {
  if (!inherits(fit, "fit_states")) stop("fit must be a fit_states object.", call. = FALSE)
  if (is.null(fit$full_encoders) || is.null(fit$model$constructs))
    stop("fit does not contain the fitted measurement encoders needed for this view.", call. = FALSE)
}

.measurement_support <- function(fit, construct, item, scale, encoder, index) {
  data <- fit$data
  n <- if (!is.null(fit$locked_scores)) nrow(fit$locked_scores) else NA_integer_
  missing <- observed <- NA_integer_; category_support <- category_counts <- NA_character_
  observed_min <- observed_max <- NA_real_; minimum_category_proportion <- NA_real_
  if (!is.null(data) && item %in% names(data)) {
    raw <- data[[item]]
    missing <- sum(is.na(raw)); observed <- sum(!is.na(raw))
    if (identical(scale, "ordinal")) {
      prepared <- .prepare_item(raw, scale, encoder$keys[[index]], encoder$levels[[index]])
      codes <- prepared$y; k <- length(encoder$levels[[index]])
      counts <- tabulate(codes[!is.na(codes)], nbins = k)
      # Reverse-keyed codes run opposite to the stored raw labels.
      if (isTRUE(encoder$keys[[index]] < 0)) counts <- rev(counts)
      labels <- encoder$levels[[index]]
      category_support <- paste(as.character(labels), collapse = " | ")
      category_counts <- paste(paste(as.character(labels), counts, sep = ":"), collapse = " | ")
      if (observed > 0L) minimum_category_proportion <- min(counts / observed)
    } else {
      values <- .as_numeric_values(raw, scale)
      observed_values <- values[is.finite(values)]
      if (length(observed_values)) {
        observed_min <- min(observed_values); observed_max <- max(observed_values)
      }
    }
  }
  sd_values <- if (is.null(fit$score_posterior_sd)) numeric() else fit$score_posterior_sd[[construct]]
  posterior_sd_median <- if (length(sd_values) && any(is.finite(sd_values))) stats::median(sd_values, na.rm = TRUE) else NA_real_
  posterior_sd_p90 <- if (length(sd_values) && any(is.finite(sd_values))) unname(stats::quantile(sd_values, .9, na.rm = TRUE)) else NA_real_
  posterior_information <- if (is.finite(posterior_sd_median)) 1 / (posterior_sd_median^2 + 1e-12) else NA_real_
  item_metrics <- fit$item_metrics
  item_rows <- if (is.null(item_metrics) || !is.data.frame(item_metrics) || !nrow(item_metrics))
    data.frame() else item_metrics[item_metrics$construct == construct & item_metrics$item == item, , drop = FALSE]
  loss_metric <- if (nrow(item_rows)) paste(unique(item_rows$metric), collapse = "; ") else NA_character_
  held_out_loss <- if (nrow(item_rows) && any(is.finite(item_rows$value))) mean(item_rows$value, na.rm = TRUE) else NA_real_
  list(n = n, observed_n = observed, missing_n = missing, category_support = category_support,
    category_counts = category_counts, observed_min = observed_min, observed_max = observed_max,
    minimum_category_proportion = minimum_category_proportion,
    posterior_sd_median = posterior_sd_median, posterior_sd_p90 = posterior_sd_p90,
    posterior_information = posterior_information, loss_metric = loss_metric,
    held_out_loss = held_out_loss)
}

.measurement_parameter_row <- function(construct, item, scale, parameter, estimate, units,
                                       encoder, index, fit, available = is.finite(estimate),
                                       reason = if (available) "" else "Encoder did not return a finite parameter.") {
  support <- .measurement_support(fit, construct, item, scale, encoder, index)
  data.frame(construct = construct, item = item, scale = scale, parameter = parameter,
    estimate = unname(estimate), units = units, basis = paste0("marginal_", scale, "_encoder"),
    reverse_key = if (scale == "manifest") encoder$key < 0 else encoder$keys[[index]] < 0,
    observed_n = support$observed_n, missing_n = support$missing_n,
    category_support = support$category_support, category_counts = support$category_counts,
    observed_min = support$observed_min, observed_max = support$observed_max,
    minimum_category_proportion = support$minimum_category_proportion,
    posterior_sd_median = support$posterior_sd_median, posterior_sd_p90 = support$posterior_sd_p90,
    posterior_information = support$posterior_information, loss_metric = support$loss_metric,
    held_out_loss = support$held_out_loss, available = available,
    availability_reason = reason, stringsAsFactors = FALSE)
}

#' Return scale-aware fitted measurement parameters
#'
#' This is the public measurement view of the fitted encoders. Ordinal rows
#' report graded-response discriminations and thresholds; continuous rows report
#' observed-item intercepts, slopes, and residual SDs; manifest rows document
#' the passthrough without inventing a fitted loading. Support counts,
#' category labels, held-out loss family, and construct-level posterior
#' information are repeated on each item-parameter row for easy reporting.
#'
#' @param fit A `fit_states` object.
#' @param construct Optional construct name. `NULL` returns all constructs.
#' @return A data frame of class `cssem_measurement_parameters`.
#' @export
measurement_parameters <- function(fit, construct = NULL) {
  .measurement_check_fit(fit)
  constructs <- names(fit$full_encoders)
  if (!is.null(construct)) {
    if (length(construct) != 1L || !construct %in% constructs) stop("construct must name one fitted construct.", call. = FALSE)
    constructs <- construct
  }
  rows <- list(); index <- 0L
  add <- function(...) { index <<- index + 1L; rows[[index]] <<- .measurement_parameter_row(...) }
  for (nm in constructs) {
    encoder <- fit$full_encoders[[nm]]; spec <- fit$model$constructs[[nm]]
    if (identical(encoder$type, "manifest")) {
      add(nm, spec$indicators[[1L]], "manifest", "manifest_scale", NA_real_,
        if (isTRUE(encoder$standardize)) "standardized manifest units" else "manifest units",
        encoder, 1L, fit, available = FALSE,
        reason = "Manifest constructs have no fitted measurement parameter.")
      next
    }
    for (j in seq_along(encoder$encoders)) {
      item <- encoder$indicators[[j]]; item_fit <- encoder$encoders[[j]]; scale <- encoder$scales[[j]]
      if (identical(item_fit$type, "ordinal")) {
        add(nm, item, scale, "discrimination", item_fit$a, "latent-node logit slope", encoder, j, fit)
        for (k in seq_along(item_fit$tau)) add(nm, item, scale, paste0("threshold_", k), item_fit$tau[[k]],
          "latent-node logit threshold", encoder, j, fit)
      } else {
        add(nm, item, scale, "intercept", item_fit$intercept, "observed item units", encoder, j, fit)
        add(nm, item, scale, "slope", item_fit$slope, "observed item units per latent-node unit", encoder, j, fit)
        add(nm, item, scale, "residual_sd", item_fit$sigma, "observed item units", encoder, j, fit)
      }
    }
  }
  out <- if (!length(rows)) data.frame() else do.call(rbind, rows)
  class(out) <- c("cssem_measurement_parameters", "data.frame")
  out
}

#' Return an item-response curve from a fitted encoder
#'
#' Ordinal curves contain one row per latent value and declared response
#' category, with probabilities summing to one at each latent value. Continuous
#' curves contain the expected response and residual SD. Curves use the full
#' data encoder retained for scoring and are not cross-fitted predictions.
#'
#' @param fit A `fit_states` object.
#' @param construct Fitted construct name.
#' @param item Indicator column name within the construct.
#' @param latent Numeric latent-node values at which to evaluate the curve.
#' @return A data frame of class `cssem_item_response_curve`.
#' @export
item_response_curve <- function(fit, construct, item, latent = seq(-4, 4, length.out = 81L)) {
  .measurement_check_fit(fit)
  if (length(construct) != 1L || !construct %in% names(fit$full_encoders)) stop("construct must name one fitted construct.", call. = FALSE)
  encoder <- fit$full_encoders[[construct]]
  if (identical(encoder$type, "manifest")) stop("Item-response curves are not defined for manifest constructs.", call. = FALSE)
  if (length(item) != 1L || !item %in% encoder$indicators) stop("item must name an indicator in the selected construct.", call. = FALSE)
  latent <- as.numeric(latent)
  if (!length(latent) || any(!is.finite(latent))) stop("latent must contain finite numeric values.", call. = FALSE)
  j <- match(item, encoder$indicators); item_fit <- encoder$encoders[[j]]; scale <- encoder$scales[[j]]
  if (identical(item_fit$type, "ordinal")) {
    probability <- .ordinal_probability(item_fit$a, item_fit$tau, latent, item_fit$k)
    labels <- encoder$levels[[j]]
    if (isTRUE(encoder$keys[[j]] < 0)) labels <- rev(labels)
    expected <- drop(probability %*% seq_len(item_fit$k))
    out <- data.frame(construct = construct, item = item, scale = scale,
      latent = rep(latent, each = item_fit$k), category = rep(as.character(labels), times = length(latent)),
      probability = as.vector(t(probability)), expected_response = rep(expected, each = item_fit$k),
      residual_sd = NA_real_, stringsAsFactors = FALSE)
  } else {
    expected <- item_fit$intercept + item_fit$slope * latent
    out <- data.frame(construct = construct, item = item, scale = scale, latent = latent,
      category = NA_character_, probability = NA_real_, expected_response = expected,
      residual_sd = rep(item_fit$sigma, length(latent)), stringsAsFactors = FALSE)
  }
  class(out) <- c("cssem_item_response_curve", "data.frame")
  out
}

#' @export
print.cssem_measurement_parameters <- function(x, ...) {
  cat("CS-SEM measurement parameters: ", nrow(x), " row(s)\n", sep = "")
  view <- x[, intersect(c("construct", "item", "scale", "parameter", "estimate", "units"), names(x)), drop = FALSE]
  print.data.frame(view, row.names = FALSE)
  invisible(x)
}

#' @export
print.cssem_item_response_curve <- function(x, ...) {
  cat("CS-SEM item-response curve: ", unique(x$construct), " / ", unique(x$item), "\n", sep = "")
  view <- utils::head(x, 10L); class(view) <- "data.frame"
  print.data.frame(view, row.names = FALSE)
  invisible(x)
}
