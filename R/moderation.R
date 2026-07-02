# Public moderated-mediation API. Conditional indirect effects are obtained by
# holding the moderator construct at fixed levels and running the mediation
# propagation engine, so any moderated path (a-path, b-path, or both; single,
# parallel, or serial) is handled uniformly. Reporting centers on the quantities
# researchers interpret: the conditional indirect effect at each moderator level
# and the index of moderated mediation.

.level_labels <- function(levels) vapply(levels, function(level)
  if (abs(level) < 1e-9) "mean" else sprintf("%+g SD", level), character(1))

# Total indirect effect of x on y at each moderator level, obtained by fixing the
# moderator column and decomposing with the supplied models.
.moderated_conditional <- function(models, scores, order, x, y, paths, moderator, levels, delta) {
  vapply(levels, function(level) {
    at_level <- scores; at_level[[moderator]] <- level
    .decompose_effects(models, at_level, order, x, y, paths, delta)$indirect_total
  }, numeric(1))
}

.moderated_index <- function(conditional, levels) {
  span <- max(levels) - min(levels)
  if (span <= 0) return(NA_real_)
  (conditional[[which.max(levels)]] - conditional[[which.min(levels)]]) / span
}

# Percentile bootstrap of the conditional indirect effects and the index, holding
# shapes and reliability fixed and resampling respondents.
.moderated_mediation_bootstrap <- function(models, scores, order, x, y, paths, moderator, levels,
                                           reliability, disattenuate, delta, replicates, seed) {
  shapes_by_outcome <- lapply(models, function(model) if (is.null(model)) NULL else model$shapes)
  n <- nrow(scores); n_levels <- length(levels)
  naive_conditional <- matrix(NA_real_, replicates, n_levels)
  dis_conditional <- matrix(NA_real_, replicates, n_levels)
  naive_index <- rep(NA_real_, replicates); dis_index <- rep(NA_real_, replicates)
  set.seed(seed + 707L)
  for (b in seq_len(replicates)) {
    resampled <- scores[sample.int(n, n, replace = TRUE), , drop = FALSE]
    refit <- .refit_models(resampled, shapes_by_outcome)
    nc <- .moderated_conditional(refit, resampled, order, x, y, paths, moderator, levels, delta)
    naive_conditional[b, ] <- nc; naive_index[b] <- .moderated_index(nc, levels)
    if (disattenuate && !is.null(reliability)) {
      corrected <- .corrected_models(refit, resampled, reliability)$models
      dc <- .moderated_conditional(corrected, resampled, order, x, y, paths, moderator, levels, delta)
      dis_conditional[b, ] <- dc; dis_index[b] <- .moderated_index(dc, levels)
    }
  }
  ci <- function(column) { column <- column[is.finite(column)]
    if (!length(column)) c(NA_real_, NA_real_) else stats::quantile(column, c(.025, .975), names = FALSE) }
  list(
    naive_conditional = t(apply(naive_conditional, 2L, ci)), dis_conditional = t(apply(dis_conditional, 2L, ci)),
    naive_index = ci(naive_index), dis_index = ci(dis_index)
  )
}

#' Decompose a moderated mediation effect on locked construct states
#'
#' Estimates the indirect effect of `x` on `y` through the declared mediators as
#' a function of a `moderator` construct: the conditional indirect effect at each
#' moderator level and the index of moderated mediation (its change per
#' standard-deviation unit of the moderator). The moderator must appear in a
#' declared interaction term (colon syntax, e.g. `"M:W"`) in the association's
#' structure. Linear and monotone edges are disattenuated as in
#' [cssem_mediation()]; the decomposition is associational and makes no causal
#' claim.
#'
#' @param association A `cssem_association` from [cssem_associate()] whose
#'   structure declares the mediating paths and the moderating interaction.
#' @param x Predictor construct name.
#' @param y Outcome construct name.
#' @param moderator Moderator construct name; must occur in an interaction term.
#' @param levels Moderator levels, in standard-deviation units of the
#'   standardized construct states. Defaults to low, mean, and high (-1, 0, 1).
#' @param disattenuate Report disattenuated effects when reliability is
#'   available. Defaults to `TRUE`.
#' @param eiv_bootstrap Percentile-bootstrap resamples for intervals. Zero omits.
#' @param delta Predictor contrast in standard-deviation units. Defaults to one.
#' @param seed Bootstrap seed.
#' @return An object of class `cssem_moderated_mediation`.
#' @examples
#' # cssem_moderated_mediation(association, "X", "Y", "W", eiv_bootstrap = 500)
#' @export
cssem_moderated_mediation <- function(association, x, y, moderator, levels = c(-1, 0, 1),
                                      disattenuate = TRUE, eiv_bootstrap = 0L, delta = 1, seed = 1L) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  scores <- association$scores
  if (is.null(scores)) stop("association does not carry locked scores; re-run cssem_associate().", call. = FALSE)
  all_names <- names(scores)
  if (!all(c(x, y, moderator) %in% all_names)) stop("x, y, and moderator must be locked construct names.", call. = FALSE)
  if (length(unique(c(x, y, moderator))) != 3L) stop("x, y, and moderator must be distinct.", call. = FALSE)
  if (!is.numeric(levels) || length(levels) < 2L || anyNA(levels)) stop("levels must be at least two finite values.", call. = FALSE)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)

  moderates <- any(vapply(association$full_models, function(model) {
    !is.null(model) && any(vapply(names(model$shapes), function(p) .is_interaction(p) && moderator %in% .interaction_terms(p), logical(1)))
  }, logical(1)))
  if (!moderates) stop("moderator does not appear in any declared interaction term.", call. = FALSE)

  models <- stats::setNames(vector("list", length(all_names)), all_names)
  for (outcome in names(association$full_models)) models[[outcome]] <- association$full_models[[outcome]]
  order <- .resolve_temporal_order(association$structure, all_names)
  paths <- .structure_paths(association$structure, x, y)
  if (!length(Filter(function(path) length(path) > 2L, paths))) stop("No mediating path connects x to y.", call. = FALSE)

  reliability <- if (isTRUE(disattenuate)) association$reliability else NULL
  if (!is.null(reliability) && all(is.na(reliability))) reliability <- NULL
  path_constructs <- unique(c(unlist(paths, use.names = FALSE), moderator))
  min_reliability <- if (is.null(reliability)) NA_real_ else {
    values <- reliability[intersect(path_constructs, names(reliability))]
    if (!length(values) || all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
  }

  naive_conditional <- .moderated_conditional(models, scores, order, x, y, paths, moderator, levels, delta)
  naive_index <- .moderated_index(naive_conditional, levels)
  disattenuated <- !is.null(reliability)
  dis_conditional <- if (disattenuated) .moderated_conditional(.corrected_models(models, scores, reliability)$models, scores, order, x, y, paths, moderator, levels, delta) else rep(NA_real_, length(levels))
  dis_index <- if (disattenuated) .moderated_index(dis_conditional, levels) else NA_real_

  intervals <- if (eiv_bootstrap > 0L) .moderated_mediation_bootstrap(models, scores, order, x, y, paths,
    moderator, levels, reliability, disattenuated, delta, eiv_bootstrap, seed) else NULL

  reported <- function(naive, dis) if (disattenuated && is.finite(dis)) dis else naive
  reported_ci <- function(naive_ci, dis_ci, use_dis) if (is.null(intervals)) c(NA_real_, NA_real_) else if (use_dis) dis_ci else naive_ci

  conditional <- data.frame(
    level = .level_labels(levels), moderator_value = levels,
    naive_indirect = naive_conditional, disattenuated_indirect = dis_conditional,
    indirect = mapply(reported, naive_conditional, dis_conditional),
    stringsAsFactors = FALSE
  )
  if (!is.null(intervals)) {
    use_dis <- disattenuated & is.finite(dis_conditional)
    ci_source <- if (disattenuated) intervals$dis_conditional else intervals$naive_conditional
    naive_source <- intervals$naive_conditional
    conditional$ci_low <- ifelse(use_dis, ci_source[, 1L], naive_source[, 1L])
    conditional$ci_high <- ifelse(use_dis, ci_source[, 2L], naive_source[, 2L])
  }

  index_use_dis <- disattenuated && is.finite(dis_index)
  index <- list(
    estimate = reported(naive_index, dis_index), basis = if (index_use_dis) "disattenuated" else "naive",
    ci = reported_ci(if (is.null(intervals)) NULL else intervals$naive_index,
                     if (is.null(intervals)) NULL else intervals$dis_index, index_use_dis)
  )

  structure(list(x = x, y = y, moderator = moderator, levels = levels, conditional = conditional, index = index,
    n = nrow(scores), disattenuated = disattenuated, bootstrap = eiv_bootstrap,
    min_reliability = min_reliability, status = "associational"), class = "cssem_moderated_mediation")
}

#' Print a moderated mediation decomposition
#'
#' @param x A `cssem_moderated_mediation` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_moderated_mediation <- function(x, ...) {
  mediators <- setdiff(names(x$conditional), NA)
  cat(sprintf("CS-SEM moderated mediation: %s -> %s, moderated by %s  (n = %d)\n", x$x, x$y, x$moderator, x$n))
  basis <- if (isTRUE(x$disattenuated)) "disattenuated (errors-in-variables)" else "naive"
  intervals <- if (x$bootstrap > 0L) sprintf("; 95%% bootstrap intervals (%d resamples)", x$bootstrap) else ""
  cat("Effects are ", basis, intervals, ".\n\n", sep = "")
  format_effect <- function(effect, low, high) if (is.na(low)) sprintf("% .3f", effect) else sprintf("% .3f  [% .3f, % .3f]", effect, low, high)
  cat(sprintf("  conditional indirect effect (%s -> %s):\n", x$x, x$y))
  has_ci <- "ci_low" %in% names(x$conditional)
  for (i in seq_len(nrow(x$conditional))) {
    cat(sprintf("    %s = %-8s %s\n", x$moderator, x$conditional$level[i],
      format_effect(x$conditional$indirect[i], if (has_ci) x$conditional$ci_low[i] else NA_real_, if (has_ci) x$conditional$ci_high[i] else NA_real_)))
  }
  cat(sprintf("\n  index of moderated mediation: %s\n", format_effect(x$index$estimate, x$index$ci[[1L]], x$index$ci[[2L]])))
  # Plain-language reading of the index.
  direction <- if (x$index$estimate > 0) "strengthens" else "weakens"
  detectable <- has_ci && is.finite(x$index$ci[[1L]]) && (x$index$ci[[1L]] > 0 || x$index$ci[[2L]] < 0)
  message_line <- if (has_ci && !detectable) sprintf("The indirect effect of %s on %s through the mediator(s) does not vary detectably with %s.", x$x, x$y, x$moderator)
    else sprintf("The indirect effect of %s on %s through the mediator(s) %s as %s increases.", x$x, x$y, direction, x$moderator)
  cat("\n  ", message_line, "\n", sep = "")
  if (is.finite(x$min_reliability) && x$min_reliability < 0.5)
    cat(sprintf("\nLowest construct reliability on this path is %.2f; disattenuation is partial and intervals can under-cover.\n", x$min_reliability))
  cat("\nAssociational decomposition under the declared temporal order; not a causal mediation claim.\n")
  invisible(x)
}
