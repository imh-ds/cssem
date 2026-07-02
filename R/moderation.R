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

# Contiguous moderator intervals over which a logical significance flag is TRUE.
# A positive interaction typically yields two significant regions (a strongly
# negative and a strongly positive slope) with a non-significant band between,
# so reporting the range of significant points alone would hide the gap.
.significant_intervals <- function(grid, significant) {
  if (!any(significant)) return(list())
  runs <- rle(significant); ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1L
  Map(function(s, e) c(grid[[s]], grid[[e]]), starts[runs$values], ends[runs$values])
}

# Extract a predictor's slope from a fitted shape model or a corrected linear
# model (the latter carries no `maps`, so fall back to positional lookup).
.model_slope <- function(model, name) {
  if (!is.null(model$maps) && !is.null(model$maps[[name]])) return(model$coefficient[[model$maps[[name]][[1L]]]])
  model$coefficient[[1L + match(name, names(model$shapes))]]
}

# The interaction predictor on `outcome` that pairs `predictor` with `moderator`.
.find_interaction <- function(model, predictor, moderator) {
  hit <- Filter(function(p) .is_interaction(p) && setequal(.interaction_terms(p), c(predictor, moderator)), names(model$shapes))
  if (length(hit)) hit[[1L]] else NA_character_
}

# Percentile bootstrap of the focal main-effect and interaction coefficients, so
# the conditional slope (linear in the moderator) has intervals at any level.
.simple_slope_bootstrap <- function(models, scores, outcome, predictor, interaction_name,
                                    reliability, disattenuate, replicates, seed) {
  shapes_by_outcome <- lapply(models, function(model) if (is.null(model)) NULL else model$shapes)
  n <- nrow(scores); main <- rep(NA_real_, replicates); interaction <- rep(NA_real_, replicates)
  set.seed(seed + 606L)
  for (b in seq_len(replicates)) {
    resampled <- scores[sample.int(n, n, replace = TRUE), , drop = FALSE]
    refit <- .refit_models(resampled, shapes_by_outcome)
    use <- if (disattenuate && !is.null(reliability)) .corrected_models(refit, resampled, reliability)$models else refit
    main[b] <- .model_slope(use[[outcome]], predictor); interaction[b] <- .model_slope(use[[outcome]], interaction_name)
  }
  list(main = main, interaction = interaction)
}

#' Conditional (simple) slopes of a moderated effect
#'
#' Reports the slope of `predictor` on `outcome` at levels of a `moderator`
#' (`slope(w) = b_predictor + b_interaction * w`), with bootstrap intervals and a
#' Johnson-Neyman region: the range of the moderator over which the conditional
#' slope is distinguishable from zero. The moderator must appear in a declared
#' `predictor`-by-`moderator` interaction on the outcome. The focal main effect is
#' disattenuated; the interaction term is treated as observed.
#'
#' @param association A `cssem_association` from [cssem_associate()].
#' @param outcome Outcome construct carrying the moderated effect.
#' @param predictor Focal predictor whose slope is conditioned.
#' @param moderator Moderator construct.
#' @param levels Moderator levels in standard-deviation units. Defaults to -1, 0, 1.
#' @param disattenuate Disattenuate the focal main effect. Defaults to `TRUE`.
#' @param eiv_bootstrap Percentile-bootstrap resamples for intervals. Zero omits.
#' @param johnson_neyman Whether to locate the Johnson-Neyman region.
#' @param seed Bootstrap seed.
#' @return An object of class `cssem_simple_slopes`.
#' @examples
#' # cssem_simple_slopes(association, "Y", "M", "W", eiv_bootstrap = 500)
#' @export
cssem_simple_slopes <- function(association, outcome, predictor, moderator, levels = c(-1, 0, 1),
                                disattenuate = TRUE, eiv_bootstrap = 0L, johnson_neyman = TRUE, seed = 1L) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  scores <- association$scores
  if (is.null(scores)) stop("association does not carry locked scores; re-run cssem_associate().", call. = FALSE)
  if (!all(c(outcome, predictor, moderator) %in% names(scores))) stop("outcome, predictor, and moderator must be locked construct names.", call. = FALSE)
  model <- association$full_models[[outcome]]
  if (is.null(model)) stop("outcome is not a declared endogenous construct.", call. = FALSE)
  interaction_name <- .find_interaction(model, predictor, moderator)
  if (is.na(interaction_name)) stop("No declared interaction between predictor and moderator on this outcome.", call. = FALSE)
  eiv_bootstrap <- as.integer(eiv_bootstrap)
  if (is.na(eiv_bootstrap) || eiv_bootstrap < 0L) stop("eiv_bootstrap must be a non-negative integer.", call. = FALSE)

  reliability <- if (isTRUE(disattenuate)) association$reliability else NULL
  if (!is.null(reliability) && all(is.na(reliability))) reliability <- NULL
  disattenuated <- !is.null(reliability)
  point_model <- if (disattenuated) {
    models <- stats::setNames(vector("list", length(scores)), names(scores))
    for (o in names(association$full_models)) models[[o]] <- association$full_models[[o]]
    .corrected_models(models, scores, reliability)$models[[outcome]]
  } else model
  main <- .model_slope(point_model, predictor); interaction <- .model_slope(point_model, interaction_name)
  slope_at <- function(w) main + interaction * w

  boot <- NULL
  if (eiv_bootstrap > 0L) {
    models <- stats::setNames(vector("list", length(scores)), names(scores))
    for (o in names(association$full_models)) models[[o]] <- association$full_models[[o]]
    boot <- .simple_slope_bootstrap(models, scores, outcome, predictor, interaction_name, reliability, disattenuated, eiv_bootstrap, seed)
  }
  ci_at <- function(w) if (is.null(boot)) c(NA_real_, NA_real_) else stats::quantile(boot$main + boot$interaction * w, c(.025, .975), na.rm = TRUE, names = FALSE)

  slopes <- data.frame(level = .level_labels(levels), moderator_value = levels,
    slope = vapply(levels, slope_at, numeric(1)), stringsAsFactors = FALSE)
  if (!is.null(boot)) {
    bounds <- vapply(levels, ci_at, numeric(2L))
    slopes$ci_low <- bounds[1L, ]; slopes$ci_high <- bounds[2L, ]
  }

  jn <- NULL
  if (isTRUE(johnson_neyman) && !is.null(boot)) {
    grid <- seq(-3, 3, length.out = 121L)
    bounds <- vapply(grid, ci_at, numeric(2L))
    significant <- bounds[1L, ] > 0 | bounds[2L, ] < 0
    jn <- list(grid = grid, significant = significant, intervals = .significant_intervals(grid, significant))
  }

  structure(list(outcome = outcome, predictor = predictor, moderator = moderator, levels = levels,
    slopes = slopes, interaction = interaction, disattenuated = disattenuated, bootstrap = eiv_bootstrap,
    johnson_neyman = jn, status = "associational"), class = "cssem_simple_slopes")
}

#' Print conditional (simple) slopes
#'
#' @param x A `cssem_simple_slopes` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_simple_slopes <- function(x, ...) {
  cat(sprintf("CS-SEM simple slopes: %s -> %s, moderated by %s\n", x$predictor, x$outcome, x$moderator))
  basis <- if (isTRUE(x$disattenuated)) "disattenuated focal effect" else "naive"
  intervals <- if (x$bootstrap > 0L) sprintf("; 95%% bootstrap intervals (%d resamples)", x$bootstrap) else ""
  cat("Effects are ", basis, intervals, ".\n\n", sep = "")
  format_effect <- function(effect, low, high) if (is.na(low)) sprintf("% .3f", effect) else sprintf("% .3f  [% .3f, % .3f]", effect, low, high)
  has_ci <- "ci_low" %in% names(x$slopes)
  for (i in seq_len(nrow(x$slopes))) {
    cat(sprintf("  %s = %-8s slope = %s\n", x$moderator, x$slopes$level[i],
      format_effect(x$slopes$slope[i], if (has_ci) x$slopes$ci_low[i] else NA_real_, if (has_ci) x$slopes$ci_high[i] else NA_real_)))
  }
  cat(sprintf("\n  interaction (%s:%s) = %.3f\n", x$predictor, x$moderator, x$interaction))
  if (!is.null(x$johnson_neyman)) {
    intervals <- x$johnson_neyman$intervals
    if (!length(intervals)) {
      cat(sprintf("  Johnson-Neyman: slope not distinguishable from zero for %s in [-3, 3] SD.\n", x$moderator))
    } else if (length(intervals) == 1L && isTRUE(all.equal(intervals[[1L]], c(-3, 3)))) {
      cat(sprintf("  Johnson-Neyman: slope significant across %s in [-3, 3] SD.\n", x$moderator))
    } else {
      described <- paste(vapply(intervals, function(i) sprintf("[%.2f, %.2f]", i[[1L]], i[[2L]]), character(1)), collapse = " and ")
      cat(sprintf("  Johnson-Neyman: slope significant for %s in %s SD.\n", x$moderator, described))
    }
  }
  cat("\nAssociational estimates under the declared temporal order; not a causal claim.\n")
  invisible(x)
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
