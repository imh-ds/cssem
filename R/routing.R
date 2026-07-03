# Edge routing: assign every declared structural edge a status
# (associational by default, or predictive / representational / causal) and emit
# a Path Routing Table. A causal status requires an adjustment set and a declared
# temporal order; the table states each edge's allowed interpretation so a path
# is never read causally by default.

#' Declare a causal edge for routing
#'
#' @param from Treatment construct name.
#' @param to Outcome construct name.
#' @param adjust Character vector of adjustment-set construct names.
#' @param estimand Causal estimand: `"adjusted_linear"` (disattenuated, linear
#'   adjustment) or `"adjusted_dml"` (flexible spline adjustment for nonlinear
#'   confounding). See [cssem_causal_effect()].
#' @return A `cssem_causal_edge` specification for [cssem_route()].
#' @examples
#' cssem_causal_edge("Satisfaction", "Loyalty", adjust = c("Trust", "PriorLoyalty"))
#' @export
cssem_causal_edge <- function(from, to, adjust, estimand = c("adjusted_linear", "adjusted_dml")) {
  estimand <- match.arg(estimand)
  if (!is.character(from) || length(from) != 1L || !is.character(to) || length(to) != 1L)
    stop("from and to must be single construct names.", call. = FALSE)
  if (missing(adjust) || !length(adjust)) stop("A causal edge requires a non-empty adjustment set.", call. = FALSE)
  structure(list(from = from, to = to, adjust = as.character(adjust), estimand = estimand), class = "cssem_causal_edge")
}

.declared_edges <- function(structure) {
  rows <- lapply(names(structure$effects), function(outcome)
    data.frame(from = names(structure$effects[[outcome]]), to = outcome, stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

.pair_edges <- function(pairs, kind) {
  lapply(pairs, function(pair) {
    if (!is.character(pair) || length(pair) != 2L) stop(sprintf("%s edges must be c(from, to) pairs.", kind), call. = FALSE)
    list(from = pair[[1L]], to = pair[[2L]])
  })
}

.associational_effect <- function(association, from, to) {
  corrected <- association$corrected_effects
  if (is.null(corrected)) return(c(effect = NA_real_))
  row <- corrected[corrected$outcome == to & corrected$predictor == from, , drop = FALSE]
  if (!nrow(row)) return(c(effect = NA_real_))
  c(effect = if (is.finite(row$corrected_estimate[[1L]])) row$corrected_estimate[[1L]] else row$naive_estimate[[1L]])
}

#' Route declared edges and build a Path Routing Table
#'
#' Assigns every declared structural edge a status and reports its allowed
#' interpretation. Edges default to `associational`; edges listed in `causal`
#' (which require an adjustment set and a declared `temporal_order`) are estimated
#' with [cssem_causal_effect()], while `predictive` and `representational` edges
#' are flagged as not for structural interpretation.
#'
#' @param association A `cssem_association` from [cssem_associate()].
#' @param causal A list of [cssem_causal_edge()] specifications.
#' @param predictive A list of `c(from, to)` pairs marked predictive.
#' @param representational A list of `c(from, to)` pairs marked representational.
#' @param temporal_order Optional temporal order; required for causal edges.
#' @param eiv_bootstrap Bootstrap resamples for causal-edge intervals.
#' @param seed Bootstrap seed.
#' @return An object of class `cssem_routing`.
#' @examples
#' # cssem_route(association,
#' #   causal = list(cssem_causal_edge("Satisfaction", "Loyalty", adjust = "Trust")),
#' #   temporal_order = c("Trust", "Satisfaction", "Loyalty"))
#' @export
cssem_route <- function(association, causal = list(), predictive = list(), representational = list(),
                        temporal_order = NULL, eiv_bootstrap = 0L, seed = 1L) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  causal <- if (inherits(causal, "cssem_causal_edge")) list(causal) else causal
  if (length(causal) && !all(vapply(causal, inherits, logical(1), "cssem_causal_edge")))
    stop("causal must be a list of cssem_causal_edge() specifications.", call. = FALSE)
  if (length(causal) && is.null(temporal_order)) stop("Causal edges require a declared temporal_order.", call. = FALSE)

  edges <- .declared_edges(association$structure)
  status <- stats::setNames(rep("associational", nrow(edges)), paste(edges$from, edges$to, sep = "→"))
  for (edge in .pair_edges(predictive, "predictive")) status[[paste(edge$from, edge$to, sep = "→")]] <- "predictive"
  for (edge in .pair_edges(representational, "representational")) status[[paste(edge$from, edge$to, sep = "→")]] <- "representational"

  causal_effects <- list()
  causal_lookup <- list()
  for (edge in causal) {
    key <- paste(edge$from, edge$to, sep = "→")
    if (!key %in% names(status)) stop(sprintf("Causal edge %s is not a declared structural edge.", key), call. = FALSE)
    status[[key]] <- "causal"
    effect <- cssem_causal_effect(association, edge$from, edge$to, adjust = edge$adjust,
      estimand = edge$estimand, temporal_order = temporal_order, eiv_bootstrap = eiv_bootstrap, seed = seed)
    causal_effects[[key]] <- effect; causal_lookup[[key]] <- edge
  }

  interpretation <- c(associational = "Adjusted association",
    predictive = "Predictive only (not interpreted)", representational = "Representational (not a structural effect)")
  causal_interpretation <- c(causal_under_assumptions = "Causal under assumptions",
    adjusted_association = "Adjusted association (weak identification)",
    unadjusted_association = "Unadjusted association")
  rows <- lapply(seq_len(nrow(edges)), function(i) {
    key <- paste(edges$from[[i]], edges$to[[i]], sep = "→"); s <- status[[key]]
    if (s == "causal") {
      effect <- causal_effects[[key]]
      data.frame(path = key, status = s, estimand = causal_lookup[[key]]$estimand,
        adjustment_set = paste(causal_lookup[[key]]$adjust, collapse = ", "),
        effect = effect$adjusted_effect, ci_low = effect$ci_low, ci_high = effect$ci_high,
        robustness_value = effect$robustness_value, interpretation = causal_interpretation[[effect$label]], stringsAsFactors = FALSE)
    } else {
      data.frame(path = key, status = s, estimand = NA_character_, adjustment_set = NA_character_,
        effect = unname(.associational_effect(association, edges$from[[i]], edges$to[[i]])[["effect"]]),
        ci_low = NA_real_, ci_high = NA_real_, robustness_value = NA_real_, interpretation = interpretation[[s]], stringsAsFactors = FALSE)
    }
  })

  structure(list(table = do.call(rbind, rows), causal_effects = causal_effects,
    temporal_order = temporal_order, status = "routed"), class = "cssem_routing")
}

#' Print a Path Routing Table
#'
#' @param x A `cssem_routing` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_routing <- function(x, ...) {
  cat("CS-SEM Path Routing Table\n")
  cat(sprintf("Temporal order: %s\n\n", if (is.null(x$temporal_order)) "(not declared)" else paste(x$temporal_order, collapse = " → ")))
  for (i in seq_len(nrow(x$table))) {
    row <- x$table[i, ]
    effect <- if (is.na(row$ci_low)) sprintf("% .3f", row$effect) else sprintf("% .3f [% .3f, % .3f]", row$effect, row$ci_low, row$ci_high)
    detail <- if (identical(row$status, "causal") && is.finite(row$robustness_value))
      sprintf("  adjust=%s  RV=%.2f", row$adjustment_set, row$robustness_value) else ""
    cat(sprintf("  %-28s %-16s %s   %s%s\n", row$path, row$status, effect, row$interpretation, detail))
  }
  cat("\nEdges are associational unless routed causal with an adjustment set and temporal order.\n")
  invisible(x)
}
