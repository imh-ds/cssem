# Unified evidence report: the single causal-aware artifact that composes the
# construct, effect, and causal layers into one profile. It replaces the
# "coefficient table + fit indices" ritual with a structured evidentiary ledger:
# per-construct recovery/distinctiveness, per-edge effect surface with its causal
# status routed in (not hardcoded associational), and a causal-claims section for
# declared direct and interventional-mediation effects. Every row carries a
# plain-language verdict derived from transparent rules over the raw signals; no
# single composite score stands in for a p-value.

# Shadow-gap thresholds follow ideation.md: <=0.08 mild underspecification.
.EVIDENCE_GAP_OK <- 0.08

# A smooth edge has no scalar estimate, so its magnitude is read from its
# predictive contribution (CV MSE increase when the edge is dropped) instead.
.edge_verdict <- function(stability, estimate, contribution, gap, status) {
  if (identical(status, "representational")) return("Representational (not an effect)")
  if (!is.finite(stability) || stability < 0.6) return("Weak / unstable")
  negligible <- (is.finite(estimate) && abs(estimate) < 0.05) || (is.finite(contribution) && contribution < 0.01)
  if (negligible) return("Weak / unstable")
  strong <- stability >= 0.85 && (!is.finite(gap) || gap <= .EVIDENCE_GAP_OK)
  strength <- if (strong) "Robust" else "Moderate"
  kind <- switch(status, causal = "causal pathway", predictive = "predictive effect", "descriptive effect")
  paste(strength, kind)
}

.construct_verdict <- function(stability, redundancy_max, warnings) {
  if (is.finite(stability) && stability < 0.7) return("Unstable")
  if (is.finite(redundancy_max) && redundancy_max >= 0.85) return("Redundancy flag")
  if (is.finite(warnings) && warnings > 0) return("Review warnings")
  "Sound"
}

.causal_verdict <- function(label, identification, robustness) {
  if (!identical(label, "causal_under_assumptions")) return("Adjusted association (not causal)")
  if (is.finite(identification) && identification < 0.10) return("Weakly identified")
  if (is.finite(robustness) && robustness < 0.10) return("Fragile (low robustness)")
  "Supported under assumptions"
}

# Per-edge effect rows, with causal status merged in from a routing table.
.evidence_effects <- function(association, routing) {
  ledger <- cssem_effect_ledger(association)
  estimate <- ifelse(is.finite(ledger$corrected_estimate), ledger$corrected_estimate, ledger$naive_estimate)
  key <- paste(ledger$predictor, ledger$outcome, sep = "→")
  status <- rep("associational", nrow(ledger)); robustness <- rep(NA_real_, nrow(ledger)); estimand <- rep(NA_character_, nrow(ledger))
  if (!is.null(routing)) {
    match_row <- match(key, routing$table$path)
    routed <- !is.na(match_row)
    status[routed] <- routing$table$status[match_row[routed]]
    robustness[routed] <- routing$table$robustness_value[match_row[routed]]
    estimand[routed] <- routing$table$estimand[match_row[routed]]
  }
  verdict <- vapply(seq_len(nrow(ledger)), function(i)
    .edge_verdict(ledger$selection_frequency[i], estimate[i], ledger$edge_drop_mse_increase[i], ledger$temporal_gap[i], status[i]), character(1))
  data.frame(path = key, shape = ledger$shape, estimate = estimate,
    contribution = ledger$edge_drop_mse_increase, stability = ledger$selection_frequency,
    shadow_gap = ledger$temporal_gap, causal_status = status, estimand = estimand,
    robustness_value = robustness, verdict = verdict, stringsAsFactors = FALSE)
}

.evidence_constructs <- function(fit) {
  ledger <- cssem_evidence_ledger(fit)
  ledger$distinctiveness <- 1 - ledger$redundancy_max
  ledger$verdict <- vapply(seq_len(nrow(ledger)), function(i)
    .construct_verdict(ledger$stability[i], ledger$redundancy_max[i], ledger$warnings[i]), character(1))
  ledger[, c("construct", "stability", "held_out_loss", "distinctiveness", "warnings", "verdict")]
}

# One causal-claims row from a cssem_causal_effect or cssem_causal_mediation.
.causal_claim_row <- function(effect) {
  if (inherits(effect, "cssem_causal_mediation")) {
    indirect <- .mediation_reported(effect$summary, isTRUE(effect$disattenuated))
    row <- indirect[indirect$component == "indirect_total", ]
    data.frame(claim = sprintf("%s → %s", effect$x, effect$y), type = "indirect (interventional)",
      estimand = effect$estimand, effect = row$reported_effect, ci_low = row$reported_ci_low, ci_high = row$reported_ci_high,
      identification = effect$identification_strength, robustness_value = effect$robustness_value,
      label = effect$label, verdict = .causal_verdict(effect$label, effect$identification_strength, effect$robustness_value),
      stringsAsFactors = FALSE)
  } else if (inherits(effect, "cssem_causal_effect")) {
    data.frame(claim = sprintf("%s → %s", effect$treatment, effect$outcome), type = "direct",
      estimand = effect$estimand, effect = effect$adjusted_effect, ci_low = effect$ci_low, ci_high = effect$ci_high,
      identification = effect$identification_strength, robustness_value = effect$robustness_value,
      label = effect$label, verdict = .causal_verdict(effect$label, effect$identification_strength, effect$robustness_value),
      stringsAsFactors = FALSE)
  } else stop("causal claims must be cssem_causal_effect or cssem_causal_mediation objects.", call. = FALSE)
}

.evidence_causal_claims <- function(routing, causal) {
  claims <- list()
  if (!is.null(routing)) for (effect in routing$causal_effects) claims[[length(claims) + 1L]] <- effect
  for (effect in causal) claims[[length(claims) + 1L]] <- effect
  if (!length(claims)) return(NULL)
  do.call(rbind, lapply(claims, .causal_claim_row))
}

#' Assemble the unified CS-SEM evidence report
#'
#' Composes the construct, effect, and causal layers into a single causal-aware
#' evidence profile: a construct section (recovery, distinctiveness, warnings), an
#' effect section (each declared edge's shape, disattenuated estimate, predictive
#' contribution, stability, shadow gap, and routed causal status), and a
#' causal-claims section (declared direct and interventional-mediation effects
#' with identification and robustness). Every row carries a plain-language verdict
#' derived from transparent rules over the raw signals. Unlike the standalone
#' `cssem_effect_ledger()`, edge status is routed in from `routing` rather than
#' fixed at associational, so a declared causal edge is reported as such.
#'
#' @param association A `cssem_association` from [cssem_associate()].
#' @param fit Optional `cssem_fit`; adds the construct evidence section.
#' @param routing Optional `cssem_routing` from [cssem_route()]; supplies per-edge
#'   causal status and contributes its causal edges to the causal-claims section.
#' @param causal Optional list of [cssem_causal_effect()] and/or
#'   [cssem_causal_mediation()] objects to add to the causal-claims section.
#' @return An object of class `cssem_evidence_report`.
#' @examples
#' # cssem_evidence_report(association, fit = fit, routing = routing,
#' #   causal = list(cssem_causal_mediation(association, "Trust", "Loyalty", adjust = "Quality")))
#' @export
cssem_evidence_report <- function(association, fit = NULL, routing = NULL, causal = list()) {
  if (!inherits(association, "cssem_association")) stop("association must be a cssem_association.", call. = FALSE)
  if (!is.null(fit) && !inherits(fit, "cssem_fit")) stop("fit must be a cssem_fit.", call. = FALSE)
  if (!is.null(routing) && !inherits(routing, "cssem_routing")) stop("routing must be a cssem_routing.", call. = FALSE)
  if (inherits(causal, c("cssem_causal_effect", "cssem_causal_mediation"))) causal <- list(causal)
  if (length(causal) && !all(vapply(causal, inherits, logical(1), "cssem_causal_effect") | vapply(causal, inherits, logical(1), "cssem_causal_mediation")))
    stop("causal must be a list of cssem_causal_effect or cssem_causal_mediation objects.", call. = FALSE)

  structure(list(
    constructs = if (!is.null(fit)) .evidence_constructs(fit) else NULL,
    effects = .evidence_effects(association, routing),
    causal_claims = .evidence_causal_claims(routing, causal),
    status = "reported"), class = "cssem_evidence_report")
}

#' Print the unified CS-SEM evidence report
#'
#' @param x A `cssem_evidence_report` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.cssem_evidence_report <- function(x, ...) {
  cat("CS-SEM Evidence Report\n")
  cat("======================\n")
  num <- function(v, digits = 3) ifelse(is.finite(v), formatC(v, format = "f", digits = digits), "-")

  if (!is.null(x$constructs)) {
    cat("\nConstructs\n")
    cat(sprintf("  %-14s %-9s %-9s %-14s %-9s %s\n", "construct", "stability", "loss", "distinct.", "warnings", "verdict"))
    for (i in seq_len(nrow(x$constructs))) {
      row <- x$constructs[i, ]
      cat(sprintf("  %-14s %-9s %-9s %-14s %-9d %s\n", row$construct, num(row$stability, 2),
        num(row$held_out_loss, 2), num(row$distinctiveness, 2), as.integer(row$warnings), row$verdict))
    }
    cat("  (loss: held-out item loss, lower is better; distinct.: 1 - max cross-construct correlation)\n")
  }

  cat("\nEffects (surface)\n")
  cat(sprintf("  %-20s %-10s %-9s %-9s %-9s %-15s %s\n", "path", "shape", "estimate", "stability", "shadowgap", "causal status", "verdict"))
  for (i in seq_len(nrow(x$effects))) {
    row <- x$effects[i, ]
    cat(sprintf("  %-20s %-10s %-9s %-9s %-9s %-15s %s\n", row$path, row$shape, num(row$estimate, 2),
      num(row$stability, 2), num(row$shadow_gap, 2), row$causal_status, row$verdict))
  }

  if (!is.null(x$causal_claims)) {
    cat("\nCausal claims\n")
    cat(sprintf("  %-18s %-26s %-18s %-9s %-6s %s\n", "claim", "type / estimand", "effect", "identif.", "RV", "verdict"))
    for (i in seq_len(nrow(x$causal_claims))) {
      row <- x$causal_claims[i, ]
      effect <- if (is.finite(row$ci_low)) sprintf("%s [%s, %s]", num(row$effect, 2), num(row$ci_low, 2), num(row$ci_high, 2)) else num(row$effect, 2)
      cat(sprintf("  %-18s %-26s %-18s %-9s %-6s %s\n", row$claim, paste(row$type, row$estimand, sep = " / "),
        effect, num(row$identification, 2), num(row$robustness_value, 2), row$verdict))
    }
  }

  cat("\nVerdicts summarize the raw signals; they are an evidentiary profile, not a significance test.\n")
  invisible(x)
}
