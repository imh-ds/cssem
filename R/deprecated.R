# Soft-deprecated aliases for the pre-rename cssem_*() export names.
# Each forwards to its replacement unchanged via ...; kept for backward
# compatibility, not for new code. See docs/naming-convention-v0.5.csv for the
# full old-name -> new-name mapping this file implements.

#' Deprecated: use `construct_card()` instead
#'
#' **Deprecated.** Use [construct_card()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [construct_card()].
#' @return See [construct_card()].
#' @export
cssem_construct_card <- function(...) {
  .Deprecated("construct_card", package = "cssem",
    msg = "cssem_construct_card() is deprecated; use construct_card() instead.")
  construct_card(...)
}

#' Deprecated: use `score_states()` instead
#'
#' **Deprecated.** Use [score_states()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [score_states()].
#' @return See [score_states()].
#' @export
cssem_score <- function(...) {
  .Deprecated("score_states", package = "cssem",
    msg = "cssem_score() is deprecated; use score_states() instead.")
  score_states(...)
}

#' Deprecated: use `fit_states()` instead
#'
#' **Deprecated.** Use [fit_states()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [fit_states()].
#' @return See [fit_states()].
#' @export
cssem_fit <- function(...) {
  .Deprecated("fit_states", package = "cssem",
    msg = "cssem_fit() is deprecated; use fit_states() instead.")
  fit_states(...)
}

#' Deprecated: use `respondent_information()` instead
#'
#' **Deprecated.** Use [respondent_information()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [respondent_information()].
#' @return See [respondent_information()].
#' @export
cssem_respondent_information <- function(...) {
  .Deprecated("respondent_information", package = "cssem",
    msg = "cssem_respondent_information() is deprecated; use respondent_information() instead.")
  respondent_information(...)
}

#' Deprecated: use `simulate_states()` instead
#'
#' **Deprecated.** Use [simulate_states()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [simulate_states()].
#' @return See [simulate_states()].
#' @export
simulate_cssem_data <- function(...) {
  .Deprecated("simulate_states", package = "cssem",
    msg = "simulate_cssem_data() is deprecated; use simulate_states() instead.")
  simulate_states(...)
}

#' Deprecated: use `associate()` instead
#'
#' **Deprecated.** Use [associate()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [associate()].
#' @return See [associate()].
#' @export
cssem_associate <- function(...) {
  .Deprecated("associate", package = "cssem",
    msg = "cssem_associate() is deprecated; use associate() instead.")
  associate(...)
}

#' Deprecated: use `route()` instead
#'
#' **Deprecated.** Use [route()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [route()].
#' @return See [route()].
#' @export
cssem_route <- function(...) {
  .Deprecated("route", package = "cssem",
    msg = "cssem_route() is deprecated; use route() instead.")
  route(...)
}

#' Deprecated: use `effect_card()` instead
#'
#' **Deprecated.** Use [effect_card()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [effect_card()].
#' @return See [effect_card()].
#' @export
cssem_effect_card <- function(...) {
  .Deprecated("effect_card", package = "cssem",
    msg = "cssem_effect_card() is deprecated; use effect_card() instead.")
  effect_card(...)
}

#' Deprecated: use `effect_ledger()` instead
#'
#' **Deprecated.** Use [effect_ledger()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [effect_ledger()].
#' @return See [effect_ledger()].
#' @export
cssem_effect_ledger <- function(...) {
  .Deprecated("effect_ledger", package = "cssem",
    msg = "cssem_effect_ledger() is deprecated; use effect_ledger() instead.")
  effect_ledger(...)
}

#' Deprecated: use `specification_gap()` instead
#'
#' **Deprecated.** Use [specification_gap()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [specification_gap()].
#' @return See [specification_gap()].
#' @export
cssem_specification_gap <- function(...) {
  .Deprecated("specification_gap", package = "cssem",
    msg = "cssem_specification_gap() is deprecated; use specification_gap() instead.")
  specification_gap(...)
}

#' Deprecated: use `residual_diagnostics()` instead
#'
#' **Deprecated.** Use [residual_diagnostics()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [residual_diagnostics()].
#' @return See [residual_diagnostics()].
#' @export
cssem_residual_diagnostics <- function(...) {
  .Deprecated("residual_diagnostics", package = "cssem",
    msg = "cssem_residual_diagnostics() is deprecated; use residual_diagnostics() instead.")
  residual_diagnostics(...)
}

#' Deprecated: use `supported_envelope()` instead
#'
#' **Deprecated.** Use [supported_envelope()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [supported_envelope()].
#' @return See [supported_envelope()].
#' @export
cssem_supported_envelope <- function(...) {
  .Deprecated("supported_envelope", package = "cssem",
    msg = "cssem_supported_envelope() is deprecated; use supported_envelope() instead.")
  supported_envelope(...)
}

#' Deprecated: use `causal_edge()` instead
#'
#' **Deprecated.** Use [causal_edge()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [causal_edge()].
#' @return See [causal_edge()].
#' @export
cssem_causal_edge <- function(...) {
  .Deprecated("causal_edge", package = "cssem",
    msg = "cssem_causal_edge() is deprecated; use causal_edge() instead.")
  causal_edge(...)
}

#' Deprecated: use `causal_effect()` instead
#'
#' **Deprecated.** Use [causal_effect()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [causal_effect()].
#' @return See [causal_effect()].
#' @export
cssem_causal_effect <- function(...) {
  .Deprecated("causal_effect", package = "cssem",
    msg = "cssem_causal_effect() is deprecated; use causal_effect() instead.")
  causal_effect(...)
}

#' Deprecated: use `causal_indirect_effect()` instead
#'
#' **Deprecated.** Use [causal_indirect_effect()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [causal_indirect_effect()].
#' @return See [causal_indirect_effect()].
#' @export
cssem_causal_mediation <- function(...) {
  .Deprecated("causal_indirect_effect", package = "cssem",
    msg = "cssem_causal_mediation() is deprecated; use causal_indirect_effect() instead.")
  causal_indirect_effect(...)
}

#' Deprecated: use `indirect_effect_ledger()` instead
#'
#' **Deprecated.** Use [indirect_effect_ledger()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [indirect_effect_ledger()].
#' @return See [indirect_effect_ledger()].
#' @export
cssem_mediation_ledger <- function(...) {
  .Deprecated("indirect_effect_ledger", package = "cssem",
    msg = "cssem_mediation_ledger() is deprecated; use indirect_effect_ledger() instead.")
  indirect_effect_ledger(...)
}

#' Deprecated: use `conditional_indirect_effect_manifest()` instead
#'
#' **Deprecated.** Use [conditional_indirect_effect_manifest()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [conditional_indirect_effect_manifest()].
#' @return See [conditional_indirect_effect_manifest()].
#' @export
cssem_moderated_mediation_validation_manifest <- function(...) {
  .Deprecated("conditional_indirect_effect_manifest", package = "cssem",
    msg = "cssem_moderated_mediation_validation_manifest() is deprecated; use conditional_indirect_effect_manifest() instead.")
  conditional_indirect_effect_manifest(...)
}

#' Deprecated: use `indirect_effect_manifest()` instead
#'
#' **Deprecated.** Use [indirect_effect_manifest()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [indirect_effect_manifest()].
#' @return See [indirect_effect_manifest()].
#' @export
cssem_mediation_validation_manifest <- function(...) {
  .Deprecated("indirect_effect_manifest", package = "cssem",
    msg = "cssem_mediation_validation_manifest() is deprecated; use indirect_effect_manifest() instead.")
  indirect_effect_manifest(...)
}

#' Deprecated: use `conditional_indirect_effect()` instead
#'
#' **Deprecated.** Use [conditional_indirect_effect()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [conditional_indirect_effect()].
#' @return See [conditional_indirect_effect()].
#' @export
cssem_moderated_mediation <- function(...) {
  .Deprecated("conditional_indirect_effect", package = "cssem",
    msg = "cssem_moderated_mediation() is deprecated; use conditional_indirect_effect() instead.")
  conditional_indirect_effect(...)
}

#' Deprecated: use `indirect_effect()` instead
#'
#' **Deprecated.** Use [indirect_effect()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [indirect_effect()].
#' @return See [indirect_effect()].
#' @export
cssem_mediation <- function(...) {
  .Deprecated("indirect_effect", package = "cssem",
    msg = "cssem_mediation() is deprecated; use indirect_effect() instead.")
  indirect_effect(...)
}

#' Deprecated: use `conditional_slopes()` instead
#'
#' **Deprecated.** Use [conditional_slopes()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [conditional_slopes()].
#' @return See [conditional_slopes()].
#' @export
cssem_simple_slopes <- function(...) {
  .Deprecated("conditional_slopes", package = "cssem",
    msg = "cssem_simple_slopes() is deprecated; use conditional_slopes() instead.")
  conditional_slopes(...)
}

#' Deprecated: use `evidence_ledger()` instead
#'
#' **Deprecated.** Use [evidence_ledger()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [evidence_ledger()].
#' @return See [evidence_ledger()].
#' @export
cssem_evidence_ledger <- function(...) {
  .Deprecated("evidence_ledger", package = "cssem",
    msg = "cssem_evidence_ledger() is deprecated; use evidence_ledger() instead.")
  evidence_ledger(...)
}

#' Deprecated: use `evidence_report()` instead
#'
#' **Deprecated.** Use [evidence_report()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [evidence_report()].
#' @return See [evidence_report()].
#' @export
cssem_evidence_report <- function(...) {
  .Deprecated("evidence_report", package = "cssem",
    msg = "cssem_evidence_report() is deprecated; use evidence_report() instead.")
  evidence_report(...)
}

#' Deprecated: use `validation_design()` instead
#'
#' **Deprecated.** Use [validation_design()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validation_design()].
#' @return See [validation_design()].
#' @export
cssem_validation_design <- function(...) {
  .Deprecated("validation_design", package = "cssem",
    msg = "cssem_validation_design() is deprecated; use validation_design() instead.")
  validation_design(...)
}

#' Deprecated: use `validation_report()` instead
#'
#' **Deprecated.** Use [validation_report()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validation_report()].
#' @return See [validation_report()].
#' @export
cssem_validation_report <- function(...) {
  .Deprecated("validation_report", package = "cssem",
    msg = "cssem_validation_report() is deprecated; use validation_report() instead.")
  validation_report(...)
}

#' Deprecated: use `measurement_manifest()` instead
#'
#' **Deprecated.** Use [measurement_manifest()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [measurement_manifest()].
#' @return See [measurement_manifest()].
#' @export
cssem_measurement_validation_manifest <- function(...) {
  .Deprecated("measurement_manifest", package = "cssem",
    msg = "cssem_measurement_validation_manifest() is deprecated; use measurement_manifest() instead.")
  measurement_manifest(...)
}

#' Deprecated: use `structural_manifest()` instead
#'
#' **Deprecated.** Use [structural_manifest()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [structural_manifest()].
#' @return See [structural_manifest()].
#' @export
cssem_structural_validation_manifest <- function(...) {
  .Deprecated("structural_manifest", package = "cssem",
    msg = "cssem_structural_validation_manifest() is deprecated; use structural_manifest() instead.")
  structural_manifest(...)
}

#' Deprecated: use `validate_measurement()` instead
#'
#' **Deprecated.** Use [validate_measurement()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_measurement()].
#' @return See [validate_measurement()].
#' @export
cssem_run_measurement_validation <- function(...) {
  .Deprecated("validate_measurement", package = "cssem",
    msg = "cssem_run_measurement_validation() is deprecated; use validate_measurement() instead.")
  validate_measurement(...)
}

#' Deprecated: use `validate_indirect_effect_comparator()` instead
#'
#' **Deprecated.** Use [validate_indirect_effect_comparator()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_indirect_effect_comparator()].
#' @return See [validate_indirect_effect_comparator()].
#' @export
cssem_run_mediation_comparator_validation <- function(...) {
  .Deprecated("validate_indirect_effect_comparator", package = "cssem",
    msg = "cssem_run_mediation_comparator_validation() is deprecated; use validate_indirect_effect_comparator() instead.")
  validate_indirect_effect_comparator(...)
}

#' Deprecated: use `validate_indirect_effect()` instead
#'
#' **Deprecated.** Use [validate_indirect_effect()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_indirect_effect()].
#' @return See [validate_indirect_effect()].
#' @export
cssem_run_mediation_validation <- function(...) {
  .Deprecated("validate_indirect_effect", package = "cssem",
    msg = "cssem_run_mediation_validation() is deprecated; use validate_indirect_effect() instead.")
  validate_indirect_effect(...)
}

#' Deprecated: use `validate_conditional_indirect_effect_comparator()` instead
#'
#' **Deprecated.** Use [validate_conditional_indirect_effect_comparator()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_conditional_indirect_effect_comparator()].
#' @return See [validate_conditional_indirect_effect_comparator()].
#' @export
cssem_run_moderated_mediation_comparator_validation <- function(...) {
  .Deprecated("validate_conditional_indirect_effect_comparator", package = "cssem",
    msg = "cssem_run_moderated_mediation_comparator_validation() is deprecated; use validate_conditional_indirect_effect_comparator() instead.")
  validate_conditional_indirect_effect_comparator(...)
}

#' Deprecated: use `validate_conditional_indirect_effect()` instead
#'
#' **Deprecated.** Use [validate_conditional_indirect_effect()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_conditional_indirect_effect()].
#' @return See [validate_conditional_indirect_effect()].
#' @export
cssem_run_moderated_mediation_validation <- function(...) {
  .Deprecated("validate_conditional_indirect_effect", package = "cssem",
    msg = "cssem_run_moderated_mediation_validation() is deprecated; use validate_conditional_indirect_effect() instead.")
  validate_conditional_indirect_effect(...)
}

#' Deprecated: use `validate_structure_comparator()` instead
#'
#' **Deprecated.** Use [validate_structure_comparator()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_structure_comparator()].
#' @return See [validate_structure_comparator()].
#' @export
cssem_run_structural_comparator_validation <- function(...) {
  .Deprecated("validate_structure_comparator", package = "cssem",
    msg = "cssem_run_structural_comparator_validation() is deprecated; use validate_structure_comparator() instead.")
  validate_structure_comparator(...)
}

#' Deprecated: use `validate_structure()` instead
#'
#' **Deprecated.** Use [validate_structure()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_structure()].
#' @return See [validate_structure()].
#' @export
cssem_run_structural_validation <- function(...) {
  .Deprecated("validate_structure", package = "cssem",
    msg = "cssem_run_structural_validation() is deprecated; use validate_structure() instead.")
  validate_structure(...)
}

#' Deprecated: use `validate_comparator()` instead
#'
#' **Deprecated.** Use [validate_comparator()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [validate_comparator()].
#' @return See [validate_comparator()].
#' @export
cssem_run_comparator_validation <- function(...) {
  .Deprecated("validate_comparator", package = "cssem",
    msg = "cssem_run_comparator_validation() is deprecated; use validate_comparator() instead.")
  validate_comparator(...)
}

#' Deprecated: use `benchmark_measurement()` instead
#'
#' **Deprecated.** Use [benchmark_measurement()] instead; this alias forwards its arguments to it
#' unchanged.
#'
#' @param ... Forwarded to [benchmark_measurement()].
#' @return See [benchmark_measurement()].
#' @export
run_measurement_benchmark <- function(...) {
  .Deprecated("benchmark_measurement", package = "cssem",
    msg = "run_measurement_benchmark() is deprecated; use benchmark_measurement() instead.")
  benchmark_measurement(...)
}

