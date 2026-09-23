# Migrating to current CS-SEM names

CS-SEM v0.5.0 keeps its earlier exported names as deprecated compatibility
wrappers. They emit a deprecation warning and forward arguments to the
replacement shown here. New code should use the current interface. Wrappers
remain available for compatibility; their future removal is not implied here.

| Deprecated name | Current interface |
| --- | --- |
| cssem_model() | specify_measurement() with ordinal(), continuous(), mixed_items(), or manifest() declarations |
| cssem_construct_card() | construct_card() |
| cssem_score() | score_states() |
| cssem_fit() | fit_states() |
| cssem_respondent_information() | respondent_information() |
| simulate_cssem_data() | simulate_states() |
| cssem_structure() | specify_structure() with formula declarations |
| cssem_effect() | Formula markers linear(), auto_monotone(), monotone_increasing(), monotone_decreasing(), or smooth() inside specify_structure() |
| cssem_associate() | associate() |
| cssem_route() | route() |
| cssem_effect_card() | effect_card() |
| cssem_effect_ledger() | effect_ledger() |
| cssem_specification_gap() | specification_gap() |
| cssem_residual_diagnostics() | residual_diagnostics() |
| cssem_supported_envelope() | supported_envelope() |
| cssem_causal_edge() | causal_edge() |
| cssem_causal_effect() | causal_effect() |
| cssem_causal_mediation() | causal_indirect_effect() |
| cssem_mediation() | indirect_effect() |
| cssem_mediation_ledger() | indirect_effect_ledger() |
| cssem_moderated_mediation() | conditional_indirect_effect() |
| cssem_simple_slopes() | conditional_slopes() |
| cssem_evidence_ledger() | evidence_ledger() |
| cssem_evidence_report() | evidence_report() |
| cssem_validation_design() | validation_design() |
| cssem_validation_report() | validation_report() |
| cssem_measurement_validation_manifest() | measurement_manifest() |
| cssem_structural_validation_manifest() | structural_manifest() |
| cssem_mediation_validation_manifest() | indirect_effect_manifest() |
| cssem_moderated_mediation_validation_manifest() | conditional_indirect_effect_manifest() |
| cssem_run_measurement_validation() | validate_measurement() |
| cssem_run_mediation_validation() | validate_indirect_effect() |
| cssem_run_moderated_mediation_validation() | validate_conditional_indirect_effect() |
| cssem_run_structural_validation() | validate_structure() |
| cssem_run_comparator_validation() | validate_comparator() |
| cssem_run_mediation_comparator_validation() | validate_indirect_effect_comparator() |
| cssem_run_moderated_mediation_comparator_validation() | validate_conditional_indirect_effect_comparator() |
| cssem_run_structural_comparator_validation() | validate_structure_comparator() |
| run_measurement_benchmark() | benchmark_measurement() |

The model and structure declarations require syntax migration in addition to
renaming. For example:

    old_model <- cssem_model(list(
      Trust = list(indicators = c("trust_1", "trust_2"), scales = "ordinal")
    ))
    model <- specify_measurement(Trust = ordinal("trust_1", "trust_2"))

    old_structure <- cssem_structure(list(Loyalty = c("Trust")))
    structure <- specify_structure(Loyalty ~ Trust)

The names inside shape-marker calls are formula syntax interpreted by
specify_structure(); those markers are not exported standalone functions. The
migration rationale and original name inventory are recorded in
[naming-convention-v0.5.csv](naming-convention-v0.5.csv).

## Interpreting evidence

supported_envelope() reports thresholds, evaluated jobs, convergence, and
release-gate status from the checked-in artifact. It describes evidence for
registered scenarios and metrics. It is not an individual-study guarantee,
sample-size formula, or power analysis. Read the [capabilities and evidence
table](capabilities.md) for each status, artifact, and limitation.

For current usage see the [README](../README.md), [method specification](method-spec.md),
[associational structural guide](associational-structure.md), and [workflow
vignette](../vignettes/cssem-workflow.Rmd).
