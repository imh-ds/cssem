# CS-SEM

CS-SEM v0.4.0 is a research release of cross-fitted construct-state
measurement plus a narrow associational structural layer. It estimates
theory-declared, one-dimensional manifestation constructs from ordinal, binary,
and continuous indicators, locks those construct states with cross-fitting, and
fits theory-declared **associational** linear, constrained monotone, or
low-complexity smooth effects on the locked scores. It does **not** assert
causal effects by default; causal estimands are available only under explicitly
declared adjustment sets and temporal order. It does not fit formative
constructs or global SEM fit indices.

The public model contract is deliberately language-neutral: a construct has a
name, ordered indicator names, scale declarations, and item keys. The R API
uses a list specification that can be serialized and reproduced by a future
Python implementation.

```r
model <- cssem_model(list(
  trust = list(indicators = c("t1", "t2", "t3"),
               scales = "ordinal", keys = c(1, 1, -1))
), folds = 5)
fit <- cssem_fit(model, survey_data, seed = 42)
cssem_construct_card(fit, "trust")
```

```r
structure <- cssem_structure(list(loyalty = "trust"))
association <- cssem_associate(fit, structure)
cssem_specification_gap(association)
```

Structural reports include a temporally admissible shadow benchmark and an
unrestricted same-wave network benchmark. Positive specification gaps mean the
declared model predicts better than the corresponding shallow-tree benchmark;
they do not establish causal direction.

See `docs/method-spec.md` for the current measurement contract and
`docs/validation-v02.md` for the v0.3 simulation validation protocol and
release gates. Legacy benchmark helpers remain available for compatibility, but
the primary validation workflow now uses the v0.3 measurement and structural
validation suites directly.

For a local v0.3 screening run, use:

```r
measurement <- cssem_run_measurement_validation(
  cssem_measurement_validation_manifest("screening"),
  reps = 1,
  seed = 2026
)
structural <- cssem_run_structural_validation(
  cssem_structural_validation_manifest("screening"),
  reps = 1,
  seed = 3026
)
cssem_validation_report(measurement, structural)
```

CI keeps a tiny one-rep smoke run separate from the release workflow. Release
artifacts are generated from confirmation runs and written into
`tests/internal/validation_results/`, including an evidence-backed
`supported_envelope.csv` summary. Comparator artifacts now distinguish
measurement-state recovery from downstream association-preservation: use
`comparator_validation.csv` for scenario-level rows, `comparator_summary.csv`
for status-aware rollups, and `comparator_benchmark_matrix.csv` for the
success-rate and success-only benchmark matrix across engines. Structural
comparator artifacts use the same CS-SEM associational selector with different
score engines to show where locked scores help with shape selection and
shadow-gap diagnostics: use `structural_comparator_validation.csv`,
`structural_comparator_summary.csv`, and
`structural_comparator_benchmark_matrix.csv` for that benchmark family, plus
`structural_comparator_coverage_adjusted_matrix.csv` when high structural fit
must be interpreted together with partial score coverage. Latent
uncertainty draws remain experimental research outputs; they are not part of
the release-validated evidence story and must not be reported as calibrated
inferential uncertainty. The dependency-light built-in comparators remain
measurement and downstream task proxies rather than published CB-SEM or
production PLS-SEM; use optional `lavaan` and `seminr` in the comparator
validation workflow.
