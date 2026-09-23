# CS-SEM v0.5.0 method contract

CS-SEM estimates cross-fitted construct states for theory-declared,
one-dimensional manifestation constructs, then models declared structural
relationships on those locked scores. Its structural layer is associational by
default. A public causal estimand is a separate workflow with explicit
adjustment and temporal-order declarations; those declarations do not establish
that causal identification assumptions hold.

## Measurement declarations and scores

Use `specify_measurement()` with `ordinal()`, `continuous()`, `mixed_items()`,
or `manifest()` declarations. Each estimated construct must have at least two
unique indicators; an indicator may belong to one construct only. A manifest
declaration passes one observed column directly into the structural model and
does not fit a measurement encoder.

Ordinal indicators use ordered whole-number category codes. Their measurement
likelihood is graded-response; continuous indicators use a linear-Gaussian
item likelihood. `mixed_items()` combines these item types within one
construct, and the encoder fits them together on a common latent grid rather
than inferring a scale from observed values. Keys of `1` and `-1` orient items
before estimation. The fit returns out-of-fold posterior-mean scores; by
default, construct scores are standardized. Manifest columns are standardized
by default as well; `manifest(..., standardize = FALSE)` preserves their
observed units. A manifest reliability is an assertion supplied by the user
(default `1`), not an estimate from the single column.

`fit_states()` cross-fits the measurement encoder: each respondent's locked
score comes from an encoder trained without that respondent's fold. Full-data
encoders support scoring new records. The default `missing_policy = "partial"`
uses each observed item and omits missing item likelihood terms; `"listwise"`
excludes incomplete rows, and `"error"` rejects them. Structural complete-case
handling is a separate step. `sample_accounting()` reports the affected rows
and counts. `retain_data = FALSE` omits retained training frames and raw
row-aligned cluster labels while leaving score-based analysis, provenance,
scoring, and prediction available. Bootstrap refits and row-dependent
measurement diagnostics need the original rows and therefore require
`retain_data = TRUE`.

Per-respondent posterior widths and latent-state draws are exposed as
experimental research outputs. They are not calibrated confidence intervals
and are not part of the release-validation evidence. Likewise,
`respondent_weighting = "information"` is experimental and is not a survey
weighting method.

## Structural selection and effect summaries

`specify_structure()` declares one formula per outcome. Undeclared shape
markers default to `auto`, which compares a linear term, constrained monotone
increasing/decreasing terms, and natural splines with the requested degrees of
freedom (3 and 4 by default). `auto_monotone()` compares only linear and the two
monotone directions. `linear()`, `monotone_increasing()`,
`monotone_decreasing()`, and `smooth()` constrain the candidates for that
declared edge. These names are formula markers interpreted by
`specify_structure()`; they are not standalone model functions. At most one
nonlinear edge is retained per outcome. Curvature is tested against the linear
baseline, and repeated cross-validation and a parsimony rule help choose among
predictively similar candidates. This is a selection procedure, not a causal
test or a general search over every possible model.

Formula `A:B` terms are explicit product interactions. They are included only
when declared; shape markers cannot wrap an interaction. The selector does not
discover unlisted interactions, latent interactions, cycles, or feedback.
Declared outcome families are Gaussian by default, with optional binomial and
ordinal structural outcomes. Categorical outcomes currently support only
linear main effects and do not support errors-in-variables correction,
information weighting, or constrained paths.

`associate()` reports selected shapes, predictive effect evidence, and
temporal/unrestricted shallow-tree shadow comparisons. Shadow gaps measure
relative predictive performance under the declared comparison; neither the
gaps nor the selected arrows establish causal direction. For eligible Gaussian
linear, monotone, and product terms, a reliability-based errors-in-variables
correction is available when reliability is available. It uses a stabilized
predictor-error covariance adjustment; reliability floors or covariance
shrinkage can affect estimates, and the correction is not an accuracy
guarantee. Smooth edges are not corrected. Optional bootstrap intervals
condition on selected shapes unless a workflow explicitly repeats selection.

The package also provides separately named associational indirect effects,
conditional effects, and causal estimands. Associational path decomposition
does not become causal by being called mediation. Causal estimands require
their documented design declarations, and software cannot verify consistency,
positivity, no unmeasured confounding, correct measurement, or the substantive
adequacy of an adjustment set from the data alone.

`causal_design()` distinguishes typed causal arrows from associational links
and records assumption statuses as analyst declarations. Its audit checks DAG
structure and graph-implied backdoor adjustment; a passing audit establishes
only consistency with that declared graph. Direct and mediation estimands can
use the audit to prevent unsupported graphs from receiving a causal label.
Continuous-treatment residual variation is reported as a support proxy, not a
positivity test. `causal_validation_manifest()` and `validate_causal()` compare
estimates with independent targets across six seeded score-level scenarios;
these simulations do not refit measurement models or verify assumptions for a
real study.

## Evidence and boundaries

The v0.5.0 release artifact records recovery for named simulation scenarios.
`supported_envelope()` returns the thresholds, job counts, convergence, and
release-gate status from that checked-in artifact. The validated ordinal
measurement envelope is narrower than the set of implemented scale
declarations: one-dimensional ordinal blocks with at least four indicators,
sample size at least 200, loading at least 0.70, and at most 10% item
missingness. Cross-loadings, strong construct overlap, sparse categories, and
local dependence remain exploratory conditions. Structural and mediation
validation artifacts likewise describe only their registered scenarios and
metrics. They are not universal guarantees, sample-size rules, power analyses,
or evidence for an untested study design. See the [capabilities and evidence
table](capabilities.md) and checked-in
[`validation_results`](../tests/internal/validation_results/).

CS-SEM is not a global covariance-structure SEM. It does not fit covariance
matrices or report chi-square, CFI/TLI, RMSEA, or SRMR. Formative and
higher-order constructs, cross-loadings, correlated item errors, and
simultaneous feedback models are outside the current estimator. Comparator
engines are dependency-light validation proxies, not replacements for a full
`lavaan` covariance SEM or production `seminr` PLS-SEM implementation.
