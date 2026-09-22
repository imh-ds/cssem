# G13 complete workflows, provenance, and support reporting design

## Problem

G13 asks CS-SEM to make its current methodology easier to use and reproduce.
The package has model and scale helpers, but a user cannot declare ordinal and
continuous indicators together in one construct through the friendly
`specify_measurement()` interface. Main result objects retain some settings,
seeds, or split details, but there is no shared provenance contract. Fitted
objects also retain their training data without an opt-out, even though some
follow-on operations need that data and others do not. Package documentation
has no tested end-to-end vignette, and the README/method notes do not consistently
describe the current release and API boundaries.

## Goals

1. Make mixed ordinal/continuous constructs straightforward to declare without
   changing the measurement estimator.
2. Give users a consistent, serializable account of the call, resolved
   settings, software, and input/split provenance for saved analysis results.
3. Let users fit and save models without retaining raw training data, while
   preserving current behavior by default and clearly identifying methods that
   need retained data.
4. Provide a runnable, seeded workflow and accurate support/method documentation
   that distinguish implemented, experimental, and validated functionality.

## Non-goals

- Changing measurement, structural, causal, or uncertainty estimators.
- Turning CS-SEM into a covariance-structure SEM or adding global fit indices.
- Claiming causal identification from provenance metadata or tutorial examples.
- Retaining raw data in provenance records or requiring a dataset to be
  embedded in a saved fit for its configuration to be reproducible.
- Replacing validation artifacts or treating the supported envelope as a
  guarantee for every user dataset.

## Public API

### Mixed-scale indicator declaration

Add and export `mixed_items(...)`. It accepts one or more existing `ordinal()`
and `continuous()` indicator specifications, concatenates their indicators,
scales, and keys in the order supplied, and returns the same internal
indicator-specification shape consumed by `specify_measurement()`.

```r
model <- specify_measurement(
  trust = mixed_items(
    ordinal("trust_1", "trust_2", keys = c(1, -1)),
    continuous("trust_duration")
  ),
  folds = 5
)
```

The helper rejects empty input, non-indicator specifications, manifest
specifications, and duplicate indicator names. Existing `ordinal()`,
`continuous()`, low-level list specifications, item ordering, and estimator
behavior remain unchanged. Existing model validation remains the final
authority for other model-level checks.

### Reproducibility provenance

Add and export `cssem_provenance(x)`, returning a versioned list for supported
analysis results. Each record contains:

- `schema_version` and the operation/result type;
- the public call as text and resolved, non-data settings used by that call;
- CS-SEM and R versions, plus versions of optional or imported packages used
  by that operation when available;
- input schema and accounting (row/column counts, declared columns, missing
  data policy, retained row positions, and split/resampling identifiers where
  applicable); and
- parent provenance for results derived from an earlier fit or association.

Records must be serializable and must not contain raw observations, row names
that could identify people, or captured calling environments. Keep exact
configuration separately from a display string so calls that use local
variables remain reproducible. Provenance describes the computational
configuration; it does not claim that the data itself is recoverable.

The first supported result classes are `fit_states`, `cssem_association`,
`cssem_outer_validation`, `cssem_bootstrap`, `cssem_contrast`,
`cssem_model_comparison`, `cssem_group_comparison`,
`cssem_measurement_invariance`, `cssem_measurement_assessment`,
`cssem_prediction`, `cssem_prediction_assessment`, `cssem_marginal_contrast`,
`evidence_report`, `indirect_effect`, `causal_effect`,
`causal_indirect_effect`, `conditional_slopes`, and
`conditional_indirect_effect`. Derived results retain their own
operation/settings and link to parent provenance instead of copying parent raw
data. Unsupported classes fail with an actionable message. Existing result
fields remain available for compatibility.

Add `retain_data = TRUE` to `fit_states()`. With the default, existing
`data`/`input_data` behavior is preserved. With `retain_data = FALSE`, the fit
omits both raw data frames but retains scores, encoders, row positions, sample
accounting, and provenance. Operations that require raw training observations
must stop before work begins with an error naming the unavailable data and
explaining that the fit must be recreated with `retain_data = TRUE`; operations
that use only fitted scores/encoders continue to work. No other output stores
raw observations solely to make its provenance record complete.

## Runnable documentation and support contract

Add `vignettes/cssem-workflow.Rmd`, backed by `knitr` and `rmarkdown` in
`Suggests` and `VignetteBuilder: knitr`. The package's core installation must
not acquire either as an `Imports` dependency. The vignette uses a deterministic
synthetic dataset and current public functions. Its tested path covers:

1. declaring a mixed-scale measurement model and inspecting scale declarations;
2. handling missing observations and reporting sample accounting;
3. fitting the theory-declared associational structure;
4. inspecting uncertainty/effect evidence and making a held-out prediction;
5. running a small outer-validation workflow; and
6. an optional, clearly labeled causal-estimand example that states the
   assumptions it requires and does not imply those assumptions were verified.

Every code chunk intended as executable is evaluated during vignette rendering.
Examples use bounded data and replicate counts so a clean package build can run
them without network access or long simulation jobs.

Add `docs/capabilities.md` with a concise table whose rows identify the
capability, status (`implemented`, `experimental`, or `validated`), available
evidence, and important conditions/limits. The table must make clear that:

- `supported_envelope()` summarizes the scenarios and metrics actually
  evaluated; it is not a universal sample-size rule or a guarantee for a new
  study;
- latent-state uncertainty and respondent weighting remain experimental where
  the current validation record says so;
- causal functions require declared estimands, adjustment, and temporal-order
  assumptions and cannot verify no unmeasured confounding; and
- covariance-SEM fit statistics and unsupported model classes remain outside
  the package's current estimator.

Update `README.md`, `docs/method-spec.md`, and relevant API help so release
version, scale transformations, selector behavior, estimands, correction
limitations, supported-envelope applicability, and migration from deprecated
wrappers agree with current exported functions and validation evidence. The
capabilities table and documentation must not upgrade a capability's status
without evidence already present in tests or validation artifacts.

## Compatibility and failure behavior

- `mixed_items()` only composes existing declarations; it does not infer item
  scale from observed values.
- `fit_states()` keeps `retain_data = TRUE` as its compatibility default.
- Existing serialized objects without provenance remain readable; the
  `cssem_provenance()` accessor reports that provenance is unavailable rather
  than fabricating it.
- Data-dependent operations on a fit without retained data fail explicitly;
  score-only prediction and association operations remain available when their
  other inputs are present.
- All documentation distinguishes associational prediction from causal
  estimands and lists estimator limitations beside relevant examples.

## Acceptance criteria

1. A mixed construct preserves indicator order, per-item scales, and keys, and
   is accepted by the current measurement pipeline; invalid declarations error
   before fitting.
2. Every supported result returns a serializable provenance record with
   operation, resolved settings, software versions, input/split accounting, and
   parent links as applicable, without embedding raw observations.
3. `fit_states(..., retain_data = FALSE)` omits raw data while preserving
   score-only workflows; every raw-data-dependent public operation gives an
   actionable error. Default `retain_data = TRUE` behavior stays compatible.
4. The vignette renders from a clean package install with current names and
   evaluates its executable chunks, including the mixed-scale declaration and
   the core measurement-to-structural-to-prediction workflow.
5. The capabilities table and updated documentation agree with current source,
   tests, and validation artifacts, and explicitly describe unsupported
   conditions and deprecated-wrapper migration.
6. Focused tests, the full test suite, vignette rendering, documentation/Rd
   checks, package build/check where locally supported, and `git diff --check`
   complete without new failures.

## Incremental delivery

1. Add mixed-scale declaration helper, tests, exports, and help page.
2. Add the provenance schema/accessor, attach it to supported core and derived
   results, add `retain_data`, and test score-only compatibility and explicit
   failures for data-dependent operations.
3. Add and render the workflow vignette; add the capabilities table and align
   the README, method notes, and migration guidance.
4. Run the complete verification contract, update G13 status and commit IDs in
   `docs/gaps.md`, and commit that tracking update separately.
