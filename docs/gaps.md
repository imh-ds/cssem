# SEM capability audit and proposed build backlog

Audit date: **2026-09-21**. Package: **cssem 0.5.0**, baseline commit
`f5a6e1b`, with the working-tree changes present during review.

**Status:** G1 through G3 have implemented workflows, G4 through G8 have
implemented core workflows with documented methodological limits, and G9 now
has a partial P2 cluster-aware workflow. G9's P3 multilevel/longitudinal
extension remains a separate unchecked research project; the other unchecked
entries are proposed work. Existing defect reproductions and fixes belong in
[bugs.md](bugs.md); this document covers missing capabilities, incomplete user
workflows, and methodological extensions.

## Scope and evidence

There are meaningful gaps, particularly in inference, diagnostics, prediction,
and the R user interface. However, CS-SEM explicitly targets cross-fitted
one-dimensional manifestation states and a narrow structural layer. It should
not acquire every feature of covariance-based SEM or PLS merely for parity.
The priorities below are this audit's recommendations, informed by the source
and comparator documentation, not claims that either comparator solves every
problem or that their estimators are interchangeable.

Reviewed: [NAMESPACE](../NAMESPACE), [DESCRIPTION](../DESCRIPTION), the public
functions and underlying measurement/structural/causal code in `R/`, generated
help, method notes, and validation entry points. Source-level R introspection
confirmed the missing generic methods and argument-level limitations discussed
below. External references are primary project documentation and the SEMinR
CRAN manual (version 2.5.0 as served on the audit date). Live documentation may
describe newer capabilities than a user's installed version. Neither package
was installed or benchmarked for this audit; no numerical superiority is
claimed. The implementation status below records work completed after that
baseline review.

For this R package, **UI/UX** means model declarations, errors, summaries,
extractors, plots, and reproducible analysis—not a requirement to build a GUI.

### Existing capabilities to preserve

| Area | Already present |
| --- | --- |
| Measurement | Ordinal/binary and continuous likelihoods, mixed-scale internal specifications, reverse keys, manifest covariates, cross-fitted states, new-data scoring |
| Diagnostics | Posterior-based reliability, respondent information, convergence warnings, item losses, residual dependence, redundancy and construct cards |
| Structure | Formula declarations, linear/monotone/smooth selection, product interactions, repeated structural CV, shadow gaps, effect tables, errors-in-variables correction |
| Effects | Single/parallel/serial mediation, conditional slopes and indirect effects, bootstrap intervals for supported effects |
| Causal layer | Explicit adjustment/order declarations, linear and flexible estimands, sensitivity panels, routing and evidence reports |
| Validation | Simulation manifests, measurement/structure/mediation/moderation runners, optional comparator engines, release gates and shard scripts |

These are implemented capabilities, not blanket validation claims. In
particular, [supported_envelope()](../R/validation.R#L115) still describes a
narrow ordinal measurement envelope, and [bugs.md](bugs.md) records unresolved
correctness concerns.

### Priority and status key

- **P1:** recommended foundations for dependable use of the present method.
- **P2:** useful extensions after those foundations are stable.
- **P3:** optional research/scope expansion requiring a separate methodological decision.
- **Partial:** internal machinery or part of the workflow exists.
- **Missing:** no supported public workflow was found; users may still script one themselves.

| ID | Priority | Status | Build target | Tracking commit(s) |
| --- | --- | --- | --- | --- |
| G1 | P1 | Implemented | Standard summaries, parameter tables, and R extractors | `a649e84` |
| G2 | P1 | Implemented | Measurement parameter and validity assessment | `ba8de9c`, `91a95e9` |
| G3 | P1 | Implemented | Unified preflight and numerical diagnostics | `a57b044`, `8bd3fd8`, `f8ef27e` |
| G4 | P1 | Partial (core workflow implemented) | Explicit, validated inference and reusable resampling | `b9e2fbe`, `d8990fc`, `43f8595`, `a6b87ea`, `f4df370` |
| G5 | P1 | Partial (core workflow implemented) | Missing-data policy and sample accounting | `311ec2d`, `8cb4304`, `d9d370b`, `e5f4ff8` |
| G6 | P1 | Partial (core workflow implemented) | User-defined measurement splits and outer validation | `b87c127`, `7283924`, `871ff19`, `5ca8c05`, `f8f087f`, `1b9c28a`, `a66c627`, `89d5c9f`, `f9f94c4`, `942337f` |
| G7 | P1 | Partial (core workflow implemented) | Structural prediction for new observations | `e7a0e4f`, `97a722d`, `f7a4cd1`, `566543f`, `b58d4df`, `cf60de7`, `64aedc7`, `5c00b1d`, `b51c2cf`, `1905842`, `a402706` |
| G8 | P2 | Partial (core workflow implemented) | Group comparison and measurement invariance | `183cc0b`, `845e274`, `23551e2`, `27e84c1` |
| G9 | P2/P3 | Partial (P2 cluster workflow implemented) | Cluster-aware analysis, then multilevel/longitudinal models | `16d38f2`, `e0cbd83`, `1bf4727`, `2118ff5` |
| G10 | P2 | Missing | Defined contrasts and model-comparison workflows |
| G11 | P3 | Missing | Binary/ordinal structural response families |
| G12 | P2 | Partial | Structural, moderation, and diagnostic plots |
| G13 | P1 | Partial | Complete examples, provenance, and support reporting |
| G14 | P1 | Partial | Causal assumptions and validation contract |
| G15 | P2 | Partial | Study-specific simulation and sample-size planning |
| G16 | P3 | Deliberate limits | Expanded construct and structural model classes |

## Proposed work

### [x] G1. Standard summaries, parameter tables, and R extractors

**Evidence/gap at the audit baseline:** [NAMESPACE](../NAMESPACE) registered
print methods and only `plot.fit_states`; the principal result classes had no package-defined
`summary`, `coef`, `vcov`, `confint`, `fitted`, `residuals`, `nobs`, or `update`
methods. Cards and ledgers provide useful data frames, but users must learn
different structures or access nested internals. A default `summary()` of a
list is not a statistical summary.

**Comparator:** lavaan provides parameter tables, standardized solutions,
residuals, covariance extraction, and inspection APIs.
[lavaan extractors](https://lavaan.ugent.be/tutorial/inspect.html)

**Implemented (2026-09-20):** [parameter_table()](../R/summary.R) now exposes
measurement encoder parameters and one stable row per selected structural edge,
mediation component, causal effect, moderated effect, routing edge, and evidence
edge. Every row carries the estimate basis (`naive`, `corrected_eiv`, or an
explicit unavailable state), units, sample size, uncertainty method, and an
availability reason. `summary()` methods return stable `constructs`, `parameters`,
`effects`, and diagnostic components without requiring private fields. The
`coef()`, `confint()`, `vcov()`, and `nobs()` methods use those tables; covariance
is an explicit unavailable matrix because the fit does not retain joint draws,
and marginal intervals are never converted into covariance estimates.
`fitted()` and `residuals()` label out-of-fold predictions and also support
explicit in-sample structural predictions. `fit_states()` and `associate()` now
retain the reproducible inputs and resolved controls needed by `update()` to
refit through their public constructors.

Existing cards and ledgers remain focused views and continue to be the source
for the corresponding summary rows.

**Tracking commits:** `a649e84`.

**Acceptance:** generic outputs agree with current ledgers; naive/corrected and
raw/standardized estimates cannot be confused; unavailable SEs remain explicitly
unavailable; downstream table generation needs no private object fields. These
contracts are covered by [test-summary-extractors.R](../tests/testthat/test-summary-extractors.R),
including real encoder parameters, ledger agreement, unavailable uncertainty,
prediction labels, and reproducible updates.

### [x] G2. Measurement parameters and validity assessment

**Evidence/gap:** [construct_card()](../R/fit.R#L319) reports diagnostics, but
ordinal discriminations/thresholds and continuous slopes/intercepts/residual
scales remain in `full_encoders`. There is no dedicated public parameter table,
item-response curve API, or HTMT/convergent-validity assessment. Existing EAP
reliability and construct-score redundancy are useful but answer narrower
questions than an entire measurement assessment.

**Comparator:** SEMinR reports reliability, AVE, HTMT, cross-loadings, and
collinearity diagnostics. [SEMinR project overview](https://seminr.io/)

**Build:** expose scale-aware item parameters and response curves, observed
category support, posterior information, and separate losses by measurement
family. Add an assessment API documenting which validity/reliability quantities
are defined for each CS-SEM model. Offer conventional descriptive measures only
with stated correlation/scale assumptions. Do not relabel graded-response
discrimination as a standardized CFA loading, or posterior reliability as
Cronbach's alpha/composite reliability. Make threshold-based verdicts optional
and distinguish diagnostics from validated decision rules.

**Acceptance:** parameter tables reconstruct encoder predictions; tests cover
ordinal, continuous, mixed, reverse-keyed, and manifest blocks; any added metric
agrees with an independent calculation under its stated assumptions.

**Implemented (2026-09-20):** [measurement_parameters()](../R/measurement.R)
now exposes ordinal graded-response discriminations and thresholds, continuous
intercepts, slopes, and residual scales, and an explicit unavailable row for
manifest passthroughs. Each row carries observed category support or numeric
ranges, missingness, reverse-key metadata, held-out loss fields, and posterior
state information. [item_response_curve()](../R/measurement.R) evaluates
ordinal category probabilities or continuous expected responses on the fitted
encoder scale, while rejecting manifest items that have no response curve.

[measurement_assessment()](../R/measurement-assessment.R) reports EAP posterior
reliability, posterior information, and descriptive item-to-state convergence
for each construct. It also reports pairwise descriptive HTMT-like ratios,
locked-score correlations, item-score correlations, and a locked-score OLS VIF
diagnostic. The result records the calculation methods and limitations: AVE is
explicitly unavailable because this encoder does not estimate CFA loadings or
communalities; EAP reliability is not alpha or composite reliability; and the
HTMT-like values are diagnostics without threshold verdicts. Reverse-keyed,
ordinal, continuous, mixed, manifest, and sparse-support behavior is covered
by focused tests and an installed-package smoke check.

**Tracking commits:** `ba8de9c`, `91a95e9`.

### [x] G3. Unified preflight and numerical diagnostics

**Evidence/gap:** validation is scattered across [model.R](../R/model.R),
[fit.R](../R/fit.R), and [structure.R](../R/structure.R). Convergence flags exist,
but [.fit_construct_mml()](../R/encoder.R#L143) does not expose a likelihood
trace, optimizer status per item, or user controls for tolerance/quadrature.
Structural regularization mainly surfaces as a stability flag, without a
user-facing explanation of rank, conditioning, or correction shrinkage.

**Comparator:** lavaan's inspection API exposes technical model information;
SEMinR documents syntax checking and convergence controls.
[lavaan inspection](https://lavaan.ugent.be/tutorial/inspect.html),
[SEMinR manual, estimate_pls](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=34)

**Build:** `check_model()`/`check_data()` returning all actionable issues with
construct, item, row/fold, and severity; a numerical diagnostic table including
objective change, optimizer outcomes, singularity, condition numbers, and
effective correction strength. Add pre-fit checks for empty/sparse folds,
zero-information blocks, scale types, graph constraints, and collinearity.
Provide validated fitting controls and actionable recovery guidance.

**Acceptance:** problematic inputs fail before expensive fitting with specific
messages; deliberately nonconvergent fits expose their cause; stabilization is
visible rather than interpreted as an accuracy guarantee. Coordinate input
validation with A6/A7 in [bugs.md](bugs.md).

**Implemented (2026-09-20):** [check_model()](../R/preflight.R) and
[check_data()](../R/preflight.R) now return structured issue tables with
severity, stage, construct, item, row, fold, stable issue codes, and recovery
actions. They cover malformed measurement declarations, duplicate or missing
indicators, scale/key mismatches, unsupported graph nodes and temporal order,
empty or sparse folds, sparse ordinal categories, non-finite numeric values,
zero-information items/constructs, and rows that rely on the prior. `fit_states()`
runs these checks before allocating encoders and stops on errors while leaving
warnings available through the public checks.

`fit_states()` now validates and stores the measurement `tolerance` and latent
`quadrature` controls. [numerical_diagnostics()](../R/numerical-diagnostics.R)
exposes marginal objective start/end/change, convergence, optimizer status,
latent design rank, and condition numbers. When paired with an association it
also reports errors-in-variables covariance rank/conditioning, reliability-floor
use, covariance shrinkage, and effective correction strength. Stabilization is
labelled as a numerical safeguard and is not presented as an accuracy guarantee.
Focused tests cover structured preflight failures, sparse support, controls,
objective/status fields, and structural correction diagnostics.

**Tracking commits:** `a57b044`, `8bd3fd8`, `f8ef27e`.

### [x] G4. Explicit inference scope and reusable resampling

**Evidence/gap:** inference exists, but it is fragmented. The
[structural bootstrap](../R/structure.R#L536) resamples locked scores;
[mediation](../R/mediation.R#L232) and
[moderation](../R/moderation.R#L42) refit selected shapes on those scores.
Measurement encoders and shape selection are not rerun. Some structural
reliability calculations use resampled posterior variances, which still does
not constitute refitting measurement. Most bounds are hard-coded at 95%; a
unified public bootstrap object with draws, failure counts, and configurable
confidence levels is absent.

**Comparator:** lavaan documents standard/robust errors and bootstrap options;
SEMinR exposes `bootstrap_model()`.
[lavaan inference](https://lavaan.ugent.be/tutorial/est.html),
[SEMinR bootstrap API](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=10)

**Build:** distinguish conditional-on-locked-scores inference from an optional
end-to-end resampling workflow; retain draws, seeds, failed-replicate reasons,
successful counts, confidence level, and the components refitted. Add progress,
safe parallel execution, and resumability for long runs. Implement robust or
full-pipeline inference only with derivation and coverage checks appropriate to
the estimand; ordinary bootstrapping is not automatically valid after selection.

**Implemented (2026-09-20, core workflow):** [bootstrap_model()](../R/bootstrap.R)
is a public resampling contract. A callback receives the resampled data and
scores and returns a named estimand vector; the caller explicitly chooses
`refit = "locked_scores"` for conditional inference or `refit = "measurement"`
for an encoder refit in every replicate. The returned `cssem_bootstrap` object
retains the point estimate, draws, per-replicate seeds and statuses, failure
reasons, successful and failed counts, confidence level, refit component,
worker count, and the callback. `summary()` and `confint()` expose the stored
percentile intervals. Runs can resume from a completed prefix, report progress,
and use deterministic worker-safe seeds: installed-package smoke tests give
identical draws and statuses for one and two workers. Callback errors become
failed rows with `NA` draws instead of disappearing; non-finite callback values
are treated as failures as well.

The structural EIV interval in [associate()](../R/structure.R) now accepts a
`level` argument and stores it in the association settings, so that interval
also no longer assumes 95% when requested. Focused tests cover deterministic
draws, configurable intervals, failure accounting, measurement-refit mode,
resumability, and the structural level control.

This remains **Partial** for methodological scope. Existing mediation,
moderation, and causal convenience helpers continue to describe their
conditional-on-locked-scores, fixed-shape bootstrap estimands. The new engine
does not silently rerun shape selection or convert plausible-value draws into
confidence intervals. Coverage, bias, and failure-rate simulations under low
and high reliability and under shape selection remain required before making
full-pipeline or robust-inference claims.

**Tracking commits:** `b9e2fbe`, `d8990fc`, `43f8595`, `a6b87ea`, `f4df370`.

**Acceptance:** repeated runs reproduce across worker counts; failures never
silently disappear; low/high reliability and shape-selection simulations report
coverage, bias, and failure rates. Do not treat experimental plausible-value
draws as calibrated confidence intervals. Fix relevant A1/A8 defects first.

### [x] G5. Missing-data policy and effective-sample accounting

**Evidence/gap:** [.eap_posterior()](../R/encoder.R#L118) omits missing item
likelihood contributions, so measurement already handles partial responses.
However, there is no unified user-selectable missing-data policy, row-level
coverage report, or consistent per-outcome effective-sample ledger across
manifest covariates and structural/causal stages. An entirely unobserved block
can supply prior information rather than observed measurement evidence.

**Comparator:** lavaan documents listwise deletion and ML missing-data handling;
SEMinR exposes a missing-value replacement function and missing-value code.
[lavaan missing data](https://lavaan.ugent.be/tutorial/est.html),
[SEMinR estimate_pls](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=34)

**Build:** document assumptions and treatment separately for indicators,
covariates, and outcomes; identify prior-only states and insufficient observed
items; expose included/excluded row IDs and reasons. Add explicit policies for
unsupported structural missingness and a carefully specified multiple-imputation
adapter if needed. Fit preprocessing inside training splits and pool compatible
estimands; do not advertise independent block likelihoods as a joint SEM FIML
solution without establishing that equivalence.

**Acceptance:** trace every input row through all stages; cover partial/all-item
missingness and missing manifest variables; simulate MCAR/MAR conditions and
state what remains unsupported. Align comparator disclosures with historical H3.

**Implemented (2026-09-21, core workflow):** [fit_states()](../R/fit.R) now
accepts explicit `missing_policy = "partial"`, `"listwise"`, or `"error"`.
The default preserves item-level partial-response likelihood contributions;
listwise fitting records the original row IDs; and error mode reports the
number of offending rows before fitting. Rows with no observed indicators are
labelled `prior_only` and are excluded from the measurement effective-sample
count rather than being presented as observed evidence. A preflight helper
argument collision that previously crashed row-level warnings was corrected so
these diagnostics are returned reliably.

[sample_accounting()](../R/missing-data.R) now reports row IDs, row names,
observed-item counts, statuses, reasons, and per-construct effective counts for
measurement. [associate()](../R/structure.R) accepts `missing_policy =
"complete"` or `"error"` for locked scores, keeps the original row IDs, and
reports per-outcome structural exclusions. Structural complete-case handling
also propagates `prior_only` and excluded measurement states for every
construct required by an outcome, so finite prior scores are not counted as
observed structural evidence. Causal, mediation, and moderation
results retain their source association, so the same ledger can be queried
through derived effect objects and traced into the causal stage. Reasons
distinguish measurement exclusion from missing structural scores; unsupported
rows are never silently treated as complete.

The focused [missing-data tests](../tests/testthat/test-missing-data.R) cover
partial and all-item indicator missingness, missing manifest covariates,
listwise retention, explicit errors, structural complete-case accounting, and
causal-stage propagation. The workflow remains **Partial** for methodology:
there is no multiple-imputation adapter or MCAR/MAR simulation evidence yet,
and independent item-block likelihoods are still not advertised as joint
lavaan-style FIML. Prior-only rows remain explicitly identified rather than
imputed or pooled into observed-information counts.

**Tracking commits:** `311ec2d`, `8cb4304`, `d9d370b`, `e5f4ff8`.

### [x] G6. Measurement split control and honest outer validation

**Evidence/gap:** [fit_states()](../R/fit.R#L40) randomly assigns folds from the
model's fold count, with no fold-vector/group/time argument. `associate()` does
accept structural folds and repeats, but operates on already locked scores.
The reported CV metrics participate in selecting the structural shape; there
is no public outer loop refitting the complete pipeline for performance
assessment. Cross-fitted measurement alone does not provide that outer loop.

**Comparator:** SEMinR offers `predict_pls()` for cross-validated prediction.
The nested, whole-pipeline design proposed here is a CS-SEM-specific requirement,
not a claim about SEMinR's implementation.
[SEMinR prediction API](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=68)

**Implemented (2026-09-21, core workflow):** [make_splits()](../R/splits.R)
creates deterministic random, repeated-group, and forward-only time partitions.
Group labels remain in one fold, tied time values remain in one block, and each
object retains integer row IDs, outer train/test partitions, and provenance,
including source group/time vectors when a split was created from a vector.
[fit_states()](../R/fit.R) accepts either a reusable `cssem_splits` object or an
explicit assignment vector through `split =`; listwise filtering subsets the
assignment and preserves the original row IDs. Refit and measurement-bootstrap
settings retain an explicit split rather than silently drawing a new one.

[validate_outer()](../R/outer-validation.R) now fits measurement encoders and
structural shape selection on each outer training sample only, scores untouched
rows with the training encoders, and returns a typed
`cssem_outer_validation` object. Its `selection_metrics` are labelled
`internal_selection`, while held-out predictions and RMSE/MAE/R-squared rows
are labelled `outer_test`. Partition provenance records train/test IDs, counts,
method, seed, and selected shapes. A failed partition is retained with its
partition ID, stage, and message so one failure cannot erase the remaining
evaluation evidence. Missing-data policies are explicit for both measurement
and structural stages, and held-out data without declared indicator columns
is rejected by the whole-pipeline outer metric workflow; predictor-only
prospective scoring is provided by G7's `predict()` contract.
Prior-only held-out outcome or predictor states are excluded from outer error
metrics instead of being evaluated as finite prior predictions, and missing
model indicators are rejected before partition fitting begins. Time partitions
whose source column or vector is unavailable, non-finite, or inconsistent with
the stored ordering fail preflight. A partition with no observed held-out
outcomes is retained as `partial` with a `no_observed_outcome` status detail.

Random and grouped outer evaluation should use at least three global folds so
each training partition retains at least two measurement folds. With two folds,
the runner keeps the partition provenance but records the one-fold training
window as a failed partition rather than pretending that cross-fitting occurred.
The first forward-only time partition has the same constraint: it contains only
the earliest time block, so it is retained as a failed partition unless the
measurement split assignment supplies at least two folds inside that training
window. Later time partitions can still provide valid outer estimates.

Focused [split tests](../tests/testthat/test-splits.R) cover deterministic,
grouped, time-ordered, explicit, and listwise-aligned assignments. The
[outer-validation tests](../tests/testthat/test-outer-validation.R) cover
train-only selection, metric-scope separation, failure retention, and group/time
partition constraints. The entry remains **Partial** for methodology: G7 now
provides the separate predictor-only prospective workflow, while independent
coverage studies for outer metrics have not yet been added.

**Tracking commits:** `b87c127`, `7283924`, `871ff19`, `5ca8c05`, `f8f087f`,
`1b9c28a`, `a66c627`, `89d5c9f`, `f9f94c4`, `942337f`.

### [x] G7. Structural prediction for new observations

**Evidence/gap:** [score_states()](../R/fit.R#L295) scores new indicator data,
and `.predict_shape_model()` predicts internally. Before this implementation
there was no supported public prediction contract, and scoring every construct
required every declared indicator column, including outcome blocks that
prospective prediction may not have.

**Comparator:** SEMinR documents both `predict()` and `predict_pls()`.
[SEMinR prediction methods](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=66)

**Implemented (2026-09-21, core workflow):**
`predict.cssem_association()` now provides predictor-only structural scoring in
`mode = "observed"`, with explicit outcome selection, required-parent
availability, locked measurement scale, interaction handling, extrapolation
flags, and `missing_policy = "error"` or `"na"`. Requested outcome indicators
are never read by the predictor path. `mode = "recursive"` resolves unavailable
endogenous upstream constructs along the declared structural models, retains
observed upstream indicators when present, reports recursive versus observed
sources, rejects cycles, and identifies unavailable exogenous inputs.

`prediction_assessment()` compares finite predictions with target states scored
by the same encoders. Its typed metrics table reports sample size, RMSE, MAE,
R-squared, calibration intercept and slope, and an optional training
target-mean baseline. Missing target indicators produce an explicit
`target_unavailable` status. Separate typed prediction and assessment objects
are available through `as.data.frame()` and `print()` methods; prediction is
kept distinct from disattenuated effect estimates.

The workflow remains **Partial** for methodology: categorical structural
outcomes, predictive intervals and coverage studies, and causal or
model-selection uncertainty are not implied by this API. Those limits remain
tracked by the later categorical-outcome, uncertainty, and causal gaps.

**Acceptance:** predicting Y works without Y's indicators; target indicators
cannot change predictions; valid inputs reproduce the selected shape model;
recursive and observed-parent modes have distinct contracts; unavailable inputs
and target-free assessments are explicit rather than silently omitted.

**Tracking commits:** `e7a0e4f`, `97a722d`, `f7a4cd1`, `566543f`, `b58d4df`,
`cf60de7`.

### [x] G8. Group comparison and measurement invariance

**Evidence/gap:** neither measurement nor association fitting accepts a group
specification, item-parameter equality constraints, or a group-difference test.
Separate user-managed fits are possible, but their state scales need not be
comparable. A manifest group variable or interaction is not an invariance test.

**Comparator:** lavaan supports group models and equality constraints;
SEMinR has `estimate_pls_mga()` for path differences.
[lavaan groups](https://lavaan.ugent.be/tutorial/groups.html),
[SEMinR group comparisons](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=36)

**Build:** establish group scale anchoring, then group-specific/shared item
parameter models, ordinal threshold/discrimination invariance or DIF diagnostics,
and path-difference inference. Document the chosen CS-SEM invariance target;
do not copy conventional metric/scalar labels without a corresponding model.

**Acceptance:** invariant simulations recover aligned states and calibrated
group contrasts; threshold shifts and loading differences are detected without
being mistaken for structural effects; small groups produce clear limitations.

**Implemented (2026-09-21):** [measurement_invariance()](../R/groups.R)
resolves a group column or vector through the fit's retained row IDs, retains
the pooled cross-fitted locked scores as the common scale, and reports group
construct means/SDs, group-specific ordinal discrimination/threshold or
continuous intercept/slope/residual parameters, and reference-group item
contrasts with descriptive difference flags. Manifest constructs remain in the
score summaries and receive explicit unavailable parameter rows. Groups below
`min_group_size` remain visible with a warning and a `small_group` flag.

[group_comparison()](../R/groups.R) reuses an association's selected structural
shapes and pooled scores, reports per-group scalar estimates for linear/product
edges, reference-directed path differences, and size-preserving permutation
p-values with null intervals. Nonlinear edges are reported as unavailable when a
single scalar contrast is not defined. Both results preserve the caller's random
stream and expose a `diagnostic_common_anchor` or
`associational_group_contrast` status.

This closes the supported common-anchor comparison workflow. It remains
**partial methodologically**: the package does not yet jointly estimate
multi-group MML models with shared/free ordinal thresholds and discriminations,
or provide calibrated likelihood-ratio/DIF tests. The item flags are conditional
diagnostics and must not be relabeled as conventional lavaan metric/scalar
invariance. Focused coverage is in
[test-groups.R](../tests/testthat/test-groups.R), including listwise row
alignment, threshold/loading shifts, manifest availability, small groups,
structural contrasts, reproducible permutations, and invalid inputs.

**Tracking commits:** `183cc0b`, `845e274`, `23551e2`, `27e84c1`.

### [x] G9. Cluster-aware analysis and repeated observations (P2 workflow)

**Evidence/gap:** fitting has no cluster/subject ID or sampling-weight contract;
current bootstrap helpers resample rows. `respondent_weighting = "information"`
is a posterior-precision option, not a survey-design weight. Custom structural
folds alone do not fix measurement splitting or sampling inference.

**Comparator:** lavaan documents explicitly specified multilevel models with
cluster membership and separately supports growth models.
[lavaan multilevel](https://lavaan.ugent.be/tutorial/multilevel.html),
[lavaan growth models](https://lavaan.ugent.be/tutorial/growth.html)

**Build:** **P2:** subject/cluster-aware splitting and resampling, explicit
independent-unit counts, and rejection/documentation of unsupported survey
designs. **P3:** separately design within/between-state models, longitudinal
measurement alignment, growth/random effects, and design-weighted estimation.
Cluster bootstrap support must not be described as multilevel SEM support.

**Acceptance:** repeated observations never cross train/test boundaries;
inference is calibrated under within-cluster dependence. Validate within/between
effects separately before enabling any multilevel causal interpretation.

**Implemented (2026-09-21, P2):** `fit_states()` resolves a cluster column or
full-length vector before listwise filtering, retains row-to-unit provenance,
and automatically uses grouped measurement folds. Explicit measurement splits
and outer train/test partitions reject cluster leakage. `make_splits()` and
outer-validation provenance report row and independent-unit counts.

`bootstrap_model(resample = "cluster")` samples complete units with replacement,
preserves source and draw-level IDs for duplicated units, supports measurement
refits, preserves caller RNG state, and reports the distinct-unit denominator.
`sample_accounting()` carries unit-level ledgers through measurement,
association, and derived-effect results. Unsupported survey weights, strata,
finite-population corrections, replicate weights, and design-based standard
errors fail with an explicit message; `respondent_weighting = "information"`
remains posterior-information weighting.

This closes the P2 cluster-aware workflow only. The implementation does not
estimate within/between latent states, longitudinal alignment, growth or random
effects, multilevel likelihoods, or survey-design standard errors. Those P3
estimands require the separate design listed below.

**Tracking commits:** `16d38f2`, `e0cbd83`, `1bf4727`, `2118ff5`.

**Plan:** [G9 cluster-aware analysis and repeated observations implementation plan](superpowers/plans/2026-09-21-g9-cluster-aware-analysis.md)
and [design specification](superpowers/specs/2026-09-21-g9-cluster-aware-analysis-design.md).
The plan deliberately ships the P2 unit-aware split, whole-cluster bootstrap,
independent-unit accounting, and unsupported-survey-design guard first. It keeps
P3 within/between, longitudinal, growth, random-effects, and design-weighted
estimators as a separate methodological project; grouped folds and cluster
bootstrap must not be reported as multilevel SEM.

**Plan outline:**

1. Resolve subject/cluster IDs before missing-data filtering, retain row-to-unit
   provenance, and enforce whole-unit measurement and outer partitions.
2. Add reproducible whole-cluster bootstrap resampling with unequal-cluster,
   duplicate-draw, and independent-unit metadata.
3. Extend sample accounting and validation reports with unit denominators, and
   reject unsupported survey weights, strata, finite-population corrections, and
   replicate-weight designs explicitly.
4. Write a separate P3 design before attempting within/between latent states,
   longitudinal alignment, growth/random-effects models, or design-based
   inference.

### [ ] G10. Defined contrasts, constraints, and model comparison

**Evidence/gap:** formulas describe predictors and shape policies; there is no
public parameter-label/equality/fixed-value system, arbitrary defined-effect
API, joint contrast test, or comparison interface for competing theories.
`specification_gap()` compares theory with a shadow benchmark, not two fitted
theory models. Mediation functions already cover common indirect effects.

**Comparator:** lavaan supports parameter labels, fixed parameters, constraints,
and defined expressions. [lavaan syntax](https://lavaan.ugent.be/tutorial/syntax2.html)

**Build:** begin with contrasts of existing estimates using joint resampling
draws, then paired model comparisons on identical outer splits. Only add fitting
constraints with a clearly specified optimizer and estimand. Define how models
with different measurement specifications are aligned. Do not manufacture
likelihood-ratio tests or global AIC/BIC from unrelated block/selection losses.

**Acceptance:** linear path contrasts match independent calculations;
uncertainty preserves covariance between paths; model comparisons use identical
observations, targets, and evaluation splits and expose their assumptions.

### [ ] G11. Categorical structural outcomes

**Evidence/gap:** ordinal/binary *measurement* is implemented, but
[.fit_shape_model()](../R/structure.R#L300) uses a continuous least-squares
response model. `manifest()` does not declare a binomial/ordinal response family.
There is no supported probability/threshold structural-output workflow.

**Comparator:** lavaan supports binary and ordinal endogenous variables with
appropriate categorical estimators; this does not extend to arbitrary nominal
outcomes. [lavaan categorical data](https://lavaan.ugent.be/tutorial/cat.html)

**Build:** explicit outcome-family/link declarations, probability-scale
prediction, likelihood-appropriate diagnostics, and marginal contrasts. Rework
measurement-error correction and mediation for each family; neither a linear
EIV correction nor a coefficient product should be carried over automatically.

**Acceptance:** probability predictions stay valid; binary/ordinal simulations
recover the declared target and calibrate intervals. Until then, clearly label
continuous-score analyses of such variables rather than implying categorical SEM.

### [ ] G12. Structural and effect visualization

**Evidence/gap:** [plot.fit_states()](../R/fit.R#L380) offers scores, redundancy,
and item loss. There are no package-defined structural path, effect-curve,
mediation, conditional-slope, or evidence-report plotting methods, despite
existing curve grids and moderation output.

**Comparator:** SEMinR supplies model diagrams and plot export support.
[SEMinR plotting API](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=61)

**Build:** accessible path diagrams, nonlinear response curves, simple-slope
plots and Johnson–Neyman intervals, mediation decompositions, and convergence
diagnostics. Return plot data for customization and support static export.
Differentiate declared direction from causal status visually; display curve
support and interval basis, and omit uncertainty bands when unavailable.

**Acceptance:** diagrams match the specification and routed status; labels
preserve scale and correction basis; plots do not convert missing intervals
into zero-width bands or display associational arrows as established causality.

### [ ] G13. Complete workflows, provenance, and support reporting

**Evidence/gap:** the repository has generated help, README examples, and useful
method notes, but no package vignette workflow was found. Several advanced help
examples are commented calls. README/method-note version and selector descriptions
lag the implementation. Objects retain some seeds, folds, and model version,
but not a uniform call/options/software/provenance record. Mixed-scale models
can be declared through low-level lists but lack an equally discoverable helper.

**Comparator:** lavaan offers a staged tutorial; SEMinR provides a worked
estimation/assessment vignette.
[lavaan tutorial](https://lavaan.ugent.be/tutorial/index.html),
[SEMinR vignette](https://sem-in-r.r-universe.dev/seminr/doc/SEMinR.html)

**Build:** runnable end-to-end examples covering measurement, missingness,
structure, uncertainty, prediction, and optional causal claims; a mixed-item
declaration recipe/helper; a capabilities table separating implemented,
experimental, and validated functionality; and reproducibility metadata in
results. Document scale transformations, estimands, correction limitations,
support-envelope applicability, and migration from deprecated wrappers. Raw data
retention should be optional rather than required for every saved result.

**Acceptance:** examples run from a clean installation using current public
names; output records enough information to recreate the configuration; readers
can identify unsupported conditions without reading source code.

### [ ] G14. Causal assumptions and validation contract

**Evidence/gap:** [causal.R](../R/causal.R),
[causal-mediation.R](../R/causal-mediation.R), and [routing.R](../R/routing.R)
provide more than ordinary associational SEM, but do not expose a complete
causal-graph/identification audit or a dedicated causal validation manifest
parallel to the structural and mediation suites. Adjustment declarations and
temporal order do not by themselves verify no unmeasured confounding.

**Comparator boundary:** this is a requirement arising from CS-SEM's own causal
claims, not a missing lavaan/SEMinR parity feature. Neither comparator's path
syntax is being treated as a causal-identification guarantee.

**Build:** machine-readable assumptions, causal-versus-associational graph
semantics, checks for graph-implied forbidden adjustments, overlap/nuisance
diagnostics appropriate to each estimand, and estimand-specific sensitivity
reporting. Add independent simulations for confounding, weak overlap, nuisance
misspecification, measurement error, nonlinear treatment, and mediation.
Separate unsupported designs from failed diagnostics and unverifiable assumptions.

**Acceptance:** address bugs A1–A4/A9/A10 before strengthening causal reporting;
invalid simulated designs never gain causal status merely from metadata;
coverage and bias are evaluated against independent targets. Temporal-order
checks are described as necessary discipline, not sufficient identification.

### [ ] G15. Study-specific simulation and sample-size planning

**Evidence/gap:** validation manifests, generators, worker support, and release
gates are substantial, but target prescribed scenarios. There is no public
workflow accepting a user's full measurement/structural design and searching
sample sizes for desired precision, coverage, detection, and convergence.
`supported_envelope()` is not a power or sample-size calculator.

**Comparator boundary:** this is a research-workflow recommendation, not a claim
that core lavaan or SEMinR supplies a universal sample-size calculation.

**Build:** parameterized data-generating specifications, custom scenario hooks,
independent truth evaluation, and simulation summaries with Monte Carlo error,
failure rates, and conditional versus unconditional coverage. Add a planning
wrapper only after G4/G6 and the independent-oracle work in bugs A10. Include
continuous/mixed blocks, manifest controls, correlated products, non-Gaussian
states, and missing-data stress beyond the current ordinal envelope.

**Acceptance:** known simple cases agree with analytic or independent targets;
sample-size recommendations include their assumed design and Monte Carlo
uncertainty; failed fits and unavailable intervals remain in the accounting.

### [ ] G16. Broader construct and structural classes: optional research

**Evidence/gap:** [model.R](../R/model.R) allows each indicator in one construct
only; there is no formative/composite, higher-order, cross-loading, or correlated
item-error model. The structural selector deliberately retains at most one
nonlinear edge per outcome. Temporal propagation assumes an acyclic order.
These restrictions are scope choices, not evidence that implemented functions
are broken.

**Comparator:** SEMinR documents composite and higher-order declarations;
lavaan supports richer parameter specifications.
[SEMinR higher-order constructs](https://cran.r-project.org/web/packages/seminr/seminr.pdf#page=47),
[lavaan model syntax](https://lavaan.ugent.be/tutorial/syntax2.html)

**Build only after a scope decision:** separate specifications, identification
conditions, and validation for new construct types; a joint measurement model
where shared indicators/errors require it; and jointly selected multiple
nonlinear terms with complexity control and outer validation. Feedback models
require an explicit simultaneous/dynamic estimand. Prefer a documented bridge
to an established engine for unsupported model classes when that serves users
better than changing CS-SEM's core method.

**Acceptance:** every new class has independent recovery/coverage evidence and
clear object typing; unsupported structures are rejected explicitly. Do not
present manually reusing scores as a validated higher-order latent model.

## Features that should not be copied mechanically

| Familiar SEM feature | Current recommendation |
| --- | --- |
| Global chi-square, CFI/TLI, RMSEA, SRMR | The package explicitly does not fit a global covariance SEM. Retain that boundary unless a joint implied model, reference model, and appropriate test theory are developed. Predictive gaps are not substitutes bearing these names. lavaan's corresponding measures belong to its fitted-model framework. [Reference](https://lavaan.ugent.be/tutorial/inspect.html) |
| Covariance-matrix-only input | Individual records are essential to the present respondent-level cross-fitting and information outputs. Document the requirement; direct summary-data SEM users to another estimator rather than inventing respondent scores from a covariance matrix. [lavaan covariance input](https://lavaan.ugent.be/tutorial/cov.html) |
| Automatic modification-index search | First improve residual/measurement diagnostics and prespecified model comparison. Do not transplant likelihood-based modification indices or automatically alter theory based on exploratory flags. [lavaan modification indices](https://lavaan.ugent.be/tutorial/modindices.html) |
| Latent means and growth factors | Current standardized construct states are not a fitted latent mean/growth model. Add anchoring and longitudinal measurement assumptions before interpreting group/time score differences as latent mean changes. [lavaan mean structures](https://lavaan.ugent.be/tutorial/means.html) |
| Universal significance stars | Add estimand-appropriate inference where justified; retain uncertainty and stability information. Shape-selection p-values are not general tests of path coefficients or causal validity. |
| Dedicated graphical application | Not required for parity with these R packages. Prioritize trustworthy R summaries, plots, documentation, and exportable tables. |

## Suggested delivery order

1. Resolve the relevant existing correctness defects in [bugs.md](bugs.md).
   Stabilize G5/G13: sample accounting and documentation. Use the result and
   scale-aware measurement contracts delivered in G1-G3.
2. Use the completed G6/G7 split, outer-validation, and prediction workflows to
   validate G4's inference choices. Develop G14's causal validation before
   widening causal claims.
3. Add G12 plots and G10 contrasts on the stable result/uncertainty schema;
   extend G8's core diagnostics to a joint constrained estimator only if the
   methodological decision is approved, then address the cluster-aware portion
   of G9 and G15 study planning.
4. Evaluate G11, multilevel/longitudinal G9, and G16 as separate research
   proposals. They change the estimator's scope and need more than UI work.

**Completion rule for future work:** an entry is complete only when its public
workflow, documentation, edge-case behavior, and stated numerical/methodological
validation are delivered. Adding an exported function alone does not close a
methodological gap. Existing completed entries record the evidence for that
completion; future entries should meet the same standard.
