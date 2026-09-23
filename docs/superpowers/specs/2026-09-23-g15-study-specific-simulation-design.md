# G15 study-specific simulation and sample-size planning

**Status:** Draft for user review. The callback-first direction and three design
sections were approved in conversation on 2026-09-23. This document must be
reviewed before an implementation plan is written.

## Problem and intended outcome

The package has deterministic validation manifests and generators for selected
measurement and structural cases, plus worker support and a reported supported
envelope. Those tools answer questions about registered scenarios; they do not
let a researcher specify a complete study, evaluate a chosen estimator against
independent truth, and compare candidate sample sizes against explicit goals.

G15 adds a reusable simulation workflow. A researcher supplies a scenario grid,
a data generator, an independent truth function, and an analysis callback. The
package runs the replications, retains failures and unavailable intervals,
reports operating characteristics with Monte Carlo uncertainty, and identifies
the smallest tested sample size meeting the researcher's declared criteria. The
result is conditional on the supplied design and callbacks and is not a
universal sample-size rule.

## Design boundary

Use a callback-first API rather than a new declarative SEM or data-generating
language. The scenario table is the parameterized design; callbacks express the
data-generating process, independent target, and estimator pipeline. This
supports custom measurement and structural designs while keeping the package's
estimator boundary explicit. A callback may call existing CS-SEM functions or
another estimator, but G15 does not claim that CS-SEM supports model classes it
does not currently fit.

The workflow records the declared analysis scope, such as conditional-on-locked-
scores or full-pipeline refitting. It evaluates only what the analysis callback
actually does. It does not infer that a callback reruns measurement, selection,
or outer validation from its label.

## Public workflow

### `study_spec()`

`study_spec(scenarios, sample_sizes, generate, truth, analyze, metadata)` creates
a validated `cssem_study_spec` object.

- `scenarios` is a non-empty data frame with a unique, non-missing character
  `scenario` key and arbitrary, documented parameter columns.
- `sample_sizes` is a non-empty vector of distinct positive integers.
- `generate(scenario, n, seed)` receives a named list for one scenario row,
  sample size, and deterministic seed. It returns the observed analysis data as
  a data frame; it does not return the oracle target.
- `truth(scenario, n)` returns a finite named numeric vector keyed by estimand.
  Every scenario/sample-size pair must define the same estimand names. It is
  evaluated separately from the analysis callback and before replications for
  that scenario/sample-size pair begin. Passing `n` allows targets such as
  expected prediction error to vary with the training sample size.
- `analyze(data, scenario, seed)` returns a list with an overall `status`
  (`completed`, `partial`, or `failed`), a
  logical-or-missing `converged` value, an optional `failure_reason`, and an
  `estimates` data frame. Each estimate row has `estimand`, `estimate`,
  `estimate_status`, `lower`, `upper`, `interval_status`, and an optional
  `status_reason`. `estimate_status` and `interval_status` are each `available`
  or `unavailable`. Missing estimates and intervals are represented explicitly
  with missing values and a reason, not omitted. There is exactly one row per
  declared estimand. An available estimate or interval must contain finite
  values; an unavailable one must contain missing values. Available interval
  bounds must be ordered.
- `metadata` is a named list of plain values. It must include a non-empty
  `description` of the study design and a non-empty `analysis_scope` that states
  what the callback refits (for example, conditional-on-locked-scores,
  measurement refit, or full pipeline). Additional assumptions and target
  descriptions are retained in results and summaries to make recommendations
  reviewable.

The truth callback is not passed into `analyze()`. This separates the package's
callback interface for estimation from the oracle used for evaluation. R cannot
prevent a user callback from referring to truth through a captured environment;
documentation and examples must require independent target derivation and must
not imply that the interface alone proves independence.

### `simulate_study()`

`simulate_study(spec, reps, seed, workers)` runs every scenario × sample-size ×
replication combination. `reps` and `workers` must be positive integers and
`seed` must be a valid scalar integer. Each job receives reproducible generator
and analysis seeds that do not depend on worker count; the runner resets the RNG
to each seed before calling the corresponding callback. The function restores
the caller's RNG state. `workers = 1` is sequential; larger values use the
package's supported PSOCK worker path and return the same ordered replication
results as sequential execution when callbacks are deterministic given their
seeds and can be serialized to workers.

The returned `cssem_simulation` object contains:

- one row per scheduled replication and estimand, including scenario, sample
  size, replication number, seeds, the scenario/sample-size-specific truth,
  estimate, interval bounds, generation and analysis statuses, convergence, and
  failure reason;
- the scenario table, sample-size grid, replication count, base seed, worker
  count, user metadata, and callback labels needed to interpret the run;
- an explicit run status for each replication: completed, partial, generation
  failed, or analysis failed.

Callback errors and malformed callback results are caught at the replication
boundary and retained as failed rows. A malformed static specification,
callback signature, or invalid truth output fails before an expensive run
begins. Callback return values are validated when each callback runs; invalid
values become stage-specific failed rows rather than triggering an undocumented
extra preflight call.
Generation failure, fitting failure, non-convergence, an unavailable estimate,
and an unavailable interval remain distinguishable. Failed replications are
never silently dropped. The result records design metadata and callback labels,
not arbitrary external R environments; users must retain their analysis script
to reconstruct callback code.

### `summarize_study()`

`summarize_study(simulation, null_values, level)` returns a typed summary
grouped by scenario, sample size, estimand, and metric scope. `null_values` is an
optional named numeric vector keyed by estimand; detection is reported for each
supplied null and marked unavailable when a null is not supplied. It reports
bias, RMSE, mean interval width, interval coverage, detection rate, convergence
rate, failure rate, and interval availability where the relevant values exist.
Every row includes its numerator or valid-observation count, planned-replication
denominator where applicable, Monte Carlo standard error, uncertainty interval,
and an availability status.
When supplied, `null_values` must have unique estimand names and finite numeric
values; `level` is a scalar confidence level strictly between zero and one.

- **Conditional coverage** is the proportion covered among replications with a
  finite estimate and available interval.
- **Unconditional coverage** uses all scheduled replications as the denominator;
  generation failures, analysis failures, and unavailable intervals count as
  uncovered.
- **Conditional detection** is the rate of intervals excluding the declared
  null among replications with available intervals. **Unconditional detection**
  uses all scheduled replications, with failed/unavailable intervals counted as
  non-detections.
- Convergence is reported both conditionally among analysis callbacks that
  returned a convergence result and unconditionally over all scheduled
  replications; generation or analysis failures count as not converged for the
  unconditional rate. The planner's convergence criterion uses the unconditional
  rate.
- Failure rate is the number of scheduled replications whose generation or
  analysis failed divided by all scheduled replications. Partial results,
  non-convergence, and unavailable intervals are separately counted and are not
  erased by the failure-rate definition.
- Bias, RMSE, and mean interval width are conditional on available finite
  estimates or intervals. Their valid-observation count and availability rate
  are reported alongside the metric.
- Coverage and detection use the scenario truth and declared null respectively.
  Missing values or non-finite estimates are unavailable evidence, not successes.

For bias, Monte Carlo standard errors are the standard error of the mean
replication error. For mean interval width, they are the standard error of the
observed widths. RMSE is the square root of mean squared error; its uncertainty
interval is formed by taking square roots of the confidence limits for mean
squared error, and its Monte Carlo standard error uses the delta method. Mean
metric intervals use a t interval on their replication-level inputs. For rates,
report binomial Monte Carlo standard errors and Wilson score intervals. A
summary states when a metric has no valid denominator instead of returning a
misleading zero. If all observed squared errors are zero, RMSE and its Monte
Carlo standard error are zero.

### `plan_sample_size()`

`plan_sample_size(simulation, criteria, level)` evaluates each tested sample size
against an explicit criteria table. The table has columns `metric`, `estimand`,
`scope`, `target`, `tolerance`, and `null_value`. A criterion is unique by
`metric`, `estimand`, and `scope`; unused metric-specific columns are `NA`.
`scope` is `conditional` or `unconditional` for coverage and detection,
`conditional` for precision, and `unconditional` for convergence. Criteria must
refer to estimands returned by the truth callback; convergence uses `"*"`.
For `coverage`, `target` must be in `(0, 1)` and `tolerance` non-negative with
the resulting target band inside `[0, 1]`. For `detection` and `convergence`,
`target` must be in `[0, 1]`. For `precision`, `target` must be positive. A
finite `null_value` is required only for `detection`; unused `tolerance` and
`null_value` cells are `NA`. `level` follows the same confidence-level rules as
`summarize_study()`.

- `precision`: `target` is the maximum acceptable mean interval width;
- `coverage`: `target` is nominal coverage and `tolerance` is the allowed
  absolute deviation; `scope` selects conditional or unconditional coverage;
- `detection`: `target` is the minimum acceptable detection rate and
  `null_value` is the declared null for interval exclusion; `scope` selects
  conditional or unconditional detection;
- `convergence`: `target` is the minimum acceptable unconditional convergence
  rate, `estimand` is `"*"`, and `scope` is `"unconditional"`.

Precision is evaluated conditionally among available finite intervals; its
summary row must also show interval availability and its denominator. Detection
nulls in criteria are passed to the summary calculation. This keeps an
apparently narrow interval from concealing a high rate of unavailable fits.

The planner returns each criterion's estimate, Monte Carlo uncertainty, and
one of `pass`, `fail`, `inconclusive`, or `unavailable` for every tested sample
size. Precision passes only when the upper
uncertainty bound for mean interval width is at or below its target. Detection
and convergence pass only when the lower bound for the rate is at or above its
target. Coverage passes only when the entire uncertainty interval is inside the
nominal target ± tolerance. A criterion fails when its uncertainty interval is
entirely on the wrong side of the target; otherwise it is inconclusive. A
criterion is unavailable when its required metric has no valid denominator or
required intervals/estimands are missing.

The recommendation is the smallest supplied sample size for which all criteria
pass. The planner does not interpolate or extrapolate beyond the grid and does
not assume performance is monotone in sample size. If no grid value passes, the
result distinguishes criteria that are clearly unmet from criteria still
inconclusive at the available Monte Carlo precision. The output retains the
criteria, tested grid, metadata, and uncertainty so readers can audit the
recommendation.

The public sample-size planner is sequenced behind the G4/G6 validation gate
below. Its design is specified here, but it must not be exported or advertised
until the gate is met.

## Required design coverage

The callback contract is intentionally general. Package examples and tests must
demonstrate parameterized scenarios that exercise continuous and mixed
measurement blocks, manifest controls, correlated product terms, non-Gaussian
latent states, and missing-data stress beyond the current ordinal validation
envelope. These cases are generated by the example callbacks; they do not imply
that the package estimator has become a joint likelihood SEM or that every
missingness mechanism is supported by its estimator.

The package also includes a small analytically checkable case where the
independent oracle and expected simulation summaries are known. That fixture
validates the simulation engine and summary arithmetic without reusing the
estimator as its own oracle.

## Error handling and reproducibility

Specification validation checks scenario keys, sample sizes, callback
callability and signatures, metadata, truth names/values, output column types,
and agreement between estimand names and truth for each scenario/sample-size
pair. Per-replication callback errors
are recorded with the stage and message; errors in preflight truth or static
specification are raised before the simulation starts. Partial analysis output
remains available when some estimands succeed and others fail.

The same specification, base seed, replication count, and worker count must
produce identical ordered results for sequential and supported parallel runs.
The package restores `.Random.seed` exactly as its other simulation tools do.
Custom callbacks must be deterministic given their supplied seed to receive this
reproducibility guarantee.

## G4/G6 prerequisite gate

The G15 entry in `docs/gaps.md` says the planning wrapper follows G4/G6 and the
independent-oracle work in A10. A10 is fixed. G4's bootstrap workflow and G6's
outer-validation workflow are implemented, but both remain Partial because
their independent coverage evidence is still missing. Therefore G15 is
sequenced in three stages:

1. Build and validate the callback-driven specification, replication runner,
   and simulation summaries against analytic fixtures.
2. Use the independent simulation engine to run and document G4's remaining
   coverage, bias, and failure-rate studies under low and high reliability and
   shape selection, and G6's remaining coverage studies for outer-validation
   metrics. Use the estimands and pipeline scopes actually implemented by those
   workflows. Record any conditions where the methods do not meet their targets;
   do not call the entries complete on infrastructure alone.
3. Add and export `plan_sample_size()` only after the G4/G6 gate has passed or
   their remaining scope has been explicitly revised and documented. If the
   simulation evidence does not support a criterion, keep the planner gated and
   G15 Partial while the method or scope is reviewed.

The implementation plan must preserve this gate. It may implement the generic
runner and summaries before the sample-size planner, but it must not bypass the
G4/G6 methodological dependency to make G15 appear complete.

`validate_outer()` currently returns point prediction metrics without interval
bounds. A G6 coverage study must therefore provide an independently justified
interval method through the analysis callback and evaluate it against a
scenario/sample-size-specific target. It must not treat dependent outer folds as
independent observations to manufacture an interval. If no suitable interval
method is justified and calibrated, the G6 gate remains open and the public
sample-size planner stays gated.

## Testing and acceptance

Focused tests must establish that:

1. a known simple estimator agrees with an independent analytic oracle within
   expected Monte Carlo uncertainty;
2. truth values are evaluated independently for each scenario/sample-size pair,
   including a fixture whose target changes with `n`;
3. conditional and unconditional coverage/detection use their stated, distinct
   denominators, with failed and unavailable replications retained;
4. bias, RMSE, interval width, rates, Monte Carlo standard errors, and uncertainty
   intervals match independently calculated fixture values;
5. the smallest qualifying grid value is selected, out-of-grid sample sizes are
   never proposed, and fail versus inconclusive decisions follow interval
   overlap rules;
6. callback and malformed-output errors are traceable by replication and stage;
7. results are identical across worker counts for seeded callbacks and caller
   RNG state is restored;
8. examples exercise continuous/mixed blocks, manifest variables, correlated
   products, non-Gaussian states, and missingness conditions.

The acceptance criteria from `docs/gaps.md` remain authoritative: known simple
cases agree with analytic or independent targets; recommendations state the
assumed design and Monte Carlo uncertainty; failed fits and unavailable
intervals remain accounted for.

## Non-goals and limitations

- No universal sample-size formula or recommendation outside the tested grid.
- No automatic guarantee that a user-supplied oracle is independent or a
  callback refits every intended pipeline component.
- No new joint estimator, latent-response DSL, model syntax, global fit index,
  or missing-data estimator.
- No claim of calibrated coverage for CS-SEM inference merely because the
  simulation framework can estimate coverage. Such claims require the relevant
  scenarios, replications, estimands, and pipeline scope to be run and reviewed.
- No automatic completion of G4/G6's remaining methodological validation. G15
  supplies infrastructure and evidence; those entries remain Partial until
  their own acceptance criteria have been met.

## Planned implementation files

The implementation plan should consider a focused `R/study-planning.R` module,
roxygen exports and generated documentation, `tests/testthat/test-study-planning.R`,
and a user-facing example or vignette. It should update the G15 record in
`docs/gaps.md` only as evidence is implemented and validated.
