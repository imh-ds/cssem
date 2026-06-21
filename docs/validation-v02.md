# CS-SEM v0.3 simulation validation

Version 0.3 validates the measurement and associational layers with known
data-generating truth. It does not validate causal effects.

## Operating envelope

The initial supported envelope is one-dimensional ordinal manifestation blocks
with at least four indicators, `n >= 200`, item signal of at least `.70`, and
no more than 10% item missingness. Cross-loadings, strong construct overlap,
sparse categories, and local dependence are retained as exploratory stress
conditions and must be reported separately.

## Measurement suite

The suite records construct recovery against truth, held-out item loss, score
stability, convergence, runtime, factor/composite proxy recovery, and the
status of exploratory diagnostics. It records sparse-warning true/false
positives separately from residual-dependence magnitude. Residual dependence
remains a reported diagnostic rather than an automatic warning until
false-positive calibration is demonstrated.

Validation fits begin with an efficient iteration budget and automatically
retry at a higher budget when an encoder has not converged. The release report
requires at least 95% convergence inside the supported envelope; CI uses a
non-retrying smoke configuration solely to verify the pipeline.

## Structural suite

The v0.3 structural suite includes linear, monotone increasing, monotone
decreasing, subtle smooth, strong non-monotone smooth, null, interaction,
omitted-predictor, and downstream-information scenarios. Selection compares
edge-level linear, constrained monotone, and predeclared natural-spline
candidates using repeated paired structural CV. Interaction truth is a
negative control: v0.3 does not discover or label interactions.

The specification gap is `theory R² - shadow R²`. Positive values favor the
declared theory model; negative values flag predictive incompleteness under the
benchmark's stated information set. Temporal and unrestricted disagreement is
evidence of same-wave dependence, not reverse causal direction.

## Release gates

`cssem_validation_report()` checks measurement non-inferiority within `.02`,
linear selection, correctly signed monotone selection, strong smooth
selection, false nonlinear selection, sensitivity to an omitted eligible
predictor, and temporal/unrestricted downstream divergence. A failed gate
narrows the operating envelope or returns the method to development; it is
never hidden.

## Parallel execution

Measurement and structural validation functions accept `workers`. Parallelism
is only across independent scenario/replication jobs; a single cross-fitted
model remains sequential. This preserves deterministic per-job seeds and avoids
nested parallel fits. The numbered local scripts use up to four workers; CI
uses one worker for a compact smoke run. Each result row records `worker_pid`;
the scripts also write a small `*_run_metadata.csv` file with wall-clock time,
requested workers, and the worker process IDs actually used.

## Diagnostic calibration

`cssem_measurement_validation_manifest("diagnostic")` and internal script 07
run sparse-category and local-dependence cases with diagnostics enabled. The
result records residual-dependence signals and their simulated false/true
positive rates. Residual correlations remain exploratory until those rates are
acceptable; they are not promoted to automatic warnings by v0.3.
