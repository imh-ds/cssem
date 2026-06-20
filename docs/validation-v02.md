# CS-SEM v0.2 simulation validation

Version 0.2 validates the measurement and associational layers with known
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

Structural truth scenarios include linear pathways, subtle and strong monotone
smooth pathways, interaction truth, an omitted earlier construct, and
downstream same-wave information. Strong smooth truth uses more indicators,
larger sample size, and unmistakable monotone curvature; subtle smooth truth is
reported descriptively. Selection compares linear with predeclared natural
splines (df 3 and df 4) using repeated paired structural CV.

The specification gap is `theory R² - shadow R²`. Positive values favor the
declared theory model; negative values flag predictive incompleteness under the
benchmark's stated information set. Temporal and unrestricted disagreement is
evidence of same-wave dependence, not reverse causal direction.

## Release gates

`cssem_validation_report()` checks measurement non-inferiority within `.02`,
linear selection, **strong** smooth selection, sensitivity to an omitted
eligible predictor, and the expected temporal/unrestricted downstream
divergence. A failed gate narrows the operating envelope or returns the method
to development; it is never hidden.

## Parallel execution

Measurement and structural validation functions accept `workers`. Parallelism
is only across independent scenario/replication jobs; a single cross-fitted
model remains sequential. This preserves deterministic per-job seeds and avoids
nested parallel fits. The numbered local scripts use up to four workers; CI
uses one worker for a compact smoke run. Each result row records `worker_pid`;
the scripts also write a small `*_run_metadata.csv` file with wall-clock time,
requested workers, and the worker process IDs actually used.
