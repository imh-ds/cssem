# G15 simulation evidence for G4 inference

**Status:** screening complete; the confirmation workflow has been dispatched
on `main` and its shard matrix has started
([run 35954818332](https://github.com/imh-ds/cssem/actions/runs/35954818332)).
Final results are pending. A local confirmation attempt was stopped after
1,064 of 2,000 outer-job audit rows. Those partial records were not summarized
or used as evidence. No acceptance threshold will be retuned from the observed
screening results.

## Registered design

The study uses a known Gaussian latent predictor and a measured outcome. The
latent path is `beta = 0.50`. The predictor `X` and nuisance construct `Z` are
jointly standard normal with correlation `0.35`. The outcome is

```text
Y = 0.50 X + 0.45 h(Z) + error,  error ~ Normal(0, 1)
```

where `h(Z) = Z` in the linear condition and
`h(Z) = Z + 0.35 Z^3` in the monotone-curved condition. The observed predictor
is `X_observed = X + measurement_error`, with independent Gaussian measurement
error variance `(1 - reliability) / reliability`. Reliability is either `0.40`
or `0.80`; `Z` and `Y` are directly observed. This produces four conditions:
low/high predictor reliability crossed with a linear/curved nuisance effect.
Each condition uses `n = 240`.
Both tiers use study seed `150415`; generator and analysis seeds are assigned
from the complete scheduled replication grid before any shard is selected, so
sharding preserves the single-run schedule. Screening used R `4.6.0`, cssem
`0.5.0`, RNG kind `Mersenne-Twister/Inversion/Rejection`, and 8 PSOCK workers.
The Actions confirmation uses 20 matrix shards with 2 PSOCK workers per shard.
Each shard artifact records the registered tier, outer and inner replication
counts, seed, confidence level, minimum bootstrap success rate, thresholds,
runtime versions, RNG kind, operating system, and requested workers.

The independent targets are the latent structural path `0.50` and the
observed-score linear projection path

```text
0.50 * (1 - correlation(X, Z)^2) /
  ((1 - correlation(X, Z)^2) + (1 - reliability) / reliability)
```

These follow from the Gaussian conditional variance of `X` given `Z` and the
independent measurement-error variance. The truth callback computes them from
the scenario declaration; it does not inspect an estimator result.

The package pipeline declares all three variables as manifest constructs,
supplies the simulated reliability to the EIV correction, fits the focal
`X -> Y` edge as linear, and lets `associate()` select the nuisance `Z` shape.
`fit_states()` uses three measurement folds; `associate()` uses two structural
repeats, three spline degrees of freedom, and unrestricted shadow scope.
Intervals use `contrast()` over row-resampled locked manifest scores, with
percentile bounds at 95%. The fixed-selection interval holds point-fit shapes
constant; the repeat-selection interval reruns shape selection in each
bootstrap draw. Both the corrected latent-path contrast and naive
observed-score contrast are retained. At least 90% of inner draws must succeed
for an interval to be available. Outer fit/analysis failures and unavailable
intervals remain in the simulation denominator.

This is deliberately a scoped structural calibration. Manifest scores avoid
an estimated multi-item measurement encoder, so the study does not test
measurement-model refitting or uncertainty from estimated indicator
reliability. It does not establish coverage for mediation, moderation,
plausible-value draws, other shape families, or the full multi-item workflow.
The score-level and corrected latent-path estimands are reported separately.

G4 does not set numeric calibration tolerances, so the runner registers these
project-specific gates before simulation:

| Criterion | Registered threshold |
| --- | --- |
| Absolute Monte Carlo bias | At most `0.10` for each path estimand and condition |
| 95% interval coverage | Monte Carlo Wilson interval wholly within `[0.91, 0.99]` |
| Unconditional outer analysis failure rate | Wilson upper bound at most `0.05` |
| Inner bootstrap availability | At least 90% successful draws per reported interval |

The coverage tolerance is nominal `0.95 +/- 0.04`. Bias is assessed with its
Monte Carlo t interval; coverage and failure with Wilson intervals. Screening
uses 100 outer replications and 50 inner draws per condition. Confirmation
uses 500 outer replications and 199 inner draws after screening checks the
replication ledger and shape-selection changes.

## Results

The screening tier completed 100 outer replications in each of the four
conditions, with 50 draws in each fixed-selection and repeated-selection
bootstrap. It retained 1,600 estimand rows (four estimands per outer
replication) and 400 selection-audit rows. There were no outer fit failures,
partial analyses, or unavailable intervals. Every interval had 50 successful
inner draws; the repeated-selection audit recorded changes in every condition.

Screening estimates were close to their independent targets: absolute bias
ranged from `0.0001` to `0.0162`. Observed 95% coverage ranged from `0.88` to
`0.95` across estimands and conditions. With only 100 outer replications, the
Wilson intervals are broad (for example, `0.80` to `0.93` for the 0.88
estimate), so screening does not establish the registered coverage gate. It
was used only to check the ledger, callback output, inner-draw availability,
and shape-selection behavior before confirmation.

The confirmation tier runs through the manually triggered GitHub Actions
workflow [G15 inference confirmation](../.github/workflows/g15-inference-confirmation.yaml).
Its 20 shards each run a deterministic subset of the registered 500 outer
replications and 199 inner draws per condition. The combine job checks that all
20 shards arrived, runtime provenance agrees, and every scenario, sample size,
replication, and estimand is present exactly once. It computes the study
summary only after those checks pass. The previous local attempt reached 1,064
outer-job audit rows, with 199/199 successful draws in both interval modes for
those rows, but was stopped before the estimator ledger and summary were saved;
the partial audits are excluded from the calibration results. Final bias,
conditional/unconditional coverage, Monte Carlo intervals, failures, partial
analyses, and availability remain pending the Actions run.

The screening runner writes ignored replication-level `.rds` files and a
tracked aggregate CSV at
[`tests/internal/validation_results/g15-inference-coverage.csv`](../tests/internal/validation_results/g15-inference-coverage.csv).
After a complete Actions run, the raw replication ledger, summary, selection
audit, runtime provenance, and aggregate CSV are uploaded as the
`g15-inference-confirmation-results` artifact. The shard runner is
[`tools/validation/g15-inference-studies.R`](../tools/validation/g15-inference-studies.R);
the completeness check and summary step is
[`tools/validation/combine-g15-inference-shards.R`](../tools/validation/combine-g15-inference-shards.R).

## G6 outer-metric coverage gate

`validate_outer()$test_metrics` contains point RMSE, MAE, and R-squared for each
outer test partition. It has no confidence bounds. The held-out folds also
overlap in their training sets, so treating fold-level metrics as independent
observations would not provide a calibrated interval for a declared
sample-size-specific target. Bootstrapping pooled out-of-fold rows would hold
the fold-specific fitted models fixed and would not capture training/selection
variability for the sample-size target. A repeated-study simulation estimates
operating characteristics for its declared generator and estimator; it does
not create an interval for one `validate_outer()` result. No justified interval
method for the G6 outer metrics is established. G6 coverage is therefore
unavailable and G15's sample-size planner remains gated; the study-specific
simulation and summary APIs do not by themselves provide a sample-size
recommendation.
