# G15 simulation evidence for G4 inference

**Status:** screening complete; confirmation is running. The runner and
predeclared design are committed. No acceptance threshold will be retuned from
the observed results.

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
Intervals use `contrast()` over row-resampled locked manifest scores, with
percentile bounds at 95%. The fixed-selection interval holds the point-fit
shapes constant; the repeat-selection interval reruns shape selection in each
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

The confirmation tier is now running the registered 500 outer replications
and 199 inner draws per condition. Final bias, conditional/unconditional
coverage, Monte Carlo intervals, failures, partial analyses, and availability
will be recorded after it completes. No confirmation result is available yet.

The runner writes ignored replication-level `.rds` files and a tracked
aggregate CSV at
[`tests/internal/validation_results/g15-inference-coverage.csv`](../tests/internal/validation_results/g15-inference-coverage.csv).
The reproducible runner is
[`tools/validation/g15-inference-studies.R`](../tools/validation/g15-inference-studies.R).

## G6 outer-metric coverage gate

`validate_outer()$test_metrics` contains point RMSE, MAE, and R-squared for each
outer test partition. It has no confidence bounds. The held-out folds also
overlap in their training sets, so treating fold-level metrics as independent
observations would not provide a calibrated interval for a declared
sample-size-specific target. No justified interval method for the G6 outer
metrics is established by this study. G6 coverage is therefore unavailable and
G15's sample-size planner remains gated; the study-specific simulation and
summary APIs do not by themselves provide a sample-size recommendation.
