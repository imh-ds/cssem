# G7 Structural Prediction for New Observations

**Date:** 2026-09-21

**Status:** Approved design

## Goal

Provide a public prediction contract for a fitted `cssem_association` that can
predict declared structural outcomes from new indicator data without requiring
the outcome indicators, while making required inputs, recursive assumptions,
missing rows, and extrapolation visible.

## Scope

The implementation adds two public entry points:

```r
predict.cssem_association(
  object,
  newdata,
  outcomes = NULL,
  mode = c("observed", "recursive"),
  missing_policy = c("error", "na"),
  ...
)

prediction_assessment(
  association,
  newdata,
  outcomes = NULL,
  mode = c("observed", "recursive"),
  missing_policy = c("error", "na"),
  baseline = c("mean", "none")
)
```

`outcomes = NULL` selects every declared structural outcome. A requested
outcome must have a selected structural model in the association.

`mode = "observed"` uses observed indicator blocks for the direct structural
parents of each requested outcome. The requested outcome's own indicators are
never required or read. An interaction such as `A:B` requires both `A` and
`B` construct states; interaction products are formed internally from those
states.

`mode = "recursive"` starts with observed indicator blocks for available
constructs and recursively predicts an unavailable upstream parent when that
construct has a selected structural model. Recursion follows the declared
acyclic structural order, rejects cycles, and reports an unavailable input when
an exogenous parent has neither indicators nor a structural model. Requested
outcome indicators are excluded from the starting observed state set so they
cannot leak into their own prediction.

`missing_policy = "error"` rejects missing required indicator columns before
prediction. `"na"` returns unavailable rows with `NA` predictions and an
explicit reason. Partial item responses remain supported by the existing
measurement encoders; rows with no observed indicators for a required
construct are reported as prior-only/unavailable rather than treated as
observed evidence.

## Result contracts

`predict.cssem_association()` returns a `cssem_prediction` object containing:

- `predictions`: one row per input row and requested outcome, with row ID,
  outcome, prediction, mode, status, and extrapolation flag;
- `availability`: required constructs and indicator columns, missing-input
  reasons, and the state source used for each prediction;
- `settings`: requested outcomes, mode, missing policy, and training support
  ranges used for extrapolation flags.

Predictions are on the selected structural model's locked-score outcome scale.
They are not disattenuated effect estimates and carry no causal interpretation.
The object has a concise print method and can be converted to its prediction
table with `as.data.frame()`.

`prediction_assessment()` uses the prediction object and, when outcome
indicators are available in `newdata`, scores those outcome blocks solely as
assessment targets. It returns a `cssem_prediction_assessment` object with one
row per outcome containing target source, status, `n`, RMSE, MAE, R-squared,
calibration intercept, calibration slope, and an optional training-mean
baseline. If target indicators are absent, the row is retained with
`status = "target_unavailable"`; no metric is fabricated for predictor-only
data.

## Support and extrapolation

For every construct state used by a prediction, the implementation compares
new scores with the finite training-score range retained in the association.
Rows outside that range are flagged as extrapolated while still being returned
when the model can evaluate them. Extrapolation is a support warning, not an
automatic failure. Missing required inputs and cycles remain hard errors under
the default policy.

## Leakage and failure rules

1. Outcome indicator columns are never required by `predict()` in either mode.
2. Recursive prediction may use an observed mediator/parent block, but only
   when that construct is a declared parent of the requested outcome path.
3. A requested outcome's indicators are never used as an observed starting
   state for that same prediction.
4. Unknown outcomes, malformed data frames, missing required columns under
   `missing_policy = "error"`, cycles, and unavailable exogenous parents have
   actionable errors.
5. `missing_policy = "na"` retains row/outcome provenance rather than dropping
   rows silently.

## Testing

Tests will cover:

- direct prediction without outcome indicators;
- invariance to changing or removing outcome indicators;
- direct-parent and interaction input discovery;
- recursive mediator/upstream prediction and cycle rejection;
- missing-column, prior-only, and partial-indicator statuses;
- training-range extrapolation flags;
- target-unavailable assessment rows;
- assessment metrics, calibration fields, and mean-baseline comparison.

The existing G6 outer-validation tests remain unchanged; G7 tests will verify
that the new predictor-only contract closes the limitation they currently
record.

## Explicit limitations

This design predicts the selected associational structural models. It does not
add categorical structural outcomes, causal prediction, model-selection
uncertainty, or automatic recursive prediction for cyclic graphs. Those remain
separate methodology work in G4, G11, and G14.
