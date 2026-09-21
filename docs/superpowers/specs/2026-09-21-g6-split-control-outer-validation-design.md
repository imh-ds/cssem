# G6 Split Control and Honest Outer Validation Design

**Date:** 2026-09-21  
**Scope:** `cssem` G6 in `docs/gaps.md`

## Goal

Give users a reusable, auditable way to define measurement and outer
evaluation splits, including grouped and time-ordered designs, and report
untouched outer-test performance separately from the internal cross-validation
used for structural selection.

The current defaults remain unchanged: `fit_states()` continues to create
deterministic random measurement folds from `model$folds`, and `associate()`
continues to perform repeated structural selection on locked scores when no
explicit split is supplied.

## Design

### Split specification

Add `make_splits(data, method = c("random", "group", "time"), folds = 5L,
group = NULL, time = NULL, seed = 1L)` as the public constructor. It returns a
`cssem_splits` object containing:

- `method`, `folds`, `seed`, and the resolved row IDs;
- an integer `assignment` vector for reusable cross-fitting folds;
- an `outer` data frame with one row per outer split and explicit `train_ids`
  and `test_ids` list-columns; and
- provenance describing the grouping or time column used.

Random splits assign rows reproducibly. Group splits assign every group to one
fold, allow repeated group labels by design, and reject missing labels or
partitions that split a group. Time splits sort by the
supplied time column, require finite/non-missing order values, and make each
test interval later than its corresponding training interval. All constructors
reject empty training/test partitions, overlap, and row IDs outside the input
data. Row names are retained only as labels; integer row IDs are authoritative.

The object has a print method that reports the method, number of folds, and
partition sizes. A small internal resolver accepts either a `cssem_splits`
object or a validated integer fold vector so existing structural code can use
the same contract.

### Measurement fold control

Extend `fit_states(..., split = NULL)`:

- `NULL` preserves the current random assignment from `model$folds`;
- a `cssem_splits` object uses its `assignment` after checking that its row IDs
  match the supplied data; and
- an explicit integer vector remains supported for programmatic callers, with
  the same fold-count and minimum-support checks used by preflight.

The fit stores the resolved split object and the fold assignment used for each
retained row. Training encoders only see rows in their training folds; the
stored provenance makes this auditable without changing score scales. Existing
`update.fit_states()` and bootstrap settings carry the split through when it is
available.

### Outer validation

Add `validate_outer(model, structure, data, splits, ...,
measurement_missing_policy = "partial", structural_missing_policy =
"complete")`. The runner accepts a `cssem_splits` object with outer
partitions and, for each partition:

1. fit the measurement model on outer-training rows using only the training
   rows and the split's measurement assignment;
2. score training and test rows with the training encoders;
3. run `associate()` on training scores only, including all structural shape
   selection and repeated structural folds;
4. predict the declared structural outcomes for the untouched test scores
   using the selected training models; and
5. return per-row predictions plus aggregate test metrics and a provenance
   table containing the outer IDs, train/test IDs, split method, and seeds.

The result class `cssem_outer_validation` separates `selection_metrics` from
`test_metrics`, labels test predictions as `outer_test`, and records failures
with the outer split and stage rather than dropping them. The first supported
prediction target is an outcome whose indicators are present in the held-out
data; predictor-only prospective scoring remains G7 and is rejected with an
actionable message when outcome indicators are absent.

For time splits, all measurement fitting, shape selection, and nuisance work
must use rows no later than the test interval. Group IDs cannot occur in both
training and test rows. No result field calls outer-test metrics a cross-
validated selection score.

## Error handling and compatibility

Invalid split lengths, duplicated IDs, group leakage, non-monotone time
assignments, missing required columns, and unsupported predictor-only targets
fail before fitting a partition. A partition that fails during fitting is
retained as a failed result with its error message, while successful partitions
remain inspectable. Existing callers that do not pass `split` or `splits`
retain current behavior and output classes.

## Testing contract

Focused tests will cover deterministic random assignments, group integrity,
time ordering, overlap/empty-partition errors, explicit measurement folds,
train-only fitting with held-out row tracing, separate internal versus outer
metrics, and failed-partition reporting. An installed-package smoke test will
exercise a small ordinal model through all three split methods. Existing
structure, bootstrap, and validation tests must remain green.

## Deliberate limits

This design does not implement recursive predictor-only scoring, calibration,
or new-outcome handling without outcome indicators; those belong to G7. It
also does not claim that a single outer split estimates generalization
reliably. Users must choose the number of outer partitions and report the
resulting uncertainty; the package reports the split structure and metrics but
does not turn them into causal or population-validity claims.
