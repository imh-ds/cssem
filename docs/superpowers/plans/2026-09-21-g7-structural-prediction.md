# G7 Structural Prediction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a leakage-safe public prediction workflow for new observations and a target-aware prediction assessment table.

**Architecture:** Add a focused `R/prediction.R` module. It will resolve requested structural outcomes, score only the required construct indicators on the fitted locked-score scale, optionally recurse through unavailable upstream structural parents, and return typed prediction/assessment objects with row-level provenance. Existing `fit_states()`, `score_states()`, and `associate()` behavior remains unchanged.

**Tech Stack:** R S3 methods, base data frames, existing CS-SEM encoders and structural shape models, testthat edition 3.

**Spec:** `docs/superpowers/specs/2026-09-21-g7-structural-prediction-design.md`

## Global Constraints

- Outcome indicator columns must never be required or read by `predict.cssem_association()` for the requested outcome.
- Predictions use the selected model on the locked-score outcome scale and never receive disattenuated effect estimates or causal labels.
- `mode = "observed"` uses direct parent states; `mode = "recursive"` may predict unavailable upstream parents only through the declared acyclic structural order.
- `missing_policy = "error"` fails before prediction for missing required columns; `"na"` retains row/outcome provenance with unavailable statuses.
- No new package dependencies may be added.
- Existing `score_states()` exact-column behavior and existing outer-validation behavior must remain unchanged.
- User-facing documentation must distinguish predictor-only scoring from target assessment and must report extrapolation rather than silently treating it as in-support prediction.

## Review Focus

- Target-indicator leakage: removing or changing requested outcome indicators must not change predictions.
- Interaction inputs: `A:B` must require and multiply both `A` and `B` construct states.
- Recursive resolution: missing upstream mediators may be predicted, while cycles and unavailable exogenous parents produce actionable failures.
- Missing measurement rows: partial indicators remain scoreable; prior-only rows become unavailable rather than observed evidence.
- Support and assessment: extrapolated rows are flagged, and predictor-only data yields `target_unavailable` metrics rather than fabricated scores.

---

### Task 1: Direct predictor-only scoring and typed prediction results

**Files:**
- Create: `R/prediction.R`
- Create: `tests/testthat/test-prediction.R`
- Modify: `NAMESPACE`
- Create/modify: `man/predict.cssem_association.Rd`

**Interfaces:**
- Consumes: `cssem_association$full_models`, `cssem_association$fit`, `.predict_shape_model()`, encoder metadata, `score_center`, and `score_scale`.
- Produces: `predict.cssem_association()`, class `cssem_prediction`, `print.cssem_prediction()`, and `as.data.frame.cssem_prediction()`.

- [ ] **Step 1: Write the failing direct-prediction tests**

Add tests that fit a small structural association, remove the requested outcome
indicator columns, and assert that `predict(association, newdata)` returns a
`cssem_prediction` object with one prediction row per input row and outcome.
Assert that changing or adding the outcome indicators does not change the
predictions. Add a test that unknown outcomes and missing direct-parent columns
produce the documented errors.

- [ ] **Step 2: Run the tests to verify they fail**

Run:

```powershell
$env:R_LIBS_USER=(Resolve-Path .test-lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe' -e "library(testthat); library(cssem); test_file('tests/testthat/test-prediction.R', reporter='summary')"
```

Expected: failure because `predict.cssem_association()` and `cssem_prediction`
do not exist.

- [ ] **Step 3: Implement direct input resolution**

Implement helpers that validate the association and requested outcomes, derive
direct parent constructs from shape metadata (including interaction terms),
derive required indicator columns, score only those constructs with the stored
full encoders and score standardization, and classify each row as complete,
partial, prior-only, or unavailable. Never call `score_states()` with the full
model indicator set for this path because that would require outcome columns.

- [ ] **Step 4: Implement observed-mode predictions and support metadata**

For each requested outcome, call `.predict_shape_model()` on the resolved
parent-state frame. Return long prediction rows with `row_id`, `outcome`,
`prediction`, `mode`, `status`, and `extrapolated`. Return availability rows
with required constructs/columns and reasons. Compare finite parent scores to
finite training score ranges in `association$scores` for extrapolation flags.
Implement `print()` and `as.data.frame()` methods and add roxygen/NAMESPACE
registration.

- [ ] **Step 5: Run focused tests and commit**

Run the prediction test file and the existing fit/structure regression tests.
Expected: direct predictor-only tests pass with only existing convergence or
deprecation warnings. Commit:

```powershell
git add R/prediction.R tests/testthat/test-prediction.R NAMESPACE man/predict.cssem_association.Rd
git commit -m "feat: add predictor-only structural scoring"
```

### Task 2: Recursive upstream prediction

**Files:**
- Modify: `R/prediction.R`
- Modify: `tests/testthat/test-prediction.R`
- Modify: `man/predict.cssem_association.Rd`

**Interfaces:**
- Consumes: Task 1 state resolver, `association$structure$order`, selected
  `full_models`, and interaction-term helpers.
- Produces: `mode = "recursive"` behavior with source/status metadata.

- [ ] **Step 1: Write failing recursive tests**

Add a mediator chain test where the new data has only exogenous indicators and
the requested outcome has no indicators. Assert that recursive mode predicts
the outcome, records upstream state sources, and does not use target indicators.
Add tests for an unavailable exogenous parent and a cyclic structure.

- [ ] **Step 2: Run the tests to verify they fail**

Run the focused prediction test file. Expected: recursive-mode assertions fail
because Task 1 rejects missing parent indicator blocks instead of resolving
selected upstream models.

- [ ] **Step 3: Implement recursive resolution**

Add a per-outcome resolver with a recursion stack. Use available non-target
indicator blocks first; otherwise recurse into a selected structural model.
Follow the declared order, detect repeated nodes in the active stack, and return
row-level unavailable reasons for exogenous parents without indicators. Ensure
interaction terms resolve both component constructs before prediction.

- [ ] **Step 4: Preserve leakage and support semantics**

Exclude the requested outcome from starting observed states even when its
indicators are present. Propagate partial/prior-only statuses and
extrapolation flags through recursive parents. Keep `missing_policy = "na"`
row-level and make `missing_policy = "error"` fail before returning a partial
result.

- [ ] **Step 5: Run focused tests and commit**

Run prediction, outer-validation, structure, and fit tests. Commit:

```powershell
git add R/prediction.R tests/testthat/test-prediction.R man/predict.cssem_association.Rd
git commit -m "feat: support recursive structural prediction"
```

### Task 3: Prediction assessment metrics

**Files:**
- Modify: `R/prediction.R`
- Modify: `tests/testthat/test-prediction.R`
- Modify: `NAMESPACE`
- Create/modify: `man/prediction_assessment.Rd`

**Interfaces:**
- Consumes: `predict.cssem_association()` results and target indicator blocks in
  an assessment data frame.
- Produces: `prediction_assessment()` and class `cssem_prediction_assessment`.

- [ ] **Step 1: Write failing assessment tests**

Assert that assessment with complete target indicators reports `n`, RMSE, MAE,
R-squared, calibration intercept/slope, and training-mean baseline metrics.
Assert that predictor-only data returns one `target_unavailable` row per
outcome with `NA` metrics. Assert that prior-only target rows are excluded.

- [ ] **Step 2: Run the tests to verify they fail**

Run the prediction test file. Expected: failure because
`prediction_assessment()` and its result class do not exist.

- [ ] **Step 3: Implement assessment**

Score target constructs only inside the assessment function. Join target and
prediction rows by row ID/outcome, calculate finite-pair RMSE/MAE/R-squared,
fit a simple calibration regression when estimable, and compare against the
training mean when `baseline = "mean"`. Preserve explicit statuses for missing
target columns, no observed targets, and no valid predictions.

- [ ] **Step 4: Document and register the assessment API**

Add roxygen/man documentation describing target leakage rules, locked-score
units, baseline interpretation, and unavailable metrics. Register
`prediction_assessment` and the `print.cssem_prediction_assessment` method.

- [ ] **Step 5: Run focused tests and commit**

Run prediction, summary-extractor, outer-validation, and structure tests. Commit:

```powershell
git add R/prediction.R tests/testthat/test-prediction.R NAMESPACE man/prediction_assessment.Rd
git commit -m "feat: add structural prediction assessment"
```

### Task 4: G7 documentation and release notes

**Files:**
- Modify: `docs/gaps.md`
- Modify: `NEWS.md`
- Modify: `man/predict.cssem_association.Rd`
- Modify: `man/prediction_assessment.Rd`

**Interfaces:**
- Consumes: the shipped APIs and commit IDs from Tasks 1–3.
- Produces: a traceable G7 implementation record and explicit remaining
  limitations.

- [ ] **Step 1: Write the documentation update**

Mark G7 as core workflow implemented/partial, document observed and recursive
prediction modes, target-unavailable assessment behavior, extrapolation flags,
and remaining limitations such as categorical structural outcomes and causal
prediction.

- [ ] **Step 2: Add commit references**

Add the Task 1–3 commit IDs to the G7 summary row and detailed section in
`docs/gaps.md`, matching the existing G1–G6 tracking format.

- [ ] **Step 3: Update NEWS and generated documentation**

Describe the new public predictor-only workflow and assessment table without
claiming calibrated causal or categorical prediction.

- [ ] **Step 4: Verify and commit**

Run `git diff --check`, parse every `R/*.R` file, run the focused G7 regression
set, and commit:

```powershell
git add docs/gaps.md NEWS.md man/predict.cssem_association.Rd man/prediction_assessment.Rd
git commit -m "docs: document G7 structural prediction"
```

## Final Verification

Run the project’s full test suite using the temporary R library when available.
Record the existing baseline failures separately from G7 results. Verify that:

- all new prediction tests pass;
- existing G6 outer-validation tests still pass;
- all 28 R files parse;
- `git diff --check` is clean;
- only the pre-existing untracked `.claude/` directory remains outside Git.
