# G6 Split Control and Honest Outer Validation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add reusable random, grouped, and time-ordered split specifications, explicit measurement fold control, and a whole-pipeline outer-validation result that separates selection metrics from untouched-test metrics.

**Architecture:** A new `R/splits.R` module owns split construction, validation, row-ID provenance, and fold resolution. `fit_states()` consumes the resolved measurement assignment without changing its default random behavior. A new `R/outer-validation.R` module loops over explicit outer partitions, fits measurement and structural selection on training rows only, scores the held-out rows with training encoders, and returns partition-level predictions, metrics, failures, and provenance.

**Tech Stack:** R S3 generics, base `data.frame` list-columns, existing `fit_states()`, `score_states()`, `associate()`, and `.predict_shape_model()` internals, `testthat` edition 3.

**Spec:** `docs/superpowers/specs/2026-09-21-g6-split-control-outer-validation-design.md`

## Global Constraints

- Preserve current defaults: `fit_states()` keeps deterministic random folds from `model$folds`; `associate()` keeps current repeated structural selection when no explicit split is supplied.
- Integer row IDs are authoritative; row names are retained only as labels.
- Group splits allow repeated group labels and never split one group across folds.
- Time splits use training rows earlier than the corresponding test interval.
- Outer-test rows cannot affect measurement fitting, shape selection, or nuisance fitting.
- Report outer-test predictions as `outer_test`; never label them as internal cross-validation.
- Predictor-only prospective scoring remains unsupported under G7 and must fail with an actionable message when held-out outcome indicators are absent.
- Use `measurement_missing_policy = "partial"` by default and `structural_missing_policy = "complete"` by default in `validate_outer()`.
- Do not add dependencies beyond the package's current Imports and testthat Suggests.

## Review Focus

- Repeated groups: one group must have one fold and must never appear in both sides of an outer partition; test in Task 1.
- Time ties and ordering: tied timestamps remain in one test interval, and no test timestamp may be earlier than its training maximum; test in Task 1.
- Listwise measurement filtering: explicit fold assignments and row IDs must remain aligned after missing-row removal; test in Task 2.
- Train-only encoders: a held-out row's indicator values must not change the training fit or selected structural shape; test in Task 3.
- Failed outer partitions: an error must be retained with partition ID and stage while other partitions remain inspectable; test in Task 3.

---

### Task 1: Add reusable split specifications

**Files:**
- Create: `R/splits.R`
- Create: `man/make_splits.Rd`
- Create: `tests/testthat/test-splits.R`
- Modify: `NAMESPACE`

**Interfaces:**
- Consumes: a data frame, optional group/time column name or vector, fold count, and seed.
- Produces: `make_splits(data, method = c("random", "group", "time"), folds = 5L, group = NULL, time = NULL, seed = 1L)` returning class `cssem_splits`; `print.cssem_splits()`; internal `.resolve_split_assignment(split, data, default_folds)`.
- `cssem_splits$assignment` is an integer vector of length `nrow(data)`; `cssem_splits$outer` is a data frame with `outer_id`, `train_ids`, `test_ids`, `train_n`, and `test_n` list/scalar columns; `$row_ids` is `seq_len(nrow(data))`; `$provenance` records method, source columns, seed, and fold count.

- [ ] **Step 1: Write failing tests for deterministic and constrained splits**

Add tests in `tests/testthat/test-splits.R`:

```r
test_that("random splits are deterministic and auditable", {
  data <- data.frame(id = seq_len(24), value = rnorm(24))
  first <- make_splits(data, method = "random", folds = 4, seed = 9)
  second <- make_splits(data, method = "random", folds = 4, seed = 9)
  expect_s3_class(first, "cssem_splits")
  expect_identical(first$assignment, second$assignment)
  expect_true(all(vapply(first$outer$train_ids, function(x) length(x) > 0, logical(1))))
  expect_true(all(vapply(first$outer$test_ids, function(x) length(x) > 0, logical(1))))
  expect_true(all(mapply(function(train, test) !any(train %in% test), first$outer$train_ids, first$outer$test_ids)))
})

test_that("group splits keep every group in one partition", {
  data <- data.frame(group = rep(letters[1:8], each = 3), value = rnorm(24))
  splits <- make_splits(data, method = "group", group = "group", folds = 4, seed = 3)
  group_fold <- tapply(splits$assignment, data$group, function(x) length(unique(x)))
  expect_true(all(group_fold == 1L))
  expect_error(make_splits(transform(data, group = NA_character_), method = "group", group = "group"), "missing")
})

test_that("time splits are forward-only", {
  data <- data.frame(time = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 20), value = rnorm(20))
  splits <- make_splits(data, method = "time", time = "time", folds = 4)
  expect_true(all(vapply(seq_len(nrow(splits$outer)), function(i) {
    max(data$time[splits$outer$train_ids[[i]]]) < min(data$time[splits$outer$test_ids[[i]]])
  }, logical(1))))
  data$time[1:2] <- data$time[1]
  tied <- make_splits(data[!is.na(data$time), ], method = "time", time = "time", folds = 4)
  expect_length(unique(tied$assignment[1:2]), 1L)
  data$time[5] <- NA
  expect_error(make_splits(data, method = "time", time = "time"), "time")
})

test_that("invalid split inputs fail before fitting", {
  data <- data.frame(x = seq_len(8))
  expect_error(make_splits(data, method = "random", folds = 1), "folds")
  expect_error(make_splits(data, method = "group", group = rep(1:2, each = 3)), "length")
})
```

- [ ] **Step 2: Run the split tests to verify the expected RED state**

Run:

```powershell
$env:R_LIBS_USER = (Resolve-Path .test-lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "library(testthat); library(cssem); test_file('tests/testthat/test-splits.R', reporter='summary')"
```

Expected: FAIL because `make_splits()` and `cssem_splits` do not exist.

- [ ] **Step 3: Implement `R/splits.R` minimally**

Implement `.split_values()` to resolve a scalar column name or length-matched vector. Validate `folds >= 2`, finite row IDs, and non-empty partitions. For random and group methods, assign folds with a seeded permutation; for group, sample unique groups and expand the assignment back to rows. For time, order rows by the supplied time values, keep equal time values in the same block, divide those contiguous time blocks into `folds` intervals, and create outer rows for intervals 2 through `folds` with all earlier intervals as training IDs. Store integer IDs rather than data-frame row names.

Register `print.cssem_splits` and export `make_splits` in `NAMESPACE`; write the generated help file to match the public signature and explain that repeated group labels are allowed.

- [ ] **Step 4: Run the split tests to verify GREEN**

Run the same `test_file()` command. Expected: all split tests pass with no failures.

- [ ] **Step 5: Commit the split subsystem**

```powershell
git add R/splits.R man/make_splits.Rd tests/testthat/test-splits.R NAMESPACE
git commit -m "Add reusable validation split specifications"
```

### Task 2: Allow explicit measurement fold assignments

**Files:**
- Modify: `R/fit.R`
- Modify: `R/bootstrap.R`
- Modify: `R/summary.R`
- Modify: `man/fit_states.Rd`
- Modify: `tests/testthat/test-splits.R`

**Interfaces:**
- Consumes: `fit_states(..., split = NULL)` where `split` is `NULL`, a `cssem_splits` object, or an integer assignment vector of length `nrow(data)`.
- Produces: `fit_states$measurement_split` containing the resolved `row_ids`, `assignment`, method, and provenance; `fit_states$folds` remains the integer assignment used by the encoder loops.

- [ ] **Step 1: Write failing tests for explicit measurement assignments**

Append tests:

```r
test_that("fit_states uses an explicit split assignment", {
  data <- simulate_states(n = 60, seed = 2)
  model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
                           B = list(indicators = paste0("b", 1:4), scales = "ordinal")), folds = 3)
  split <- make_splits(data, method = "random", folds = 3, seed = 12)
  fit <- fit_states(model, data, split = split, iterations = 1, diagnostics = FALSE)
  expect_identical(fit$folds, split$assignment)
  expect_identical(fit$measurement_split$row_ids, seq_len(nrow(data)))
  expect_identical(fit$measurement_split$assignment, split$assignment)
})

test_that("explicit assignments follow listwise row filtering", {
  data <- simulate_states(n = 60, seed = 3)
  data[5, "a1"] <- NA
  model <- cssem_model(list(A = list(indicators = paste0("a", 1:4), scales = "ordinal")), folds = 3)
  split <- make_splits(data, method = "random", folds = 3, seed = 13)
  fit <- fit_states(model, data, split = split, missing_policy = "listwise", iterations = 1, diagnostics = FALSE)
  expect_false(5L %in% fit$measurement_split$row_ids)
  expect_equal(length(fit$measurement_split$assignment), nrow(fit$data))
  expect_error(fit_states(model, data, split = rep(1:2, length.out = 10), iterations = 1, diagnostics = FALSE), "split")
})
```

- [ ] **Step 2: Run the new tests to verify RED**

Run `test-splits.R` with the command from Task 1. Expected: the new fit tests fail because `fit_states()` has no `split` parameter or stored measurement split.

- [ ] **Step 3: Implement split resolution in `fit_states()`**

Add `split = NULL` after `missing_policy` to preserve existing positional arguments. Resolve `split$assignment` or the explicit integer vector before fold sampling; validate length against the original input data, require at least two populated folds, and pass the assignment to `check_data()`. When listwise filtering removes rows, subset both `row_ids` and assignment before fitting. Store the resolved split object and assignment. Keep the existing random `sample(rep(...))` branch when `split = NULL`.

Update `fit_settings`, `update.fit_states()`, and `.bootstrap_context()` so refits reuse the explicit split when it is available. Add the argument and behavior to `man/fit_states.Rd`.

- [ ] **Step 4: Run focused and regression tests**

Run:

```powershell
$env:R_LIBS_USER = (Resolve-Path .test-lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "library(testthat); library(cssem); test_file('tests/testthat/test-splits.R', reporter='summary'); test_file('tests/testthat/test-structure.R', reporter='summary'); test_file('tests/testthat/test-bootstrap.R', reporter='summary')"
```

Expected: all tests pass; existing tests may emit their established convergence/deprecation warnings.

- [ ] **Step 5: Commit explicit measurement fold control**

```powershell
git add R/fit.R R/bootstrap.R R/summary.R man/fit_states.Rd tests/testthat/test-splits.R
git commit -m "Allow explicit measurement split assignments"
```

### Task 3: Add whole-pipeline outer validation

**Files:**
- Create: `R/outer-validation.R`
- Create: `man/validate_outer.Rd`
- Create: `tests/testthat/test-outer-validation.R`
- Modify: `NAMESPACE`

**Interfaces:**
- Consumes: `validate_outer(model, structure, data, splits, seed = 1L, iterations = 15L, tolerance = 1e-3, quadrature = seq(-4, 4, length.out = 31L), diagnostics = FALSE, structural_args = list(), measurement_missing_policy = c("partial", "listwise", "error"), structural_missing_policy = c("complete", "error"))`.
- Produces: class `cssem_outer_validation` with `predictions`, `selection_metrics`, `test_metrics`, `provenance`, `failures`, `settings`, and `status`; `print.cssem_outer_validation()` reports successful/failed partitions and the metric scope.

- [ ] **Step 1: Write failing tests for outer isolation and result contracts**

Create `tests/testthat/test-outer-validation.R`:

```r
test_that("outer validation separates training selection from test metrics", {
  generated <- cssem:::.structural_validation_data("linear", n = 72, seed = 21, items = 3L, missing = 0)
  generated$model$folds <- 3L
  splits <- make_splits(generated$data, method = "random", folds = 3, seed = 8)
  result <- validate_outer(generated$model, generated$structure, generated$data, splits,
    iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1L, shadow_scope = "temporal"))
  expect_s3_class(result, "cssem_outer_validation")
  expect_true(all(result$test_metrics$metric_scope == "outer_test"))
  expect_true(all(result$selection_metrics$metric_scope == "internal_selection"))
  expect_true(all(result$provenance$train_n > 0 & result$provenance$test_n > 0))
  expect_true(all(vapply(result$provenance$train_ids, function(x) length(x) > 0, logical(1))))
})

test_that("group and time outer partitions preserve their constraints", {
  generated <- cssem:::.structural_validation_data("linear", n = 72, seed = 22, items = 3L, missing = 0)
  generated$model$folds <- 3L
  generated$data$entity <- rep(seq_len(18), each = 4)
  grouped <- make_splits(generated$data, method = "group", group = "entity", folds = 3, seed = 4)
  expect_true(all(vapply(grouped$outer$test_ids, function(test) {
    !any(generated$data$entity[test] %in% generated$data$entity[setdiff(seq_len(nrow(generated$data)), test)])
  }, logical(1))))
  generated$data$time <- seq_len(nrow(generated$data))
  timed <- make_splits(generated$data, method = "time", time = "time", folds = 3)
  expect_true(all(vapply(seq_len(nrow(timed$outer)), function(i) {
    max(generated$data$time[timed$outer$train_ids[[i]]]) < min(generated$data$time[timed$outer$test_ids[[i]]])
  }, logical(1))))
})

test_that("failed outer partitions are retained", {
  generated <- cssem:::.structural_validation_data("linear", n = 48, seed = 23, items = 3L, missing = 0)
  generated$model$folds <- 2L
  splits <- make_splits(generated$data, method = "random", folds = 2, seed = 5)
  generated$data[ splits$outer$test_ids[[1]], "a1"] <- NA
  result <- validate_outer(generated$model, generated$structure, generated$data, splits,
    iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1L, shadow_scope = "temporal"),
    measurement_missing_policy = "error")
  expect_true(nrow(result$failures) >= 1L)
  expect_true(all(c("outer_id", "stage", "message") %in% names(result$failures)))
})
```

- [ ] **Step 2: Run the outer-validation tests to verify RED**

Run:

```powershell
$env:R_LIBS_USER = (Resolve-Path .test-lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "library(testthat); library(cssem); test_file('tests/testthat/test-outer-validation.R', reporter='summary')"
```

Expected: FAIL because `validate_outer()` and `cssem_outer_validation` do not exist.

- [ ] **Step 3: Implement partition fitting and test prediction**

For each row of `splits$outer`, subset training and test data by integer IDs. Build a training-only measurement split by subsetting the global assignment and remapping its levels to consecutive integers. Call `fit_states()` on training rows with that split and `measurement_missing_policy`; before calling `score_states()`, select exactly the model's declared indicator columns so split metadata columns do not violate its existing column contract. Call `associate()` on the training fit with `structural_args`, forcing `missing_policy = structural_missing_policy` and rejecting overrides of `fit`, `structure`, or `folds`.

For every selected outcome model in `association$models`, call `.predict_shape_model()` on the held-out construct scores. Join predictions to the held-out outcome score and calculate RMSE, MAE, and R-squared only on finite pairs. Store prediction rows with `metric_scope = "outer_test"`. Store selected training edge rows from `effect_ledger(association)` with `metric_scope = "internal_selection"`. Record train/test IDs, counts, method, seeds, and selected shapes in `provenance`; catch partition errors into `failures` with stage and message while continuing other partitions.

Reject a test partition when the held-out data lack any declared outcome indicator needed by `score_states()` and report that this is the G7 predictor-only limitation. Keep the result object valid when every partition fails, with empty typed tables and `status = "failed"`.

Register the constructor and print method in `NAMESPACE`; add generated help for the public arguments, metric scope, and G7 limitation.

- [ ] **Step 4: Run focused and full relevant tests**

Run the outer-validation, split, structure, bootstrap, and validation files. Expected: all pass; existing convergence warnings are acceptable, but no failed expectations or dropped partition records are allowed.

- [ ] **Step 5: Commit the outer-validation subsystem**

```powershell
git add R/outer-validation.R man/validate_outer.Rd tests/testthat/test-outer-validation.R NAMESPACE
git commit -m "Add honest outer validation"
```

### Task 4: Document G6 and run release verification

**Files:**
- Modify: `docs/gaps.md`
- Modify: `NEWS.md` only if the repository's current release-note section lacks the new public APIs.

**Interfaces:**
- Consumes: the committed `cssem_splits`, explicit `fit_states(split = ...)`, and `validate_outer()` behavior from Tasks 1–3.
- Produces: a checked G6 entry that records implemented scope, defaults, test evidence, and the remaining G7 limitation.

- [ ] **Step 1: Write the documentation update**

Change G6 to `[x]` with status `Partial (core workflow implemented)`. Document `make_splits()` random/group/time behavior, explicit measurement assignments, train-only outer fitting, `outer_test` versus `internal_selection` metrics, failed-partition retention, and the fact that predictor-only prospective scoring remains G7.

- [ ] **Step 2: Verify documentation and source hygiene**

Run `git diff --check`, parse every file under `R/`, install the package into a temporary library, and run the focused G6 tests plus `test-structure.R`, `test-bootstrap.R`, and `test-validation.R`.

- [ ] **Step 3: Commit the G6 documentation**

```powershell
git add docs/gaps.md NEWS.md
git commit -m "Document G6 split control and outer validation"
```

- [ ] **Step 4: Report verification evidence**

Report the exact test commands and any pre-existing package-check limitations. Remove temporary libraries and leave only pre-existing untracked files untouched.
