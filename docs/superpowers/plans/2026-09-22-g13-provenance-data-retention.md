# G13 provenance and optional data retention implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give supported analysis results a uniform, serializable provenance record and allow fits to omit raw training rows when users do not want to retain them.

**Architecture:** Put record construction and the public accessor in `R/provenance.R`. Instrument each result constructor with resolved settings, a text call, software versions, data schema/accounting, and parent records. Add a compatibility-default `retain_data = TRUE` option to `fit_states()`; explicitly guard operations that need raw observations when a fit was created with `retain_data = FALSE`.

**Tech Stack:** Base R, existing result list classes, testthat edition 3; no new runtime dependency.

**Spec:** `docs/superpowers/specs/2026-09-22-g13-workflows-provenance-support-design.md`

## Global Constraints

- Provenance records must not contain raw observations, identifying row names, or captured calling environments.
- Vector-valued data arguments such as group labels are summarized without storing their raw values.
- Settings are curated; never copy a result's complete settings list when it can include data vectors or row/unit assignments.
- `fit_states()` keeps `retain_data = TRUE` as its compatibility default.
- Existing result fields remain available for compatibility.
- Existing objects without provenance report it unavailable; the accessor does not synthesize historical metadata.
- Data-dependent operations on a fit without retained data fail explicitly; score-only prediction and association operations remain available when their other inputs are present.

## Review Focus

1. Calls that pass local symbols retain resolved settings as values, not only unevaluated call text.
2. Listwise missingness keeps original row-position provenance while reporting retained rows accurately.
3. Grouped/time split and bootstrap results retain split identifiers without serializing row names or observations.
4. Supported legacy objects return an unavailable result; unsupported classes fail with an actionable class message.
5. A no-data fit still supports scoring, association, and sample accounting, while every public operation that needs raw rows fails before accessing them.

---

### Task 1: Define the provenance record and accessor

**Files:**
- Create: `R/provenance.R`
- Modify: `NAMESPACE`
- Create: `man/cssem_provenance.Rd`
- Create: `tests/testthat/helper-provenance.R`
- Create: `tests/testthat/test-provenance.R`

**Interfaces:**
- Produces: `.cssem_provenance_record(operation, call, settings, input = NULL, parent = list(), packages = character())`; `parent` is a named list of parent records.
- Produces: exported `cssem_provenance(x)`, which returns a stored record for supported classes, returns `NULL` for a supported legacy object without a record, and errors for unsupported classes.
- Record fields: `schema_version`, `operation`, `call`, `settings`, `software`, `input`, and `parent`.

- [x] **Step 1: Write failing helper/accessor tests** for serialization, text-only calls, package/R version fields, settings preservation, supported legacy objects, and unsupported input classes:

```r
test_that("provenance records serialize only configuration metadata", {
  record <- .cssem_provenance_record(
    "fit_states", quote(fit_states(seed = seed)), list(seed = 19L),
    input = list(n_input = 20L, columns = c("x1", "x2")),
    packages = "MASS"
  )
  expect_identical(unserialize(serialize(record, NULL)), record)
  expect_true(is.character(record$call))
  expect_identical(record$settings$seed, 19L)
  expect_true(nzchar(record$software$R))
  expect_true(nzchar(record$software$cssem))
  expect_false(any(c("data", "input_data", "environment") %in% names(record)))
})

test_that("cssem_provenance distinguishes legacy supported and unsupported objects", {
  legacy <- structure(list(), class = "fit_states")
  expect_null(cssem_provenance(legacy))
  expect_error(cssem_provenance(structure(list(), class = "unrecognized_result")), "supported")
})
```

```r
expect_cssem_provenance <- function(x, operation, parent_operation = NULL) {
  record <- cssem_provenance(x)
  expect_true(is.list(record))
  expect_identical(record$operation, operation)
  if (!is.null(parent_operation))
    expect_true(any(vapply(record$parent,
      function(parent) identical(parent$operation, parent_operation), logical(1))))
  invisible(record)
}
```

- [x] **Step 2: Run the provenance test** and confirm it fails because neither constructor nor accessor exists.

Run: `Rscript -e "testthat::test_file('tests/testthat/test-provenance.R')"`
Expected: missing-function failures for `.cssem_provenance_record()` and `cssem_provenance()`.

- [x] **Step 3: Implement serializable metadata construction.** Deparse calls immediately; store settings as evaluated lists; store `utils::packageVersion("cssem")`, `getRversion()`, and available requested package versions as strings; do not retain environments. Add an input-summary helper that records dimensions, column names, missing counts, integer row positions, and split IDs but never `rownames(data)` or cell values.

- [x] **Step 4: Register and document the accessor.** Support exactly the result classes listed in the approved spec. Return `NULL` when such a class predates provenance. Add the export and Rd page, including the return shape and legacy behavior.

- [x] **Step 5: Run the provenance tests and commit.**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-provenance.R')"`
Expected: all record/accessor tests pass.

```bash
git add R/provenance.R NAMESPACE man/cssem_provenance.Rd tests/testthat/helper-provenance.R tests/testthat/test-provenance.R
git commit -m "feat: add reproducibility provenance records"
```

### Task 2: Attach provenance to fitting, association, and outer validation

**Files:**
- Modify: `R/fit.R`
- Modify: `R/structure.R`
- Modify: `R/outer-validation.R`
- Test: `tests/testthat/test-provenance.R`, `tests/testthat/test-outer-validation.R`

**Interfaces:**
- Consumes: `.cssem_provenance_record()` from Task 1.
- Produces: provenance on `fit_states`, `cssem_association`, and `cssem_outer_validation` objects. Association provenance has the fit record as its parent; fit settings include the normalized measurement model and resolved fit options; outer settings include model/structure declarations, split method/fingerprint, and validation options.

- [x] **Step 1: Add failing integration tests** using a local `model` symbol, listwise missingness, and a seeded split. Assert operation names, resolved seeds/folds/missing policies, input/retained counts, row positions, split IDs, parent links, and absence of raw cell values.

```r
test_that("fit and association provenance records resolved settings and parents", {
  data <- simulate_states(n = 48, seed = 401, missing = 0)
  rownames(data) <- paste0("respondent-id-", seq_len(nrow(data)))
  data$private_note <- "SECRET-CSSEM-ROW-VALUE"
  data$a1[[1L]] <- NA_integer_
  model <- specify_measurement(
    A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3
  )
  fit <- fit_states(model, data, seed = 17, iterations = 2, diagnostics = FALSE,
    missing_policy = "listwise")
  structure <- specify_structure(B ~ linear(A))
  expect_equal(cssem_provenance(fit)$settings$seed, 17)
  expect_equal(cssem_provenance(fit)$input$n_input, 48L)
  expect_equal(cssem_provenance(fit)$input$n_retained, 47L)
  expect_identical(cssem_provenance(fit)$input$row_positions, 2:48)
  expect_false("respondent-id-1" %in% unlist(cssem_provenance(fit), recursive = TRUE, use.names = FALSE))
  expect_identical(cssem_provenance(fit)$settings$model$constructs$A$indicators, paste0("a", 1:4))
  expect_false("SECRET-CSSEM-ROW-VALUE" %in% unlist(cssem_provenance(fit), recursive = TRUE, use.names = FALSE))
  expect_identical(cssem_provenance(associate(fit, structure, structural_repeats = 1))$parent$fit$operation,
    "fit_states")
})
```

- [x] **Step 2: Run the integration tests and confirm they fail** because these result constructors do not yet attach provenance.

Run: `Rscript -e "testthat::test_file('tests/testthat/test-provenance.R'); testthat::test_file('tests/testthat/test-outer-validation.R')"`
Expected: the new assertions fail on absent provenance fields.

- [x] **Step 3: Attach provenance at each result constructor** using curated resolved values from `fit_settings`, `association_settings`, and outer `settings`; do not copy raw group/cluster vectors or full split objects into settings. Store split fingerprints plus integer row-position IDs/counts where those identify the actual partitions. Capture public calls as text and provide parent provenance for `associate()` as `parent = list(fit = fit_provenance)`.

- [x] **Step 4: Add an assertion to the existing successful outer-validation test** that `cssem_provenance(result)$operation` is `"validate_outer"` and that the stored split fingerprint matches `result$settings$split_fingerprint`; then run fit, structure, split, outer-validation, and provenance tests.

Run: `Rscript -e "testthat::test_file('tests/testthat/test-provenance.R'); testthat::test_file('tests/testthat/test-fit.R'); testthat::test_file('tests/testthat/test-outer-validation.R')"`
Expected: all focused tests pass, including existing split and outer provenance checks.

- [x] **Step 5: Commit the core result integration.**

```bash
git add R/fit.R R/structure.R R/outer-validation.R tests/testthat/test-provenance.R
git commit -m "feat: record provenance for core model workflows"
```

### Task 3: Attach provenance to resampling, comparison, and prediction results

**Files:**
- Modify: `R/bootstrap.R`, `R/contrasts.R`, `R/model-comparison.R`, `R/prediction.R`, `R/categorical.R`
- Test: `tests/testthat/test-bootstrap.R`, `tests/testthat/test-contrasts.R`, `tests/testthat/test-model-comparison.R`, `tests/testthat/test-prediction.R`, `tests/testthat/test-structural-outcomes.R`, `tests/testthat/test-provenance.R`

**Interfaces:**
- Consumes: provenance helpers from Task 1 and records attached to core fit/association/outer objects in Task 2.
- Produces: records for `cssem_bootstrap`, `cssem_contrast`, `cssem_model_comparison`, `cssem_prediction`, `cssem_prediction_assessment`, and `cssem_marginal_contrast`.

- [x] **Step 1: Add failing assertions at the existing result construction sites:** expect operations `bootstrap_model` for `boot`, `contrast` for the contrast `result`, `compare_outer` for the result from `compare_outer()`, `predict` for the prediction `result`, `prediction_assessment` for `assessment`, and `marginal_contrast` for `contrast`. Add a small `compare_models()` integration case with two identical measurement/structure specifications and assert it records operation `compare_models` with two outer-validation parents. Direct `compare_outer()` fixtures predate provenance, so assert its operation but do not invent parent records. Use this helper from `tests/testthat/helper-provenance.R`:

```r
expect_cssem_provenance <- function(x, operation, parent_operation = NULL) {
  record <- cssem_provenance(x)
  expect_true(is.list(record))
  expect_identical(record$operation, operation)
  if (!is.null(parent_operation)) {
    expect_true(any(vapply(record$parent,
      function(parent) identical(parent$operation, parent_operation), logical(1))))
  }
  invisible(record)
}
```

The `compare_models()` integration assertion is:

```r
test_that("compare_models provenance records both outer parents", {
  data <- simulate_states(n = 60, seed = 404, missing = 0)
  model <- specify_measurement(A = ordinal("a1", "a2"),
    B = ordinal("b1", "b2"), folds = 3)
  structure <- specify_structure(B ~ linear(A))
  splits <- make_splits(data, method = "random", folds = 3, seed = 405)
  result <- compare_models(model, structure, model, structure, data, splits,
    seed = 406, iterations = 1, diagnostics = FALSE,
    structural_args = list(structural_repeats = 1, shadow_scope = "temporal"))
  provenance <- expect_cssem_provenance(result, "compare_models")
  expect_length(provenance$parent, 2L)
  expect_true(all(vapply(provenance$parent,
    function(parent) identical(parent$operation, "validate_outer"), logical(1))))
})
```

Ruling: the example's two measurement folds conflicted with its three-fold
`splits` object — `validate_outer()` rejects fold-count mismatches — cost if
wrong: the integration check no longer covers a two-fold measurement split.

- [x] **Step 2: Run a representative owner test** and confirm the comparison-result assertions fail because those constructors do not yet attach records.

Ruling: only `test-model-comparison.R` was run before implementation — its direct
`compare_outer()` fixture exposed the missing record and the real integration
case exposed an invalid two-versus-three-fold setup — cost if wrong: an owning
test for another result constructor might not have shown the same missing-record
failure before that constructor was implemented.
- [x] **Step 3: Attach records at each public constructor.** Store the resolved resampling/contrast/metric/prediction settings and integer resample/partition IDs where applicable; omit raw observations, group-label vectors, and row names from input summaries; and link parent records as a named list (`fit`, `association`, `first`, or `second`) only when those parents carry provenance. Do not synthesize records for legacy parent objects. Preserve the public operation name when `compare_models()` delegates to `compare_outer()`.
- [x] **Step 4: Re-run the six owning test files and `test-provenance.R`.** Confirm the assertion helper validates the operation and parent records for every class in this task.
- [x] **Step 5: Commit this result family.**

```bash
git add R/bootstrap.R R/contrasts.R R/model-comparison.R R/prediction.R R/categorical.R tests/testthat/helper-provenance.R tests/testthat/test-bootstrap.R tests/testthat/test-contrasts.R tests/testthat/test-model-comparison.R tests/testthat/test-prediction.R tests/testthat/test-structural-outcomes.R tests/testthat/test-provenance.R
git commit -m "feat: record provenance for prediction and resampling results"
```

### Task 4: Attach provenance to measurement, group, and evidence results

**Files:**
- Modify: `R/groups.R`, `R/measurement-assessment.R`, `R/evidence-report.R`
- Test: `tests/testthat/test-groups.R`, `tests/testthat/test-measurement-assessment.R`, `tests/testthat/test-evidence-report.R`

**Interfaces:**
- Consumes: provenance helpers and fit/association records.
- Produces: records for `cssem_group_comparison`, `cssem_measurement_invariance`, `cssem_measurement_assessment`, and `evidence_report`.

- [ ] **Step 1: Add failing `expect_cssem_provenance()` assertions** to the existing tests using `out` from `measurement_invariance()` (`measurement_invariance`), `first` from `group_comparison()` (`group_comparison`), `assessment` from `measurement_assessment()` (`measurement_assessment`), and `report` from `evidence_report()` (`evidence_report`). Assert the association parent on `first` and fit parent on `out`/`assessment` when the input carries provenance; the synthetic evidence-report fixture should verify its own operation without inventing a parent record.
- [ ] **Step 2: Run those three test files** and confirm the new assertions fail on absent provenance.
- [ ] **Step 3: Attach records with resolved controls and parent links.** For explicit group vectors, record vector length/count summaries only; do not include group labels or respondent-level values in settings or provenance.
- [ ] **Step 4: Run the three owning test files and verify group/missing-row behavior is unchanged.**
- [ ] **Step 5: Commit this result family.**

```bash
git add R/groups.R R/measurement-assessment.R R/evidence-report.R tests/testthat/helper-provenance.R tests/testthat/test-groups.R tests/testthat/test-measurement-assessment.R tests/testthat/test-evidence-report.R
git commit -m "feat: record provenance for diagnostic results"
```

### Task 5: Attach provenance to mediation, causal, and moderation results

**Files:**
- Modify: `R/causal.R`, `R/causal-mediation.R`, `R/mediation.R`, `R/moderation.R`
- Test: `tests/testthat/test-causal.R`, `tests/testthat/test-causal-mediation.R`, `tests/testthat/test-mediation.R`, `tests/testthat/test-moderation.R`

**Interfaces:**
- Consumes: association provenance from Task 2.
- Produces: records for `indirect_effect`, `causal_effect`, `causal_indirect_effect`, `conditional_slopes`, and `conditional_indirect_effect`.

- [ ] **Step 1: Add failing `expect_cssem_provenance()` assertions** to existing tests on `effect`, `cm`, `med`, `ss`, and `mm`, checking operations `causal_effect`, `causal_indirect_effect`, `indirect_effect`, `conditional_slopes`, and `conditional_indirect_effect` respectively. These existing fixtures often construct association objects directly, so assert the named `association` parent only when present. Add one small real `fit_states()` → `associate()` → `causal_effect()` integration test and assert that it links the stored association provenance.
- [ ] **Step 2: Run the four owning test files** and confirm the new assertions fail because these result constructors lack provenance.
- [ ] **Step 3: Attach records containing the declared estimand, adjustments, temporal-order declaration, reliability/correction settings, bootstrap settings, and association parent.** Never encode the output label as proof that causal assumptions are true.
- [ ] **Step 4: Run the four owning test files and `test-provenance.R`.**
- [ ] **Step 5: Commit this result family.**

```bash
git add R/causal.R R/causal-mediation.R R/mediation.R R/moderation.R tests/testthat/test-causal.R tests/testthat/test-causal-mediation.R tests/testthat/test-mediation.R tests/testthat/test-moderation.R
git commit -m "feat: record provenance for effect estimands"
```

### Task 6: Make fit data retention optional without breaking score-only workflows

**Files:**
- Modify: `R/fit.R`, `R/bootstrap.R`, `R/measurement.R`, `R/measurement-assessment.R`, `R/groups.R`, `R/missing-data.R`, and `R/summary.R`
- Test: `tests/testthat/test-fit.R`, `tests/testthat/test-missing-data.R`, `tests/testthat/test-bootstrap.R`, `tests/testthat/test-groups.R`, `tests/testthat/test-measurement-assessment.R`, and `tests/testthat/test-prediction.R`

**Interfaces:**
- Consumes: `fit_states(..., retain_data = TRUE)`; when false, `$data` and `$input_data` are `NULL`, but scores, encoders, row IDs, and sample ledger remain.
- Produces: score-only operations continue; data-required operations fail with an error directing users to refit using `retain_data = TRUE`.

- [ ] **Step 1: Add failing fit tests** proving the new option is accepted, raw frames and row-aligned cluster labels are omitted only when false, default behavior remains unchanged, `score_states()`, `associate()`, explicitly vector-grouped `group_comparison()`, and `sample_accounting()` still work, and bootstrap/measurement methods fail with the documented message. Include non-default caller row names and cluster labels in the fixture; assert neither appears in the no-data fit or its sample ledger, while anonymous unit counts remain available:

```r
test_that("fits can omit raw data while retaining score-only workflows", {
  data <- simulate_states(n = 60, seed = 403, missing = 0)
  rownames(data) <- paste0("respondent-id-", seq_len(nrow(data)))
  cluster <- rep(paste0("person-id-", seq_len(nrow(data) / 2L)), each = 2L)
  model <- specify_measurement(A = ordinal(paste0("a", 1:4)),
    B = ordinal(paste0("b", 1:4)), folds = 3)
  retained <- fit_states(model, data, seed = 14, iterations = 2,
    diagnostics = FALSE, cluster = cluster)
  expect_true(is.data.frame(retained$data))
  fit <- fit_states(model, data, seed = 14, iterations = 2,
    diagnostics = FALSE, cluster = cluster, retain_data = FALSE)
  expect_null(fit$data)
  expect_null(fit$input_data)
  expect_false(any(grepl("respondent-id-", fit$sample_ledger$rows$row_name, fixed = TRUE)))
  expect_null(fit$input_cluster_ids)
  expect_null(fit$cluster_ids)
  expect_null(fit$measurement_split$cluster_values)
  expect_false(any(grepl("person-id-", unlist(fit, recursive = TRUE, use.names = FALSE), fixed = TRUE)))
  accounting <- sample_accounting(fit)
  expect_s3_class(accounting, "cssem_sample_accounting")
  expect_equal(nrow(accounting$unit_summary), length(unique(cluster)))
  expect_true("cluster_id" %in% names(accounting$unit_summary))
  expect_false(any(grepl("person-id-", accounting$unit_summary$cluster_id, fixed = TRUE)))
  expect_true(is.data.frame(score_states(fit, data[, c(paste0("a", 1:4), paste0("b", 1:4))])))
  expect_true(all(is.finite(measurement_parameters(fit)$estimate[
    measurement_parameters(fit)$parameter != "manifest_scale"])))
  expect_s3_class(item_response_curve(fit, "A", "a1"), "cssem_item_response_curve")
  association <- associate(fit, specify_structure(B ~ linear(A)), structural_repeats = 1)
  expect_s3_class(association, "cssem_association")
  new_data <- simulate_states(n = 12, seed = 404, missing = 0)
  prediction <- predict(association, new_data, outcomes = "B")
  expect_s3_class(prediction, "cssem_prediction")
  expect_s3_class(group_comparison(association, rep(c("g1", "g2"), each = 30),
    reference = "g1", permutations = 5, min_group_size = 10), "cssem_group_comparison")
  expect_error(bootstrap_model(fit, function(context) mean(context$scores$A), reps = 2),
    "retain_data = TRUE")
  expect_error(measurement_assessment(fit), "retain_data = TRUE")
  expect_error(measurement_invariance(fit, rep(c("g1", "g2"), 30)), "retain_data = TRUE")
  expect_error(update(fit), "retain_data = TRUE")
})
```
- [ ] **Step 2: Run the fit/missing-data/bootstrap tests** and confirm they fail because `retain_data` is not a formal argument and raw-data methods have no shared guard.
- [ ] **Step 3: Add the final `retain_data = TRUE` formal argument.** Record it in `fit_settings` and provenance. Complete all calculations that require data before constructing the result, then set `data` and `input_data` to `NULL` when false; replace sample-ledger `row_name` values with their integer `row_id` strings so row positions remain while caller row names do not. Remove raw row-aligned cluster vectors from `input_cluster_ids`, `cluster_ids`, `measurement_split$cluster_values`, and stored settings; sanitize any retained split object by dropping its group/time/unit value vectors; anonymize unit-summary identifiers while preserving per-unit counts and integer row positions.
- [ ] **Step 4: Audit every direct `$data`/`$input_data` use on fitted objects** with `rg -n -F '$data' R` and `rg -n -F '$input_data' R`. Add an early, actionable guard to `bootstrap_model()`, `measurement_assessment()`, `measurement_invariance()` when it needs item rows, and `update.fit_states()` when stored inputs are absent. Keep `measurement_parameters()` and `item_response_curve()` score/encoder-only behavior available; fields requiring raw support may be `NA` when retention is off. Update `.resolve_fit_groups()` so explicit group vectors can align against sample-ledger input/retained counts without a data frame, while column-name groups fail clearly when raw data is unavailable. Keep `group_comparison()` available for explicitly supplied vectors when its calculations only need locked scores. Make `sample_accounting()` rely on the retained sample ledger and integer row positions when raw row names are absent. Keep `update.fit_states(fit, model = ..., data = ...)` available when the caller supplies both inputs.
- [ ] **Step 5: Run all affected test files** and verify association, score transformation, sample accounting, and new-record prediction pass on a fit with `retain_data = FALSE`.
- [ ] **Step 6: Commit the optional-retention boundary.**

```bash
git add R/fit.R R/bootstrap.R R/measurement.R R/measurement-assessment.R R/groups.R R/missing-data.R R/summary.R tests/testthat/test-fit.R tests/testthat/test-missing-data.R tests/testthat/test-bootstrap.R tests/testthat/test-groups.R tests/testthat/test-measurement-assessment.R tests/testthat/test-prediction.R
git commit -m "feat: allow fits without retained training data"
```

## Test commands

- Focused: `Rscript -e "testthat::test_file('tests/testthat/test-provenance.R')"`
- Regression: `Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary')"`
- Serialization: `Rscript -e "testthat::test_file('tests/testthat/test-provenance.R')"`
