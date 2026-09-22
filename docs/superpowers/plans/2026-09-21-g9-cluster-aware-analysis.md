# G9 Cluster-Aware Analysis and Repeated Observations Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add cluster-aware splitting, resampling, provenance, and independent-unit accounting while keeping multilevel and longitudinal SEM outside the supported estimator.

**Architecture:** A focused design helper resolves cluster labels against original row IDs and validates row-to-unit mappings. `fit_states()`, grouped `make_splits()`, and `validate_outer()` carry that mapping through listwise filtering and train/test partitions. `bootstrap_model()` gains a whole-cluster resampling mode that reuses existing measurement refit machinery and records unit-level provenance. P3 multilevel/longitudinal estimation remains a documented follow-up design rather than an implementation hidden behind the P2 API.

**Tech Stack:** Base R, existing `cssem_splits`, `fit_states`, `bootstrap_model`, `validate_outer`, and `sample_accounting` infrastructure; testthat edition 3; no new dependencies.

**Spec:** `docs/superpowers/specs/2026-09-21-g9-cluster-aware-analysis-design.md`

## Global Constraints

- Preserve current row-resampling behavior when `resample = "row"` and when no cluster is supplied.
- Never allow a cluster to cross measurement folds or outer train/test partitions.
- Resolve cluster labels against original input row IDs before listwise filtering.
- Report rows and independent units separately in fit, split, bootstrap, and accounting metadata.
- Preserve caller RNG state and deterministic replicate results across worker counts.
- Do not add survey-weight, strata, finite-population, replicate-weight, or design-based standard-error support.
- Do not describe grouped splits or cluster bootstrap as multilevel, longitudinal, growth, or causal multilevel estimation.

## Review Focus

- A listwise-filtered fit with a full-length cluster vector must retain the correct labels and original row IDs.
- An explicit integer split must fail if one cluster appears in multiple folds, even when row counts look balanced.
- An unequal-cluster bootstrap must sample complete clusters, retain duplicated draw IDs, and report the distinct-unit denominator.
- A singleton cluster and a cluster with missing labels must produce distinct, actionable diagnostics.
- Repeated cluster bootstrap calls must be identical for the same seed and must not modify `.Random.seed`.

---

### Task 1: Add the cluster design contract and unit resolver

**Files:**
- Create: `R/design.R`
- Modify: `R/fit.R`
- Modify: `R/summary.R`
- Test: `tests/testthat/test-design.R`

**Interfaces:**
- Add `fit_states(..., cluster = NULL)`.
- Add private helpers `.resolve_cluster_ids()`, `.cluster_summary()`, `.validate_cluster_labels()`, and `.validate_cluster_assignment()`.
- Store `fit$cluster_ids`, `fit$cluster_summary`, `fit$independent_unit_n`, and `cluster` inside `fit$fit_settings`.

- [ ] **Step 1: Write failing tests** for a cluster column, a full-length cluster vector, missing labels, non-finite numeric labels, and listwise filtering. Pin the expected retained IDs and unit counts:

```r
fit <- fit_states(model, data, cluster = "subject", missing_policy = "listwise",
  iterations = 1, diagnostics = FALSE)
expect_identical(fit$cluster_ids, data$subject[fit$row_ids])
expect_equal(fit$independent_unit_n, length(unique(fit$cluster_ids)))
expect_equal(
  fit$cluster_summary$n_rows[match(names(table(fit$cluster_ids)), fit$cluster_summary$cluster_id)],
  as.integer(table(fit$cluster_ids))
)
```

- [ ] **Step 2: Run the new design tests** and confirm the cluster fields and resolver are missing.
- [ ] **Step 3: Implement `.resolve_cluster_ids()`** to accept a column name or one-value-per-input-row vector, reject missing/non-finite labels, and return labels aligned to `seq_len(nrow(input_data))`.
- [ ] **Step 4: Implement `.cluster_summary()`** with one row per unit (`cluster_id`, `n_rows`, `retained_rows`); keep total row and independent-unit counts as scalar fields on the fit and split objects.
- [ ] **Step 5: Integrate `cluster` into `fit_states()`** before missing-policy filtering, subset it by `input_row_ids`, and retain it in the returned object and `fit_settings` so `update.fit_states()` preserves the contract.
- [ ] **Step 6: Add `.reject_unsupported_design_fields()`** for any optional design metadata list used by the new path. It must reject named fields such as `weights`, `sampling_weights`, `strata`, `fpc`, and `replicate_weights` with a message that distinguishes survey design from `respondent_weighting = "information"`; no survey-weight argument is enabled by this plan.
- [ ] **Step 7: Run the focused design tests** and commit:

```bash
git add R/design.R R/fit.R R/summary.R tests/testthat/test-design.R
git commit -m "feat: add cluster design metadata and validation"
```

### Task 2: Enforce cluster-safe measurement and outer splits

**Files:**
- Modify: `R/splits.R`
- Modify: `R/fit.R`
- Modify: `R/outer-validation.R`
- Test: `tests/testthat/test-splits.R`
- Test: `tests/testthat/test-outer-validation.R`

**Interfaces:**
- Extend `cssem_splits` with `unit_n`, `train_unit_n`, `test_unit_n`, and grouped provenance.
- `fit_states(cluster = ids)` automatically creates a grouped measurement split when `split = NULL`; an explicit split is accepted only after `.validate_cluster_assignment()` confirms one fold per cluster.
- `validate_outer(..., cluster = NULL)` validates and propagates cluster IDs into each training refit and partition provenance.

- [ ] **Step 1: Write failing tests** for automatic grouped folds, an explicit leaking assignment, listwise-aligned grouped folds, and grouped outer partitions:

```r
fold_by_cluster <- tapply(split$assignment, data$subject, function(x) length(unique(x)))
expect_true(all(fold_by_cluster == 1L))
expect_error(fit_states(model, data, cluster = data$subject,
  split = leaking_assignment, diagnostics = FALSE), "cluster")
expect_true(all(outer$provenance$train_unit_n > 0))
expect_true(all(outer$provenance$test_unit_n > 0))
```

- [ ] **Step 2: Run the split and outer-validation tests** and confirm leakage is not rejected and unit provenance is absent.
- [ ] **Step 3: Update grouped split construction** to calculate unit counts and attach source labels when `method = "group"`; keep random and time behavior unchanged when no cluster/group is requested.
- [ ] **Step 4: Update `fit_states()`** so an omitted split with `cluster` uses the grouped split constructor, while an explicit split with cluster labels is checked for one-fold-per-unit membership before fitting.
- [ ] **Step 5: Update outer partition validation** to check train/test unit disjointness, pass training cluster labels into each `fit_states()` call, and add unit counts to the provenance table and printed status.
- [ ] **Step 6: Run all split and outer-validation tests** and commit:

```bash
git add R/splits.R R/fit.R R/outer-validation.R tests/testthat/test-splits.R tests/testthat/test-outer-validation.R
git commit -m "feat: enforce cluster-safe validation splits"
```

### Task 3: Add whole-cluster bootstrap resampling

**Files:**
- Modify: `R/bootstrap.R`
- Modify: `R/fit.R`
- Test: `tests/testthat/test-bootstrap.R`

**Interfaces:**
- Add `bootstrap_model(..., resample = c("row", "cluster"), cluster = NULL)`.
- Add private helpers `.resolve_bootstrap_clusters()`, `.cluster_bootstrap_indices()`, and `.bootstrap_unit_metadata()`.
- Extend `cssem_bootstrap` with `resample`, `original_unit_n`, `resampled_unit_n`, and per-replicate unit counts.

- [ ] **Step 1: Write failing tests** for complete-cluster sampling, duplicated draw IDs, unequal cluster sizes, deterministic seeds, caller RNG preservation, worker-count invariance, and measurement refits.
- [ ] **Step 2: Run the bootstrap tests** and confirm `resample` is not accepted.
- [ ] **Step 3: Implement `.cluster_bootstrap_indices()`** by sampling the original unit labels with replacement exactly `n_units` times and concatenating all source rows for each draw; assign draw IDs such as `draw_1`, `draw_2`, and retain source IDs in metadata.
- [ ] **Step 4: Update `.bootstrap_context()` and `.bootstrap_one()`** to use row or cluster indices, subset/refit grouped measurement splits, and return unit metadata without changing statistic callback fields.
- [ ] **Step 5: Validate cluster bootstrap support**: require cluster labels from the argument or fitted object, reject missing labels and too few sampled units for grouped refits, and preserve `.preserve_seed()` behavior.
- [ ] **Step 6: Add print/summary metadata** that says `cluster_resampling_only` and reports rows versus independent units.
- [ ] **Step 7: Run focused bootstrap tests** and commit:

```bash
git add R/bootstrap.R R/fit.R tests/testthat/test-bootstrap.R
git commit -m "feat: add whole-cluster bootstrap resampling"
```

### Task 4: Extend sample accounting and document the methodological boundary

**Files:**
- Modify: `R/missing-data.R`
- Modify: `R/summary.R`
- Modify: `R/bootstrap.R`
- Modify: `man/fit_states.Rd`
- Modify: `man/bootstrap_model.Rd`
- Modify: `man/make_splits.Rd`
- Modify: `man/validate_outer.Rd`
- Test: `tests/testthat/test-missing-data.R`
- Test: `tests/testthat/test-bootstrap.R`

**Interfaces:**
- `sample_accounting()` gains `unit_summary` and `independent_unit_n` for fits, associations, and derived effects.
- Bootstrap summaries expose the same denominator and never call information weights survey weights.

- [ ] **Step 1: Write failing tests** for unit counts after listwise exclusion, association/effect accounting, singleton units, unsupported survey metadata, and the status/limitation text.
- [ ] **Step 2: Implement unit-level ledgers** by joining the existing row ledger to retained cluster IDs and reporting row count, unit count, and rows-per-unit distribution at each stage.
- [ ] **Step 3: Add explicit design validation documentation**: unsupported weights, strata, finite-population corrections, replicate weights, and design-based standard errors fail with actionable messages; `respondent_weighting = "information"` remains posterior-information weighting.
- [ ] **Step 4: Update the generated help pages for `fit_states()`, `make_splits()`, `validate_outer()`, and `bootstrap_model()` with the cluster contract and non-multilevel limitation.**
- [ ] **Step 5: Run focused tests, package load checks, and the full suite; record unrelated baseline failures without changing them.**
- [ ] **Step 6: Commit the accounting and documentation changes:**

```bash
git add R/missing-data.R R/summary.R R/bootstrap.R man/fit_states.Rd man/make_splits.Rd man/validate_outer.Rd man/bootstrap_model.Rd tests/testthat/test-missing-data.R tests/testthat/test-bootstrap.R
git commit -m "docs: expose independent-unit accounting and design limits"
```

### Task 5: Update G9 tracking and define the P3 follow-up boundary

**Files:**
- Modify: `docs/gaps.md`
- Modify: `NEWS.md`
- Test: `tests/testthat/test-design.R`

- [ ] **Step 1: Add a documentation assertion** that the returned status and print output say cluster resampling is not multilevel SEM.
- [ ] **Step 2: Update `docs/gaps.md`** to mark G9 `Partial (P2 cluster workflow implemented)`, link this spec and plan, list the implementation commit IDs, and retain P3 as a separate unchecked multilevel/longitudinal extension.
- [ ] **Step 3: Add a G9 NEWS entry** describing cluster-safe folds, whole-cluster bootstrap, independent-unit counts, and unsupported survey designs.
- [ ] **Step 4: Run `git diff --check`, the focused design/split/bootstrap tests, and the full suite; verify only pre-existing `.claude/` remains untracked.**
- [ ] **Step 5: Commit the tracking update:**

```bash
git add docs/gaps.md NEWS.md tests/testthat/test-design.R
git commit -m "docs: track G9 cluster-aware workflow"
```

## P3 follow-up (separate design before implementation)

Do not start this work as part of the P2 implementation commits. Prepare a new
specification and plan for:

1. within/between latent-state decomposition and estimands;
2. cluster-level likelihood or estimating equations with identified variance;
3. longitudinal measurement alignment and time-varying missingness;
4. growth/random-effects structures and prediction;
5. survey weights, strata, finite-population corrections, and design-based
   standard errors; and
6. validation simulations that distinguish cluster bootstrap coverage from
   multilevel parameter recovery.

## Verification

Run the focused design, split, outer-validation, bootstrap, and missing-data
tests through `testthat::test_local('.', filter = 'design|splits|outer|bootstrap|missing')`,
then run all existing `tests/testthat` tests. Confirm deterministic results with
one and multiple workers, inspect unit counts on unequal clusters, and check
`git diff --check` before each commit.
