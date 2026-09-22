# G8 Group Comparison and Measurement Invariance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add common-anchor measurement invariance diagnostics and group-level structural path contrasts with reproducible permutation inference.

**Architecture:** A new focused `R/groups.R` module resolves group labels against the fit's retained row IDs, summarizes aligned pooled scores, refits item parameters conditional on each construct's pooled locked score, and compares each group with a reference. A second public function reuses an existing association's selected structural shapes and pooled scores to estimate group effects and fixed-size permutation contrasts. No independent group measurement fits are used as the comparison scale.

**Tech Stack:** Base R, existing CS-SEM encoder helpers, existing association shape helpers, testthat edition 3.

**Spec:** `docs/superpowers/specs/2026-09-21-g8-group-comparison-invariance-design.md`

## Global Constraints

- Keep pooled `fit$locked_scores` as the only comparison scale.
- Do not label conditional parameter contrasts as lavaan metric/scalar invariance.
- Preserve caller RNG state with `.preserve_seed()` and deterministic `seed` arguments.
- Retain small groups with explicit flags and warnings; never silently discard them.
- Do not add imports or external dependencies.
- Mark unavailable sparse, manifest, and nonlinear estimates explicitly.

## Review Focus

- A listwise-filtered fit with a full-length group vector must align through `fit$row_ids`.
- An ordered factor's category labels must use the pooled encoder level order.
- A group with missing or unseen item categories must produce unavailable diagnostics, not a recoding.
- A manifest construct must appear in score summaries but not invent item parameters.
- Repeated permutation comparisons with the same seed must be identical and must not alter `.Random.seed`.

---

### Task 1: Group resolution and measurement invariance diagnostics

**Files:**
- Create: `R/groups.R`
- Modify: `NAMESPACE`
- Test: `tests/testthat/test-groups.R`

**Interfaces:**
- Produces `measurement_invariance(fit, group, reference = NULL, min_group_size = 20L, level = .95, tolerance = .10)` returning `cssem_measurement_invariance`.
- Internal helpers `.resolve_fit_groups()`, `.group_item_parameters()`, and `.group_parameter_contrasts()` are private.

- [ ] **Step 1: Write failing tests** for group-column/vector alignment, score summaries, reference contrasts, manifest unavailable rows, threshold and slope differences, sparse-group warnings, invalid groups, and deterministic output.
- [ ] **Step 2: Run `testthat::test_file('tests/testthat/test-groups.R')`** and confirm the new function is missing.
- [ ] **Step 3: Implement group resolution and validation** in `R/groups.R`, including original-row and filtered-row alignment, labels, minimum sizes, and `reference - group` metadata.
- [ ] **Step 4: Implement common-anchor item diagnostics** by conditioning `.fit_ordinal()` and `.fit_continuous()` on the pooled construct score, returning one row per item parameter and explicit unavailable reasons.
- [ ] **Step 5: Implement `measurement_invariance()`** with pooled construct summaries, item parameter contrasts, status, limitations, and a print method.
- [ ] **Step 6: Add exports** for `measurement_invariance()` and the S3 print method in `NAMESPACE`.
- [ ] **Step 7: Run the focused tests** and commit:

```bash
git add R/groups.R NAMESPACE tests/testthat/test-groups.R
git commit -m "feat: add common-anchor measurement invariance diagnostics"
```

### Task 2: Structural group comparison and permutation inference

**Files:**
- Modify: `R/groups.R`
- Modify: `NAMESPACE`
- Test: `tests/testthat/test-groups.R`

**Interfaces:**
- Produces `group_comparison(association, group, reference = NULL, permutations = 999L, seed = 1L, min_group_size = 20L, level = .95)` returning `cssem_group_comparison`.
- Internal `.group_structural_effects()` and `.group_permutation_p()` return fixed-shape estimates and two-sided p-values.

- [ ] **Step 1: Add failing tests** for group-specific linear/product effects, reference-directed contrasts, reproducible permutation p-values, zero-permutation behavior, small-group flags, and unavailable nonlinear effects.
- [ ] **Step 2: Run the focused tests** and confirm the structural function is missing.
- [ ] **Step 3: Implement fixed-shape group fitting** with `.fit_shape_model()` on each group's pooled locked scores and explicit complete-case checks.
- [ ] **Step 4: Implement size-preserving label permutations** using the same edge estimator and isolated seed; return p-values, permutation counts, and interval metadata without changing caller RNG state.
- [ ] **Step 5: Add `group_comparison()` and its print method**, including associational status and limitations.
- [ ] **Step 6: Run focused measurement and structural tests** and commit:

```bash
git add R/groups.R NAMESPACE tests/testthat/test-groups.R
git commit -m "feat: add group structural path contrasts"
```

### Task 3: Documentation and release verification

**Files:**
- Modify: `docs/gaps.md`
- Modify: `NEWS.md`
- Test: `tests/testthat/test-groups.R`

- [ ] **Step 1: Add a failing documentation assertion** in the focused test that checks the returned status and limitations name the common-anchor diagnostic boundary.
- [ ] **Step 2: Update `docs/gaps.md`**: mark G8 as `Partial (core workflow implemented)`, add implementation details, limitations, acceptance evidence, and all G8 tracking commit IDs; update the top status sentence and suggested delivery order.
- [ ] **Step 3: Add a G8 section to `NEWS.md`** with user-facing behavior and the implementation commit IDs.
- [ ] **Step 4: Run focused tests, package load checks, and the full test suite; record unrelated baseline failures without changing them.
- [ ] **Step 5: Commit documentation:**

```bash
git add docs/gaps.md NEWS.md tests/testthat/test-groups.R
git commit -m "docs: document G8 group comparison workflow"
```

## Verification

Run the focused group test with the installed package or `pkgload::load_all('.')`, then run all existing `tests/testthat` tests. Check `git status --short` and ensure only pre-existing `.claude/` remains untracked.
