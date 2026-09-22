# G10 Defined Contrasts, Constraints, and Model Comparison Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add stable defined contrasts, covariance-preserving uncertainty,
paired predictive model comparisons, and narrowly scoped linear structural
constraints to CS-SEM.

**Architecture:** Stable parameter IDs are added to existing parameter tables.
`contrast_spec()` and `contrast()` parse and evaluate safe arithmetic over those
IDs. A dedicated contrast bootstrap refits one association per resample so all
terms share draws. `compare_outer()` and `compare_models()` compare predictive
metrics only on identical partitions. Constraints are isolated in a separate
module and initially support deterministic pooled least squares for selected
linear locked-score edges.

**Tech Stack:** Base R, existing `parameter_table()`, `bootstrap_model()`,
`associate()`, `validate_outer()`, and `testthat` edition 3; no new runtime
dependencies.

**Spec:** `docs/superpowers/specs/2026-09-22-g10-defined-contrasts-design.md`

## Global Constraints

- Do not expose a covariance-SEM likelihood, AIC/BIC, likelihood-ratio test, or global fit statistic.
- Contrast expressions accept only numeric literals, stable IDs, `+`, `-`, `*`, `/`, and parentheses.
- One bootstrap resample supplies every term in a contrast; unavailable terms remain unavailable.
- Fixed-selection contrast intervals are conditional on the selected structural shapes; repeated selection records selection changes.
- Paired model comparisons require identical rows, targets, score bases, and outer partitions unless an explicit observed-target alignment is supplied.
- Initial constraints apply only to selected linear structural coefficients on locked scores using deterministic pooled least squares.
- Constraints reject EIV correction, information weighting, nonlinear shapes, measurement equality, and ordinal structural outcomes.
- Preserve caller RNG state and G9 row/cluster provenance.

## Review Focus

- Parameter IDs must remain unique and stable when naive and corrected rows coexist.
- Unsafe expression syntax and unknown or unavailable IDs must fail without evaluation.
- Ratios with a zero denominator must invalidate only the affected draw/expression and retain a failure reason.
- A repeated source cluster must use one measurement fold in every cluster bootstrap contrast draw.
- Comparison must reject two validation objects with equal row counts but different row IDs or target availability.
- Constraint rank deficiency and incompatible shape/EIV settings must be explicit failures.

---

### Task 1: Stable estimand identities and expression parser

**Files:**
- Modify: `R/summary.R`
- Create: `R/contrasts.R`
- Test: `tests/testthat/test-contrasts.R`

**Interfaces:**
- `contrast_spec(definitions, basis = c("auto", "naive", "corrected"))`
- `contrast(object, spec, reps = 0L, level = .95, seed = 1L, resample = c("row", "cluster"), cluster = NULL, selection = c("fixed", "repeat"))`
- `parameter_table()` rows gain `parameter_id`, `estimate_basis`, `shape`, `available`, and `status`.

- [ ] Write failing tests for stable edge/effect IDs, linear differences,
  products, malformed expressions, unknown IDs, and unavailable terms.
- [ ] Run `testthat::test_file("tests/testthat/test-contrasts.R")` and verify
  failures are due to missing functions/columns.
- [ ] Add deterministic ID constructors to `R/summary.R` and apply them to
  association, mediation, causal, conditional-effect, routing, and evidence
  parameter tables without changing existing estimates.
- [ ] Implement the restricted arithmetic tokenizer/parser and evaluator in
  `R/contrasts.R`; reject calls, indexing, assignments, and unknown symbols.
- [ ] Implement point-only `contrast()` returning a `cssem_contrast` object
  with parsed definitions, referenced rows, basis, values, and reasons.
- [ ] Run the contrast tests and commit:
  `git commit -m "feat: add stable contrast estimands"`.

### Task 2: Covariance-preserving contrast bootstrap

**Files:**
- Create: `R/contrast-bootstrap.R`
- Modify: `R/bootstrap.R`
- Modify: `R/structure.R`
- Test: `tests/testthat/test-contrasts.R`

**Interfaces:**
- Internal `.bootstrap_association(context, association, selection)` refits
  one association for each existing bootstrap context.
- `cssem_contrast` gains joint draws, intervals, selection mode, replicate
  status, and failure reasons.

- [ ] Add a failing test showing a joint difference uses the same resample and
  differs from subtracting independent intervals.
- [ ] Add a failing test for fixed versus repeated shape selection and source
  cluster fold isolation.
- [ ] Implement fixed-shape association refits from the original selected
  shapes and repeated-selection refits through `associate()`.
- [ ] Wire `contrast()` to `bootstrap_model()` so one context generates all
  referenced estimates; preserve row/cluster provenance and RNG state.
- [ ] Record zero-denominator, unavailable-term, refit, and statistic failures
  per expression/replicate without coercing values to zero.
- [ ] Verify row and cluster reproducibility, workers 1 vs. 2, and caller RNG;
  commit `feat: add covariance-preserving defined contrasts`.

### Task 3: Paired outer model comparisons

**Files:**
- Create: `R/model-comparison.R`
- Modify: `R/outer-validation.R`
- Create: `man/compare_outer.Rd`
- Create: `man/compare_models.Rd`
- Test: `tests/testthat/test-model-comparison.R`

**Interfaces:**
- `compare_outer(first, second, metrics = c("rmse", "mae", "r_squared"), reps = 999L, seed = 1L)`.
- `compare_models(model_a, structure_a, model_b, structure_b, data, splits, seed = 1L, args_a = list(), args_b = list(), alignment = NULL, ...)`.

- [ ] Write failing tests for paired metric arithmetic, split/row/target
  mismatch, missing held-out outcomes, and explicit alignment requirements.
- [ ] Add stable split and observation fingerprints to outer-validation
  provenance/result settings.
- [ ] Implement `compare_outer()` with strict identity checks, per-partition
  metric deltas, direction, comparable-row counts, and partition bootstrap
  intervals.
- [ ] Implement `compare_models()` using one caller-supplied split object and
  retain both validation results plus model, target, observation, and alignment
  metadata.
- [ ] Reject AIC/BIC, likelihood-ratio, global-fit, and inferred latent-scale
  comparisons; run tests and commit `feat: add paired outer model comparisons`.

### Task 4: Narrow linear constraint contract

**Files:**
- Create: `docs/superpowers/specs/2026-09-22-g10-constraints-design.md`
- Create: `R/constraints.R`
- Modify: `R/structure.R`
- Modify: `R/summary.R`
- Create: `man/cssem_constraint.Rd`
- Test: `tests/testthat/test-constraints.R`

**Interfaces:**
- `cssem_constraint(equal = list(c("Y~X", "Z~X")), fixed = c("Y~X" = 0))`.
- `associate(..., constraints = NULL)` only for selected linear locked-score
  edges under the design contract.

- [ ] Write the constraint design spec and failing tests for equal slopes,
  fixed zero paths, conflicting labels, rank deficiency, and unsupported
  nonlinear/EIV/weighting combinations.
- [ ] Implement validated equality groups and fixed-value declarations.
- [ ] Implement deterministic pooled constrained least squares and expose
  optimizer status, rank, conditioning, declarations, and limitations.
- [ ] Integrate constrained rows into parameter tables, summaries, and effect
  cards without changing the claim type to ML SEM.
- [ ] Run constraint tests and commit `feat: add constrained linear structural estimates`.

### Task 5: Validation, documentation, and G10 tracking

**Files:**
- Modify: `R/validation.R`
- Modify: `NEWS.md`
- Modify: `README.md`
- Modify: `docs/associational-structure.md`
- Modify: `docs/gaps.md`
- Test: `tests/testthat/test-contrasts.R`
- Test: `tests/testthat/test-model-comparison.R`
- Test: `tests/testthat/test-constraints.R`

- [ ] Add independent analytic targets for path differences, products,
  constrained coefficients, and paired held-out loss.
- [ ] Add release checks for unavailable contrast terms, mismatched comparison
  partitions, and constraints outside the linear score-scale contract.
- [ ] Add examples and help text documenting basis, selection, alignment,
  assumptions, limitations, and non-goals.
- [ ] Run focused G10 tests, package load/Rd checks, worker invariance, and the
  full suite; record unrelated baseline failures without altering them.
- [ ] Update G10 in `docs/gaps.md` with status and exact commit IDs, update
  `NEWS.md`, and commit `docs: track G10 defined contrasts and comparison`.

## Verification

Run `testthat::test_local('.', filter = 'contrasts|model-comparison|constraints')`,
the installed-package worker reproducibility check, `git diff --check`, R parse
and Rd parsing checks, then the full existing test suite. The G10 acceptance
bar is stable analytic contrasts, covariance-preserving uncertainty, strict
paired comparison provenance, and explicit constraint limitations.
