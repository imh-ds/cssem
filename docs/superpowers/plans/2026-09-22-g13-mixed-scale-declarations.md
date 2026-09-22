# G13 mixed-scale declarations implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let one construct declare ordinal and continuous indicators through the existing `specify_measurement()` front door.

**Architecture:** Add `mixed_items(...)` as a composition helper over `ordinal()` and `continuous()`. It returns the existing indicator-list shape, so `.build_measurement()` remains the single normalizer and estimator behavior does not change.

**Tech Stack:** Base R, package roxygen/manual Rd conventions, testthat edition 3.

**Spec:** `docs/superpowers/specs/2026-09-22-g13-workflows-provenance-support-design.md`

## Global Constraints

- `mixed_items()` only composes existing declarations; it does not infer item scale from observed values.
- Existing `ordinal()`, `continuous()`, low-level list specifications, item ordering, and estimator behavior remain unchanged.
- No new runtime dependency is introduced.

## Review Focus

1. An omitted `keys` value in one component becomes `1` for each of that component's indicators without shifting later keys.
2. A manifest, arbitrary list, or unsupported scale is rejected as a mixed component before model fitting.
3. Duplicate names within or across components are rejected rather than silently deduplicated.
4. Interleaved ordinal/continuous components preserve their exact declared item order and per-item scale.
5. The normalized mixed model runs through `fit_states()` with finite locked scores and reliability while existing pure-scale estimator tests remain unchanged.

---

### Task 1: Add the mixed-item composition helper

**Files:**
- Modify: `R/model.R`
- Modify: `NAMESPACE`
- Create: `man/mixed_items.Rd`
- Test: `tests/testthat/test-model.R`
- Test: `tests/testthat/test-fit.R`

**Interfaces:**
- Consumes: `ordinal(...)` and `continuous(...)`, each returning a list with `indicators`, `scales`, and optional `keys`.
- Produces: exported `mixed_items(...)`, returning `list(indicators = character(), scales = character(), keys = integer())` in component/item order.

- [ ] **Step 1: Write failing declaration tests** for order, scales, default and explicit keys, model normalization, empty input, invalid components, manifests, and duplicates. Put the first two tests in `test-model.R` and the end-to-end fit test in `test-fit.R`:

```r
test_that("mixed_items preserves declaration order, scales, and keys", {
  mixed <- mixed_items(
    ordinal("o1", "o2", keys = c(1, -1)),
    continuous("c1"),
    ordinal("o3")
  )
  expect_identical(mixed$indicators, c("o1", "o2", "c1", "o3"))
  expect_identical(mixed$scales, c("ordinal", "ordinal", "continuous", "ordinal"))
  expect_identical(mixed$keys, c(1L, -1L, 1L, 1L))
  model <- specify_measurement(A = mixed)
  expect_identical(model$constructs$A$indicators, mixed$indicators)
  expect_identical(model$constructs$A$scales, mixed$scales)
  expect_identical(model$constructs$A$keys, mixed$keys)
})

test_that("mixed_items rejects invalid or repeated indicator specifications", {
  expect_error(mixed_items(), "at least one")
  expect_error(mixed_items(manifest("age")), "ordinal|continuous")
  expect_error(mixed_items(list(indicators = "x", scales = "ordinal")), "ordinal|continuous")
  expect_error(mixed_items(ordinal("x"), continuous("x")), "unique")
  expect_error(mixed_items(ordinal("x", "x")), "unique")
})

test_that("a mixed construct fits through the existing measurement pipeline", {
  data <- simulate_states(n = 60, seed = 402, missing = 0)
  data$duration <- seq_len(nrow(data)) / 10
  model <- specify_measurement(
    Mixed = mixed_items(ordinal("a1", "a2"), continuous("duration")),
    Other = ordinal("b1", "b2"), folds = 3
  )
  fit <- fit_states(model, data, seed = 12, iterations = 2, diagnostics = FALSE)
  expect_true(all(is.finite(fit$locked_scores$Mixed)))
  expect_true(all(is.finite(fit$reliability)))
})
```

- [ ] **Step 2: Run the model and fit tests** and confirm the new tests fail because `mixed_items()` is not defined.

Run: `Rscript -e "testthat::test_file('tests/testthat/test-model.R'); testthat::test_file('tests/testthat/test-fit.R')"`
Expected: the new tests fail on the missing helper; existing tests continue to run.

- [ ] **Step 3: Implement the minimal helper and public documentation.** Validate that every input is an ordinal/continuous specification, materialize one scale and default key per indicator, concatenate in argument order, reject duplicate indicator names, add `@export`, add `export(mixed_items)` to `NAMESPACE`, and document its order-preserving composition in `man/mixed_items.Rd`.

```r
mixed_items <- function(...) {
  specs <- list(...)
  if (!length(specs)) stop("Supply at least one ordinal() or continuous() specification.", call. = FALSE)
  valid <- vapply(specs, function(x) {
    is.list(x) && !is.null(x$indicators) && length(x$scales) == 1L &&
      x$scales %in% c("ordinal", "continuous")
  }, logical(1))
  if (!all(valid)) stop("mixed_items() accepts only ordinal() and continuous() specifications.", call. = FALSE)
  indicators <- unlist(lapply(specs, `[[`, "indicators"), use.names = FALSE)
  scales <- unlist(Map(function(x) rep(x$scales, length(x$indicators)), specs), use.names = FALSE)
  keys <- unlist(Map(function(x) {
    if (is.null(x$keys)) rep(1L, length(x$indicators)) else
      rep(as.integer(x$keys), length.out = length(x$indicators))
  }, specs), use.names = FALSE)
  if (anyDuplicated(indicators)) stop("Indicator names in mixed_items() must be unique.", call. = FALSE)
  list(indicators = indicators, scales = scales, keys = keys)
}
```

- [ ] **Step 4: Run model and fit tests.**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-model.R'); testthat::test_file('tests/testthat/test-fit.R')"`
Expected: helper tests pass and the mixed-scale fit produces finite locked scores and reliability without changing pure ordinal/continuous behavior.

- [ ] **Step 5: Commit the independently usable helper.**

```bash
git add R/model.R NAMESPACE man/mixed_items.Rd tests/testthat/test-model.R tests/testthat/test-fit.R
git commit -m "feat: add mixed-scale measurement helper"
```
