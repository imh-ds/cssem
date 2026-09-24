# G15 Study-Specific Simulation and Sample-Size Planning Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. The user selected inline execution and no subagents. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a callback-driven, reproducible study simulation workflow with independent truths, full failure accounting, Monte Carlo summaries, and a sample-size planner only after the G4/G6 validation gate passes.

**Architecture:** `study_spec()` validates a scenario grid and three separate callbacks; `simulate_study()` schedules deterministic scenario × sample-size × replication jobs and retains every result; `summarize_study()` reports conditional and unconditional operating characteristics with Monte Carlo uncertainty. The public `plan_sample_size()` is a later gated task: G4's remaining inference studies and G6's outer-metric coverage method must be independently validated first.

**Tech Stack:** R package APIs and S3 result classes, base `parallel` PSOCK workers, `testthat` edition 3, roxygen2, and R Markdown. No new runtime dependency.

**Spec:** `docs/superpowers/specs/2026-09-23-g15-study-specific-simulation-design.md`

## Global Constraints

- Keep the scenario table and callbacks as the user-defined parameterized DGP; do not create a second SEM syntax.
- Treat metadata as `NULL`, atomic vectors, or recursively nested named lists of those values; reject functions, environments, external pointers, and other live R objects.
- `truth(scenario, n)` is evaluated separately from `analyze()` for each scenario/sample-size pair; never pass its result to `analyze()`.
- Require `truth()` to be deterministic and analytically/externally derived; the callback has no seed argument by design.
- Preserve every scheduled replication, including generation errors, analysis errors, partial estimates, non-convergence, and unavailable intervals.
- Restore the caller's `.Random.seed` and keep job seeds invariant to worker count.
- Do not add package dependencies; use current `Imports`, `Suggests`, and base `parallel`.
- The planner must recommend only from the supplied sample-size grid and must include Monte Carlo uncertainty.
- Do not export or advertise `plan_sample_size()` until the G4/G6 gate in the approved spec passes; do not treat dependent outer folds as independent interval observations.
- Demonstrate continuous/mixed blocks, manifest controls, correlated products, non-Gaussian states, and missingness through callback examples/tests without implying new estimator support.
- Leave the pre-existing untracked `.claude/` directory untouched.

## Review Focus

1. **Sample-size-specific targets:** a truth callback can return a different value at each `n`; test this in Task 2.
2. **Unavailable results:** an unavailable estimate, unavailable interval, failed fit, and failed generator remain distinct; test them in Tasks 2 and 4.
3. **Denominator errors:** unconditional coverage/detection/convergence use all scheduled replications; test exact counts in Task 4.
4. **Worker/RNG behavior:** local callback closures yield ordered, identical outputs across one and two workers while restoring caller RNG; test in Task 3.
5. **Planner boundary decisions:** criteria at confidence-bound edges return pass/fail/inconclusive/unavailable correctly and never extrapolate; test in the gated Task 8.

## File Structure

- Create `R/study-planning.R` for `study_spec()` and specification/callback validation.
- Create `R/study-simulation.R` for deterministic job creation, callback execution, failure capture, and PSOCK mapping.
- Create `R/study-summary.R` for Monte Carlo metrics and, only after the gate, criteria evaluation and `plan_sample_size()`.
- Create `tests/testthat/test-study-planning.R` for public contract, ledger, metric, and gated planner tests. Keep expensive confirmation campaigns outside the ordinary package test suite.
- Create `tests/internal/validation_results/g15-inference-coverage.csv` and `docs/validation-g15.md` only for completed, reproducible calibration evidence.
- Add a narrow `.gitignore` exception for the small aggregate G15 coverage CSV; leave large raw replication outputs ignored and reproducible from the committed script and seeds.
- Create `tools/validation/g15-inference-studies.R` for longer G4/G6 confirmation studies; these scripts are not package runtime code.
- Create `vignettes/study-planning.Rmd` for a small callback-driven example; update `DESCRIPTION` only if it needs an existing suggested vignette builder that is not already declared.
- Update generated `man/` pages and `NAMESPACE` through roxygen2, then update `docs/gaps.md` and `NEWS.md` with the exact delivered scope and tracking commits.

## Test Command

In this Windows workspace, use the project-local library so `testthat` and `pkgload` are available:

```powershell
$env:R_LIBS_USER = (Resolve-Path .\local_r_lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "pkgload::load_all('.', quiet=TRUE); testthat::test_file('tests/testthat/test-study-planning.R', reporter='summary')"
```

For PSOCK checks, first install the source package to the local library so workers load the current namespace:

```powershell
$env:R_LIBS_USER = (Resolve-Path .\local_r_lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\R.exe' CMD INSTALL --library=local_r_lib .
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "library(testthat); library(cssem); test_file('tests/testthat/test-study-planning.R', reporter='summary')"
```

---

### Task 1: Define and validate the study specification

**Files:**
- Create: `R/study-planning.R`
- Create: `tests/testthat/test-study-planning.R`

**Interfaces:**
- Consumes: scenario data frame, positive-integer sample-size vector, callbacks with signatures `generate(scenario, n, seed)`, `truth(scenario, n)`, `analyze(data, scenario, seed)`, and metadata.
- Produces: `study_spec()` returning a `cssem_study_spec` with validated inputs and preserved callbacks.

- [ ] **Step 1: Write failing tests for the valid constructor and required fields.** Add this fixture to `test-study-planning.R`:

```r
make_study_spec <- function() {
  scenarios <- data.frame(scenario = c("base", "stress"), beta = c(.30, .30))
  generate <- function(scenario, n, seed) data.frame(y = rep(0, n))
  truth <- function(scenario, n) c(beta = scenario$beta, risk = .5 + 1 / n)
  analyze <- function(data, scenario, seed) list(
    status = "completed", converged = TRUE, failure_reason = "",
    estimates = data.frame(
      estimand = c("beta", "risk"), estimate = c(.30, .51),
      estimate_status = "available", lower = c(.1, .2), upper = c(.5, .8),
      interval_status = "available", status_reason = "",
      stringsAsFactors = FALSE
    )
  )
  study_spec(scenarios, c(80L, 160L), generate, truth, analyze,
    metadata = list(description = "Known linear study", analysis_scope = "locked_score"))
}

test_that("study_spec preserves the callback contract", {
  spec <- make_study_spec()
  expect_s3_class(spec, "cssem_study_spec")
  expect_equal(spec$sample_sizes, c(80L, 160L))
  expect_equal(spec$scenarios$scenario, c("base", "stress"))
  expect_identical(names(spec$metadata), c("description", "analysis_scope"))
})
```

- [ ] **Step 2: Run the focused test to verify RED.** Run the Test Command above. Expected: failure because `study_spec()` is not defined.

- [ ] **Step 3: Implement the constructor and static validation.** Validate a non-empty data frame, unique non-missing character `scenario` keys, distinct positive integer sample sizes, callable functions with required formals (or `...`), and non-empty metadata `description` and `analysis_scope`. Store the scenario table, integer sample-size vector, function objects, and plain metadata values in the `cssem_study_spec` list. Do not call the data generator or analysis callback during construction. Accept metadata only when recursively composed of `NULL`, atomic vectors, and named lists; reject executable or external objects.

- [ ] **Step 4: Add invalid-input tests and rerun RED/GREEN.** Cover duplicate scenario keys, missing scenario IDs, fractional/zero/duplicate sample sizes, non-functions, missing callback formals, absent metadata fields, and non-plain metadata values. For example:

```r
expect_error(study_spec(scenarios, 10L, generate, truth, analyze,
  metadata = list(description = "x", analysis_scope = "locked", callback = generate)),
  "metadata")
```

Run the focused file and require all tests to pass.

- [ ] **Step 5: Document and commit the specification constructor.** Add roxygen documenting the three callback signatures and independent-truth boundary. Run `roxygen2::roxygenise()` and commit `R/study-planning.R`, `tests/testthat/test-study-planning.R`, `NAMESPACE`, and `man/study_spec.Rd` as `feat: define study simulation specifications`.

### Task 2: Run sequential replications and retain the complete ledger

**Files:**
- Create: `R/study-simulation.R`
- Modify: `R/study-planning.R`
- Test: `tests/testthat/test-study-planning.R`

**Interfaces:**
- Consumes: `cssem_study_spec` from Task 1.
- Produces: `simulate_study(spec, reps, seed = 1L, workers = 1L)` returning `cssem_simulation` with a `replications` table containing one row per scheduled replication and estimand.

- [ ] **Step 1: Write tests for cross-product scheduling and `n`-dependent truth.** Use `make_study_spec()` with two scenarios, two `n` values, and `reps = 2L`; assert eight scheduled replications and sixteen ledger rows after expanding by two estimands. Assert that the `risk` truth equals `.5 + 1 / n` at each `n` and stable scenario/sample-size/replication/estimand order. For example:

```r
sim <- simulate_study(make_study_spec(), reps = 2L, seed = 19L)
expect_equal(nrow(unique(sim$replications[c("scenario", "n", "replication")])), 8L)
expect_equal(nrow(sim$replications), 16L)
expect_equal(sim$replications$truth[sim$replications$estimand == "risk"],
  .5 + 1 / sim$replications$n[sim$replications$estimand == "risk"])
```

- [ ] **Step 2: Write failure and RNG tests before implementation.** Add generator-error and analysis-error scenarios; return one partial result and one unavailable interval from `analyze()`. Assert that all scheduled rows remain, each failure has its stage and message, statuses distinguish generation failure from analysis failure and partial output, and the caller's `.Random.seed` is identical before and after the call.

- [ ] **Step 3: Run the focused simulation tests to verify RED.** Expected: `simulate_study()` is not defined and the ledger assertions fail.

- [ ] **Step 4: Implement deterministic jobs and the sequential runner.** Build jobs in scenario order, then sample-size order, then replication order. Under `.preserve_seed()`, call `set.seed(seed)` and precompute two distinct scalar seeds per job with `sample.int(.Machine$integer.max, ...)`; each callback is reset to its assigned seed immediately before invocation. Evaluate deterministic `truth(scenario, n)` once per scenario/sample-size pair before starting replications, require finite named numeric targets with identical estimand names across the grid, and restore the caller RNG on success or error. Require `generate()` to return a data frame with exactly `n` rows. For each job, catch generator and analysis errors separately, validate the exact analysis return contract, and create an unavailable row for every declared estimand if a callback fails. Retain callback labels and metadata, never callback environments, in the result. Do not drop any scheduled row.

- [ ] **Step 5: Run GREEN and commit the sequential simulation.** Run the focused test file. Commit `R/study-simulation.R`, constructor changes, and tests as `feat: run reproducible study replications`.

### Task 3: Add deterministic PSOCK worker execution

**Files:**
- Modify: `R/study-simulation.R`
- Test: `tests/testthat/test-study-planning.R`

**Interfaces:**
- Consumes: deterministic job list and `.study_run_one(job, spec, truth_values)` from Task 2.
- Produces: `simulate_study(..., workers > 1L)` with ordered results identical to `workers = 1L` for seed-deterministic callbacks.

- [ ] **Step 1: Write a worker-invariance test using a captured local constant.** Run the same small spec with `workers = 1L` and `workers = 2L`; compare the entire `replications` table, including seeds, statuses, estimates, and failure reasons. Capture a local constant in `generate()` and `analyze()` so serialization is exercised:

```r
one <- simulate_study(spec, reps = 2L, seed = 31L, workers = 1L)
two <- simulate_study(spec, reps = 2L, seed = 31L, workers = 2L)
expect_identical(one$replications, two$replications)
```

- [ ] **Step 2: Run the new worker test to verify RED.** Install the current package to `local_r_lib` with the PSOCK command above; run the focused test file. Expected: the current runner rejects or ignores `workers > 1L`.

- [ ] **Step 3: Implement the PSOCK path using the existing `.validation_map()`.** Reuse `.validation_map(jobs, .study_run_one, workers)` in `R/validation.R` after confirming it propagates `.libPaths()` and loads `cssem` on workers. Pass precomputed seeds in each job; never seed workers by worker number or job completion order. Preserve result order as the original job order.

- [ ] **Step 4: Test callback failure, worker cleanup, and caller RNG.** Assert an error in one worker becomes one failed ledger row while the other jobs complete; assert `.Random.seed` restoration after both worker modes. Rerun the focused file and verify no PSOCK workers remain after completion or error.

- [ ] **Step 5: Commit worker-safe simulation.** Commit `R/study-simulation.R` and test changes as `feat: support deterministic study workers`.

### Task 4: Summarize metrics and Monte Carlo uncertainty

**Files:**
- Create: `R/study-summary.R`
- Modify: `R/study-simulation.R`
- Test: `tests/testthat/test-study-planning.R`

**Interfaces:**
- Consumes: `cssem_simulation$replications` with truth, estimate/interval statuses, convergence, and run status.
- Produces: `summarize_study(simulation, null_values = NULL, level = .95)` returning `cssem_study_summary` with one row per metric, scenario, `n`, estimand, and scope.

- [ ] **Step 1: Create a four-replication fixture with exact denominators.** Include three available estimates with intervals (two cover truth) and one unavailable estimate/interval; include one failed generation and three convergence values. Assert conditional coverage `2 / 3`, unconditional coverage `2 / 4`, and distinct interval-availability and failure rates:

```r
simulation <- structure(list(replications = data.frame(
  scenario = rep("base", 4), n = rep(100L, 4), replication = 1:4,
  estimand = rep("beta", 4), truth = rep(.30, 4),
  estimate = c(.28, .32, .40, NA_real_),
  estimate_status = c("available", "available", "available", "unavailable"),
  lower = c(.20, .25, .35, NA_real_), upper = c(.40, .35, .45, NA_real_),
  interval_status = c("available", "available", "available", "unavailable"),
  converged = c(TRUE, TRUE, FALSE, NA),
  run_status = c("completed", "completed", "completed", "generation_failed")
)), class = "cssem_simulation")
summary <- summarize_study(simulation)
coverage <- subset(summary$metrics, metric == "coverage" & estimand == "beta")
expect_equal(coverage$estimate[coverage$scope == "conditional"], 2 / 3)
expect_equal(coverage$estimate[coverage$scope == "unconditional"], 2 / 4)
```

- [ ] **Step 2: Add exact metric and uncertainty assertions.** Check bias, RMSE, mean interval width, conditional/unconditional detection at a supplied named null, convergence rates, Wilson rate intervals, t intervals for means, and `sqrt(p * (1 - p) / denominator)` for rate MCSE. Assert missing nulls and zero denominators are marked unavailable rather than set to zero.

- [ ] **Step 3: Run the focused summary tests to verify RED.** Expected: `summarize_study()` and its summary class are absent.

- [ ] **Step 4: Implement summary helpers.** Emit metric names `bias`, `rmse`, `mean_interval_width`, `coverage`, `detection`, `convergence`, `failure`, `partial`, and `interval_availability`. Emit conditional/unconditional rows for coverage, detection, and convergence; failure and partial use a study-level `estimand = "*"` and the unconditional denominator; interval availability is estimand-specific and unconditional; bias, RMSE, and width are conditional. Count scheduled replication keys, not duplicated estimand rows, in all denominators. Add separate helpers for finite-value means, RMSE and its transformed mean-squared-error interval, Wilson rate intervals, denominator selection, and unavailable metric rows. Keep scope in a `scope` column. Treat failed/unavailable intervals as uncovered/non-detections only in unconditional metrics. For each sample size, planner criteria apply to every scenario row; a recommendation is available only when every criterion passes in every scenario.

- [ ] **Step 5: Run GREEN and commit summaries.** Run the focused test file and commit `R/study-summary.R`, `R/study-simulation.R`, tests, `NAMESPACE`, and `man/summarize_study.Rd` as `feat: summarize study operating characteristics`.

### Task 5: Add a bounded user example and callback coverage fixtures

**Files:**
- Create: `vignettes/study-planning.Rmd`
- Create: `tests/testthat/helper-study-planning.R`
- Modify: `DESCRIPTION` only if an existing vignette dependency is missing

**Interfaces:**
- Consumes: `study_spec()`, `simulate_study()`, and `summarize_study()`.
- Produces: a rendered example that identifies the estimand and analysis scope, and fixtures for the G15 data-generation conditions.

- [x] **Step 1: Add test generators for the required design dimensions.** In `helper-study-planning.R`, use a fixed seed and scenario row to generate a continuous item block, an ordinal/continuous mixed block, a manifest control, a correlated product term, a skewed latent state, and separate MCAR/MAR missingness settings. Assert each requested feature changes or appears in the generated data and that the helper restores `.Random.seed`. Keep these as data-generation contract tests; do not imply the current estimator fits all generated designs.

- [x] **Step 2: Add an analytic-oracle test and the vignette.** Add a focused test with a deterministic, centered predictor whose sum of squares is `n`, `y = beta * x + error`, and an `analyze()` callback that estimates the no-intercept slope. Since `error` has unit variance, the estimator has known variance `1/n`; for `R` replications, assert the mean estimate is within four Monte Carlo standard errors, `4 / sqrt(n * R)`, of the independent truth. Also use a modest vignette case whose prediction-risk target changes with `n` and has a closed-form expression. Show `study_spec()`, `simulate_study()`, and `summarize_study()`; state that a CS-SEM user supplies their actual `fit_states()`/`associate()`/inference pipeline and that this example does not validate those estimators. Example test:

```r
sim <- simulate_study(make_gaussian_slope_spec(n = 120L), reps = 200L, seed = 407L)
beta <- subset(sim$replications, estimand == "beta" & estimate_status == "available")$estimate
expect_lt(abs(mean(beta) - .30), 4 / sqrt(120 * length(beta)))
```

- [x] **Step 3: Render and test the example.** Render with `rmarkdown::render()` to a temporary output directory and run the focused test file. If Pandoc is unavailable, use `knitr::knit()` to a temporary Markdown file so all chunks still execute, and record the HTML-render limitation. Keep all generated output outside `vignettes/`.

- [x] **Step 4: Commit the example and fixtures.** Commit the vignette and helper as `docs: demonstrate study simulation callbacks`.

### Task 6: Run the G4 bootstrap calibration study

**Files:**
- Create: `tools/validation/g15-inference-studies.R`
- Create: `tests/internal/validation_results/g15-inference-coverage.csv`
- Create: `docs/validation-g15.md`
- Modify: `.gitignore` to track only the small aggregate coverage CSV under the otherwise ignored validation-results directory
- Modify: `docs/gaps.md` only with the actual results from this task

**Interfaces:**
- Consumes: `study_spec()`, `simulate_study()`, `summarize_study()`, `bootstrap_model()`, and `contrast(selection = "fixed" or "repeat")`.
- Produces: independent low/high-reliability and shape-selection evidence with coverage, bias, failure rates, denominators, and Monte Carlo uncertainty.

- [x] **Step 1: Define the DGP, estimands, and validation thresholds independently of the estimator.** Use a Gaussian latent predictor/outcome with known path coefficient, observed low/high-reliability indicator schedules, and a separate linear-versus-curved response condition. Before running simulations, record the target coefficient, tolerable absolute bias, nominal coverage and allowed coverage deviation, and maximum acceptable unconditional failure rate for each estimand and scope. Source thresholds from the applicable G4 acceptance criteria where specified; otherwise justify and label the project-specific threshold in the report. Define analytic target functions in `truth(scenario, n)` from the data-generating equations; do not obtain targets from `associate()`, `bootstrap_model()`, or `contrast()`.

- [x] **Step 2: Add the calibration analysis callback.** Fit the declared measurement/structural pipeline, return the explicitly supported locked-score or measurement-refit bootstrap intervals, and use `contrast(..., selection = "repeat")` for the selected-shape condition. Include fixed-selection results as a labelled comparator. Map each output to the estimand defined in Step 1 (including whether it is a score-level or latent-path target); do not compare unlike quantities. Each callback reports failed fits and unavailable intervals rather than filtering them.

- [ ] **Step 3: Run a screening tier and inspect failures.** Execute 100 outer study replications per scenario with 50 inner bootstrap draws. Save the raw replication table to the ignored validation-results directory and check that truth, estimate basis, selection mode, and interval statuses are interpretable before running confirmation. Do not commit raw replication-level records.

- [ ] **Step 4: Run confirmation and compute Monte Carlo uncertainty.** Execute 500 outer study replications per scenario with 199 inner bootstrap draws after the screening design and thresholds are frozen. Calculate coverage with conditional and unconditional denominators, bias with MCSE, failure rates with Wilson intervals, and report the exact seed, sample sizes, bootstrap mode, pipeline components, and threshold decisions. Any missed coverage/bias/failure target stays in the report; do not relabel it as passed.

- [ ] **Step 5: Record G4 evidence and commit it.** Write the scenario conditions, estimands, target formulas, results, MC uncertainty, and limitations into `docs/validation-g15.md`; retain the machine-readable results in `tests/internal/validation_results/`. Update the G4 note in `docs/gaps.md` only to describe observed evidence. Commit the script, report, artifact, and doc change as `test: report G4 bootstrap coverage simulations`.

### Task 7: Evaluate and enforce the G6 coverage gate

**Files:**
- Modify: `tools/validation/g15-inference-studies.R`
- Modify: `tests/testthat/test-outer-validation.R`
- Modify: `docs/validation-g15.md`
- Modify: `docs/gaps.md`

**Interfaces:**
- Consumes: `validate_outer()` and the G15 callback interval contract.
- Produces: a G6 gate record that either documents a justified, independently calibrated interval method for a declared outer metric or explicitly records that coverage is unavailable and the planner remains gated.

- [ ] **Step 1: Assert the current G6 result contract.** Extend the first test in `tests/testthat/test-outer-validation.R` so `validate_outer()$test_metrics` exposes point `rmse`, `mae`, and `r_squared` but no confidence bounds. Do not add interval columns to `validate_outer()` as part of this task:

```r
expect_true(all(c("rmse", "mae", "r_squared") %in% names(result$test_metrics)))
expect_length(intersect(c("lower", "upper", "conf_low", "conf_high"),
  names(result$test_metrics)), 0L)
```

- [ ] **Step 2: Audit candidate uncertainty construction against the approved G6 estimand.** A candidate method must state whether it is conditional on the training fit or includes training/selection variability, use an independent scenario/sample-size-specific truth, and avoid treating overlapping outer folds as independent. A method that cannot meet all three checks is rejected and recorded as unavailable coverage. Current `validate_outer()` returns point RMSE/MAE/R-squared only; do not infer an interval method from its result shape.

- [ ] **Step 3: Run the G6 gate only for a justified method.** If a method passes Step 2, execute its predeclared independent coverage study through `study_spec()` and store the full denominator and MC interval. Require the predeclared G6 accuracy, coverage, and failure-rate thresholds to pass with the stated uncertainty; merely obtaining simulation output does not close the gate. If no method passes Step 2, record `coverage = unavailable`, cite the point-metric-only `validate_outer()` contract, leave G6 Partial, and keep the G15 planner unexported. Do not replace this step with an ordinary t interval over dependent folds.

- [ ] **Step 4: Commit the G6 gate result.** Commit the test, validation report, and exact G6 documentation change as `test: record G6 outer metric coverage gate`.

### Task 8: Add the sample-size planner only after the gate passes

**Execution precondition:** Do not execute this task unless Task 7 records that the G4/G6 gate passed or the user explicitly approves a documented scope revision.

**Files:**
- Modify: `R/study-summary.R`
- Modify: `tests/testthat/test-study-planning.R`
- Modify: `NAMESPACE`
- Create: `man/plan_sample_size.Rd`

**Interfaces:**
- Consumes: `cssem_simulation`, summary metric rows, and a criteria table with `metric`, `estimand`, `scope`, `target`, `tolerance`, and `null_value`.
- Produces: `plan_sample_size(simulation, criteria, level = .95)` with per-criterion decisions and the smallest tested `n` at which every criterion passes.

- [ ] **Step 1: Write boundary tests for all four criterion types.** Use fixtures where precision upper bounds respectively fall below, above, and across the target; rate intervals respectively fall above, below, and across minimum targets; and coverage intervals are inside, outside, or overlap the nominal band. Assert statuses `pass`, `fail`, `inconclusive`, and `unavailable`.

- [ ] **Step 2: Test grid-only selection.** Supply unsorted sample sizes and verify the planner chooses the smallest qualifying supplied value, returns no recommendation if none pass, and never interpolates or extrapolates.

- [ ] **Step 3: Run planner tests to verify RED.** This task is conditional. Only after the G4/G6 gate passes, first add the tests in Steps 1–2 and confirm they fail because `plan_sample_size()` is not defined; if the gate does not pass, skip the entire task and keep the symbol absent from `NAMESPACE`.

- [ ] **Step 4: Implement criteria validation and decision rules.** Enforce metric-specific `scope`, target ranges, `NA`-only unused fields, unique criterion keys, and estimand/null correspondence. Require all conditional/unconditional detection criteria for an estimand to use the same null value passed into `summarize_study()`. Use the summary's Monte Carlo confidence bounds: precision passes if its upper bound is at/below target and fails if its lower bound is above target; detection/convergence pass if lower bounds are at/above targets and fail if upper bounds are below targets; coverage passes if its full interval is inside nominal ± tolerance and fails if it does not overlap that band. Mark remaining boundary overlap inconclusive and absent metrics unavailable.

- [ ] **Step 5: Run GREEN and commit the planner.** Run the focused test file and commit `R/study-summary.R`, tests, `NAMESPACE`, and `man/plan_sample_size.Rd` as `feat: plan sample size from simulation evidence`.

### Task 9: Synchronize G15 documentation and run release verification

**Files:**
- Modify: `docs/gaps.md`
- Modify: `NEWS.md`
- Modify: `vignettes/study-planning.Rmd`
- Modify: generated `man/` files only through roxygen2

**Interfaces:**
- Consumes: shipped functions, committed validation artifacts, and the Task 7 gate result.
- Produces: a G15 entry with exact public scope, status, validation evidence, known limits, and tracking commit IDs.

- [ ] **Step 1: Update G15 from delivered evidence only.** If Task 7 passes and Task 8 ships, mark G15 implemented only when all acceptance criteria and required design cases pass. If Task 7 does not pass, mark only the delivered callback engine/summaries as partial, state that `plan_sample_size()` remains gated, and link the G6 interval limitation. Match the repository's convention of a checked heading for a delivered core workflow while keeping the matrix status `Partial` until methodological acceptance criteria pass. Add each meaningful implementation commit ID.

- [ ] **Step 2: Update NEWS and the vignette.** Describe the shipped APIs, how the truth callback works across `n`, how to interpret conditional/unconditional rates, and whether the public planner is available. Do not describe a gated function as implemented.

- [ ] **Step 3: Run focused and package verification.** Run `test-study-planning.R`, `test-validation.R`, `test-bootstrap.R`, `test-contrasts.R`, `test-splits.R`, and `test-outer-validation.R`; render the vignette; run the package check with `local_r_lib` in `R_LIBS_USER`:

```powershell
$env:R_LIBS_USER = (Resolve-Path .\local_r_lib).Path
& 'C:\Program Files\R\R-4.6.0\bin\R.exe' CMD check --no-manual .
```

Review `git diff --check` and verify `R CMD build` excludes internal calibration artifacts (already matched by `^tests/internal$` in `.Rbuildignore`).

- [ ] **Step 4: Commit the documentation and final verification record.** Commit `docs/gaps.md`, `NEWS.md`, and vignette changes as `docs: record G15 simulation workflow`.

## Execution Stop Condition

If Task 7 cannot justify and calibrate coverage for G6 outer metrics, stop before Task 8, keep the public planner gated, and report G15 as partial with the exact G6 limitation. The next work item is a separate G6 interval-method design; it is not authorized by this plan to invent inferential intervals from dependent folds.
