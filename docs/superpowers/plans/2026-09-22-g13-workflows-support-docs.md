# G13 runnable workflows and support documentation implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give users a tested end-to-end package workflow and accurate documentation of CS-SEM's current capabilities, evidence, assumptions, limitations, and migration path.

**Architecture:** Add one executable R Markdown vignette using deterministic simulated records and current public APIs. Keep `knitr` and `rmarkdown` in `Suggests`; summarize existing implementation and validation evidence in a capabilities table; align the README, method contract, support-envelope interpretation, and deprecated API migration notes without changing estimators.

**Execution order:** Implement the mixed-scale declaration and provenance/data-retention plans first; this documentation plan consumes both new APIs.

**Tech Stack:** R Markdown, knitr, rmarkdown, base R simulation, existing CS-SEM exports and validation artifacts.

**Spec:** `docs/superpowers/specs/2026-09-22-g13-workflows-provenance-support-design.md`

## Global Constraints

- Every executable vignette chunk is evaluated during rendering.
- `knitr` and `rmarkdown` are `Suggests` and are not `Imports`.
- The vignette uses bounded synthetic data and does not require network access or long validation simulations.
- The capabilities table must not upgrade a status without evidence in tests or validation artifacts.
- Documentation distinguishes associational prediction from causal estimands and reports unsupported conditions beside examples.

## Review Focus

1. A vignette rendered from a clean package build resolves current exports rather than undocumented development-only objects.
2. Missing-data and held-out examples label sample counts and metric scopes correctly.
3. The causal example names its identifying assumptions and does not imply code verified no unmeasured confounding.
4. Experimental uncertainty and respondent weighting are not presented as validated inference.
5. Version, selector, transformation, support-envelope, and deprecated-wrapper statements match current source and artifacts.

---

### Task 1: Add and render the end-to-end workflow vignette

**Files:**
- Create: `vignettes/cssem-workflow.Rmd`
- Modify: `DESCRIPTION`

**Interfaces:**
- Consumes: exported measurement, fit, missing-data, structural, evidence, prediction, outer-validation, provenance, and optional causal APIs.
- Produces: an HTML package vignette whose evaluated code can be built from the source package when `knitr` and `rmarkdown` are installed.

- [x] **Step 1: Create the vignette scaffold** with `\VignetteIndexEntry{A CS-SEM analysis workflow}`, `\VignetteEngine{knitr::rmarkdown}`, setup chunk, and a deterministic data generator using a fixed seed and modest sample size.
- [x] **Step 2: Add `knitr` and `rmarkdown` under `Suggests` and set `VignetteBuilder: knitr`** without adding either package to `Imports`.
- [x] **Step 3: Add and evaluate the following deterministic workflow** using only package exports. Follow the causal call with prose stating that declared adjustment and temporal order do not verify no unmeasured confounding.

```r
item5 <- function(x) pmin(pmax(as.integer(round(x) + 3L), 1L), 5L)
make_survey_data <- function(n, seed, add_missing = FALSE) {
  set.seed(seed)
  trust_state <- stats::rnorm(n)
  age <- stats::rnorm(n, mean = 40, sd = 8)
  loyalty_state <- .45 * trust_state + .02 * age + stats::rnorm(n)
  data <- data.frame(
    trust_1 = item5(trust_state + stats::rnorm(n, sd = .6)),
    trust_2 = item5(trust_state + stats::rnorm(n, sd = .7)),
    trust_time = trust_state + stats::rnorm(n, sd = .5),
    loyalty_1 = item5(loyalty_state + stats::rnorm(n, sd = .6)),
    loyalty_2 = item5(loyalty_state + stats::rnorm(n, sd = .7)),
    age = age
  )
  if (add_missing) data$trust_2[seq(5L, n, by = 19L)] <- NA_integer_
  data
}
survey_data <- make_survey_data(120L, 2026L, add_missing = TRUE)
new_data <- make_survey_data(30L, 2027L)
model <- specify_measurement(
  Trust = mixed_items(ordinal("trust_1", "trust_2"), continuous("trust_time")),
  Loyalty = ordinal("loyalty_1", "loyalty_2"),
  Age = manifest("age"), folds = 3
)
fit <- fit_states(model, survey_data, seed = 23, iterations = 6,
  diagnostics = FALSE, missing_policy = "partial", retain_data = FALSE)
sample_accounting(fit)
cssem_provenance(fit)

structure <- specify_structure(
  Loyalty ~ linear(Trust) + linear(Age),
  order = c("Age", "Trust", "Loyalty")
)
association <- associate(fit, structure, structural_repeats = 1,
  shadow_scope = "temporal", seed = 24)
effect_ledger(association)
cssem_provenance(association)
contrast <- marginal_contrast(association, "Loyalty", "Trust", values = c(-1, 1),
  reps = 40, seed = 27)
contrast
cssem_provenance(contrast)
predict(association, new_data, outcomes = "Loyalty")

splits <- make_splits(survey_data, method = "random", folds = 3, seed = 25)
outer <- validate_outer(model, structure, survey_data, splits, seed = 26,
  iterations = 6, diagnostics = FALSE,
  structural_args = list(structural_repeats = 1, shadow_scope = "temporal"))
outer

causal_effect(association, treatment = "Trust", outcome = "Loyalty",
  adjust = "Age", estimand = "adjusted_linear", disattenuate = FALSE,
  temporal_order = c("Age", "Trust", "Loyalty"))
```

All executable chunks stay evaluated; use `warning=FALSE` only on the fitting chunk if the deterministic low-iteration example emits its expected convergence warning, and retain explanatory text that tells readers how to increase `iterations`.
- [x] **Step 4: Render the vignette** and confirm every executable chunk succeeds.

Run: `Rscript -e "pkgload::load_all('.'); rmarkdown::render('vignettes/cssem-workflow.Rmd', output_file = 'cssem-workflow.html', output_dir = tempdir(), quiet = TRUE)"`
Expected: render exits successfully into a temporary directory without leaving generated HTML in `vignettes/`.

- [x] **Step 5: Commit the runnable workflow.**

Ruling: Pandoc was not on `PATH`, but the RStudio-bundled Pandoc 3.4 was available. The render command selected that directory with `rmarkdown::find_pandoc()`; no package download was needed. The first render revealed a capitalization mismatch between the YAML title and vignette index title, which was corrected and rerendered successfully.

```bash
git add DESCRIPTION vignettes/cssem-workflow.Rmd
git commit -m "docs: add runnable CS-SEM workflow vignette"
```

### Task 2: Return checked-in release evidence from `supported_envelope()`

**Files:**
- Create: `inst/extdata/supported_envelope.csv` (the package-installed copy of the checked-in release artifact)
- Modify: `R/validation.R`, `man/supported_envelope.Rd`
- Test: `tests/testthat/test-validation.R`

**Interfaces:**
- Consumes: `tests/internal/validation_results/supported_envelope.csv` generated from confirmation results.
- Produces: `supported_envelope()` with the existing envelope columns plus the confirmation job counts, inside-envelope convergence, release-gate status, and exploratory-condition list from the installed evidence file.

- [x] **Step 1: Extend the existing test** to assert that `supported_envelope()` includes `confirmation_measurement_jobs`, `confirmation_structural_jobs`, `confirmation_jobs_inside_envelope`, `inside_envelope_convergence`, `release_gates_passed`, and `exploratory_conditions`, and that its values match the checked-in release evidence.
- [x] **Step 2: Run `test-validation.R` red** and confirm the current hand-maintained helper lacks the evidence columns.
- [x] **Step 3: Copy the exact checked-in evidence row into package `inst/extdata/`** and change `supported_envelope()` to read that installed CSV. Update its roxygen description to identify the reported release evidence and state that it does not guarantee performance in a new study; update the generated Rd help to match.
- [x] **Step 4: Run `test-validation.R` green** and confirm the helper preserves the existing threshold columns while returning the artifact metrics. The focused test passed all six assertions.
- [x] **Step 5: Commit the artifact-backed support helper.**

Ruling: the new test was first run red because the helper returned six hand-maintained fields instead of the artifact's twelve. The installed resource and public helper now match the checked-in release row exactly; the report explicitly says it is not a guarantee for a new study.

```bash
git add R/validation.R tests/testthat/test-validation.R man/supported_envelope.Rd inst/extdata/supported_envelope.csv
git commit -m "feat: expose release evidence in supported envelope"
```

Ruling: the existing function returned the old thresholds but omitted the generated job, convergence, and gate metrics. The installed helper will read the same artifact row used by the documentation so the release report cannot drift independently.

### Task 3: Publish the capabilities and evidence table

**Files:**
- Create: `docs/capabilities.md`

**Interfaces:**
- Consumes: implemented public exports, current tests, `docs/validation-v02.md`, `docs/validation.md`, and checked-in `supported_envelope` artifacts.
- Produces: a table with columns `Capability`, `Status`, `Evidence`, and `Conditions and limits`; statuses are exactly `implemented`, `experimental`, or `validated`.

- [x] **Step 1: Create the table from evidence already present.** Include measurement families and missingness, cross-fitting/splits, structural selector and outer prediction, uncertainty and inference, groups/contrasts, and causal/mediation. Use only the statuses `implemented`, `experimental`, or `validated`; state excluded model classes and causal limits in the conditions column. Link every `validated` row to a named checked-in validation artifact and every other row to its public API or test documentation.
- [x] **Step 2: Check the table's required boundaries** by reading it alongside `supported_envelope()` documentation and the validation files. Confirm the envelope is described as scenario evidence, not a universal guarantee; latent uncertainty and respondent weighting retain their experimental status; causal assumptions are not described as tested facts; covariance-fit indices remain outside the estimator.
- [x] **Step 3: Commit the evidence table.**

```bash
git add docs/capabilities.md
git commit -m "docs: publish CS-SEM capability evidence table"
```

### Task 4: Align user-facing method, migration, and release documentation

**Files:**
- Modify: `README.md`
- Modify: `docs/method-spec.md`
- Modify: `docs/associational-structure.md`
- Create: `docs/migration.md`
- Modify: only generated help pages whose current example or replacement link is stale

**Interfaces:**
- Consumes: public API signatures, G13 vignette, capabilities table, deprecated wrappers, and support-envelope definitions.
- Produces: matching release/version statements; a scale/standardization explanation; selector and estimand descriptions; correction and support-envelope limits; and a deprecated-wrapper-to-current-function migration table.

- [x] **Step 1: Inspect current names and selectors** in `NAMESPACE`, `R/model.R`, `R/structure.R`, `R/fit.R`, and `R/validation.R`; use `DESCRIPTION` as the release-version source and read `R/deprecated.R` for every public migration pair.
- [x] **Step 2: Update README and method specification** against those APIs. Set the release label to the DESCRIPTION version, explain scale standardization/transforms as implemented, describe selector choices and estimands, and remove historical instructions presented as the current workflow.
- [x] **Step 3: Create `docs/migration.md`** with each public deprecated function and its preferred replacement; preserve the wrappers themselves. Link the README to this page and to `docs/capabilities.md`; state that the supported envelope describes evidence for named scenarios/metrics, not an individual-study guarantee.
- [x] **Step 4: Parse affected Rd files with `tools::checkRd()`, render the vignette again, compare capability statuses with source/tests/artifacts, search for stale version strings using `rg -n 'v0\.[0-9]' README.md docs`, and run `git diff --check`.**
- [x] **Step 5: Commit the aligned method and migration documentation.**

```bash
git add README.md docs/method-spec.md docs/associational-structure.md docs/migration.md man
git commit -m "docs: align method limits and migration guidance"
```

Ruling: the migration inventory contains all 39 functions that call
`.Deprecated()` across the current source. All 120 Rd files parse and pass
`tools::checkRd()`, and the workflow vignette rendered successfully. The
version scan shows v0.5.0 in current user documentation; older version labels
remain in explicitly historical validation and bug-history documents.

## Final verification and tracking

After all three G13 plans are implemented:

1. Run `Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary')"` and report any pre-existing failures by name.
2. Render `vignettes/cssem-workflow.Rmd` and run `R CMD build .` with suggested vignette packages available.
3. Parse Rd files, run `git diff --check`, and perform `R CMD check` where the local environment permits it.
4. Update the G13 row and section in `docs/gaps.md` with scoped status, acceptance evidence, and every implementation/documentation commit ID; commit that tracking update separately.

Ruling: the vignette rendered and `R CMD build --no-manual` completed. All 120
Rd files passed `tools::parse_Rd()` and `tools::checkRd()`, and
`git diff --check` passed. The complete test suite ran with the package loaded
and reported six failures outside this documentation-only step: causal
mediation's expected path count (7 instead of 2), an unsupported `info`
argument to testthat `expect_lt()`, a mediation truth comparison that differs
in names/attributes, a numerical-diagnostics class/row-name comparison, and
two summary-extractor availability/basis assertions. One comparator test was
skipped because optional `seminr` is unavailable. `R CMD check` installed the
package successfully but stopped at DESCRIPTION metadata checking after R
startup reported unavailable `C.UTF-8` locales; it also noted the existing
hidden `.superpowers` directory and unavailable optional `lavaan`, `seminr`,
and `roxygen2` packages. No estimator or test code was changed in this docs
step.
