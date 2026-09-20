# Package audit: confirmed bugs and fix steps

Audit of every exported and internal function in `R/`, conducted 2026-09-20 at
commit `290740b`. Each entry states what is wrong, the evidence that confirmed
it (a probe that was actually run, not a reading), the fix, and how the fix
should be tested. Severity is about consequence for a user's results:

- **Severe** — silently wrong numbers.
- **Moderate** — reports that misdescribe correct numbers, or an avoidable
  crash.
- **Minor** — cosmetic, internal, or documentation-only.

Status legend: `[ ]` open, `[x]` fixed (commit noted).

**Progress.** The two severe, seven moderate, and six minor entries are all
fixed, each with a regression test confirmed to fail on the pre-fix code where
a test applies. The suite has grown from 331 to 388 expectations. Remaining:
the five harness entries (H1-H5), of which H3 and H4 are documentation duties
for the regenerated simulation studies rather than code changes.

**Follow-up owed to the paper.** Section 6 of the JSS manuscript must be
regenerated once more: `prop. mediated` now reports 0.554 on a disattenuated
basis rather than 0.466 (M1), causal claims are typed `"total (adjusted)"`
rather than `"direct"` (M6), and §6.7's prose caveat about the `direct` label
can be dropped. The ridge-penalty description in §3.2 has already been
corrected (N6).

**Audit coverage.** Complete, all 18 files in `R/`: measurement core
(`encoder.R`, `fit.R`), structural layer (`structure.R`), propagation
(`mediation.R`, `moderation.R`), causal layer (`causal.R`,
`causal-mediation.R`, `routing.R`), reporting (`evidence-report.R`,
`model.R`), simulation harness (`validation.R`, `comparators.R`,
`simulate.R`, `mediation-validation.R`, `mediation-comparator.R`,
`moderation-validation.R`, `moderation-comparator.R`) and `deprecated.R`.

Harness entries are prefixed `H`. They cannot produce a wrong answer for a
user, since the harness is only used to run simulations, but two of them could
corrupt a study's numbers and therefore the paper's tables.

---

## Severe

### [x] S1. Character-coded ordinal items are ordered alphabetically — fixed in `018a468`

**Where:** `.prepare_item()`, [R/encoder.R:22](../R/encoder.R#L22).

**What happens:** for a character indicator, categories are derived with
`as.integer(factor(x, ordered = TRUE))`, whose level order is alphabetical.
Responses `"never"/"rarely"/"sometimes"/"often"/"always"` are coded in the
order `always < never < often < rarely < sometimes`, which scrambles the
response scale. Nothing warns.

**Evidence:** four-item construct, N = 300, loading .8. Locked score against
the generating latent: **r = −0.265** with character codes, **r = 0.905** with
the identical data integer-coded (scratchpad `audit1.R`, probe 1).

**Fix:**

1. In `.prepare_item()`, branch on storage type:
   - **factor** (ordered or not): use `levels(x)` — the user's declared order —
     and keep the labels as the stored `levels`.
   - **character**: accept only if every non-missing value parses as a number
     (then treat as numeric codes); otherwise `stop()` with a message telling
     the user to supply an ordered factor or integer codes, because label
     order cannot be inferred.
   - **numeric/integer**: unchanged (whole-number check, sorted unique codes).
2. Store the *labels* in `levels`, not re-derived integer codes, so the stored
   schema is self-describing.

**Tests:** added to `tests/testthat/test-fit.R` ("text category labels are
rejected, and an ordered factor keeps its declared order"): a text Likert frame
errors; numeric strings still fit; an ordered factor whose alphabetical order
differs from its scale order gives scores identical to the integer-coded fit.
Fails on the pre-fix code.

### [x] S2. `score_states()` re-derives category codes from the new data — fixed in `d1483db`

**Where:** `.prepare_for_encoder()`, [R/encoder.R:29](../R/encoder.R#L29).

**What happens:** the function recomputes `as.integer(factor(x, ordered =
TRUE))` on whatever frame it is given, then matches against stored integer
levels. For factor or character items the mapping therefore depends on which
categories happen to appear in the scoring frame, so scoring a subset silently
shifts scores. (Numeric codes are unaffected: `as.integer(x)` preserves them.)

**Evidence:** scoring 20 rows selected to omit one category returned scores
differing from the same respondents' scores in the full frame by up to
**0.51 SD**; the integer-coded control differed by exactly 0 (scratchpad
`audit1.R`, probes 2 and 3).

**Fix (applied):** the stored schema is now the category values themselves — a
factor's labels in its declared order, or the numeric codes — and both
`.prepare_item()` and `.prepare_for_encoder()` match against those values. The
existing "unseen ordinal category" error is unchanged.

**Tests:** added to `tests/testthat/test-fit.R` ("scoring maps categories by
label, not by position in the scoring frame"): records whose factors are
rebuilt from the rows in hand score identically to those rows in the training
frame, the stored schema is the labels, and an unseen category still errors.
Three of its expectations fail on the pre-fix code.

---

## Moderate

### [x] M1. `prop. mediated` is computed from naive effects under a disattenuated heading — fixed in `c205bf6`

**Where:** `.assemble_mediation()`, [R/mediation.R:196](../R/mediation.R#L196);
printed by `print.cssem_mediation()` and
`print.causal_indirect_effect()`.

**What happens:** `proportion_mediated` is always
`naive$indirect_total / naive$total`, while the printed effects above it are
the disattenuated ones whenever disattenuation applies, under the heading
"Effects are disattenuated (errors-in-variables)".

**Evidence:** the paper's §6 example prints total 0.424, indirect 0.235 (both
disattenuated) and `prop. mediated 0.466`; 0.235 / 0.424 = **0.554**. The
printed ratio does not correspond to the printed effects.

**Fix:** compute the proportion from the same basis as the reported effects.
Return both (`proportion_mediated`, `proportion_mediated_naive`) so the
disattenuated figure is used when the summary reports disattenuated effects and
the naive one otherwise; guard on a near-zero total as now. Print the basis
alongside the value.

**Tests:** added to `tests/testthat/test-mediation.R` ("the reported proportion
mediated divides the effects that were reported"): the proportion equals the
reported indirect over the reported total, its basis is `"disattenuated"`, it
differs from the retained naive ratio, the printout names the basis, and
`disattenuate = FALSE` returns the naive ratio. Five of its expectations fail
on the pre-fix code.

### [x] M2. `route()` marks an edge causal even when identification failed — fixed in `1f82a71`

**Where:** [R/routing.R:96](../R/routing.R#L96).

**What happens:** `status[[key]] <- "causal"` is assigned before the effect is
estimated, so a row whose `causal_effect()` label came back
`adjusted_association` (identification strength < .10) is still reported with
`status = "causal"`. The same row's `interpretation` reads "Adjusted
association (weak identification)", and `evidence_report()` reads `status`, so
the effect surfaces as a causal pathway.

**Evidence:** code path is unconditional; `causal_effect()` computes its label
independently at [R/causal.R:215](../R/causal.R#L215).

**Fix:** set the routed status from the estimated label — `"causal"` only for
`causal_under_assumptions`, otherwise a distinct status (e.g.
`"causal_declared_weak"`) that carries the declaration but does not read as an
established causal pathway. Keep the estimand and adjustment set on the row,
and make `evidence_report()` map the new status to a clearly non-causal
verdict.

**Tests:** added to `tests/testthat/test-causal.R` ("a weakly identified
declared causal edge is not routed as causal"): an adjuster explaining ~99% of
the treatment yields identification strength below .10, status `"causal_weak"`,
retained estimand/adjustment set/estimate, and no causal-pathway verdict; a
companion unit test on `.edge_verdict()` covers the wording directly, since a
weakly identified edge usually also trips the strength rule first. Three
expectations fail on the pre-fix code.

### [x] M3. `conditional_slopes()` mislabels moderator levels for an unstandardized moderator — fixed in `dc649f5`

**Where:** `.level_labels()` and the `levels` default,
[R/moderation.R:8](../R/moderation.R#L8) and `conditional_slopes()`.

**What happens:** levels are treated as standard-deviation units, which is
correct only because locked scores are standardized. A
`manifest(..., standardize = FALSE)` construct keeps natural units, so the
"−1 SD / mean / +1 SD" rows are evaluated at the raw values −1, 0, 1.

**Evidence:** `manifest("age", standardize = FALSE)` with SD 11.4 years
produced moderator values `-1 0 1` labelled as ∓1 SD (scratchpad `audit3.R`,
probe 9).

**Fix:** convert requested levels to the moderator's own scale —
`mean(w) + level * sd(w)` — and label with the resulting value; for a
standardized construct this is unchanged. Apply the same conversion in
`conditional_indirect_effect()`, which sets `at_level[[moderator]] <- level`
directly.

**Tests:** added to `tests/testthat/test-moderation.R` ("moderator levels are
evaluated on the moderator's own scale" and "standardized moderators are
unaffected by the scale conversion"): with age in years the rows sit at mean
± SD and the slope varies materially across them; with standardized constructs
the values stay exactly -1, 0, 1. Three expectations fail on the pre-fix code.

### [x] M4. `print.conditional_indirect_effect()` crashes on a missing index and misreads a zero index — fixed in `393f35a`

**Where:** [R/moderation.R:318](../R/moderation.R#L318).

**What happens:** `direction <- if (x$index$estimate > 0) "strengthens" else
"weakens"` errors with "missing value where TRUE/FALSE needed" when the index
is `NA` (possible when the moderator levels degenerate or a path is
uncorrectable), and reports "weakens" when the index is exactly 0.

**Fix:** handle the three cases explicitly — non-finite (report that the index
could not be computed), exactly zero or not distinguishable from zero (report
no detectable variation), otherwise the direction.

**Tests:** added to `tests/testthat/test-moderation.R` ("printing a moderated
mediation handles a zero or missing index"): zero reports no detectable
variation, `NA` reports that the index could not be computed and prints "not
available", and signed indices still report a direction. Two expectations fail
on the pre-fix code (one of them an error, not a wrong string).

### [x] M5. `evidence_report()` lists a claim twice — fixed in `0683b5e`

**Where:** `.evidence_causal_claims()`,
[R/evidence-report.R:90](../R/evidence-report.R#L90).

**What happens:** claims from `routing$causal_effects` and from `causal=` are
concatenated without de-duplication, so an effect passed through both appears
twice in the causal-claims section.

**Fix:** de-duplicate by claim key (treatment → outcome plus estimand),
preferring the explicitly passed `causal=` object, which may carry intervals
the routed one lacks.

**Tests:** added to `tests/testthat/test-causal.R` ("a claim arriving through
both routing and causal is listed once"): one row survives, and it is the
explicitly passed object, proved by the routed-only report having no interval
while the combined one does.

### [x] M6. `adjusted_linear` claims are typed `"direct"` — fixed in `bfba3f9`

**Where:** `.causal_claim_row()`,
[R/evidence-report.R:80](../R/evidence-report.R#L80).

**What happens:** every `causal_effect` row is typed `"direct"`. With a
pre-treatment adjustment set that excludes mediators, the estimand is a
total-effect-type contrast, so "direct" is wrong — as the paper's §6.7
discussion has to explain.

**Fix:** derive the type from the estimand and the adjustment set: when no
declared mediator of the treatment–outcome pair is in `adjust`, type it
`"total (adjusted)"`; when mediators are adjusted, `"direct (adjusted)"`.
Requires passing the association's declared structure, or recording the
mediator status on the `causal_effect` object at construction.

**Tests:** added to `tests/testthat/test-causal.R` ("a causal claim is typed
from its adjustment set, not named direct by default"), and the stale
expectation in `test-evidence-report.R` was updated from `"direct"` to
`"total (adjusted)"`.

**Note found while fixing:** with a declared temporal order the post-treatment
guard already refuses mediator adjustment outright, so every causal-labelled
`adjusted_linear` claim is necessarily a total-effect contrast. The
`"direct (adjusted)"` branch is reachable only without a declared order, where
the claim is not causal anyway. This is the point §6.7 of the paper argues in
prose, and the paper can now cite the reported type instead.

### [x] M7. `print.causal_indirect_effect()` gives the wrong reason for a non-causal label — fixed in `2411631`

**Where:** [R/causal-mediation.R:188](../R/causal-mediation.R#L188).

**What happens:** the label text for `adjusted_association` is hard-coded to
"not causal: no declared temporal order", but the label is also assigned when
the order *was* declared and identification strength fell below .10.

**Fix:** choose the explanatory clause from the actual cause, which is already
on the object (`temporal_order_declared`, `identification_strength`).

**Tests:** added to `tests/testthat/test-causal.R` ("an interventional
mediation names the actual reason it is not causal"): the undeclared-order case
still names the order, the weakly identified case names the strength and does
not mention the order, and a causal-labelled object is unchanged.

---

## Minor

### [x] N1. Interaction edges are labelled `"linear"` in the ledger — fixed in `854b7f5`

`effect_ledger()` reported `shape = "linear"` for an `X:W` edge while
`effect_card()` reported `"product"` for the same edge (confirmed in
`audit2.R`, probe 6). The candidate row now takes the baseline shape, which is
`"product"` for interaction predictors. Tested together with N2 in
`test-structure.R` ("an interaction edge is labelled a product everywhere, and
curves are built quietly"); two expectations fail on the pre-fix code.

### [x] N2. Spurious warning from `.effect_rows()` — fixed in `854b7f5`

[R/structure.R:519](../R/structure.R#L519): the fitted-curve grid builds a mean
row for every name in `model$shapes`, including interaction names, which are
not columns of `scores`, producing "argument is not numeric or logical:
returning NA". The NA column is unused, since
`.predict_shape_model()` rebuilds the product from its constituents, so
estimates are unaffected. Build the grid over constituent constructs only.
Triggers only when a model has both an interaction and a selected nonlinear
edge.

### [x] N3. RNG state is clobbered — fixed in `db04d08`

`fit_states()`, `associate()`, `.eiv_bootstrap()`, `.structural_fold_sets()`,
and the mediation/moderation bootstraps all call `set.seed()` and never
restore the caller's stream, so a user's random numbers change after calling
them (confirmed in `audit2.R`, probe 5; determinism itself is fine). Save
`.Random.seed` on entry and restore it on exit in the exported functions.

### [x] N4. Stale version string — fixed in `77fcab1`

`cssem_model()` sets `version = "0.1"` and an error message refers to a
"v0.1 construct" ([R/model.R:35](../R/model.R#L35)). Use the package version,
and drop the version from the error text.

### [x] N5. Stale comment contradicting the documented behavior — fixed in `77fcab1`

[R/structure.R:513](../R/structure.R#L513) still says the eigenvalue limiter
makes the corrected estimate "never worse than naive". The claim was withdrawn
in the paper; the comment should describe the limiter as numerical
stabilization only. A test name at
`tests/testthat/test-mediation.R:101` carries the same stale phrasing.

### [x] N6. Paper misstates the ridge penalty — fixed in the manuscript (untracked)

Paper §3.2 documents a ridge penalty of `0.02 * a_j^2` on the discrimination;
`.ordinal_em_nll()` penalizes `0.02 * log(a_j)^2`
([R/encoder.R:41](../R/encoder.R#L41)). Fix the paper, not the code — the
penalty on the log scale is the sensible one.

---

## Harness (simulation and comparator code)

### [ ] H1. `.validation_items()` discards sparse thresholds when skew is set

**Where:** [R/validation.R:6](../R/validation.R#L6).

**What happens:** the sparse cutpoints are assigned first, then the skew branch
overwrites `thresholds` with the *non-sparse* base plus the skew shift, so a
scenario asking for both gets a skewed non-sparse item.

**Evidence:** share in category 1 over 4,000 draws: sparse only **0.017**,
sparse + skew **0.573**, non-sparse + skew **0.575** — the sparse+skew item is
indistinguishable from the non-sparse one (scratchpad `audit5.R`).

**Impact:** latent. No shipped manifest combines `sparse = TRUE` with a
non-zero `skew`, so no published result is affected.

**Fix:** apply the skew shift to whichever threshold vector was selected
(`thresholds <- thresholds + skew`) instead of rebuilding the base vector.

**Test:** with `sparse = TRUE, skew = 1.2`, the category-1 share stays far
below the non-sparse+skew share.

### [ ] H2. `.fill_score_frame()` silently misaligns scores when an engine drops cases

**Where:** [R/comparators.R:280](../R/comparators.R#L280).

**What happens:** with `case_idx = NULL` and an engine returning fewer rows
than the data, the scores are written to the *first* `nrow(scores)` rows, so
every score is attributed to the wrong respondent and compared against the
wrong latent truth.

**Evidence:** three scores for a six-row dataset land in rows 1,2,3 without
`case_idx` and in rows 4,5,6 with it (scratchpad `audit4.R`, probe H2).

**Impact:** latent. Both current callers are safe — the lavaan branch passes
`lavInspect(fit, "case.idx")`, and the seminr branch mean-imputes first, so it
always returns `n` rows. A future engine that drops rows would corrupt results
silently.

**Fix:** when `case_idx` is `NULL`, require `nrow(scores) == n` and stop
otherwise, rather than filling from the top.

**Test:** the mismatched-length call errors.

### [ ] H3. Comparators see three different missing-data treatments

**Where:** [R/comparators.R:441](../R/comparators.R#L441) (PLS: mean
imputation), [R/comparators.R:497](../R/comparators.R#L497) (lavaan WLSMV: its
own deletion), [R/comparators.R:568](../R/comparators.R#L568) (SAM: continuous
items with FIML).

**What happens:** each engine is run the way its own users would run it, which
is defensible, but it means an engine comparison at non-zero missingness
confounds the estimator with its missing-data handling. Only the SAM choice is
documented in the paper.

**Fix:** none in code. Document all three in the paper's comparator
configuration paragraph (§7.1), as the SAM row already is.

### [ ] H4. `.headline_estimate()` mixes estimands across replications

**Where:** [R/comparators.R:640](../R/comparators.R#L640).

**What happens:** the headline **cssem** estimate is the corrected slope when
the errors-in-variables correction applies and the *naive* slope otherwise
(that is, whenever a smooth shape was selected). A deviation statistic pooled
over replications therefore averages corrected and uncorrected estimates, in
proportions that depend on how often a curve was selected — which the new
selection rule changes.

**Fix:** none in code (the fallback is the honest "what this pipeline would
report"), but the regenerated Studies 2 and 3 must report the share of
replications contributing a corrected versus a naive estimate, and the paper
should state that the pooled deviation is over a mixture.

### [ ] H5. CI shard seeds depend on the shard count

**Where:** [inst/scripts/run-sim-shard.R:45](../inst/scripts/run-sim-shard.R#L45).

**What happens:** scenarios are strided across shards and each shard offsets
its seed by `(shard - 1) * 10000`, so changing `NSHARDS` changes which seed is
paired with which scenario. Results reproduce only at a fixed shard count.

**Fix:** derive each job's seed from its index in the *unsharded* job list
before striding, as `inst/scripts/run-shape-bench-shard.R` already does.

**Test:** the same scenario-replication draws an identical seed at two
different shard counts.

---

## Verified correct (no action)

Checked by probe, not by reading:

- Reverse-keyed items reproduce manually reversed data exactly.
- `X:W` and `W:X` declarations give identical interaction estimates.
- 5% MCAR missingness: all locked scores and corrected estimates finite.
- `respondent_weighting = "information"` runs and returns finite estimates.
- `associate()` is deterministic across repeated calls at one seed.
- Binary indicators: reliability in (0, 1], score-truth correlation 0.85.
- A constant indicator errors cleanly rather than producing a degenerate fit.
- Deprecated `cssem_model()` front door still fits.
- `validate_structure()` is reproducible at a fixed seed.
- `validate_structure_comparator()` is reproducible at a fixed seed (all
  columns identical apart from runtimes), and all eight engines report
  `success` on the screening scenario.
- All 38 deprecated wrappers forward to functions that still exist.
- Mediation and moderated-mediation harness targets are computed with the same
  propagation engine on the error-free states, so the estimate and its target
  are defined consistently (they are sample oracles, as the paper states).
