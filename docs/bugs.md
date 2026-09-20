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

**Audit coverage.** Measurement core (`encoder.R`, `fit.R`), structural layer
(`structure.R`), propagation (`mediation.R`, `moderation.R`), causal layer
(`causal.R`, `causal-mediation.R`, `routing.R`), reporting
(`evidence-report.R`, `model.R`). Simulation harness (`validation.R`,
`comparators.R`, `simulate.R`, `*-validation.R`, `*-comparator.R`) and
`deprecated.R`: **in progress**.

---

## Severe

### [ ] S1. Character-coded ordinal items are ordered alphabetically

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

**Tests:** a character Likert frame errors with the new message; the same data
as an ordered factor with a scrambled alphabetical order recovers the latent at
the integer-coded correlation (within Monte Carlo noise).

### [ ] S2. `score_states()` re-derives category codes from the new data

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

**Fix:** map by the stored labels from S1 — `raw <- as.character(x)` for
factors, the value itself for numeric codes — and keep the existing
"unseen ordinal category" error for values absent from the stored levels.
S1 and S2 share one fix and should land in one commit.

**Tests:** scoring any subset of the training frame reproduces those rows'
`locked_scores`-scale scores exactly, for numeric, factor, and ordered-factor
indicators; an unseen category still errors.

---

## Moderate

### [ ] M1. `prop. mediated` is computed from naive effects under a disattenuated heading

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

**Tests:** with disattenuation on, the printed proportion equals the printed
indirect divided by the printed total; with `disattenuate = FALSE` it equals the
naive ratio.

### [ ] M2. `route()` marks an edge causal even when identification failed

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

**Tests:** an edge whose adjusters explain nearly all treatment variance is
routed with the weak status and is not reported as causal by
`evidence_report()`; a well-identified edge is unchanged.

### [ ] M3. `conditional_slopes()` mislabels moderator levels for an unstandardized moderator

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

**Tests:** with an unstandardized manifest moderator, the evaluated values are
mean ± SD in natural units, and the labels report those values; with
standardized constructs the results are identical to current behavior.

### [ ] M4. `print.conditional_indirect_effect()` crashes on a missing index and misreads a zero index

**Where:** [R/moderation.R:318](../R/moderation.R#L318).

**What happens:** `direction <- if (x$index$estimate > 0) "strengthens" else
"weakens"` errors with "missing value where TRUE/FALSE needed" when the index
is `NA` (possible when the moderator levels degenerate or a path is
uncorrectable), and reports "weakens" when the index is exactly 0.

**Fix:** handle the three cases explicitly — non-finite (report that the index
could not be computed), exactly zero or not distinguishable from zero (report
no detectable variation), otherwise the direction.

**Tests:** printing an object with a zero index and with an `NA` index both
succeed and say the right thing.

### [ ] M5. `evidence_report()` lists a claim twice

**Where:** `.evidence_causal_claims()`,
[R/evidence-report.R:90](../R/evidence-report.R#L90).

**What happens:** claims from `routing$causal_effects` and from `causal=` are
concatenated without de-duplication, so an effect passed through both appears
twice in the causal-claims section.

**Fix:** de-duplicate by claim key (treatment → outcome plus estimand),
preferring the explicitly passed `causal=` object, which may carry intervals
the routed one lacks.

**Tests:** passing the same edge through both arguments yields one row, and it
is the one carrying the interval.

### [ ] M6. `adjusted_linear` claims are typed `"direct"`

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

**Tests:** an effect adjusting only pre-treatment covariates is typed as a
total-effect contrast; one adjusting a declared mediator is typed direct.

### [ ] M7. `print.causal_indirect_effect()` gives the wrong reason for a non-causal label

**Where:** [R/causal-mediation.R:188](../R/causal-mediation.R#L188).

**What happens:** the label text for `adjusted_association` is hard-coded to
"not causal: no declared temporal order", but the label is also assigned when
the order *was* declared and identification strength fell below .10.

**Fix:** choose the explanatory clause from the actual cause, which is already
on the object (`temporal_order_declared`, `identification_strength`).

**Tests:** both routes to the label print their own reason.

---

## Minor

### [ ] N1. Interaction edges are labelled `"linear"` in the ledger

`effect_ledger()` reports `shape = "linear"` for an `X:W` edge while
`effect_card()` reports `"product"` for the same edge (confirmed in
`audit2.R`, probe 6). Report `"product"` in both; the baseline shape vector in
`associate()` already uses that label, so the ledger is reading the candidate
row rather than the model's shape.

### [ ] N2. Spurious warning from `.effect_rows()`

[R/structure.R:519](../R/structure.R#L519): the fitted-curve grid builds a mean
row for every name in `model$shapes`, including interaction names, which are
not columns of `scores`, producing "argument is not numeric or logical:
returning NA". The NA column is unused, since
`.predict_shape_model()` rebuilds the product from its constituents, so
estimates are unaffected. Build the grid over constituent constructs only.
Triggers only when a model has both an interaction and a selected nonlinear
edge.

### [ ] N3. RNG state is clobbered

`fit_states()`, `associate()`, `.eiv_bootstrap()`, `.structural_fold_sets()`,
and the mediation/moderation bootstraps all call `set.seed()` and never
restore the caller's stream, so a user's random numbers change after calling
them (confirmed in `audit2.R`, probe 5; determinism itself is fine). Save
`.Random.seed` on entry and restore it on exit in the exported functions.

### [ ] N4. Stale version string

`cssem_model()` sets `version = "0.1"` and an error message refers to a
"v0.1 construct" ([R/model.R:35](../R/model.R#L35)). Use the package version,
and drop the version from the error text.

### [ ] N5. Stale comment contradicting the documented behavior

[R/structure.R:513](../R/structure.R#L513) still says the eigenvalue limiter
makes the corrected estimate "never worse than naive". The claim was withdrawn
in the paper; the comment should describe the limiter as numerical
stabilization only. A test name at
`tests/testthat/test-mediation.R:101` carries the same stale phrasing.

### [ ] N6. Paper misstates the ridge penalty

Paper §3.2 documents a ridge penalty of `0.02 * a_j^2` on the discrimination;
`.ordinal_em_nll()` penalizes `0.02 * log(a_j)^2`
([R/encoder.R:41](../R/encoder.R#L41)). Fix the paper, not the code — the
penalty on the log scale is the sensible one.

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
