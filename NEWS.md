# cssem (development version)

## Bug fixes (2026-09-20 audit)

* **A1 — Nonlinear mediation baselines:** mediation contrasts now use the same
  active edge set for the shifted and zero-shift predictions. This removes the
  observed-versus-fitted mediator substitution that could create divergent or
  spurious nonlinear effects as the intervention step shrank.

* **A2 — Causal mediation temporal order:** causal mediation now validates the
  supplied order against every included path and adjustment variable, rejects
  contradictory orders, and checks that confounders precede treatment and
  mediators lie between treatment and outcome before assigning a causal label.

* **A3 — Mediator-subset adjustment checks:** causal mediation now applies
  adjustment admissibility checks to every declared treatment-to-outcome path,
  even when a caller requests only a subset of mediators for reporting.

* **A4 — Flexible causal identification:** DML and average-marginal-effect
  estimands now diagnose treatment variation with their cross-fitted nonlinear
  residuals. Near-deterministic or weak residual variation is unavailable or
  unstable and cannot receive a causal-under-assumptions label.

* **A5 — Evidence verdict direction:** specification-gap verdicts now interpret
  theory-minus-shadow gaps with the correct sign, so large negative gaps weaken
  a robustness verdict instead of being treated as strong evidence.

* **A6 — Ordinal score validation:** fitting and scoring share validation that
  rejects fractional or non-finite ordinal values before integer conversion;
  fractional values can no longer be silently truncated into other categories.

* **A7 — Numeric factor conversion:** continuous and manifest inputs now parse
  factor labels and numeric strings by their measured values rather than factor
  level positions, with malformed and non-finite values rejected consistently
  during fitting and scoring.

* **A8 — Interaction corrections:** product interaction edges are now eligible
  for structural errors-in-variables correction and bootstrap reporting, with
  constituent reliability products shown consistently across structural and
  moderation reports.

* **A9 — Causal-effect reporting:** `print.causal_effect()` now distinguishes
  an undeclared temporal order from a declared but weakly identified effect,
  reporting the actual reason for a non-causal label.

* **A10 — Validation independence:** mediation validation now uses independent
  analytic path-product and interaction targets, plus a separately implemented
  intervention integration target for smooth mediation. The former propagation
  results remain available as explicitly named sample oracles, and validation
  output reports errors against both bases.

## Historical fixes newly recorded

* Interaction edges are now labelled as product terms throughout structural
  ledgers and effect cards, and fitted-curve grids no longer emit spurious
  warnings for interaction names.

* Measurement fitting, association, structural-fold construction, and
  mediation/moderation bootstraps now preserve the caller's random-number
  stream after using deterministic internal seeds.

* Model metadata now uses the package version, and stale comments no longer
  claim that the eigenvalue limiter guarantees corrected estimates are never
  worse than naive estimates.

* The manuscript's ridge-penalty description now matches the log-scale penalty
  used by ordinal estimation.

* Validation item generation now preserves sparse threshold schedules when a
  skew shift is also requested.

* Comparator score frames now reject row-count mismatches unless an explicit
  case index is supplied, preventing scores from being assigned to the wrong
  respondents when an engine drops cases.

* Simulation shard seeds are now derived from unsharded job indices, so
  changing the shard count no longer changes the scenario-to-seed pairing.

## Breaking changes

* Ordinal indicators supplied as **text** are now rejected. They were coded
  with `factor(x, ordered = TRUE)`, whose level order is alphabetical, so an
  ordinary frequency scale became `"always" < "never" < "often" < "rarely" <
  "sometimes"`: the response scale was scrambled and nothing warned. On a
  four-item construct at N = 300 with loadings of .80, the locked score's
  correlation with the generating latent was **-0.265**, against **0.905** for
  the identical data as integer codes. Supply such an item as an ordered factor
  whose levels are in scale order, or as integer category codes; columns of
  numeric strings (`"1"`, `"2"`) are unambiguous and are still accepted. A
  factor's own level order is now honoured rather than re-sorted, so an ordered
  factor and the equivalent integer codes give identical scores.

## Breaking changes (reporting)

* `route()` no longer reports a declared causal edge as `status = "causal"`
  when the estimate is not identified. The status was assigned before the
  effect was estimated, so an edge whose `causal_effect()` label came back
  `adjusted_association` -- identification strength below .10, meaning almost
  no treatment variation survives adjustment -- still read as a causal pathway
  in the routing table and in `evidence_report()`, beside an interpretation
  saying "weak identification". Such an edge now carries the new status
  `"causal_weak"`, which keeps its declaration, estimand, adjustment set,
  estimate, and robustness value while reporting that it is not an established
  causal pathway. Code matching on `status == "causal"` will now exclude these
  edges, which is the intent.

* `evidence_report()` types a `causal_effect` claim from its adjustment set
  rather than always calling it `"direct"`. A claim whose adjustment set
  excludes the declared mediators of its treatment-outcome pair leaves every
  mediating path open, so it is now typed `"total (adjusted)"`;
  `"direct (adjusted)"` is used when a declared mediator is adjusted. Note that
  with a declared temporal order the post-treatment guard refuses mediator
  adjustment outright, so a causal-labelled `"adjusted_linear"` claim is always
  a total-effect contrast. `causal_effect()` objects carry `claim_type` and
  `adjusted_mediators`.

## Bug fixes (reporting and propagation)

* `indirect_effect()` and `causal_indirect_effect()` report a proportion
  mediated computed from the effects they actually report. It was always the
  naive indirect over the naive total, printed directly beneath disattenuated
  effects under a heading reading "Effects are disattenuated": in the package's
  own worked example it printed 0.466 while the effects above it, 0.235 and
  0.424, give 0.554. The proportion is now withheld when the total and the
  indirect are reported on different bases, which happens when only one of them
  is correction-eligible; the objects carry `proportion_mediated_basis` and
  retain `proportion_mediated_naive`, and the printed value names its basis
  when that is not naive.

* `conditional_slopes()` and `conditional_indirect_effect()` evaluate moderator
  levels on the moderator's own scale. Levels are documented as
  standard-deviation units but were used as raw values, which is correct only
  because modeled constructs are standardized. A `manifest()` covariate kept in
  natural units was not: with age in years (SD about 12), the
  "-1 SD / mean / +1 SD" rows were evaluated at ages -1, 0, and 1 -- three
  years of a forty-year range -- and labelled as spanning two standard
  deviations. Levels are now converted to mean plus level times SD, the
  reported `moderator_value` is on that scale, and the index of moderated
  mediation divides by the span in the same units so it is per unit of the
  moderator either way. Standardized constructs are unaffected.

* `print()` on a `conditional_indirect_effect` no longer fails when the index
  of moderated mediation could not be computed. The narrative read
  `if (estimate > 0)`, which errors on `NA`, and described an index of exactly
  zero as "weakens". The three cases -- unavailable, not distinguishable from
  zero, and signed -- are now reported separately.

* `evidence_report()` lists a causal claim once when the same effect arrives
  both through `routing` and through `causal=`; previously it appeared twice
  and read as two independent pieces of evidence. The explicitly passed object
  is kept, since it may carry a bootstrap interval the routed one lacks.

* `print()` on a `causal_indirect_effect` names the actual reason a claim is
  not causal. The line was hard-coded to "no declared temporal order", but the
  same label is assigned when the order was declared and identification
  strength fell below .10; the strength is now reported in that case.

## Bug fixes (measurement)

* The stored ordinal category schema is now the category values themselves -- a
  factor's labels in their declared order, or the numeric codes -- rather than
  the positions of the categories observed in the frame at hand, and
  `score_states()` maps new records against those values. Previously the
  positions were re-derived from whatever frame was passed, so for factor
  indicators new records built independently of the estimation sample (a
  different set of observed categories, hence a different level set) were mapped
  to the wrong categories: scoring records that happened to omit one category
  shifted their scores by up to **0.51 SD**, silently. Numeric codes were
  unaffected. An unseen category still raises an error rather than being
  remapped.

## New features

* Added `manifest()`, a single-item, non-construct covariate declaration for
  [specify_measurement()]. Its locked score is the (by default standardized)
  observed column itself, computed out-of-fold from training-fold statistics
  only, exactly like every other construct's cross-fitted score. Its
  reliability is asserted, not estimated: `1` by default, or an externally
  supplied value (e.g. a published test-retest reliability for a
  deliberately single-item measure) that `associate()`'s errors-in-variables
  correction consumes exactly as a measured construct's estimated
  reliability. This closes a real gap: previously nothing could enter the
  structural model without being a fitted multi-item construct, so a raw
  control (age, a binary group indicator) had no path in at all.

* Continuous-only and mixed ordinal/continuous constructs now share the same
  marginal-ML/EAP measurement model as ordinal-only constructs, instead of
  falling back to the weaker, posterior-free `"alternating_mixed_scale"`
  estimator (removed). Ordinal items contribute a graded-response category
  log-probability and continuous items a Gaussian log-density to the same
  quadrature-grid posterior; ordinal items are still updated by BFGS on the
  posterior-weighted graded-response likelihood, continuous items by
  closed-form posterior-weighted least squares. This means continuous and
  mixed constructs now get a real posterior, a real marginal EAP
  reliability, and therefore real errors-in-variables disattenuation in
  `associate()` -- previously these constructs always reported `NA`
  reliability and were silently excluded from correction. An all-ordinal
  construct runs the identical sequence of operations as before this change
  (verified via the full existing regression suite); this is a strict
  superset of the prior ordinal-only estimator, not a rewrite of it.

* `ordinal()`-declared indicators now reject non-integer category codes
  (e.g. an averaged sub-scale accidentally declared ordinal) with a clear
  error, instead of `as.integer()` silently truncating them and discarding
  information.

* Added reusable split control and whole-pipeline outer validation. `make_splits()`
  supports deterministic random, grouped, and forward-only time partitions with
  integer row-ID provenance; `fit_states(split = ...)` accepts those assignments
  explicitly; and `validate_outer()` refits measurement and structural selection
  inside each outer training sample. Its result separates `internal_selection`
  metrics from untouched `outer_test` predictions and retains failed partitions
  with their stage and message. Predictor-only prospective scoring remains the
  documented G7 limitation.

## Bug fixes

* `indirect_effect()` and `causal_indirect_effect()` no longer fail with
  "subscript out of bounds" on full-mediation structures (no declared direct
  `x -> y` edge) when disattenuating. The undeclared direct effect is zero by
  construction and no longer blocks the disattenuated total.

* `associate(reliability = ...)` now changes the errors-in-variables
  correction. Previously, whenever the fit carried posterior variances (every
  `fit_states()` fit does), reliability was re-estimated from them and the
  supplied values were silently ignored, while `effect_ledger()` still
  reported them as `predictor_reliability`. Supplied values now override the
  fit's reliability for the named constructs only; unnamed constructs keep
  the fit's value (previously a supplied vector replaced all of them). Values
  must be named by locked constructs and lie in `(0, 1]`.

* `score_states()` now returns new-record scores on the `locked_scores`
  scale. It previously returned raw posterior means (SD about
  `sqrt(reliability)` rather than 1), so structural coefficients estimated on
  locked scores understated effects for new records by roughly
  `1 - sqrt(reliability)`. `fit_states()` now stores the standardization it
  applies (`score_center`, `score_scale`); fits created before this change
  keep the raw scale with a warning to refit.

## Measurement convergence

* `fit_states()` now defaults to `iterations = 15` (an EM cap of 30) instead
  of 6 (a cap of 12). The old default stopped before convergence on ordinary
  data: all four constructs of a 600-respondent, four-item example, and 59 of
  175 fits in the continuous-integration simulation. EM still stops as soon
  as it converges, so the extra budget costs little (55 s to 59 s on that
  example, now converged in every fold).

* `fit_states()` now warns, with a condition of class
  `cssem_nonconvergence`, when any construct's full-data or fold encoder
  reaches the EM cap; previously non-convergence was visible only in
  `fit$measurement_engine`. `measurement_engine` also reports
  `folds_converged` out of `folds`. The simulation harnesses, which record
  convergence per job, muffle the warning. The `"exploratory"` preset's
  deliberately light budget now warns too.

## Changes to shape selection

* `associate()` now decides whether an edge is nonlinear with a curvature test
  that has a stated error rate, and uses cross-validation only to choose which
  shape to report. The test is a heteroskedasticity-robust (HC3) Wald test of
  the spline terms against the linear fit, Bonferroni-combined over
  `spline_df` and Holm-adjusted across an outcome's shape-searched predictors,
  controlled by the new `shape_alpha` argument (default `.05`). Its p-value is
  reported in `candidate_metrics` and `effect_ledger()` as `nonlinearity_p`. A
  flagged edge must still improve on the linear baseline out of fold to be
  reported as curved. Robust standard errors are used because the regressors
  are cross-fitted posterior means whose precision varies by respondent, which
  a classical F-test's constant-variance assumption does not cover.

  The previous rule required a tuned repeated-CV margin (`smooth_uncertainty`,
  `shape_stability_min`) to be cleared both within repeats and overall. A
  cross-validated loss improvement has no calibrated null distribution, so any
  threshold on it is tuned rather than justified, and the settings that held
  the false-curve rate near zero did so by discarding real curvature. Those two
  arguments are retained, but now govern only which shape is reported: whether
  a monotone candidate is preferred over a spline that predicts equally well.

  In a 60-replication benchmark (N = 300, four ordinal items, three folds,
  Quality ~ Trust), detection of modest monotone kinks rose from 55% to 83%
  (increasing) and 85% to 98% (decreasing), and from 47% to 78% for diminishing
  returns, while false nonlinear selection stayed at or below 5% for
  unrelated, linear, and skewed-indicator data. On the true latent scores the
  same test detects these shapes 100% of the time, so the remaining gap is
  measurement error, not the selection rule.

* A curved shape must now remove at least `shape_min_gain` (default `.005`) of
  the linear model's out-of-fold error before it is reported in place of the
  straight line. The curvature test answers whether curvature is real, not
  whether it is worth reporting, and those differ at large samples: strongly
  skewed indicators leave the scores mildly curved even when the relation
  between the constructs is linear, and a confirmation benchmark flagged that
  curvature in 20% of replications at N = 1000 despite it removing about 0.5%
  of the prediction error. The floor cut that to 3% while leaving detection of
  real shapes essentially unchanged (for example, diminishing returns at low
  reliability, 70% to 60%). Curvature under strongly skewed indicators should
  still be interpreted cautiously; it can be a property of the scores rather
  than of the constructs.

* A monotone shape is now reported whenever it predicts indistinguishably from
  the best candidate, instead of only when the best improvement fell under an
  absolute .05 cross-validated threshold. That cap had no justification and
  sent the strongest monotone effects to the spline label precisely because
  they were strong: in benchmark, thresholds were labelled monotone in 65% of
  replications and saturating curves in 32%, despite both being monotone by
  construction. Detection of curvature is unaffected; only the reported label
  changes.

* Monotone shape candidates now face the same acceptance rule as spline
  candidates in `associate()`. Previously a monotone candidate counted as
  supported in a cross-validation repeat whenever its mean improvement was
  merely positive (which happens in about half of all repeats with no signal),
  and a monotone winner could be accepted on the strength of a different
  candidate's significance. Every candidate must now improve on the linear
  baseline by more than `smooth_uncertainty` standard errors, both within
  repeats (selection frequency) and overall. In a 40-replication benchmark
  (N = 300, four ordinal items, three folds) this cut false nonlinear
  selection from 29% to 3% when the predictor is unrelated to the outcome,
  and from 7% to 0% when the relation is linear. The price is lower detection
  of modest monotone kinks (88% to 47% for the harness's monotone-increasing
  scenario, 95% to 83% for monotone-decreasing); strong, smooth, threshold,
  and plateau shapes are detected at unchanged rates.

* Monotone shapes can now be concave as well as convex. The increasing basis
  was the linear term plus `(x - knot)_+` hinges with nonnegative
  coefficients, so its slope could only increase: `monotone_increasing()`
  could not represent diminishing returns or saturation, the most common
  monotone nonlinearities. The basis now pairs a convex hinge
  `(x - knot)_+` with a concave hinge `min(x - knot, 0)` at each knot; both
  are nondecreasing, so the sign constraint still guarantees a monotone fit
  while spanning convex, concave, S-shaped, and linear curves. In the same
  benchmark, the monotone family went from never being selected to being
  selected in 55% of replications for a concave-increasing relation and 38%
  for a saturating one, overall nonlinear detection there rose from 52% to
  68% and from 88% to 97%, and false nonlinear selection fell from 3% to 0%.

# cssem 0.5.0 (2026-08-15)

Frozen as a known reference point before work begins on manifest
(single-item, non-construct) covariates and continuous/mixed-scale
measurement support.

## New features

* Added `specify_measurement()` and `specify_structure()`, friendlier front
  doors for `cssem_model()`/`cssem_structure()` that resolve to identical
  internal specifications.
  * `specify_measurement()` declares constructs with `ordinal()`/`continuous()`
    helpers, bundling indicators, scale, and default keys instead of spelling
    out `list(indicators = , scales = )` by hand.
  * `specify_structure()` declares each outcome with a formula
    (`Outcome ~ predictor1 + predictor2`), using standard `A:B` colon syntax
    for interaction predictors. Wrap a predictor in `linear()`,
    `auto_monotone()`, `monotone_increasing()`, `monotone_decreasing()`, or
    `smooth()` to declare a non-default edge shape policy; these are formula
    markers only (parsed via `stats::terms()` specials, never evaluated as
    functions), so they never shadow `base`/`stats` names.
  * `cssem_model()`, `cssem_structure()`, and `cssem_effect()` are now
    soft-deprecated (`.Deprecated()`, one warning per call) in favor of
    `specify_measurement()`/`specify_structure()`. They remain exported and
    fully functional; both front doors resolve to identical internal
    specifications and are interchangeable.

* Renamed the remaining 36 exported `cssem_*()` functions to bare names
  (e.g. `cssem_fit()` -> `fit_states()`, `cssem_associate()` -> `associate()`,
  `cssem_mediation()` -> `indirect_effect()`), so the public API reads as its
  own vocabulary rather than a forked branch of `lavaan`/`seminr`. The full
  old-name -> new-name mapping, including the collision analysis behind each
  choice, is recorded in `docs/naming-convention-v0.5.csv`. Notable renames:
  * `cssem_mediation()`/`cssem_mediation_ledger()`/`cssem_moderated_mediation()`
    moved off "mediation" vocabulary entirely (to `indirect_effect()`,
    `indirect_effect_ledger()`, `conditional_indirect_effect()`) to avoid
    reading as a fork of the `mediation` package's own `mediate()`/
    `mediations()`.
  * `cssem_simple_slopes()` -> `conditional_slopes()`, avoiding an exact name
    collision with `reghelper::simple_slopes()`.
  * `cssem_structure()`/`cssem_effect()` -> `specify_structure()` avoids
    `base::structure()`.
  * Every old `cssem_*()` name remains exported as a soft-deprecated
    `.Deprecated()` alias that forwards to its replacement unchanged
    (see `R/deprecated.R`); no existing code breaks.
  * S3 classes and their `print()`/`plot()` methods were renamed consistently
    alongside their constructor (e.g. class `"cssem_fit"` -> `"fit_states"`,
    `print.cssem_fit()` -> `print.fit_states()`) so dispatch stays coherent
    with the new names. Classes whose constructor name changed but which
    already used a different class string (e.g. `cssem_associate()`'s
    `"cssem_association"`, `cssem_route()`'s `"cssem_routing"`) were left
    unchanged.

## Bug fixes and hardening

* `cssem_causal_effect()` (and therefore `cssem_route()`) now refuses an
  adjustment set containing constructs at or after the treatment in the
  declared `temporal_order`. Previously a post-treatment adjustment (e.g.
  conditioning on a mediator) passed silently and could earn the
  "causal under assumptions" label; it now errors with a pointer to
  `cssem_causal_mediation()` for mediator analysis.

* Smooth-shape candidate bases no longer abort the fit when heavily tied scores
  (e.g. composites of few ordinal items) collapse the spline's quantile-based
  interior knots onto a boundary knot. `.train_basis()` retries with the
  deduplicated interior quantiles and degrades to the linear column when no
  interior knot survives, so the smooth candidate competes as linear instead of
  erroring.

## Validation harness

* The `ordinal_factor_proxy` comparator engine sign-aligns its first
  principal component with the block's row-mean composite. `prcomp()`'s
  rotation sign is arbitrary, so the unaligned proxy randomly flipped the sign
  of downstream structural slopes across replications, inflating its recorded
  bias to roughly the magnitude of the true slope; aligned, its structural
  bias matches the composite proxy's.
* `cssem_run_structural_comparator_validation()` gains a `lavaan_sam` native
  comparator: Croon-corrected factor-score regression via lavaan's
  structural-after-measurement estimator (`sam(..., sam.method = "local")`,
  items treated as continuous with FIML because lavaan's local SAM does not yet
  support standard errors for categorical indicators). It is scored on the same
  truth-referenced bias and coverage metrics as the CB-SEM and PLS-SEM native
  comparators.

# cssem 0.4.0

## Bug fixes and hardening

* Fixes mediation propagation zeroing every effect when the treatment `x` is
  itself an endogenous construct: the forward pass overwrote `x` with its own
  stage-model prediction, erasing the injected shift before it could propagate.
  `cssem_mediation()`, `cssem_causal_mediation()`, and
  `cssem_moderated_mediation()` now recover the correct total, direct, and
  indirect effects for an endogenous treatment.
* `cssem_route()` now rejects `predictive`/`representational` pairs that are not
  declared structural edges instead of silently dropping them.
* `cssem_construct_card()` matches item-level warnings by exact indicator name,
  so item `a1` no longer also captures warnings for `a10`.

## New features

* Adds `cssem_evidence_report()`: the unified, causal-aware evidence artifact that
  composes the construct, effect, and causal layers into one profile -- a
  construct section (recovery, distinctiveness, warnings), an effect-surface
  section (shape, disattenuated estimate, predictive contribution, stability,
  shadow gap), and a causal-claims section (declared direct and
  interventional-mediation effects with identification and robustness). Each edge
  now carries its routed causal status instead of a hardcoded associational label,
  and every row gets a plain-language verdict derived from transparent rules over
  the raw signals -- an evidentiary profile, not a significance test. This is the
  primary artifact intended to replace the coefficient-table-plus-fit-indices
  ritual.
* Adds `cssem_causal_mediation()`: an interventional (causal) mediation estimand
  that elevates the disattenuated mediation decomposition to interventional
  total, direct, and indirect effects under a declared adjustment set. It
  enforces causal discipline the associational `cssem_mediation()` does not --
  requiring a non-empty adjustment set, requiring those confounders to be
  declared predictors of the outcome and every mediator, refusing to adjust for
  any construct downstream of the treatment, and reporting a causal-admissibility
  panel (identification strength, stage-model fit, a mediator-outcome robustness
  value, and minimum path reliability). A `causal_under_assumptions` label also
  requires a declared temporal order. For additive mediator models the g-computed
  indirect effect equals the interventional indirect effect; on a
  measurement-error benchmark the disattenuated interventional indirect recovers
  the truth (bias 0.009) that a composite-score product attenuates by ~44%.
  Exposure-induced mediator-outcome confounding and mediator interactions are not
  yet modeled and are flagged.
* Adds `cssem_causal_effect()`: a declared, adjusted causal effect on locked
  construct states, disattenuated for measurement error and adjusted for linear
  confounding. It reports the unadjusted, adjusted-attenuated, and
  adjusted-disattenuated estimates, an identification diagnostic, a
  Cinelli-Hazlett robustness value, and a reliability-sensitivity curve, and it
  requires both an adjustment set and a declared temporal order before applying
  a causal-under-assumptions label -- discipline and sensitivity that CB-SEM and
  PLS-SEM do not provide.
* Adds `estimand = "adjusted_ame"` to `cssem_causal_effect()` (and
  `cssem_causal_edge()`): the cross-fitted doubly-robust average marginal effect
  (average derivative `E[d/dx E[Y | X = x, C]]`) with an analytic
  influence-function interval. Unlike the partially-linear `"adjusted_dml"`, it
  does not assume a constant slope, so it is the correct summary when the
  treatment effect is nonlinear in the treatment; on a nonlinear dose-response
  with a skewed treatment it recovers the true average marginal effect (bias
  0.008) where the partially-linear estimand answers a different, curvature-
  weighted question (bias 0.244). Estimated on the denoised construct states and
  not disattenuated.
* Adds an optional `estimand = "adjusted_dml"` to `cssem_causal_effect()` (and
  `cssem_causal_edge()`): a cross-fitted partially-linear double-machine-learning
  estimate with flexible spline nuisances and an analytic orthogonal-score
  interval. It removes *nonlinear* confounding that a linear structural
  adjustment leaves behind -- a gain over CB-SEM's linear-only adjustment -- and
  is estimated on the denoised construct states. Prototyping found that
  disattenuating or otherwise measurement-error-correcting the DML estimate
  over-corrects in the nonlinear setting (treatment attenuation and residual
  confounding are opposite-signed) and that on denoised states the DML estimate
  is not better than the same estimator on composite scores; the estimand is
  therefore offered as flexible confounder adjustment, without claiming a
  measurement-error advantage over dedicated causal-ML.
* Adds edge routing via `cssem_route()` and `cssem_causal_edge()`: assigns every
  declared structural edge a status (associational by default, or predictive,
  representational, or causal) and prints a Path Routing Table stating each
  edge's allowed interpretation. A causal status requires an adjustment set and a
  declared temporal order, so no path is read causally by default.
* Adds moderation and moderated mediation. Declared `"A:B"` interaction terms
  enter the effect surface as product moderation effects; `cssem_simple_slopes()`
  reports conditional slopes with Johnson-Neyman regions; and
  `cssem_moderated_mediation()` reports conditional indirect effects and the index
  of moderated mediation with plain-language output. Interaction terms are
  disattenuated using the product of the constituents' reliabilities, which
  recovers the index of moderated mediation that measurement error attenuates.
  Adds a moderated-mediation validation harness.
* Adds associational mediation via `cssem_mediation()`: a simulation-based path
  decomposition (total, direct, indirect, and per-path effects) that handles
  single, parallel, and serial mediation uniformly, disattenuates linear and
  monotone edges, and reports bootstrap intervals. Adds a validation harness and
  a benchmark against native CB-SEM (`lavaan`) and PLS-SEM (`seminr`) mediation.
  On the benchmark grid the disattenuated indirect effect matches CB-SEM on bias
  and coverage and is far less attenuated than PLS-SEM.
* Propagates measurement uncertainty instead of discarding it: `cssem_fit()` now
  reports per-construct marginal reliability (`reliability`), per-respondent
  posterior SD (`score_posterior_sd`), and real posterior plausible-value draws
  (replacing the prior fixed-variance placeholder bag).
* Adds an errors-in-variables structural correction to `cssem_associate()`. For
  linear and monotone edges, structural slopes are disattenuated using the
  posterior reliability and reported with a bootstrap interval
  (`eiv_bootstrap`), recovering the true effect that composite and PLS pipelines
  attenuate. Smooth edges are reported but not yet corrected.
* Extends the structural comparator with truth-referenced metrics (structural
  coefficient bias, interval coverage, shape recovery) and adds native CB-SEM
  (`lavaan` latent SEM) and PLS-SEM (`seminr` path) structural comparators, so
  the disattenuation and shape-recovery advantages are visible and fairly scored
  against each incumbent's own structural estimates.
* Adds realistic social/behavioral validation scenarios: saturating `plateau`,
  `threshold`, and concave `diminishing` structural effects, plus
  `low_reliability`, `careless` (heteroskedastic responder noise), and `skewed`
  (floor-effect) measurement stress. Existing release gates are unchanged; new
  scenarios appear in the structural and comparator artifacts.
* Adds `cssem_respondent_information()` and a Construct Card reliability summary
  reporting per-respondent posterior SD and information weights, surfacing
  careless responding that CB-SEM and PLS-SEM cannot flag. An experimental,
  default-off inverse-variance `respondent_weighting` option is available in
  `cssem_associate()`; validation showed it does not improve point-estimate bias
  (posterior width is score-dependent), so it is not used for confirmatory
  estimates.
* Constrains the monotone shape basis to be genuinely monotone (the linear term
  was previously left free, letting a symmetric U-shape masquerade as a monotone
  effect). Strong symmetric nonlinear effects are now correctly selected as
  smooth, and false-nonlinear selection on linear and null data is lower.
* Propagates reliability-estimation uncertainty into the errors-in-variables
  bootstrap interval by re-estimating reliability on each resample, restoring
  interval coverage under low measurement reliability.

# cssem 0.3.0

* Positions the package as a v0.3.0 research release of cross-fitted
  manifestation measurement plus associational structural modeling.
* Adds cross-fitted locked-score structural selection with temporal and
  unrestricted shadow-model specification gaps.
* Adds supported-envelope reporting and deterministic measurement/structural
  validation suites for the associational release.
* Adds optional `lavaan`/`seminr` comparator artifacts that separate latent
  recovery from held-out downstream association-preservation benchmarks, with
  success-rate reporting for handoff-ready summaries.
* Adds structural comparator artifacts that hold the associational selector
  fixed while swapping score engines, so shape-selection and shadow-gap
  benchmarks can show where CS-SEM specifically stands out.
* Adds a coverage-adjusted structural comparator summary so high structural
  fit can be interpreted alongside partial score coverage in external
  comparator handoffs.
* Keeps the release scope explicitly non-causal: no causal claims, formative
  constructs, or global SEM fit indices are supported.

# cssem 0.2.0

* Adds deterministic measurement and associational-structure validation suites.
* Adds temporal and unrestricted shadow-gap release gates and a supported
  operating-envelope report.

# cssem 0.1.1

* Initial research release of the CS-SEM measurement foundation.
* Adds cross-fitted construct states for theory-declared manifestation blocks.
* Adds marginal graded-response estimation for ordinal indicators, EAP scoring,
  measurement diagnostics, and deterministic validation simulations.

# cssem 0.1.0

* Project initialization.
