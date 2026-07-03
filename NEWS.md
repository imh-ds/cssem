# cssem 0.4.0 (in development)

* Adds `cssem_causal_effect()`: a declared, adjusted causal effect on locked
  construct states, disattenuated for measurement error and adjusted for linear
  confounding. It reports the unadjusted, adjusted-attenuated, and
  adjusted-disattenuated estimates, an identification diagnostic, a
  Cinelli-Hazlett robustness value, and a reliability-sensitivity curve, and it
  requires both an adjustment set and a declared temporal order before applying
  a causal-under-assumptions label -- discipline and sensitivity that CB-SEM and
  PLS-SEM do not provide.
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
