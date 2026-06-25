# cssem 0.4.0 (in development)

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
