# CS-SEM v0.3 method contract

CS-SEM v0.1 estimates **theory-declared manifestation constructs** only. Each
construct is one-dimensional and is represented by an out-of-fold construct
state. A construct declaration specifies ordered indicators, their observed
scales (`ordinal` or `continuous`), and their direction (`key`, -1 or 1).

For ordinal and binary items, the measurement decoder is a monotone,
regularized graded-response model: `P(X <= c | z) = logistic(tau[c] - a z)`,
where `a > 0` and thresholds are ordered. Continuous items use a
linear-Gaussian decoder: `X = intercept + slope * z + N(0, sigma^2)`. Both
item types are estimated by the same marginal-ML/EM procedure over a shared
standard-normal quadrature grid: at each E-step, an item's contribution to
the respondent-by-node posterior is its graded-response category
log-probability (ordinal) or its Gaussian log-density (continuous), summed
across items in log-space; at each M-step, ordinal item parameters are
updated by BFGS on the posterior-weighted graded-response likelihood and
continuous item parameters by closed-form posterior-weighted least squares.
An all-ordinal construct therefore runs the identical estimator it always
has; a construct built entirely from continuous items, or mixing ordinal and
continuous items, gets the same posterior-based EAP scoring and reliability
as an all-ordinal one, rather than a separate weaker path. Missing item
responses contribute no likelihood term. Ordinal indicators must carry
whole-number category codes; a non-integer value errors rather than being
silently truncated. Scores are standardized and positive keys make larger
states correspond to larger item responses.

A `manifest()` declaration bypasses the measurement model entirely for a
single observed column that is not itself a multi-item construct (a control
variable such as age, or a deliberately single-item measure). Its locked
score is the column's own (by default standardized) value, computed
out-of-fold from training-fold statistics only, exactly like every other
construct's cross-fitted score. Its reliability is asserted rather than
estimated -- `1` by default (treated as measurement-error-free), or an
externally supplied value (e.g. a published test-retest reliability for a
single-item scale) that `associate()`'s errors-in-variables correction
consumes exactly as it would a measured construct's estimated reliability.

`fit_states()` uses K-fold cross-fitting. Every returned locked score is
predicted by an encoder trained without that observation. Full-data encoders
are retained only for `score_states()` on new records. The API rejects missing,
extra, or reordered scoring columns rather than aligning them silently.

Version 0.3 reports held-out decoder log loss/RMSE, fold stability, item
warnings, exploratory leave-one-item-out residual dependence, and construct
redundancy. Residual dependence is returned as a diagnostic table, not an
automatic warning, until the simulation study calibrates its false-positive
rate.

The structural extension is deliberately associational. A `cssem_structure`
declares locked-state predictors and may declare edge-level shape policies via
`cssem_effect()`. `associate()` cross-validates linear, constrained
monotone, and low-complexity smooth candidates one declared edge at a time,
then reports temporal and unrestricted shadow-model specification gaps and an
effect evidence ledger. These effects are not causal claims and do not provide
mediation, adjustment, or treatment-effect estimates. See
`docs/associational-structure.md`.

Measurement uncertainty is now propagated rather than discarded. The marginal
graded-response model retains each respondent's out-of-fold posterior, from
which `fit_states()` reports a per-construct marginal reliability
(`fit$reliability`) and a per-respondent posterior SD
(`fit$score_posterior_sd`). Locked construct states carry measurement error, so
naive structural slopes among them are attenuated exactly as composite and PLS
scores are. For linear and monotone edges, `associate()` applies a
classical (Fuller) errors-in-variables correction that subtracts the predictor
error covariance `diag((1 - reliability) * var)` before solving the structural
normal equations, recovering the disattenuated slope. A percentile bootstrap
(`eiv_bootstrap`) reports its sampling interval. Smooth edges are reported but
not yet corrected. The correction requires a reliability estimate; it is applied
only when one is available (CS-SEM derives it from the posterior), so score-only
pipelines report the naive slope unchanged. Latent-state bags are now real
posterior draws (plausible values), replacing the earlier fixed-variance
placeholder.

Per-respondent measurement information is reported through
`respondent_information()` and the Construct Card, exposing wide-posterior
(for example careless) respondents that covariance- and composite-based methods
cannot flag. An experimental inverse-variance `respondent_weighting` option in
`associate()` is off by default: because posterior width is
score-dependent, weighting induces range restriction and did not reduce
structural point-estimate bias in validation, so it is excluded from
confirmatory estimation.

The v0.4 validation manifests add behaviorally realistic structural shapes
(saturating plateau, threshold, and concave diminishing returns) and
measurement-stress scenarios (low reliability, careless responding, and
floor-effect skew). These exercise the disattenuation and shape-recovery
machinery on the kinds of survey and behavioral data CS-SEM targets; the v0.3
release gates are unchanged because they select scenarios by name.

Latent uncertainty draws remain experimental research aids in v0.3. They are
not calibrated confidence intervals, are excluded from the release-validation
story, and must not be reported as confirmatory uncertainty until coverage
validation is complete.
