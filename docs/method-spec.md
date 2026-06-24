# CS-SEM v0.3 method contract

CS-SEM v0.1 estimates **theory-declared manifestation constructs** only. Each
construct is one-dimensional and is represented by an out-of-fold construct
state. A construct declaration specifies ordered indicators, their observed
scales (`ordinal` or `continuous`), and their direction (`key`, -1 or 1).

For ordinal and binary items, the measurement decoder is a monotone,
regularized graded-response model: `P(X <= c | z) = logistic(tau[c] - a z)`,
where `a > 0` and thresholds are ordered. The default ordinal engine estimates
item parameters by marginal likelihood over a standard-normal latent grid and
returns posterior-mean (EAP) respondent scores. This avoids optimizing
respondent scores directly as PLS-style composites. Continuous items currently
use a robust Gaussian linear decoder through a clearly labeled experimental
mixed-scale fallback. Missing item responses contribute no likelihood term.
Scores are standardized and positive keys make larger states correspond to
larger item responses.

`cssem_fit()` uses K-fold cross-fitting. Every returned locked score is
predicted by an encoder trained without that observation. Full-data encoders
are retained only for `cssem_score()` on new records. The API rejects missing,
extra, or reordered scoring columns rather than aligning them silently.

Version 0.3 reports held-out decoder log loss/RMSE, fold stability, item
warnings, exploratory leave-one-item-out residual dependence, and construct
redundancy. Residual dependence is returned as a diagnostic table, not an
automatic warning, until the simulation study calibrates its false-positive
rate.

The structural extension is deliberately associational. A `cssem_structure`
declares locked-state predictors and may declare edge-level shape policies via
`cssem_effect()`. `cssem_associate()` cross-validates linear, constrained
monotone, and low-complexity smooth candidates one declared edge at a time,
then reports temporal and unrestricted shadow-model specification gaps and an
effect evidence ledger. These effects are not causal claims and do not provide
mediation, adjustment, or treatment-effect estimates. See
`docs/associational-structure.md`.

Measurement uncertainty is now propagated rather than discarded. The marginal
graded-response model retains each respondent's out-of-fold posterior, from
which `cssem_fit()` reports a per-construct marginal reliability
(`fit$reliability`) and a per-respondent posterior SD
(`fit$score_posterior_sd`). Locked construct states carry measurement error, so
naive structural slopes among them are attenuated exactly as composite and PLS
scores are. For linear and monotone edges, `cssem_associate()` applies a
classical (Fuller) errors-in-variables correction that subtracts the predictor
error covariance `diag((1 - reliability) * var)` before solving the structural
normal equations, recovering the disattenuated slope. A percentile bootstrap
(`eiv_bootstrap`) reports its sampling interval. Smooth edges are reported but
not yet corrected. The correction requires a reliability estimate; it is applied
only when one is available (CS-SEM derives it from the posterior), so score-only
pipelines report the naive slope unchanged. Latent-state bags are now real
posterior draws (plausible values), replacing the earlier fixed-variance
placeholder.

Latent uncertainty draws remain experimental research aids in v0.3. They are
not calibrated confidence intervals, are excluded from the release-validation
story, and must not be reported as confirmatory uncertainty until coverage
validation is complete.
