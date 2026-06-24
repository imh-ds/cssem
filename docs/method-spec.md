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

Latent uncertainty draws remain experimental research aids in v0.3. They are
not calibrated confidence intervals, are excluded from the release-validation
story, and must not be reported as confirmatory uncertainty until coverage
validation is complete.
