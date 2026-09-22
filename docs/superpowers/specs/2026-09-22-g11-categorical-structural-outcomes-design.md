# G11 categorical structural outcomes design

## Problem

The structural layer currently treats every locked outcome as a continuous
least-squares response. That is inappropriate when a declared endogenous
construct is binary or ordinal: fitted values can leave the outcome support,
cross-validation is evaluated with a continuous loss, and downstream
prediction has no probability or category representation.

## Scope

G11 adds an explicit response-family declaration to `specify_structure()` and
`cssem_structure()` for `gaussian` (the unchanged default), `binomial` with a
logit link, and `ordinal` with a cumulative-logit link. Binary and ordinal
outcomes are fit on the locked score scale and must be represented by valid
binary or integer ordered categories. Nominal outcomes, alternative links,
nonlinear structural shapes, interaction products, EIV corrections, and
coefficient-product mediation remain unsupported and are rejected clearly.

The categorical estimator is deliberately separate from measurement scoring:
measurement may still use continuous posterior means, but the structural
response must be an observed categorical locked score. This prevents a
continuous EAP score from being silently presented as an ordinal likelihood.

## Public API

`structural_outcome(family, link = NULL, levels = NULL)` returns a validated
response-family declaration. `binary_outcome()` and `ordinal_outcome()` are
convenience constructors. A named `families` argument on the structure
declaration maps outcome names to declarations; omitted outcomes use
`gaussian(identity)`.

Examples:

```r
specify_structure(
  Success ~ Effort,
  families = list(Success = binary_outcome())
)

specify_structure(
  Satisfaction ~ Trust,
  families = list(Satisfaction = ordinal_outcome(levels = 1:5))
)
```

## Estimation and diagnostics

Binary outcomes use `stats::glm(..., family = binomial(link = "logit"))`.
Ordinal outcomes use `MASS::polr(..., method = "logistic")`. Shape search is
disabled for categorical outcomes; only the declared linear predictor is
allowed. Repeated structural cross-validation uses negative log-likelihood as
the selection loss and reports probability RMSE, log loss, Brier score, and
classification accuracy. The existing Gaussian metrics remain unchanged.

Prediction returns expected category values by default and supports
`type = "probability"` (a probability matrix) and `type = "class"`. Probability
rows are finite, bounded, and sum to one. Prediction assessment reports the
same categorical metrics where a target score is available.

EIV/reliability correction, respondent information weighting, constraints,
and structural mediation products are rejected for categorical outcomes until
family-specific estimators and estimands are designed. Shadow-model R-squared
gaps are marked unavailable rather than calculated on the wrong scale.

## Compatibility and failure behavior

Existing structures with no family declaration behave exactly as before.
Family names must match declared outcomes. Binary responses must contain only
0/1 values; ordinal responses must be finite integer categories with at least
two levels. Invalid link/family combinations and unsupported shape declarations
fail before fitting with an actionable error.

## Acceptance criteria

1. Binary predictions are valid probabilities and class labels, and repeated
   CV reports finite log loss/Brier/accuracy metrics.
2. Ordinal predictions return a complete probability matrix whose rows sum to
   one and expected category values remain within the declared levels.
3. Invalid response values and unsupported corrections fail explicitly.
4. Gaussian behavior and the existing test suite remain unchanged.
