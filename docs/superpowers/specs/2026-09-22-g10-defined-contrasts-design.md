# G10 Defined Contrasts, Constraints, and Model Comparison Design

**Date:** 2026-09-22

**Goal:** Add transparent contrasts of existing CS-SEM estimates, paired
predictive comparison of competing theories, and a deliberately narrow linear
constraint contract without presenting block losses as covariance-SEM
likelihoods.

## Context and scope

CS-SEM already exposes structural edge rows, corrected estimates, mediation
decompositions, conditional effects, bootstrap draws, and outer-validation
metrics. Those values do not yet share a stable parameter identity, so users
cannot safely define a difference or product across result objects. The package
also has no way to compare two fitted theories on exactly the same held-out
partitions.

G10 is delivered in three methodological layers. The first layer is a stable
estimand table and a restricted arithmetic expression evaluator. The second
layer evaluates all referenced terms on one resample, preserving covariance
between terms and exposing whether shape selection was held fixed or repeated.
The third layer compares two predictive validation results pairwise. A later
constraint layer supports only deterministic pooled least squares for selected
linear structural coefficients on the locked-score scale.

This design does not add a global covariance matrix, likelihood-ratio tests,
AIC/BIC, modification-index search, latent mean constraints, ordinal
structural-outcome constraints, or measurement-parameter equality. Those
features require a different joint estimator and are outside this package's
current estimand.

## Stable estimand identities

`parameter_table()` gains a `parameter_id` column that is stable under row
reordering. IDs use explicit namespaces and edge names, for example
`edge:Y~X:naive`, `edge:Y~X:corrected`, `edge:Y~A:B:naive`, and
`effect:X->Y:indirect_total`. Every row also carries `estimate_basis`,
`shape`, `available`, and `status`. An unavailable value remains unavailable
with a reason; it is never represented as zero.

`contrast_spec(definitions, basis = c("auto", "naive", "corrected"))`
accepts a named list of expression strings. Expressions contain numeric
literals, stable parameter IDs, `+`, `-`, `*`, `/`, and parentheses. The parser
rejects unknown identifiers, function calls, assignment, indexing, and other
R expressions. `contrast()` evaluates a specification against an association
or supported derived-effect object and returns a `cssem_contrast` object with
the parsed definitions, referenced parameter rows, point estimates, basis,
selection mode, and availability reasons.

## Joint uncertainty and selection

`contrast(..., reps = 0L)` is a point calculation. When `reps > 0`, one
`bootstrap_model()` resample produces all referenced estimates before any
expression is evaluated. This preserves covariance for differences, products,
and ratios. Row and cluster resampling use the existing G9 RNG, provenance,
and source-unit guarantees.

The `selection` argument is explicit. `selection = "fixed"` is the default;
it refits the original selected structural shapes and labels intervals as
conditional on those shapes. `selection = "repeat"` reruns shape selection in
each replicate and records selection changes. A failed term or zero denominator
invalidates that expression for the replicate and is recorded in failure
metadata; it is not silently imputed.

## Paired model comparison

`compare_outer(first, second, metrics = c("rmse", "mae", "r_squared"),
reps = 999L, seed = 1L)` accepts two completed `cssem_outer_validation` objects.
It requires identical outer IDs, train/test row IDs, target outcomes, metric
scope, and held-out availability. It returns per-partition paired differences,
metric direction, comparable-row counts, and a bootstrap interval over outer
partitions.

`compare_models(model_a, structure_a, model_b, structure_b, data, splits,
seed = 1L, args_a = list(), args_b = list(), alignment = NULL, ...)` runs both
theories through the same supplied `cssem_splits` object, then delegates to
`compare_outer()`. It retains both validation objects and fingerprints for the
splits, observations, targets, and measurement declarations. Different
measurement declarations require an explicit named `alignment` map from
model-B construct names to model-A names; mapped constructs must share an
observed target and score basis. No latent scale is inferred from names alone.

## Narrow linear constraints

`cssem_constraint(equal = list(c("Y~X", "Z~X")), fixed = c("Y~X" = 0))`
declares equality groups and fixed values for selected structural edges.
Initial fitting support is limited to selected linear edges, locked scores,
ordinary pooled least squares, and no errors-in-variables correction or
information weighting. The implementation uses a deterministic constrained
least-squares solve, reports rank and conditioning diagnostics, and retains the
original declaration and limitation in the association object.

Smooth, monotone, product-interaction, disattenuated, information-weighted,
measurement-parameter, and ordinal structural constraints fail explicitly.
This boundary prevents a constraint label from implying a likelihood or an
estimand that the current structural engine does not define.

## Validation and failure handling

- Parameter IDs remain unique when corrected and naive estimates coexist.
- Arithmetic expressions reject unsafe syntax and unavailable references.
- Joint contrast draws use one resample for every term and preserve caller RNG.
- Cluster contrast draws retain G9 source-unit and draw-occurrence metadata.
- Paired comparison rejects mismatched partitions, targets, rows, and score
  bases before computing differences.
- Constraint declarations reject duplicate/conflicting labels, rank deficiency,
  unsupported shapes, and incompatible EIV or weighting settings.
- All results expose assumptions, selection mode, basis, failures, and
  unavailable reasons through print, summary, and parameter-table methods.

## Testing contract

Tests cover stable IDs, linear differences, products, malformed expressions,
unavailable terms, covariance-preserving intervals, selection modes, row and
cluster reproducibility, worker invariance, paired outer metric arithmetic,
mismatched split/target/alignment diagnostics, equal and fixed linear slopes,
rank deficiency, and unsupported constraint combinations. Independent analytic
validation targets cover path differences, products, constrained coefficients,
and paired held-out losses.

## Future extension boundary

Joint measurement constraints, shared ordinal thresholds, nonlinear equality,
multilevel constraints, categorical structural outcomes, and likelihood-based
model comparison require separate estimators and designs. G10 must not add
those features behind the current contrast or comparison APIs.
