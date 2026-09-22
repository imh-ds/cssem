# Associational structural layer

`associate()` accepts a theory-declared `cssem_structure` and uses only
the measurement fit's locked construct states. It begins with the complete
declared linear model, then tests one declared edge at a time as nonlinear.
At most one nonlinear edge is retained for an outcome in v0.3. This limits
search multiplicity and makes the selected shape interpretable.

The default `"auto"` edge policy compares linear, monotone increasing,
monotone decreasing, and natural-spline (`df = 3, 4`) forms. A nonlinear
candidate must both clear the paired one-standard-error CV rule and be
supported in at least 70% of repeated fold assignments. Monotone curves use
training-fold quantile knots and constrained hinge slopes; held-out records do
not influence knots, constraints, or shape selection.

When nonlinear candidates are indistinguishable within their paired CV
uncertainty, CS-SEM prefers the lower-complexity monotone form over an
unconstrained spline. This is a parsimony convention, not evidence that the
effect is causal.

The output is explicitly **associational**. It estimates neither causal effects
nor mediation, adjustment, endogeneity correction, heterogeneous treatment
effects, nor configuration rules.

## Two shadow benchmarks

Declare an ordering when the theory makes one available:

```r
structure <- specify_structure(
  Quality ~ auto_monotone(Trust),
  Loyalty ~ linear(Trust) + Quality,
  order = c("Trust", "Quality", "Loyalty")
)
```

(`cssem_structure(list(Quality = list(Trust = cssem_effect("auto_monotone")), Loyalty = list(Trust = cssem_effect("linear"), Quality = cssem_effect("auto"))), order = c("Trust", "Quality", "Loyalty"))`
is the equivalent list-based declaration and remains supported.)

With the default `shadow_scope = "both"`, every outcome receives two
cross-validated shallow-tree benchmarks.

| Benchmark | Eligible predictors | Substantive question |
| --- | --- | --- |
| `temporal` | All locked constructs earlier than the outcome in `order` | Given the declared temporal assumptions, is the theory model predictively adequate? |
| `unrestricted` | All other same-wave locked constructs | How much same-wave network information lies outside the declared directional model? |

For example, with `Trust → Quality → Loyalty`, Loyalty is excluded from the
temporal Quality shadow because it is later in the declared order. It remains
available to the unrestricted Quality shadow. A better unrestricted benchmark
can reflect downstream association, feedback, common causes, or measurement
overlap; it does not establish that the directional arrow is reversed.

The specification gap is:

```text
theory cross-validated R² − shadow cross-validated R²
```

Positive values mean the declared theory model predicts better than that
shallow-tree benchmark. Negative values mean the benchmark predicts better and
flag possible omitted eligible predictors, interactions, threshold behavior,
heterogeneity, or same-wave network dependence. They do not automatically
replace theory or establish causal direction.

```r
association <- associate(fit, structure)
effect_card(association, "Loyalty")
effect_ledger(association)
specification_gap(association, "temporal")
specification_gap(association, "unrestricted")
```

If the unrestricted gap is much more negative than the temporal gap, report
that as same-wave interdependence outside the acyclic theory model. Consider
longitudinal or dynamic designs before assigning directional meaning.

`effect_ledger()` reports each edge's selected shape, repeated-CV
selection stability, predictive contribution when the edge is removed, and
both shadow gaps. It is an evidence profile, not a causal verdict or a
confirmatory confidence interval.

## Defined contrasts and constrained paths

`parameter_table()` assigns stable IDs to structural edges and derived effects,
including separate `:naive` and `:corrected` edge aliases. Use
`contrast_spec()` for safe arithmetic over those IDs:

```r
spec <- contrast_spec(list(
  difference = "edge:Loyalty~Trust:naive - edge:Loyalty~Quality:naive",
  product = "edge:Quality~Trust:naive * edge:Loyalty~Quality:naive"
))
contrast(association, spec, reps = 999, seed = 42)
```

All referenced terms share each row or cluster bootstrap draw. Fixed-selection
intervals condition on the selected shapes. `selection = "repeat"` reruns the
shape decision and retains per-replicate shape signatures. Unavailable
corrected terms and invalid ratios remain `NA` with an availability reason.

For a prespecified linear locked-score restriction, declare equality groups or
fixed coefficients with `cssem_constraint()` and pass the object to
`associate()`. The estimator is deterministic pooled constrained least squares
and reports its KKT rank and conditioning. Constraints reject EIV correction,
information weighting, interactions, and nonlinear selected edges; they do not
turn this associational layer into a likelihood SEM.

Use `compare_outer()` for paired held-out comparison of two completed
`validate_outer()` results. Both results must retain identical partitions,
observation rows, score bases, target availability, and `outer_test` metric
scope. `compare_models()` reuses one `cssem_splits` object for two theories.
When construct names differ, pass an explicit named model-B-to-model-A
alignment map only when indicators and observed target bases are genuinely
common. The comparison reports predictive metric deltas and partition
bootstrap intervals; it does not expose AIC/BIC, likelihood-ratio tests, or
global SEM fit statistics.
