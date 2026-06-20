# Associational structural layer

`cssem_associate()` accepts a theory-declared `cssem_structure` and uses only
the measurement fit's locked construct states. For each declared outcome it
compares an additive linear model with a low-complexity additive natural-spline
model using structural-fold cross-validation. A smooth model is used only when
its average paired foldwise loss improvement exceeds one cross-fold standard
error. This replaces a fixed RMSE cutoff and makes shape selection responsive
to validation uncertainty. The default candidate library is linear plus
natural splines with 3 and 4 degrees of freedom; CS-SEM reports the chosen
smooth candidate and its foldwise improvement uncertainty.

The output is explicitly **associational**. It estimates neither causal effects
nor mediation, adjustment, endogeneity correction, heterogeneous treatment
effects, nor configuration rules.

## Two shadow benchmarks

Declare an ordering when the theory makes one available:

```r
structure <- cssem_structure(
  list(
    Quality = "Trust",
    Loyalty = c("Trust", "Quality")
  ),
  order = c("Trust", "Quality", "Loyalty")
)
```

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
association <- cssem_associate(fit, structure)
cssem_effect_card(association, "Loyalty")
cssem_specification_gap(association, "temporal")
cssem_specification_gap(association, "unrestricted")
```

If the unrestricted gap is much more negative than the temporal gap, report
that as same-wave interdependence outside the acyclic theory model. Consider
longitudinal or dynamic designs before assigning directional meaning.
