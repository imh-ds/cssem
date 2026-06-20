# CS-SEM

CS-SEM v0.1 is a research implementation of cross-fitted construct-state
measurement. It estimates theory-declared, one-dimensional manifestation
constructs from ordinal, binary, and continuous indicators. Its narrow
structural layer fits theory-declared **associational** effects on locked scores
only; it does **not** fit causal effects, formative constructs, or global SEM
fit indices.

The public model contract is deliberately language-neutral: a construct has a
name, ordered indicator names, scale declarations, and item keys. The R API
uses a list specification that can be serialized and reproduced by a future
Python implementation.

```r
model <- cssem_model(list(
  trust = list(indicators = c("t1", "t2", "t3"),
               scales = "ordinal", keys = c(1, 1, -1))
), folds = 5)
fit <- cssem_fit(model, survey_data, seed = 42)
cssem_construct_card(fit, "trust")
```

```r
structure <- cssem_structure(list(loyalty = "trust"))
association <- cssem_associate(fit, structure)
cssem_specification_gap(association)
```

Structural reports include a temporally admissible shadow benchmark and an
unrestricted same-wave network benchmark. Positive specification gaps mean the
declared model predicts better than the corresponding shallow-tree benchmark;
they do not establish causal direction.

See `docs/method-spec.md` for the v0.1 contract and `inst/scripts/` for the
deterministic simulation benchmark. The benchmark reports a predeclared
recovery criterion; it is evidence generation, not a hard-coded claim that the
prototype has already surpassed CB-SEM or PLS-SEM.

For a local screening run, use
`run_measurement_benchmark(design = cssem_validation_design("screening"), reps = 3)`.
The 32-condition, 20-replication full stress test is intentionally expensive
and should be reserved for confirmation. The default implementation has dependency-free ordinal-factor and composite
proxies; use the optional `lavaan` and `plspm` packages in the next comparator
validation milestone for published CB-SEM/PLS-SEM comparisons.
