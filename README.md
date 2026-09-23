# CS-SEM

CS-SEM v0.5.0 is a research package for cross-fitted construct-state
measurement and a theory-declared structural layer. It estimates
one-dimensional manifestation constructs from ordinal, continuous, or mixed
items, then selects supported structural shapes using the resulting locked
scores. Structural relationships are associational by default.

```r
model <- specify_measurement(
  Trust = mixed_items(
    ordinal("trust_1", "trust_2", keys = c(1, -1)),
    continuous("trust_duration")
  ),
  Loyalty = ordinal("loyalty_1", "loyalty_2"),
  Age = manifest("age"),
  folds = 5
)
fit <- fit_states(model, survey_data, seed = 42, retain_data = FALSE)

structure <- specify_structure(
  Loyalty ~ linear(Trust) + linear(Age),
  order = c("Age", "Trust", "Loyalty")
)
association <- associate(fit, structure, seed = 43)
effect_ledger(association)
cssem_provenance(association)
```

`ordinal()` declares whole-number category codes; `continuous()` uses a
linear-Gaussian item model; `mixed_items()` combines those declarations in one
construct without inferring scales from observed values. Item keys reverse an
indicator's direction before estimation. Cross-fitted construct scores are
standardized by default. A `manifest()` covariate bypasses the item encoder and
is standardized by default; use `standardize = FALSE` to retain its observed
units. See the [workflow vignette](vignettes/cssem-workflow.Rmd) for a complete
example including missingness, outer validation, prediction, and a carefully
qualified optional causal estimand.

The structural selector compares declared linear, monotone, and smooth shapes
using repeated cross-validation. It retains at most one nonlinear edge per
outcome. Formula interactions such as `A:B` are explicit product terms; the
selector does not search for unlisted interactions. Shadow gaps compare the
declared model with specified predictive benchmarks and do not establish
causal direction. `associate()` also reports a reliability-based
errors-in-variables correction for eligible linear, monotone, and product
terms. Smooth-edge correction is unavailable, and stabilized correction is
not an accuracy guarantee. Shape labels such as `linear()` and
`auto_monotone()` are formula markers parsed by `specify_structure()`, not
standalone callable functions.

The `retain_data = FALSE` option omits training data frames and row-aligned
cluster labels while keeping locked scores, encoders, row positions, sample
accounting, and provenance. Scoring, association, and new-record prediction
can continue from those stored results. Bootstrap refits and raw-item
measurement diagnostics require a fit made with `retain_data = TRUE`.

CS-SEM does not fit a global covariance-structure SEM. It does not report
chi-square, CFI/TLI, RMSEA, or SRMR, and it does not currently fit formative or
higher-order constructs, cross-loadings, correlated item errors, or simultaneous
feedback models. These are method boundaries, not missing names for predictive
metrics.

The released ordinal measurement envelope and selector evidence cover named
simulation scenarios. [`supported_envelope()`](man/supported_envelope.Rd) reports
the thresholds, evaluated job counts, convergence, and release-gate status from
the checked-in artifact; it is not a universal sample-size rule, a power
analysis, or a guarantee for a new study. Latent-state uncertainty draws and
information-based respondent weighting remain experimental. The package's
dependency-light comparator engines are proxies, not full covariance SEM or
production PLS-SEM implementations.

See the [capabilities and evidence table](docs/capabilities.md),
[method specification](docs/method-spec.md),
[associational structural guide](docs/associational-structure.md), and
[migration table](docs/migration.md). Release scenarios and outputs are under
[`tests/internal/validation_results/`](tests/internal/validation_results/).
