# A CS-SEM tutorial: from raw survey data to a reported analysis

This is an informal walk-through of the `cssem` workflow. It uses simulated survey
records, so you can run every chunk without data or network access. The same code
ships as the package vignette (`vignettes/cssem_tutorial.Rmd`; run
`vignette("cssem_tutorial", package = "cssem")` after installing). Long provenance
records are shortened here.

```r
# install.packages("remotes")
remotes::install_github("imh-ds/cssem")
library(cssem)
```

First, a helper that simulates a small five-point survey with a trust scale, a loyalty scale and age:

```r
item5 <- function(x) pmin(pmax(as.integer(round(x) + 3L), 1L), 5L)
make_survey_data <- function(n, seed, add_missing = FALSE) {
  set.seed(seed)
  trust_state <- stats::rnorm(n)
  age <- stats::rnorm(n, mean = 40, sd = 8)
  loyalty_state <- .45 * trust_state + .02 * age + stats::rnorm(n)
  data <- data.frame(
    trust_1 = item5(trust_state + stats::rnorm(n, sd = .6)),
    trust_2 = item5(trust_state + stats::rnorm(n, sd = .7)),
    trust_time = trust_state + stats::rnorm(n, sd = .5),
    loyalty_1 = item5(loyalty_state + stats::rnorm(n, sd = .6)),
    loyalty_2 = item5(loyalty_state + stats::rnorm(n, sd = .7)),
    age = age
  )
  if (add_missing) data$trust_2[seq(5L, n, by = 19L)] <- NA_integer_
  data
}
```

This tutorial walks through a small, deterministic study from scale declarations
to prediction and support checks. It uses simulated records so it can run without
network access. The sample size and resampling counts are deliberately modest;
use the study's full design and stronger convergence settings for substantive
analysis.

## Declare scales and inspect sample coverage

`mixed_items()` combines existing ordinal and continuous item declarations; it
does not infer scales from the observed values. `partial` missingness lets each
observed indicator contribute to the respondent's measurement posterior.


``` r
survey_data <- make_survey_data(120L, 2026L, add_missing = TRUE)
new_data <- make_survey_data(30L, 2027L)
model <- specify_measurement(
  Trust = mixed_items(ordinal("trust_1", "trust_2"), continuous("trust_time")),
  Loyalty = ordinal("loyalty_1", "loyalty_2"),
  Age = manifest("age"), folds = 3
)
fit <- fit_states(model, survey_data, seed = 23, iterations = 6,
  diagnostics = FALSE, missing_policy = "partial", retain_data = FALSE)
sample_accounting(fit)
#> CS-SEM sample accounting: 120/120 rows retained (policy: partial)
#>        stage  target n_total n_retained n_effective n_score_finite n_complete
#>  measurement   Trust     120        120         120            120        113
#>  measurement Loyalty     120        120         120            120        120
#>  measurement     Age     120        120         120            120        120
#>  n_partial n_prior_only independent_unit_n n_excluded
#>          7            0                 NA          0
#>          0            0                 NA          0
#>          0            0                 NA          0
```

The fit keeps locked construct scores and encoders but omits the training data
frames. `sample_accounting()` preserves row positions and counts. Functions that
need original item rows, such as measurement assessment and bootstrap refits,
require a fit made with `retain_data = TRUE`. Increase `iterations` when the
measurement engine reports that a construct reached its iteration cap; a
suppressed warning in this compact example does not imply convergence.

The provenance record keeps the call, resolved options, software versions,
schema, row positions, and split metadata without embedding observations:


``` r
cssem_provenance(fit)
#> $schema_version
#> [1] 1
#> 
#> $operation
#> [1] "fit_states"
#> 
#> $call
#> [1] "fit_states(model = model, data = \"<omitted>\", seed = 23, iterations = 6, diagnostics = FALSE, missing_policy = \"partial\", retain_data = FALSE)"
#> 
#> $settings
#> $settings$model
#> $settings$model$constructs
#> $settings$model$constructs$Trust
#> $settings$model$constructs$Trust$indicators
#> [1] "trust_1"    "trust_2"    "trust_time"
#> 
#> $settings$model$constructs$Trust$scales
#> [1] "ordinal"    "ordinal"    "continuous"
#> 
#> $settings$model$constructs$Trust$keys
#> [1] 1 1 1
#> 
#> $settings$model$constructs$Trust$manifest
#> [1] FALSE
#> 
#> ... (183 more lines omitted; run the code to see the full record)
```

## Fit and inspect the associational structure

Construct states are standardized scores. The declared paths below are
associational predictive relationships, not automatically causal effects.


``` r
structure <- specify_structure(
  Loyalty ~ linear(Trust) + linear(Age),
  order = c("Age", "Trust", "Loyalty")
)
association <- associate(fit, structure, structural_repeats = 1,
  shadow_scope = "temporal", seed = 24)
effect_ledger(association)
#>   outcome predictor  shape theory_r_squared mean_mse_improvement
#> 1 Loyalty     Trust linear       0.07226245                    0
#> 2 Loyalty       Age linear       0.07226245                    0
#>   mse_improvement_se selection_frequency nonlinearity_p edge_drop_mse_increase
#> 1                 NA                   1             NA              0.1654254
#> 2                 NA                   1             NA              0.1112053
#>   edge_drop_mse_se temporal_gap unrestricted_gap naive_estimate
#> 1       0.04148732    0.1185591               NA      0.3532884
#> 2       0.03265759    0.1185591               NA      0.2516084
#>   corrected_estimate predictor_reliability outcome_reliability
#> 1          0.3975690             0.8894984           0.8047917
#> 2          0.2506546             1.0000000           0.8047917
#>   latent_standardized_estimate latent_standardized_ci_low
#> 1                    0.4179683                         NA
#> 2                    0.2794048                         NA
#>   latent_standardized_ci_high corrected_ci_low corrected_ci_high eiv_applicable
#> 1                          NA               NA                NA           TRUE
#> 2                          NA               NA                NA           TRUE
#>   eiv_stable eiv_n eiv_rank eiv_condition_number eiv_corrected_condition_number
#> 1       TRUE   120        2             1.065568                        1.13884
#> 2       TRUE   120        2             1.065568                        1.13884
#>   eiv_singular correction_shrink correction_strength reliability_floor_applied
#> 1        FALSE                 1                   1                     FALSE
#> 2        FALSE                 1                   1                     FALSE
#>                                                eiv_diagnostic        status
cssem_provenance(association)
#> ... (452 more lines omitted; run the code to see the full record)
```

## Contrasts and new-record prediction

The marginal contrast compares the expected outcome at two locked-score values.
The interval is conditional on the selected structural shape. Prediction scores
the new records with the fitted measurement encoders and reports the structural
outcome on the locked-score scale.


``` r
contrast <- marginal_contrast(association, "Loyalty", "Trust", values = c(-1, 1),
  reps = 40, seed = 27)
contrast
#> CS-SEM marginal contrast: Loyalty by Trust
#>  outcome predictor category  estimate probability_contrast low high
#>  Loyalty     Trust     <NA> 0.7065767                   NA  -1    1
#>  estimate_ci_low estimate_ci_high probability_contrast_ci_low
#>        0.3589319        0.9985478                          NA
#>  probability_contrast_ci_high            interval_method interval_status
#>                            NA percentile pairs bootstrap       available
#>  interval_reps successful_replicates level   n
#>             40                    40  0.95 120
cssem_provenance(contrast)
#> $schema_version
#> [1] 1
#> 
#> $operation
#> [1] "marginal_contrast"
#> 
#> $call
#> [1] "marginal_contrast(association = \"<omitted>\", outcome = \"Loyalty\", predictor = \"Trust\", values = c(-1, 1), reps = 40, seed = 27)"
#> 
#> $settings
#> $settings$outcome
#> [1] "Loyalty"
#> 
#> $settings$predictor
#> [1] "Trust"
#> 
predict(association, new_data, outcomes = "Loyalty")
#> ... (519 more lines omitted; run the code to see the full record)
```

## Check held-out performance

Outer validation refits measurement and structural selection on each training
partition. Its `outer_test` metrics refer to held-out records; internal selector
metrics remain separately labeled and must not be presented as held-out results.


``` r
splits <- make_splits(survey_data, method = "random", folds = 3, seed = 25)
outer <- validate_outer(model, structure, survey_data, splits, seed = 26,
  iterations = 6, diagnostics = FALSE,
  structural_args = list(structural_repeats = 1, shadow_scope = "temporal"))
#> Warning: The measurement model reached its 12-iteration EM cap before
#> converging for: Trust, Loyalty. Scores may still be moving; refit with a larger
#> `iterations` (see fit$measurement_engine).
#> Warning: The measurement model reached its 12-iteration EM cap before
#> converging for: Trust, Loyalty. Scores may still be moving; refit with a larger
#> `iterations` (see fit$measurement_engine).
#> Warning: The measurement model reached its 12-iteration EM cap before
#> converging for: Trust, Loyalty. Scores may still be moving; refit with a larger
#> `iterations` (see fit$measurement_engine).
outer
#> CS-SEM outer validation: 3/3 partitions successful; status = complete
#>   held-out metrics: outer_test; selection metrics: internal_selection
```

## Optional causal estimand

This call demonstrates the package's declared adjusted-linear estimand. Its
interpretation requires a defensible temporal order, an adequate adjustment set,
consistency, positivity, and no unmeasured confounding. The software checks some
structural and temporal-order constraints and can audit a typed causal graph,
but it cannot verify those study assumptions from the data or from the
declarations. The result label is not evidence that the assumptions hold.


``` r
trust_loyalty_design <- causal_design(
  edges = data.frame(
    from = c("Age", "Age", "Trust"),
    to = c("Trust", "Loyalty", "Loyalty")
  ),
  treatment = "Trust", outcome = "Loyalty", adjust = "Age",
  assumptions = list(
    consistency = "assumed",
    no_unmeasured_confounding = "assumed",
    positivity = "assumed",
    measurement_validity = "assumed",
    temporal_order = "assumed"
  )
)
causal_effect(association, treatment = "Trust", outcome = "Loyalty",
  adjust = "Age", estimand = "adjusted_linear", disattenuate = FALSE,
  temporal_order = c("Age", "Trust", "Loyalty"),
  design = trust_loyalty_design)
#> CS-SEM effect: Trust -> Loyalty  (n = 120)
#> Interpretation: Causal under assumptions
#> Adjustment set:  Age 
#> Estimand:       adjusted_linear (disattenuated, linear adjustment)
#> 
#>   unadjusted effect      0.359  (confounded if the adjustment set matters)
#>   adjusted, attenuated   0.353  (adjusted but not disattenuated)
#>   adjusted effect       0.353  (naive)
#> 
#>   identification strength 1.00 (residual treatment variation after adjustment)
#>   support proxy            adequate  (linear_adjustment_residual_variance_ratio; not a positivity proof)
#>   nuisance diagnostics     linear_adjustment  (treatment RMSE 0.996; outcome RMSE 0.895)
#>   robustness value        0.32  (an unmeasured confounder explaining 32% of residual variance in both treatment and outcome would null the effect)
#> 
#> Associational-to-causal estimate under declared assumptions; not a randomized-experiment guarantee.
```

The [causal validation manifest](../R/causal-validation.R) also provides
independent structural targets for six score-level stress scenarios; it does
not refit measurement models or establish identification for the example data.

For the estimator's current evidence and explicit boundaries, see
[`capabilities.md`](capabilities.md) and the
[`method specification`](method-spec.md).
