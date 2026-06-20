library(cssem)

# Screening first: six representative conditions times three replications.
# Run the full design only after this identifies a promising encoder.
design <- cssem_validation_design("screening")
results <- run_measurement_benchmark(design = design, reps = 3, seed = 2026)

summary_by_scenario <- aggregate(
  cbind(cssem_recovery, cbsem_ordinal_factor_proxy_recovery,
        pls_composite_recovery, warning_count) ~
    n + loading + missing + local_dependence + cross_loading,
  data = results,
  FUN = mean
)

print(summary_by_scenario)
cat(attr(results, "success_criterion"), "\n")
cat("Overall success:", attr(results, "success"), "\n")
