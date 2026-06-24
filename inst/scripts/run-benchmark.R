source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)
result <- run_measurement_benchmark(reps = 50, n = 400, seed = 2026)
print(summary(result))
cat(attr(result, "success_criterion"), "\n")
cat("Success:", attr(result, "success"), "\n")
