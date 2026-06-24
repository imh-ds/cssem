source(file.path("inst", "scripts", "script-utils.R"))
prefer_workspace_library()

library(cssem)

d <- simulate_cssem_data(n = 80, seed = 11)
m <- cssem_model(list(
  A = list(indicators = paste0("a", 1:4), scales = "ordinal"),
  B = list(indicators = paste0("b", 1:4), scales = "ordinal")
), folds = 4)
f <- cssem_fit(m, d, seed = 12, iterations = 3)
stopifnot(nrow(f$locked_scores) == nrow(d), all(is.finite(as.matrix(f$locked_scores))))
stopifnot(nrow(cssem_score(f, d[c(paste0("a", 1:4), paste0("b", 1:4))])) == nrow(d))
stopifnot(nrow(cssem_residual_diagnostics(f, "A")) == 6L)
bad_schema <- try(cssem_score(f, d[c(paste0("b", 1:4), paste0("a", 1:4))]), silent = TRUE)
stopifnot(inherits(bad_schema, "try-error"))
print(cssem_evidence_ledger(f))
message("CS-SEM smoke test passed.")
