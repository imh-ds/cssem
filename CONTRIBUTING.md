# Contributing to CS-SEM

CS-SEM is a research package. Contributions should preserve the separation
between measurement validation and later structural/causal modules.

Before proposing a change:

1. Add or update a deterministic test or simulation scenario.
2. Run `inst/scripts/smoke-test.R` after installing the package locally.
3. Do not claim performance gains from selected scenarios; report the complete
   validation design and all convergence failures.
4. Document every exported function with roxygen2 and run
   `roxygen2::roxygenise()` before submitting documentation changes.
