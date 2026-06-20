# Measurement validation protocol

Start with `cssem_validation_design("screening")`: six representative
conditions across sample size, loading, missingness, local dependence, and
cross-loading. Use three replications during development. It is intended to
finish on an ordinary local machine.

`cssem_validation_design("full")` produces the 32-condition confirmation grid.
Run that larger design only after screening identifies a promising encoder, and
increase replications gradually (for example 5, then 20) rather than starting
at 20. Each benchmark row contains construct recovery, dependency-free
ordinal-factor and composite-proxy recovery, and warning count.

The predeclared prototype gate is that mean CS-SEM recovery must be within
0.02 of both comparators. A failed gate is a result, not an error: do not add
structural, causal, or construct-discovery modules until the encoder improves
or its supported operating conditions are narrowed.

Benchmark fits use three folds, at least eight marginal-IRT iterations, and
skip exploratory residual diagnostics because those diagnostics do not affect
latent recovery and otherwise dominate runtime. Production fits retain five
folds and diagnostics by default.

For a publication-grade comparison, install `lavaan` and `plspm` and retain
the exact same simulated data, seeds, metrics, and scenario grid. Report the
package versions, convergence failures, and all scenarios—not only favorable
ones. The current package does not label its built-in proxy comparators as full
DWLS CFA or production PLS-SEM.
