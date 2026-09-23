% Generated from R/study-simulation.R
\name{simulate_study}
\alias{simulate_study}
\title{Run a callback-driven simulation study}
\usage{
simulate_study(spec, reps, seed = 1L, workers = 1L)
}
\arguments{
\item{spec}{A \code{cssem_study_spec} created by \code{study_spec()}.}

\item{reps}{Number of replications for each scenario and sample size.}

\item{seed}{Non-negative integer base seed used to assign callback seeds.}

\item{workers}{Positive integer worker count. One runs sequentially; larger
values use the package's PSOCK worker path.}
}
\value{
A \code{cssem_simulation} object. Its \code{replications} data frame has one
row per scheduled replication and estimand, including independent truth,
estimates, intervals, seeds, callback statuses, convergence, and failure
stage/reason. The object also records the design grid, metadata, callback
labels, base seed, and replication/worker counts. Callback environments are
not stored.
}
\description{
Run every scenario, sample-size, and replication combination in a study
specification while retaining failed and unavailable results.
}
\details{
The truth callback runs for each scenario and sample size before replications
start and is not passed to the analysis callback. Job seeds are assigned before
execution, so ordered outputs are invariant to worker count for callbacks that
are deterministic given their seeds and can be serialized to workers. The
caller's random-number state is restored when the function returns.
}
\examples{
scenarios <- data.frame(scenario = "base", beta = .3)
generate <- function(scenario, n, seed) {
  set.seed(seed)
  x <- stats::rnorm(n)
  data.frame(x = x, y = scenario$beta * x + stats::rnorm(n))
}
truth <- function(scenario, n) c(beta = scenario$beta)
analyze <- function(data, scenario, seed) {
  fit <- stats::lm(y ~ x, data = data)
  interval <- stats::confint(fit)["x", ]
  list(status = "completed", converged = TRUE, failure_reason = "",
    estimates = data.frame(estimand = "beta", estimate = unname(stats::coef(fit)["x"]),
      estimate_status = "available", lower = interval[[1L]], upper = interval[[2L]],
      interval_status = "available", status_reason = ""))
}
spec <- study_spec(scenarios, c(100L, 200L), generate, truth, analyze,
  metadata = list(description = "Gaussian linear study", analysis_scope = "OLS"))
simulation <- simulate_study(spec, reps = 2L, seed = 42L)
head(simulation$replications)
}
