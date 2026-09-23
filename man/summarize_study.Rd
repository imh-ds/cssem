% Generated from R/study-summary.R
\name{summarize_study}
\alias{summarize_study}
\title{Summarize simulation-study operating characteristics}
\usage{
summarize_study(simulation, null_values = NULL, level = 0.95)
}
\arguments{
\item{simulation}{A \code{cssem_simulation} object returned by
\code{simulate_study()}.}

\item{null_values}{Optional finite named numeric vector giving a null value
for each estimand whose detection rate should be summarized.}

\item{level}{Confidence level for Monte Carlo uncertainty intervals.}
}
\value{
A \code{cssem_study_summary} object with a \code{metrics} data frame containing
point estimates, valid counts, planned denominators, Monte Carlo standard
errors, uncertainty bounds, scopes, and metric availability. The metric column
includes bias, RMSE, mean interval width, coverage, detection, convergence,
failure, partial-analysis, and interval-availability rates.
}
\description{
Summarize conditional and unconditional operating characteristics for each
scenario, sample size, and estimand in a simulation study.
}
\details{
Bias, RMSE, and mean interval width are conditional on available finite values.
Coverage and detection are returned conditionally among available intervals
and unconditionally over all scheduled replications. Failed and unavailable
results count as uncovered and as non-detections for the unconditional rates.
Convergence is reported conditionally among callbacks that returned a
convergence value and unconditionally over all scheduled replications; failed
runs are not converged for the unconditional rate. Failure and partial rates
use all scheduled replications as their denominator. Mean uncertainty
intervals use Student's t intervals; rate intervals use Wilson score bounds.
RMSE uncertainty is based on an interval for mean squared error and the delta
method Monte Carlo standard error.
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
simulation <- simulate_study(spec, reps = 5L, seed = 42L)
summary <- summarize_study(simulation, null_values = c(beta = 0))
head(summary$metrics)
}
