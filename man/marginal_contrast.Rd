\name{marginal_contrast}
\alias{marginal_contrast}
\title{Compute marginal contrasts for structural outcomes}
\usage{
marginal_contrast(association, outcome, predictor, values, newdata = NULL,
  reps = 0L, level = 0.95, seed = 1L)
}
\arguments{
  \item{association}{A fitted \code{cssem_association} object.}
  \item{outcome}{Name of a declared structural outcome.}
  \item{predictor}{Name of a main-effect structural predictor.}
  \item{values}{A finite numeric vector of length two giving the low and high predictor settings.}
  \item{newdata}{Optional non-empty data frame of finite locked predictor scores over which to average. Defaults to the fitted structural score rows.}
  \item{reps}{Number of pairs-bootstrap replicates. Zero returns point contrasts without intervals.}
  \item{level}{Confidence level for percentile intervals.}
  \item{seed}{Integer seed for deterministic resampling. The caller's random stream is restored.}
}
\description{
Changes one locked-score predictor between two supplied settings and averages
the fitted response over the supplied score rows. For binomial and ordinal
outcomes, the result includes an expected-category contrast and one
probability contrast per category. This is an associational predictive
contrast, not a causal treatment effect.
}
\details{
When \code{reps} is positive, rows of the fitted locked-score data are
resampled and the selected family-specific linear estimator is refitted in
each replicate. Intervals are percentile pairs-bootstrap intervals conditional
on the locked scores and fixed predictor specification. With explicit
\code{newdata}, each bootstrap estimator is evaluated over those same rows.
The result retains bootstrap draws, the successful replicate count, failure
count, confidence level, and interval status. Intervals are marked unavailable
when fewer than \code{max(5, ceiling(reps / 2))} replicates succeed.
}
\value{
A \code{cssem_marginal_contrast} object containing a contrast data frame,
bootstrap draws, resampling metadata, and interval status.
}
\seealso{\code{\link{specify_structure}}, \code{\link{predict.cssem_association}}}
