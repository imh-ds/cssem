\name{measurement_invariance}
\alias{measurement_invariance}
\title{Diagnose common-anchor measurement invariance by group}
\usage{
measurement_invariance(fit, group, reference = NULL, min_group_size = 20L,
  level = 0.95, tolerance = 0.1)
}
\arguments{
  \item{fit}{A \code{fit_states} object.}
  \item{group}{A column name in the input data, or a group vector aligned to
  input or retained fit rows.}
  \item{reference}{Optional reference-group label. Defaults to the first
  observed group.}
  \item{min_group_size}{Minimum group size used for the small-group warning.}
  \item{level}{Confidence level recorded in result metadata.}
  \item{tolerance}{Absolute item-parameter difference that receives a flag.}
}
\description{
Estimates group-specific item parameters conditional on the pooled locked
construct scores. The result is a scale-safe diagnostic for item functioning and
score differences, not a joint equality-constrained CFA or conventional
metric/scalar invariance test.
}
\value{
An object of class \code{cssem_measurement_invariance} containing group sizes,
construct score summaries, group-specific item parameters, reference contrasts,
status metadata, and methodological limitations.
}
\seealso{\code{\link{measurement_parameters}}, \code{\link{fit_states}}}
