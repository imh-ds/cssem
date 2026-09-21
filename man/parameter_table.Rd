\name{parameter_table}
\alias{parameter_table}
\title{Extract a stable CS-SEM parameter or effect table}
\usage{
parameter_table(x, ...)
}
\arguments{
  \item{x}{A fitted CS-SEM result object.}
  \item{...}{Additional arguments reserved for methods.}
}
\description{
Returns a data frame with estimates, their reporting basis and units, sample
size, uncertainty method, and an explicit availability reason. Measurement
fits expose encoder parameters; structural and effect objects expose the
reported edge or effect table. Missing standard errors and intervals remain
missing rather than being inferred from marginal intervals.
}
\value{
A data frame with stable estimate and availability columns.
}
\seealso{\code{\link{effect_ledger}}, \code{\link{construct_card}}}
