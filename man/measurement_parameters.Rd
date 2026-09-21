\name{measurement_parameters}
\alias{measurement_parameters}
\title{Return fitted scale-aware measurement parameters}
\usage{
measurement_parameters(fit, construct = NULL)
}
\arguments{
  \item{fit}{A \code{fit_states} object.}
  \item{construct}{Optional fitted construct name.}
}
\description{
Returns ordinal graded-response discriminations and thresholds, continuous
item intercepts, slopes, and residual scales, or an explicit manifest
passthrough row. The table also reports observed support, held-out loss family,
and construct-level posterior information. Encoder parameters retain their
model-specific units and are not relabeled as CFA loadings.
}
\value{A data frame of class \code{cssem_measurement_parameters}.}
\seealso{\code{\link{item_response_curve}}, \code{\link{fit_states}}}
