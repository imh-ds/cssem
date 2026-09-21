\name{item_response_curve}
\alias{item_response_curve}
\title{Evaluate a fitted item-response curve}
\usage{
item_response_curve(fit, construct, item, latent = seq(-4, 4, length.out = 81L))
}
\arguments{
  \item{fit}{A \code{fit_states} object.}
  \item{construct}{Fitted construct name.}
  \item{item}{Indicator column name within the construct.}
  \item{latent}{Finite latent-node values at which to evaluate the curve.}
}
\description{
Evaluates the full-data encoder retained for future scoring. Ordinal output
contains category probabilities that sum to one at each latent value;
continuous output contains the expected response and residual scale. Curves
are not cross-fitted predictions, and manifest constructs have no item-response
curve.
}
\value{A data frame of class \code{cssem_item_response_curve}.}
\seealso{\code{\link{measurement_parameters}}, \code{\link{score_states}}}
