\name{measurement_assessment}
\alias{measurement_assessment}
\title{Assess scale-aware reliability and construct distinctiveness}
\usage{
measurement_assessment(fit, construct = NULL)
}
\arguments{
  \item{fit}{A \code{fit_states} object with retained training data.}
  \item{construct}{Optional fitted construct name.}
}
\description{
Reports EAP posterior reliability, descriptive item-to-state convergence,
posterior information, a descriptive Spearman HTMT-like ratio, locked-state
collinearity, and item-score correlations. AVE and CFA loadings are explicitly
marked unavailable because the CS-SEM encoder does not estimate them. No
threshold verdicts are applied.
}
\value{
An object of class \code{cssem_measurement_assessment} containing
\code{constructs}, \code{validity}, \code{item_score_correlations},
\code{collinearity}, and a \code{methodology} description.
}
\seealso{\code{\link{measurement_parameters}}, \code{\link{construct_card}}}
