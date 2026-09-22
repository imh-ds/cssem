% Generated from R/constraints.R
\name{cssem_constraint}
\alias{cssem_constraint}
\title{Declare narrow linear structural coefficient constraints}
\usage{cssem_constraint(equal = list(), fixed = numeric())}
\arguments{
\item{equal}{A list of character edge groups constrained to equal locked-score slopes.}
\item{fixed}{A named numeric vector of fixed locked-score coefficients.}
}
\value{An object of class \code{cssem_constraint}.}
\description{Constraints apply only to selected linear structural edges on locked scores and use deterministic pooled constrained least squares. EIV correction, information weighting, interactions, nonlinear shapes, measurement equality, and ordinal structural outcomes are outside this contract.}
\examples{
constraint <- cssem_constraint(equal = list(c("Y~X", "Z~X")), fixed = c("W~X" = 0))
}
