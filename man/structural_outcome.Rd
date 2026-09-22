\name{structural_outcome}
\alias{structural_outcome}
\alias{binary_outcome}
\alias{ordinal_outcome}
\title{Declare a structural response family}
\usage{
structural_outcome(family = c("gaussian", "binomial", "ordinal"),
  link = NULL, levels = NULL)

binary_outcome(levels = c(0, 1))

ordinal_outcome(levels = NULL)
}
\arguments{
  \item{family}{Response family: \code{"gaussian"}, \code{"binomial"}, or \code{"ordinal"}.}
  \item{link}{Response link. Gaussian supports \code{"identity"}; binomial and ordinal support \code{"logit"}.}
  \item{levels}{Optional numeric outcome levels. Binomial levels must be \code{c(0, 1)}. Ordinal levels must be increasing, unique integer values with at least two categories.}
}
\description{
Creates a validated response-family declaration for use in the named
\code{families} argument to \code{specify_structure()} or
\code{cssem_structure()}. Family declarations are explicit; measurement-scale
declarations do not determine the structural likelihood.
}
\details{
Binomial outcomes use a logistic regression. Ordinal outcomes use a
cumulative-logit proportional-odds model. Gaussian outcomes remain the
default. Categorical outcomes currently support linear main effects only.
}
\value{
A validated \code{cssem_structural_outcome} object.
}
\seealso{\code{\link{specify_structure}}, \code{\link{cssem_structure}}}
