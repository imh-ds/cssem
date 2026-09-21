\name{numerical_diagnostics}
\alias{numerical_diagnostics}
\title{Inspect CS-SEM numerical and stabilization diagnostics}
\usage{
numerical_diagnostics(fit, association = NULL)
}
\arguments{
  \item{fit}{A \code{fit_states} object.}
  \item{association}{Optional \code{cssem_association} object built from \code{fit}.}
}
\description{
Returns measurement objective traces, optimizer statuses, convergence flags,
latent design rank and condition numbers. Structural rows, when requested,
report errors-in-variables covariance conditioning, reliability-floor use, and
the effective correction strength. These are numerical diagnostics rather than
accuracy guarantees or inferential tests.
}
\value{
A data frame of class \code{cssem_numerical_diagnostics}.
}
\seealso{\code{\link{fit_states}}, \code{\link{associate}}, \code{\link{check_data}}}
