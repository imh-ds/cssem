\name{check_model}
\alias{check_model}
\title{Check a CS-SEM measurement model and structural graph}
\usage{
check_model(model, structure = NULL)
}
\arguments{
  \item{model}{A \code{cssem_model} object.}
  \item{structure}{Optional \code{cssem_structure} object.}
}
\description{
Returns structured model and graph issues with severity, location, stable
codes, and recovery actions. A valid model produces a zero-row
\code{cssem_model_check}.
}
\value{
A data frame inheriting from \code{cssem_model_check} and
\code{cssem_preflight}.
}
\seealso{\code{\link{check_data}}, \code{\link{fit_states}}}
