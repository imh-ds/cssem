\name{check_data}
\alias{check_data}
\title{Check CS-SEM data compatibility and information support}
\usage{
check_data(data, model, folds = NULL)
}
\arguments{
  \item{data}{A data frame containing declared indicators.}
  \item{model}{A \code{cssem_model} object.}
  \item{folds}{Optional fold count or complete integer fold assignment.}
}
\description{
Returns structured data issues for missing columns, invalid scale values,
sparse categories, fold support, zero-information items or constructs, and
rows with no observed indicators. Warnings are reported as rows; errors are
the conditions that \code{fit_states()} rejects before fitting.
}
\value{
A data frame inheriting from \code{cssem_data_check} and
\code{cssem_preflight}.
}
\seealso{\code{\link{check_model}}, \code{\link{fit_states}}}
