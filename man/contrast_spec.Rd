% Generated from R/contrasts.R
\name{contrast_spec}
\alias{contrast_spec}
\title{Declare safe arithmetic contrasts over CS-SEM parameter IDs}
\usage{contrast_spec(definitions, basis = c("auto", "naive", "corrected"))}
\arguments{
\item{definitions}{A non-empty named list of arithmetic expression strings.}
\item{basis}{Default reporting basis, one of \code{"auto"}, \code{"naive"}, or \code{"corrected"}.}
}
\value{An object of class \code{cssem_contrast_spec}.}
\description{Expressions use stable parameter IDs, numeric literals, \code{+}, \code{-}, \code{*}, \code{/}, and parentheses. Function calls, indexing, assignments, and other R syntax are rejected.}
\examples{
spec <- contrast_spec(list(path_difference = "edge:Y~X:naive - edge:Z~X:naive"))
}
