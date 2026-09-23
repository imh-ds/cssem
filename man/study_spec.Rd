% Generated from R/study-planning.R
\name{study_spec}
\alias{study_spec}
\title{Define a callback-driven simulation study}
\usage{
study_spec(scenarios, sample_sizes, generate, truth, analyze, metadata)
}
\arguments{
\item{scenarios}{A non-empty data frame with a unique, non-missing character
\code{scenario} column. Each row is passed to callbacks as a named list.}

\item{sample_sizes}{A non-empty vector of distinct positive whole-number
sample sizes.}

\item{generate}{A function \code{generate(scenario, n, seed)} returning the
observed analysis data as a data frame.}

\item{truth}{A deterministic function \code{truth(scenario, n)} returning a
finite, named numeric vector of independently derived targets. The target
may vary with \code{n}; it is never passed to \code{analyze()}.}

\item{analyze}{A function \code{analyze(data, scenario, seed)} returning one
estimate/interval status row per declared estimand. See the study
simulation documentation for the complete return contract.}

\item{metadata}{A named list containing non-empty character scalars
\code{description} and \code{analysis_scope}, plus optional assumptions and
target descriptions. Values must be plain atomic vectors or recursively named
lists of plain values.}
}
\value{
An object of class \code{cssem_study_spec} containing the validated design,
callbacks, sample-size grid, and metadata. Construction does not call any
callbacks.
}
\description{
Create a validated callback-driven simulation-study specification.
}
\details{
The \code{truth} callback must derive targets independently of the analysis
callback. It may return targets that depend on sample size. Specification
construction validates inputs and callback signatures without running the
callbacks.
}
\examples{
scenarios <- data.frame(scenario = "base", beta = .3)
generate <- function(scenario, n, seed) data.frame(y = rep(0, n))
truth <- function(scenario, n) c(beta = scenario$beta)
analyze <- function(data, scenario, seed) NULL
study <- study_spec(scenarios, c(100L, 200L), generate, truth, analyze,
  metadata = list(description = "A simple study", analysis_scope = "locked scores"))
study$sample_sizes
}
