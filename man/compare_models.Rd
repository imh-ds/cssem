% Generated from R/model-comparison.R
\name{compare_models}
\alias{compare_models}
\title{Compare two model specifications on one outer split design}
\usage{compare_models(model_a, structure_a, model_b, structure_b, data, splits, seed = 1L, args_a = list(), args_b = list(), alignment = NULL, ...)}
\arguments{
\item{model_a,model_b}{\code{cssem_model} objects.}
\item{structure_a,structure_b}{\code{cssem_structure} objects.}
\item{data}{Data frame containing declared indicators.}
\item{splits}{One \code{cssem_splits} object reused for both validations.}
\item{seed}{Base validation seed.}
\item{args_a,args_b}{Named arguments forwarded to \code{validate_outer()}.}
\item{alignment}{Optional named model-B-to-model-A construct map or observed-row alignment data frame.}
\item{...}{Additional arguments forwarded to both validations.}
}
\value{A \code{cssem_model_comparison} retaining both validation objects and provenance.}
