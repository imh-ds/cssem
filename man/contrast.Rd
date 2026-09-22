% Generated from R/contrasts.R
\name{contrast}
\alias{contrast}
\title{Evaluate a CS-SEM contrast specification}
\usage{
contrast(object, spec, reps = 0L, level = 0.95, seed = 1L,
  resample = c("row", "cluster"), cluster = NULL,
  selection = c("fixed", "repeat"))
}
\arguments{
\item{object}{A supported CS-SEM result, currently a \code{cssem_association} for bootstrap contrasts.}
\item{spec}{A \code{cssem_contrast_spec} returned by \code{contrast_spec()}.}
\item{reps}{Number of joint row or cluster bootstrap replicates; zero returns a point estimate.}
\item{level}{Percentile interval confidence level.}
\item{seed}{Integer seed for deterministic resampling.}
\item{resample}{Resampling unit. Cluster mode preserves G9 source-unit provenance.}
\item{cluster}{Optional cluster column name or vector.}
\item{selection}{Whether selected structural shapes are fixed or rerun in each replicate.}
}
\value{A \code{cssem_contrast} object with point estimates, availability reasons, joint draws, intervals, and replicate metadata.}
\description{All referenced terms are evaluated on one resample, preserving covariance between paths. Unavailable terms and invalid arithmetic remain unavailable with a reason.}
