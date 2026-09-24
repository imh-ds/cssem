% Generated from R/model-comparison.R
\name{compare_outer}
\alias{compare_outer}
\title{Compare two outer-validation results on identical held-out partitions}
\usage{compare_outer(first, second, metrics = c("rmse", "mae", "r_squared"), reps = 999L, seed = 1L, alignment = NULL)}
\arguments{
\item{first,second}{Completed \code{cssem_outer_validation} objects.}
\item{metrics}{Held-out metrics among \code{rmse}, \code{mae}, and \code{r_squared}.}
\item{reps}{Number of paired partition-bootstrap replicates.}
\item{seed}{Integer bootstrap seed.}
\item{alignment}{Optional named construct map or observed-row alignment data frame.}
}
\value{A \code{cssem_model_comparison} object with paired per-partition differences and intervals.}
\description{Comparisons require identical partition row IDs, observations, outcome score bases (the indicators and scales of each compared outcome), target availability, and held-out metric scope unless an explicit alignment is supplied; the compared models' predictor sets may differ. The partition-bootstrap interval treats overlapping outer partitions as independent, so it is reported with \code{interval_status = "descriptive_uncalibrated"} rather than as a calibrated confidence interval. The function does not compare likelihood, AIC/BIC, or global fit.}
