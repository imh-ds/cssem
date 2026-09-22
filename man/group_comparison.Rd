\name{group_comparison}
\alias{group_comparison}
\title{Compare declared structural paths between groups}
\usage{
group_comparison(association, group, reference = NULL, permutations = 999L,
  seed = 1L, min_group_size = 20L, level = 0.95)
}
\arguments{
  \item{association}{A \code{cssem_association} result from \code{associate()}.}
  \item{group}{A column name or group vector aligned to the association input.}
  \item{reference}{Optional reference-group label. Defaults to the first
  observed group.}
  \item{permutations}{Number of size-preserving label permutations. Zero
  returns raw contrasts without a p-value.}
  \item{seed}{Integer permutation seed.}
  \item{min_group_size}{Minimum group size used for the small-group warning.}
  \item{level}{Confidence level recorded in result metadata.}
}
\description{
Refits the association's selected structural shapes within groups using the
pooled locked construct scores. Linear and product edges receive scalar,
reference-directed contrasts. Nonlinear edges remain explicitly unavailable
when a single scalar contrast is not defined.
}
\value{
An object of class \code{cssem_group_comparison} containing group effects,
reference contrasts, permutation settings, group-size flags, status, and
methodological limitations.
}
\seealso{\code{\link{associate}}, \code{\link{measurement_invariance}}}
