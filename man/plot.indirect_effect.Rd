\name{plot.indirect_effect}
\alias{plot.indirect_effect}
\title{Plot an Associational Mediation Decomposition}
\usage{
\method{plot}{indirect_effect}(x, ...)
}
\arguments{
\item{x}{An \code{indirect_effect} object.}

\item{...}{Additional arguments passed to base graphics.}
}
\value{
\code{x}, invisibly. Use \code{plot_data()} to inspect and customize the
plotted values.
}
\description{
Plots total, direct, indirect, and path-specific mediation components, retaining
the naive or disattenuated basis and drawing intervals only when available.
The decomposition is associational, not a causal mediation claim.
}
\examples{
# plot(mediation)
}
