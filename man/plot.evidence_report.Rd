\name{plot.evidence_report}
\alias{plot.evidence_report}
\title{Plot a Section of a CS-SEM Evidence Report}
\usage{
\method{plot}{evidence_report}(x,
  type = c("effects", "constructs", "causal_claims"), ...)
}
\arguments{
\item{x}{An \code{evidence_report} object.}

\item{type}{The section to plot: structural effects, construct diagnostics, or
routed causal claims.}

\item{...}{Additional arguments passed to base graphics.}
}
\value{
\code{x}, invisibly. Use \code{plot_data()} to inspect and customize the
plotted values.
}
\description{
Plots predictive contribution and routing status, construct stability, or
explicitly routed causal estimates. Associational paths remain descriptive.
}
\examples{
# plot(report, type = "effects")
# plot(report, type = "constructs")
# plot(report, type = "causal_claims")
}
