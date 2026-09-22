\name{plot.cssem_association}
\alias{plot.cssem_association}
\title{Plot a CS-SEM Structural Diagram or Response Curve}
\usage{
\method{plot}{cssem_association}(x, type = c("paths", "curve"),
  routing = NULL, outcome = NULL, predictor = NULL, n = 101L,
  at = NULL, ...)
}
\arguments{
\item{x}{A \code{cssem_association} from \code{associate()}.}

\item{type}{\code{"paths"} for declared structural edges or \code{"curve"} for
one selected edge's response curve.}

\item{routing}{Optional \code{cssem_routing} from \code{route()}.}

\item{outcome}{Endogenous construct for a response curve.}

\item{predictor}{Single-construct predictor for a response curve.}

\item{n}{Number of points in a response curve.}

\item{at}{Optional named locked-score values at which other inputs are held.}

\item{...}{Additional arguments passed to base graphics calls.}
}
\value{
\code{x}, invisibly. Use \code{plot_data()} to inspect and customize the
plotted values.
}
\description{
Draws declared path directions, estimates, scale/correction basis, and any
explicitly routed status. An associational arrow is labeled as descriptive and
is not presented as causal. Response curves use the observed locked-score
support and omit confidence bands when unavailable.
}
\examples{
# plot(association, type = "paths")
# plot(association, type = "curve", outcome = "Y", predictor = "X")
}
