\name{plot.conditional_slopes}
\alias{plot.conditional_slopes}
\title{Plot Conditional Slopes and Johnson-Neyman Results}
\usage{
\method{plot}{conditional_slopes}(x,
  type = c("slopes", "johnson_neyman"), ...)
}
\arguments{
\item{x}{A \code{conditional_slopes} object from \code{conditional_slopes()}.}

\item{type}{\code{"slopes"} for simple slopes or \code{"johnson_neyman"} for
the bootstrap confidence region.}

\item{...}{Additional arguments passed to base graphics.}
}
\value{
\code{x}, invisibly. Use \code{plot_data()} to inspect and customize the
plotted values.
}
\description{
Plots the conditional slope estimates and, when available, bootstrap intervals
and the Johnson-Neyman significance region. Missing intervals are omitted.
Results are explicitly associational.
}
\examples{
# plot(slopes, type = "slopes")
# plot(slopes, type = "johnson_neyman")
}
