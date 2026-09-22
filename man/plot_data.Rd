\name{plot_data}
\alias{plot_data}
\alias{plot_data.default}
\alias{plot_data.cssem_association}
\alias{plot_data.conditional_slopes}
\alias{plot_data.indirect_effect}
\alias{plot_data.evidence_report}
\alias{plot_data.fit_states}
\title{Extract Data Used by CS-SEM Plots}
\usage{
plot_data(x, ...)

\method{plot_data}{default}(x, ...)
\method{plot_data}{cssem_association}(x,
  type = c("paths", "curve"), routing = NULL, outcome = NULL,
  predictor = NULL, n = 101L, at = NULL, ...)
\method{plot_data}{conditional_slopes}(x,
  type = c("slopes", "johnson_neyman"), ...)
\method{plot_data}{indirect_effect}(x, ...)
\method{plot_data}{evidence_report}(x,
  type = c("effects", "constructs", "causal_claims"), ...)
\method{plot_data}{fit_states}(x,
  type = c("convergence", "scores", "redundancy", "item_loss"), ...)
}
\arguments{
\item{x}{A CS-SEM result object supported by a plot-data method.}

\item{...}{Additional arguments passed to class-specific methods.}

\item{type}{The plot-data section to return.}

\item{routing}{Optional path-routing result used to show edge interpretation and status.}

\item{outcome}{Endogenous construct for a response curve.}

\item{predictor}{Single-construct predictor for a response curve.}

\item{n}{Number of points in a response curve; must be at least two.}

\item{at}{Optional named locked-score values at which other inputs are held.}
}
\value{
A data frame retaining the values, units, basis, interval availability, and
routing status relevant to the selected plot. Path-diagram data also carries a
\code{nodes} data frame as an attribute. Missing confidence bounds remain
\code{NA}; unavailable intervals are not replaced with zero-width intervals.
}
\description{
Extract the data behind CS-SEM plots for customization or use with a different
graphics system. Base graphics plots can be written to static files with
standard R graphics devices, including \code{grDevices::pdf()} and
\code{grDevices::png()}.
}
\examples{
# path_data <- plot_data(association, type = "paths")
# curve_data <- plot_data(association, type = "curve", outcome = "Y", predictor = "X")
# convergence_data <- plot_data(fit, type = "convergence")
}
