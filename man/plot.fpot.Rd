% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfa_plot_fpot.R
\name{plot.fpot}
\alias{plot.fpot}
\title{Return level plot for peaks over threshold}
\usage{
\method{plot}{fpot}(
  x,
  main = "Return level plot",
  xlab = "Return period (year)",
  ylab = "Flood quantiles",
  ci = FALSE,
  col.ci = 2,
  lty.ci = 2,
  lwd.ci = 1,
  xlim = NULL,
  ylim = NULL,
  ...
)
}
\arguments{
\item{x}{Output from \link{FitPot}.}

\item{main, ylab, xlab}{Graphical parameters. See \code{\link{par}}.}

\item{ci}{Logical. Should confidence intervals be displayed. 
See \link{predict.amax} with argument Delta method.}

\item{col.ci, lty.ci, lwd.ci}{Graphical parameters determining the 
confidence intervals.}

\item{xlim, ylim}{Limits of the graphic.}

\item{...}{Other graphical parameters. See \code{\link{par}}.}
}
\description{
Create a plot of the estimated versus sample flood quantiles, where the 
x-axis is expressed in terms of return period.
}
\examples{

data(flowStJohn)

fit <- FitPot(flow~date, flowStJohn, u = 1000,
               declust = 'wrc', r = 14)

plot(fit, ci = TRUE)
}
\author{
Martin Durocher <mduroche@uwaterloo.ca>
}
