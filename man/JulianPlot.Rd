% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfa_JulianPlot.R
\name{JulianPlot}
\alias{JulianPlot}
\title{Plotting Julian date}
\usage{
JulianPlot(
  rose.col = "gray40",
  rose.lwd = 1.5,
  rose.cex = 1.5,
  rose.radius = seq(0.25, 1, 0.25),
  ...
)
}
\arguments{
\item{rose.col, rose.lwd, rose.cex}{Property of the polar axes.}

\item{rose.radius}{Vector of the position of the circular axis.}

\item{...}{Other parameter passed to \link{points}.}
}
\description{
Create axis for plotting circular statistics in a unitary circle.
}
\examples{

data(flowAtlantic)

ss <- SeasonStat(date ~ id, flowAtlantic$ams)

JulianPlot()
points(y ~ x, ss, pch = 16, col = cut(ss[,'radius'], c(0,.5,.75,1)))

}
\seealso{
\link{SeasonStat}.
}
\author{
Martin Durocher <mduroche@uwaterloo.ca>
}
