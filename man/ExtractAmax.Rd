% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfa_ExtractAmax.R, R/rfa_ExtractMmax.R
\name{ExtractAmax}
\alias{ExtractAmax}
\alias{ExtractAmax.formula}
\alias{ExtractAmax.default}
\alias{ExtractMmax}
\alias{ExtractMmax.default}
\alias{ExtractMmax.formula}
\title{Extracts the annual maximums of a daily time series}
\usage{
ExtractAmax(x, ...)

\method{ExtractAmax}{formula}(form, x, tol = 0, ...)

\method{ExtractAmax}{default}(x, tol = 0, nlab = "n", ylab = "yy", ...)

ExtractMmax(x, ...)

\method{ExtractMmax}{default}(x, tol = 28, ...)

\method{ExtractMmax}{formula}(form, x, tol = 28, ...)
}
\arguments{
\item{x}{Data. If no formula is passed, the first column must be the
value and the second the date.}

\item{...}{Other parameters.}

\item{form}{Formula of the form \code{value ~ date} that specifies the
variable from which the annual maximums are extracted and a date variable.}

\item{tol}{Filter the years having less than \code{tol} days.}

\item{nlab, ylab}{Names for the added columns representing respectively 
the number of yearly observations and the year. 
If set to NULL the given column is not added.}
}
\description{
Returns a dataset containing the annual (Monthly) maximums,
the date and the number of observations during the year.
}
\examples{

out <- ExtractAmax(flow ~ date, flowStJohn, tol = 350)
head(out)

}
\author{
Martin Durocher <mduroche@uwaterloo.ca>
}
