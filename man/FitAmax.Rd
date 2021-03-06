% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfa_FitAmax.R
\name{FitAmax}
\alias{FitAmax}
\title{At-site frequency analysis using annual maximums}
\usage{
FitAmax(
  x,
  distr = c("gev", "gno", "pe3", "glo"),
  method = "lmom",
  varcov = TRUE,
  nsim = 500,
  k = 2,
  ...,
  tol.gev = 0
)
}
\arguments{
\item{x}{Data.}

\item{distr}{Distribution to fit. See \code{\link{Amax}} for the list
of available distribution.}

\item{method}{Estimation method.
Either maximum likelihood ('mle') or L-moments ('lmom').}

\item{varcov}{Should the variance-covariance matrix of the parameters
be computed. For \code{mle} the covariance matrix is derived from the
hessian matrix. For L-moments, non-parametric bootstrap is used.}

\item{nsim}{Number of simulations used to evaluate the covariance matrix
when using L-moment estimator.}

\item{k}{Penality terms for the AIC.}

\item{...}{Other parameters.}

\item{tol.gev}{Accepted difference between the AIC of the GEV and the best 
best distribution. If the difference is inferior to \code{tol.gev}, the 
GEV distribution is prefered.}
}
\value{
\item{data}{Data Values.}
\item{lmom}{L-moments.}
\item{para}{Parameter estimates.}
\item{varcov}{Covariance matrix of the parameter}
\item{llik}{Value of the log-likelihood}
}
\description{
Return a fitting of a distribution, normally representing annual maximums.
Both maximum likelihood and L-moments estimation methods are available.
If the maximum likelihood is used and fails, the L-moments solution
will be returned with a warning message is issued.
When more than one distribution is passed. The
best distribution is selected automatically according to the AIC criteria.
}
\section{References}{


Coles, S. (2001). An introduction to statistical
  modeling of extreme values. Springer Verlag.

Hosking, J. R. M., & Wallis, J. R. (1997). Regional frequency analysis:
  an approach based on L-moments. Cambridge Univ Pr.
}

\examples{

## Extract a time series of annual maxima
x <- ExtractAmax(flow~date, flowStJohn)$flow

## Fitting of GEV distribution using L-moments

fit <- FitAmax(x,'gev')
print(fit)
coef(fit)
AIC(fit)
fit$lmom

## The evaluation of the variance-covariance matrix can be turn down
fit <- FitAmax(x,'gev', varcov = FALSE)

## Using Maximum likelihood
fit <- FitAmax(x,'gev', method ='mle')
print(fit)
vcov(fit)

## Standard deviation of the parameter
sqrt(diag(vcov(fit)))

## Chose the best distribution according to AIC
FitAmax(x, distr = c('gev','glo','gno','pe3'), method = 'mle')

}
\seealso{
\link{predict.amax}, \link{GofTest}, \link{plot.amax}.
}
\author{
Martin Durocher <mduroche@uwaterloo.ca>
}
