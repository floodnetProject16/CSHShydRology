#############################################
#' Return level plot
#' 
#' Create a plot of the estimated versus sample flood quantiles, where the 
#' x-axis is expressed in terms of return period.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Output from \link{FitAmax}.
#'
#' @param ci Logical. Should confidence intervals be displayed. 
#'   See \link{predict.amax} with argument Delta method.
#'   
#' @param main,ylab,xlab Graphical parameters. See \code{\link{par}}.
#'
#' @param col.ci,lty.ci,lwd.ci Graphical parameters determining the 
#'   confidence intervals.
#'
#' @param ... Other graphical parameters. See \code{\link{par}}.
#'
#' @export
#'
#' @examples
#'
#' data(flowStJohn)
#' 
#' x <- ExtractAmax(flow~date, flowStJohn)$flow
#'
#' fit <- FitAmax(x, distr = 'gev', method = 'mle')
#'
#' plot(fit, ci = TRUE)

plot.amax <- 
  function(x, 
           main = 'Return level plot',
           xlab = 'Return period (year)',
           ylab = 'Flood quantiles',
           ci = FALSE, 
           col.ci = 'red', 
           lty.ci = 2,
           lwd.ci = 1, 
           ...){

      n <- length(x$data)
      xd <- sort(x$data)
      p <- x$para
      nrt <- 200

      ## Plotting axis
      xat <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 5000, 10000)

      prt <- (seq(n) - .5) / n
      ip <- seq(min(prt), max(prt), l = nrt)
      llip <- -log(-log(ip))

      pat <- 1-1/xat
      irt <- 1-1/ip

      ## Plot empirical points
      plot(-log(-log(prt)), xd, axes = FALSE, main = main,
           ylab = ylab, xlab = xlab, ...)

      axis(2)

      axis(1, at = -log(-log(pat)), labels = xat)

      ## if Confident interval are to be plotted
      if(ci){

        bnd <- predict(x, q = ip, se = FALSE, ci = 'delta')

        lines(llip, bnd[,1], col = col.ci, lwd = lwd.ci)

        lines(llip,smooth.spline(irt,bnd[,2])$y,
              lty = lty.ci, col = col.ci, lwd = lwd.ci)

        lines(llip,smooth.spline(irt,bnd[,3])$y,
              lty = lty.ci, col = col.ci, lwd = lwd.ci)

      } else{
        bnd <- predict(x, q = ip, se = FALSE, ci = 'none')
        lines(llip, bnd, col = col.ci, lwd = lwd.ci)
      }
}