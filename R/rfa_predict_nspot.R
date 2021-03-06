##############################################################################
#' Flood quantiles for the nonstationary POT model
#' 
#' Predict the return levels or design level of the nonstationary POT model.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output form \cite{FitNsPot}.
#' 
#' @param rt Vector of return period to estimate.
#' 
#' @param newdata Dataset of the covariates at the predicted dates.
#' 
#' @param reliability Logical. Should the reliability be returned instead of the
#'   flood quantiles.  
#'   
#' @param reg.kappa Optional value to overwrite the shape parameter. Can be
#'   used to obtain flood quantile using a regional estimate.
#' 
#' @param ... Other parameters.
#'
#' @details 
#' 
#' The reliability is defined as the probability that there is no event that 
#' will exceed as given design level for a specific period. 
#' When \code{reliability = TRUE}, the elements \code{newdata} are used to define
#' that period. The reliability level is control 
#' by \code{p} and is \code{p^s} where \code{s} is the size of \code{newdata}.
#' As the data are daily, the exceeding probability for a given year could be
#' approximated by the value of one days, for instance July 15th. 
#' Therefore the reliability of the last 30 years could be approximated by the 
#' reliability of the last 30 July 15th. Daily data can also be passed but at
#' a greater computational cost. 
#' 
#' For the function \code{simulate}, if no dataset is passed to \code{newdata}, 
#' the simulation is done at the date of the extracted peaks. 
#' If a dataset is passed, for each date of the dataset the probability of being
#' a peaks is determined based on the exceedance rate of the model and 
#' follows an homogenous Poisson process.
#' 
#' @references 
#' 
#' Durocher, M., Burn, D. H., & Ashkar, F. (2019). Comparison of estimation 
#'   methods for a nonstationary index-flood model in flood frequency analysis 
#'   using peaks over threshold [Preprint]. 
#'   https://doi.org/10.31223/osf.io/rnepc
#'   
#' @seealso \link{predict.nspot}, \link{FitNsAmax}. 
#'
#' @export
#'
#' @examples
#' 
#' data(flowStJohn)
#'  
#' ## Fit the data using the ns-index-flood model 
#' fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95,
#'                 trend = ~ date, thresh = ~ date, declust = 'wrc', r = 14)
#' 
#' plot(fit, legend = FALSE)
#' 
#' ## Create a datasets of the July 15th to use as annual values 
#' id <- which.day(flowStJohn$date)
#' xref <- flowStJohn[id, ]
#' 
#' ## Add the return levels to the graphics 
#' qhat <- predict(fit, rt = c(10, 100), newdata = xref)
#' 
#' for(ii in 1:2) lines(xref$date, qhat[,ii], col = 'magenta', lty = 2)
#' 
#' ## Compute the design level of the last 30 years
#' xref30 <- xref[59:88,]
#' qhat <- predict(fit, rt = c(10, 50), reliability = TRUE, newdata =xref30)
#' 
#' for(ii in 1:2) 
#'   arrows(min(xref30$date), qhat[ii], max(xref30$date), 
#'          lwd = 2, col = 'cyan',
#'          code = 3, angle = 90, length = .05)
#' 
#' ## evaluating the flood quantiles assuming that a regional estimate of 
#' ## the shape was available.        
#' predict(fit, rt = c(10, 100), newdata = xref30, reg.kappa = .3)
#' 
predict.nspot <- 
  function(object, 
           rt = c(2, 5, 10, 20, 50, 100), 
           newdata = NULL, 
           reliability = FALSE,
           reg.kappa = NULL, ...){
  
  if(is.null(reg.kappa)){
    para <- coef(object, 'kappa')
  } else{
    para <- reg.kappa + c(1,0)
  }
    
  if(is.null(newdata)){
    ftd <- fitted(object)
  } else{
    ftd <- fitted(object, newdata)
  }
 
  ## quantile without trend ##
  p <- 1 - 1 / (object$ppy * rt)
  qua <- qgpa(p, para[1], para[2])
  
  if(ncol(object$trend$data) == 0 & ncol(object$threshold$data) == 0){
    ans <- qua * object$trend$beta + object$threshold$beta
    names(ans) <- paste0('q',round(rt,2))
    return(ans)
  }

  ## quantile with trend ##
  
  quas <- outer(ftd$trend, qua)
  quas <- apply(quas, 2, '+', ftd$thresh)
  
  if(!reliability){
    colnames(quas) <- paste0('q',round(rt,2))
    return(quas)
  }

  
  ##----- reliability -------##

  lp <- log(p) * nrow(ftd)
  
  Frel <- function(z, jj){
    u <- (z-ftd$thresh)/ftd$trend
    lp[jj]-sum(log(pgpa(u, para[1], para[2])))
  }
    
  ## Determine boundary for searching design level
  bnd <- apply(quas, 2, range)
  bnd[1,] <- bnd[1,] - sqrt(.Machine$double.eps)
  bnd[2,] <- bnd[2,] + sqrt(.Machine$double.eps)
  
  ## Evaluate all design levels
  rel <- rep(0,length(lp))
  for(jj in seq_along(lp))
    rel[jj] <- uniroot(Frel, bnd[,jj], jj)$root
  
  names(rel) <- paste0('q',round(rt,2))
  return(rel)
}
