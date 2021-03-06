#' @export
#' @rdname FitRegLmom
coef.reglmom <- function(object, distr = NULL, ...){
  
  if(object$type == 'amax'){
    ans <- .CoefRegAmax(object, distr = NULL, ...)
  } else if (object$type == 'pot'){
    ans <- .CoefRegPot(object, distr = NULL, ...)
  }
  
  return(ans)
}

.CoefRegAmax <- function(object, distr = NULL, ...){

  if(is.null(distr))
    distr <- object$distr
  
  ## Extract function lmom -> parameter
  ffunc <- getFromNamespace(paste0('pel', distr), 'lmom')
    
  ## Rescale LCV to L2
  lmom <- object$lmom
  lmom[,2] <- lmom[,2]*lmom[,1]

  ans <- matrix(NA, nrow(lmom), length(object$para))

  for(ii in 1:nrow(lmom)){
    p0 <- try(ffunc(lmom[ii,]))

    if(class(p0) == 'numeric')
      ans[ii, ] <- p0
  }

  colnames(ans) <- names(p0)
  rownames(ans) <- rownames(object$lmom)

  return(ans)
}

.CoefRegPot <- function(object, distr = NULL, ...){
  
  k <- 1 / object$lmom[,2] - 2
  ans <- data.frame(xi = 0, alpha = 1 + k, kappa = k)
  rownames(ans) <- rownames(object$lmom)
  
  return(ans)
}
