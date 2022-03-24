#' Update metabolism jags fit
#' 
#' Update metabolism jags fit
#' 
#' @param metabfit initial jags metabolism output
#' @param update.chains logical to update, only if TRUE
#' @param n.iter number of iterations 
#' 
#' @return Updated jags metabolism output
#'
#' @export
#' 
#' @details This function is used by \code{\link{ebase}} and is not to be called directly by the user.  It provides additional model iterations if convergence is not achieved.
metab_update <- function(metabfit, update.chains, n.iter){
  
  ## diagnostic summaries
  # Rhat (srf) test
  srf <- metabfit$BUGSoutput$summary[,8]
  Rhat.test <- NULL
  Rhat.test <- ifelse(any(srf > 1.1, na.rm = T) == TRUE, "Check convergence", "Fine")
  
  # Check for convergence and update once if requested
  if(update.chains == TRUE) {
    if(Rhat.test == "Check convergence") {
      recompile(metabfit)
      metabfit <- update(metabfit, n.iter=n.iter*1) 
      
      # Rhat (srf) test - second round in case metabfit is updated
      srf <- metabfit$BUGSoutput$summary[,8]
      Rhat.test <- NULL
      Rhat.test <- ifelse(any(srf > 1.1, na.rm = T) == TRUE, "Check convergence", "Fine")
    }
  }
  
  return(metabfit)
  
}