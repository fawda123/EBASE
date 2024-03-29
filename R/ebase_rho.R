#' Seawater density calculation
#'
#' @param temp numeric for temperature (C)
#' @param salt numeric for salinity (PSU)
#' @param P numeric for pressure above atmospheric (dbar)
#'
#' @details
#' 
#' Density of seawater is calculated according to the internationally accepted (UNESCO) equations. The standard error of the equation is 3.6 x 10-3 kg/m-3.
#'
#' @references
#' Millero, F.J., Poisson, A., 1981. International one-atmosphere equation of state of seawater. Deep Sea Research 28, 625-629. https://doi.org/10.1016/0198-0149(81)90122-9
#'
#' @return Rho (kg/m^3)
#' @export
#' 
#' @examples
#' temp <- c(10, 20, 30)
#' salt <- c(30, 35, 40)
#' ebase_rho(temp = temp, salt = salt, P = 0)
ebase_rho <- function(temp, salt, P){
  
  # Define some new variables
  
  P1 <- P/10
  T2 <- temp^2
  T3 <- temp^3
  T4 <- temp^4
  T5 <- temp^5
  S2 <- salt^2
  S32 <- salt^(3/2)
  P2 <- P1^2
  
  # First compute density of fresh water at atmospheric pressure
  
  A <- 8.24493e-1 - 4.0899e-3*temp + 7.6438e-5*T2 - 8.2467e-7*T3 + 5.3875e-9*T4
  B <- -5.72466e-3 + 1.0227e-4*temp - 1.6546e-6*T2
  C <- 4.8314e-4
  
  RH01 <- 999.842594 + 6.793952e-2*temp - 9.095290e-3*T2 + 1.001685e-4*T3 - 1.120083e-6*T4 + 6.536332e-9*T5
  
  # Then add in effect of salinity
  RHOP <- RH01 + A*salt + B*S32 + C*S2
  
  # Then add in effect of pressure
  K0 <- 19652.21 + 148.4206*temp - 2.327105*T2 + 1.360447e-2*T3 - 5.155288e-5*T4
  KA <- 54.6746 - 0.603459*temp + 1.09987e-2*T2 - 6.1670e-5*T3
  KB <- 7.944e-2 + 1.6483e-2*temp -5.3009e-4*T2
  AW <- 3.239908 + 1.43713e-3*temp + 1.16092e-4*T2 - 5.77905e-7*T3
  A1 <- 2.2838e-3 - 1.0981e-5*temp - 1.6078e-6*T2
  A2 <- 1.91075e-4
  BW <- 8.50935e-5 - 6.12293e-6*temp + 5.2787e-8*T2
  B1 <- -9.9348e-7 + 2.0816e-8*temp + 9.1697e-10*T2
  
  K <- K0 + salt*KA + S32*KB + P1*(AW + salt*A1 + S32*A2) + P2*(BW + salt*B1)
  
  RHO <- RHOP/(1-P1/K)
  
  return(RHO)
  
}