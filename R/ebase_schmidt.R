#' Schmidt number calculation
#'
#' @param temp numeric for temperature (C)
#' @param salt numeric for salinity (PSU)
#'
#' @details
#'  
#' The Schmidt number is calculated for the air-sea gas transfer velocity.
#'
#' @return sc (unitless)
#' @export
#' 
#' @examples
#' temp <- c(10, 20, 30)
#' salt <- c(30, 35, 40)
#' ebase_schmidt(temp = temp, salt = salt)
ebase_schmidt <- function(temp, salt){
  
  salt0 <- 0 #S=0
  param0 <- c(1745.1, -124.34, 4.8055, -0.10115, 0.00086842) # polynomial coeff at S=0
  salt35 <-  35 #S=35
  param35 <- c(1920.4, -135.6, 5.2122, -0.10939, 0.00093777) # polynomial coeff at S=35
  
  # calculate Sc at Salt=0"
  param <- param0
  sc0 <- (param[1]) + (param[2] * temp) + (param[3] * temp^2) + (param[4] * temp^3) + (param[5] * temp^4)
  
  # calculate Sc at Salt=35
  param <- param35
  sc35 <- param[1] + (param[2] * temp) + (param[3] * temp^2) + (param[4] * temp^3) + (param[5] * temp^4)
  
  # linearly interpolate to call Sc at given Salinity
  sc <- sc0 + (salt * ((sc35 - sc0) / (salt35 - salt0)))
  
  return(sc)
  
}