#' Oxygen saturation
#'
#' @param temp numeric for temperature (C)
#' @param salt numeric for salinity (PSU)
#'
#' @details
#'
#' Function to calculate equilibrium OXYGEN concentration in seawater, from water temparure (C) and salinity (PSU)
#' @references 
#' Garcia, H., Gordon, L.I., 1992. Oxygen solubility in seawater: Better fitting equations. Limnology and Oceanography 37, 1307-1312. https://doi.org/10.4319/lo.1992.37.6.1307
#'
#' @return oxysat (mmol/m^3)
#' @export
#' 
#' @examples
#' temp <- c(10, 20, 30)
#' salt <- c(30, 35, 40)
#' ebase_eqboxy(temp = temp, salt = salt)
ebase_eqboxy <- function(temp, salt){
  
  # coefficients
  A_0 <- 5.80818
  A_1 <- 3.20684
  A_2 <- 4.11890
  A_3 <- 4.93845
  A_4 <- 1.01567
  A_5 <- 1.41575
  B_0 <- -7.01211e-3
  B_1 <- -7.25958e-3
  B_2 <- -7.93334e-3
  B_3 <- -5.54491e-3
  C_0 <- -1.32412e-7
  
  ts <- log((298.15 - temp)/(273.15 + temp)) # scaled temp
  
  eqoxy <- A_0 + A_1*ts + A_2*(ts^2) + A_3*(ts^3) + A_4*(ts^4) +
    A_5*(ts^5) + salt*(B_0 + B_1*ts + B_2*(ts^2) + B_3*(ts^3)) + C_0*(salt^2)
  
  eqoxy1 <- exp(eqoxy) # umol/kg
  
  oxysat <- eqoxy1 * ebase_rho(temp = temp, salt = salt, P = 0) * 1e-3 # mmol/m3
  
  return(oxysat)
  
}