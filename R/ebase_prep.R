#' Prepare data for ebase
#'
#' Prepare data for ebase
#' 
#' @param dat input data frame
#' @param H numeric as single value for water column depth (m) or vector equal in length to number of rows in \code{dat}
#' 
#' @importFrom fwoxy fun_schmidt_oxygen fun_eqb_oxygen
#' 
#' @details Checks if all columns are present by matching those in \code{\link{exdat}}, converts dissolved oxygen from mg/L to mmol/m3, calculates the Schmidt number (unitless) from water temp (C) and salinity (psu), and calculates dissolved oxygen equilibrium concentration (mmol/m3) from salinity and temperature
#' 
#' @return A data frame with additional columns required for \code{\link{ebase}}
#' 
#' @importFrom dplyr %>%
#' 
#' @export
#' 
#' @examples 
#' ebase_prep(exdat, H = 1.85)
ebase_prep <- function(dat, H){
  
  ##
  # sanity checks
  
  # check if missing input data
  chk <- !names(exdat) %in% names(dat)
  if(any(chk)){
    msg <- names(exdat)[chk] %>% 
      paste(., collapse = ', ') %>% 
      paste('Missing the following columns:', .)
    
    stop(msg)
  }
  
  # # check if more than one time step
  # chk <- unique(diff(dat$DateTimeStamp))
  # if(length(chk) > 1){
  #   msg <- paste(chk, collapse = ', ') %>% 
  #     paste('More than one time step observed:', .)
  #   
  #   stop(msg)
  # }  
  
  # convert DO to mmol/m3
  # add schmidt number as unitless
  # add DO sat as mmol/m3
  out <- dat %>% 
    dplyr::mutate(
      DO_obs = DO_obs / 32 * 1000,
      sc = fun_schmidt_oxygen(Temp, Sal), 
      DO_sat = fun_eqb_oxygen(Temp, Sal), 
      H = H, 
      Date = as.Date(DateTimeStamp, tz = attr(DateTimeStamp, 'tzone'))
    ) %>% 
    dplyr::select(Date, DateTimeStamp, DO_obs, DO_sat, H, dplyr::everything())
  
  return(out)
  
}