#' Prepare data for ebase
#'
#' Prepare data for ebase
#' 
#' @param dat input data frame
#' @param H numeric as single value for water column depth (m) or vector equal in length to number of rows in \code{dat}
#' @param ndays numeric for number of days in \code{dat} for optimizing the metabolic equation, see details
#' 
#' @importFrom fwoxy fun_schmidt_oxygen fun_eqb_oxygen
#' 
#' @details Checks if all columns are present by matching those in \code{\link{exdat}}, converts dissolved oxygen from mg/L to mmol/m3, calculates the Schmidt number (unitless) from water temp (C) and salinity (psu), and calculates dissolved oxygen equilibrium concentration (mmol/m3) from salinity and temperature
#' 
#' The \code{ndays} argument defines the number of days that are used for optimizing the above mass balance equation.  By default, this is done each day, i.e., \code{ndays= 1} such that a loop is used that applies the model equation to observations within each day, evaluated iteratively from the first observation in a day to the last.  Individual parameter estimates for \emph{a}, \emph{r}, and \emph{b} are then returned for each day.  However, more days can be used to estimate the unknown parameters, such that the loop can be evaluated for every \code{ndays} specified by the argument.  The \code{ndays} argument will separate the input data into groups of consecutive days, where each group has a total number of days equal to \code{ndays}.  The final block may not include the complete number of days specified by \code{ndays} if the number of unique dates in the input data includes a remainder when divided by \code{ndays}, e.g., if seven days are in the input data and \code{ndays = 5}, there will be two groups where the first has five days and the second has two days. The output data from \code{\link{ebase}} includes a column that specifies the grouping that was used based on \code{ndays}.
#' 
#' @return A data frame with additional columns required for \code{\link{ebase}}
#' 
#' @importFrom dplyr %>%
#' 
#' @export
#' 
#' @examples 
#' dat <- ebase_prep(exdat, H = 1.85)
#' head(dat)
ebase_prep <- function(dat, H, ndays = 1){
  
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

  # add groups defined by ndays to out
  # its an even cut by ndays with remainder assigned
  out <- out %>% 
    dplyr::mutate(
      grp = as.numeric(Date),
      grp = grp - min(grp) + 1
    )

  if(length(unique(out$grp)) >= ndays) {
    out$grp <- 1
  } else {
    brks <- seq(min(out$grp), max(out$grp), by = ndays)
    out$grp <- as.numeric(cut(out$grp, breaks = brks, right = FALSE))
    maxgrp <- unique(out$grp) %>% max(na.rm = T)
    maxgrp <- 1 + maxgrp
    out$grp <- ifelse(is.na(out$grp), maxgrp, out$grp)
  }
  
  return(out)
  
}