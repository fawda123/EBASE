#' Prepare data for ebase
#'
#' Prepare data for ebase
#' 
#' @param dat input data frame
#' @param H numeric as single value for water column depth (m) or vector equal in length to number of rows in \code{dat}
#' @param interval timestep interval in seconds
#' @param ndays numeric for number of days in \code{dat} for optimizing the metabolic equation, see details
#' 
#' @importFrom fwoxy fun_schmidt_oxygen fun_eqb_oxygen
#' 
#' @details Checks if all columns are present by matching those in \code{\link{exdat}}, converts dissolved oxygen from mg/L to mmol/m3, calculates the Schmidt number (unitless) from water temp (C) and salinity (psu), and calculates dissolved oxygen equilibrium concentration (mmol/m3) from salinity and temperature
#' 
#' The \code{ndays} argument defines the number of days that are used for optimizing the above mass balance equation.  By default, this is done each day, i.e., \code{ndays= 1} such that a loop is used that applies the model equation to observations within each day, evaluated iteratively from the first observation in a day to the last.  Individual parameter estimates for \emph{a}, \emph{R}, and \emph{b} are then returned for each day.  However, more days can be used to estimate the unknown parameters, such that the loop can be evaluated for every \code{ndays} specified by the argument.  The \code{ndays} argument will separate the input data into groups of consecutive days, where each group has a total number of days equal to \code{ndays}.  The final block may not include the complete number of days specified by \code{ndays} if the number of unique dates in the input data includes a remainder when divided by \code{ndays}, e.g., if seven days are in the input data and \code{ndays = 5}, there will be two groups where the first has five days and the second has two days. The output data from \code{\link{ebase}} includes a column that specifies the grouping that was used based on \code{ndays}.
#' 
#' Missing values are interpolated at the interval specified by the \code{interval} argument for conformance with the core model equation. Records at the start or end of the input time series that do not include a full day are also removed.  A warning is returned to the console if gaps are found or dangling records are found. 
#' 
#' @return A data frame with additional columns required for \code{\link{ebase}}. Dissolved oxygen as a volumetric concentration in \code{dat} as mg/L is returned in areal units as mmol/m2. If multiple time steps are identified, the number of rows in data frame is expanded based on the time step define by \code{interval}.  Numeric values in the expanded rows will be interpolated if \code{interp = TRUE}, otherwise they will remain as \code{NA} values.
#' 
#' @importFrom dplyr %>%
#' 
#' @export
#' 
#' @examples 
#' dat <- ebase_prep(exdat, H = 1.85, interval = 900)
#' head(dat)
ebase_prep <- function(dat, H, interval, ndays = 1){
  
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

  # check if H is equal in length to dat if not equal to 1
  lenh <- length(H)
  chk <- !(lenh == 1 | lenh == nrow(dat))
  if(chk){
    msg <- paste0('Supplied value for H has length ', lenh, ', should be 1 or ', nrow(dat))
    stop(msg)
  }

  # sort dat by DateTimeStamp, add H
  dat <- dat %>% 
    dplyr::arrange(DateTimeStamp) %>% 
    dplyr::mutate(
      H = H
    )
  
  # check if dangling start or stop observations
  # interpolation must be done first
  dat <- dat %>% 
    dplyr::mutate(
      Date = as.Date(DateTimeStamp, tz = attr(DateTimeStamp, 'tzone'))
    ) %>% 
    dplyr::group_by(Date) %>% 
    dplyr::mutate(
      uniobs = length(Date)
    ) %>% 
    dplyr::ungroup()
  chk <- c(dat$uniobs[1], dat$uniobs[nrow(dat)]) != 86400 / interval
  if(any(chk)){
    if(chk[1]){
      msg <- 'Incomplete daily observations removed at start of dat'
      warning(msg, call. = FALSE)
      dat <- dat %>% 
        dplyr::filter(!Date %in% min(Date))
    }
    if(chk[2]){
      msg <- 'Incomplete daily observations removed at end of dat'
      warning(msg, call. = FALSE)
      dat <- dat %>% 
        dplyr::filter(Date != max(Date))
    }
  }
  dat <- dat %>% 
    dplyr::select(-uniobs, -Date)

  # check if more than one time step or missing obs
  dat <- na.omit(dat)
  chk <- diff(dat$DateTimeStamp) %>% 
    as.numeric(units = 'secs') %>% 
    unique
  
  intervalmin <- paste(interval / 60, "min")

  dat <- data.frame(
      DateTimeStamp = seq(min(dat$DateTimeStamp), max(dat$DateTimeStamp), by = intervalmin)
    ) %>% 
    dplyr::left_join(., dat, by = 'DateTimeStamp') %>% 
    dplyr::mutate(isinterp = FALSE)

  # interpolate missing values
  if(length(chk) > 1){

    msg <- 'More than one time step or missing values will be interpolated'
    warning(msg, call. = FALSE)

    dat <- dat %>% 
      dplyr::mutate(isinterp = rowSums(is.na(.)) > 0) %>% 
      dplyr::mutate_if(is.numeric, function(x){
        
        interp <- zoo::na.approx(x, na.rm = FALSE)
        
        return(interp)
        
      })

  }
  
  # convert DO to mmol/m2
  # add schmidt number as unitless
  # add DO sat as mmol/m2
  dat <- dat %>% 
    dplyr::mutate(
      DO_obs = H * DO_obs / 32 * 1000,
      sc = fun_schmidt_oxygen(Temp, Sal), 
      DO_sat = H * fun_eqb_oxygen(Temp, Sal), 
      Date = as.Date(DateTimeStamp, tz = attr(DateTimeStamp, 'tzone'))
    ) %>% 
    dplyr::select(Date, DateTimeStamp, isinterp, DO_obs, DO_sat, H, dplyr::everything())
  
  # add groups defined by ndays to out
  # its an even cut by ndays with remainder assigned
  out <- dat %>% 
    dplyr::mutate(
      grp = as.numeric(Date),
      grp = grp - min(grp) + 1
    )

  if(ndays >= length(unique(out$grp))) {
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