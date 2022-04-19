#' Sample data from Appalachicola NERRS
#'
#' Sample data from Appalachicola NERRS
#'
#' @format A \code{data.frame} object with 27645 rows and 6 columns
#' \describe{
#'   \item{DateTimeStamp}{date and time, Pacific/Jamaica time zone, 15 minute time step}
#'   \item{DO_obs}{dissolved oxygen, mg/L}
#'   \item{Temp}{water temperature, C}
#'   \item{Sal}{salinity, psu}
#'   \item{PAR}{total PAR, W/m2/s}
#'   \item{WSpd}{num, m/s}
#' }
#' @family utilities
#' 
#' @details The DO time series has been detided using weighted regression in WtRegDO: \url{https://github.com/fawda123/WtRegDO}
#' 
#' @examples 
#' 
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' exdat <- read.csv('data-raw/APNERR2012dtd.csv') %>% 
#'   rename(
#'     PAR = I,
#'     Sal = salinity,
#'     Temp = tempC, 
#'     DO_obs = DO.meas
#'   ) %>% 
#'   unite('DateTimeStamp', Date, Time, sep = ' ') %>% 
#'   mutate(
#'     DateTimeStamp = lubridate::ymd_hms(DateTimeStamp, tz = 'America/Jamaica')
#'   ) %>% 
#'   select(DateTimeStamp, DO_obs, Temp, Sal, PAR, WSpd) 
#' 
#' save(exdat, file = 'data/exdat.RData', compress = 'xz')
#'}
"exdat"