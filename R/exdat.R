#' Sample data from Apalachicola NERRS
#'
#' Sample data from Apalachicola NERRS
#'
#' @format A \code{data.frame} object with 27648 rows and 6 columns
#' \describe{
#'   \item{DateTimeStamp}{date and time, America/Jamaica time zone, 15 minute time step}
#'   \item{DO_obs}{dissolved oxygen, mg/L}
#'   \item{Temp}{water temperature, C}
#'   \item{Sal}{salinity, psu}
#'   \item{PAR}{total PAR, W/m2}
#'   \item{WSpd}{num, m/s}
#' }
#' @family utilities
#' 
#' @examples 
#' 
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' exdat <- read.csv('inst/APNERR2012.csv') %>% 
#'   rename(
#'     Sal = salinity,
#'     Temp = tempC, 
#'     DO_obs = DO.meas
#'   ) %>% 
#'   unite('DateTimeStamp', Date, Time, sep = ' ') %>% 
#'   mutate(
#'     DateTimeStamp = lubridate::mdy_hm(DateTimeStamp, tz = 'America/Jamaica')
#'   ) %>% 
#'   select(DateTimeStamp, DO_obs, Temp, Sal, PAR, WSpd) 
#' 
#' save(exdat, file = 'data/exdat.RData', compress = 'xz')
#'}
"exdat"