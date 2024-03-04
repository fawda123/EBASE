library(dplyr)
library(tidyr)

exdat <- read.csv('inst/APNERR2012.csv') %>%
  rename(
    Sal = salinity,
    Temp = tempC,
    DO_obs = DO.meas
  ) %>%
  unite('DateTimeStamp', Date, Time, sep = ' ') %>%
  mutate(
    DateTimeStamp = lubridate::mdy_hm(DateTimeStamp, tz = 'America/Jamaica')
  ) %>%
  select(DateTimeStamp, DO_obs, Temp, Sal, PAR, WSpd)

usethis::use_data(exdat, overwrite = TRUE)