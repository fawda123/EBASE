library(dplyr)
library(EBASE)
library(tidyr)

# get three days of data
dat <- exdat %>%
  filter(lubridate::month(DateTimeStamp) == 6) %>% 
  filter(lubridate::day(DateTimeStamp) %in% 1:3)