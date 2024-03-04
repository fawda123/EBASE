library(dplyr)
library(lubridate)

# get four days of data
dat <- exdat %>%
  filter(month(DateTimeStamp) == 6 & day(DateTimeStamp) %in% 1:4)

exres <- ebase(dat, interval = 900, Z = 1.85, progress = FALSE, n.chains = 2)

usethis::use_data(exres, overwrite = TRUE)