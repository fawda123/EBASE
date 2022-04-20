library(lubridate)
library(dplyr)
library(EBASE)

# get four days of data
dat <- exdat %>%
  filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:2)

res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2, 
                model_file = file.path(system.file(package="EBASE"), "ebase_model.txt")) 