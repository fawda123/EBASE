library(dplyr)
library(EBASE)

# get four days of data
dat <- exdat %>%
  filter(lubridate::month(exdat$DateTimeStamp) == 6 & lubridate::day(exdat$DateTimeStamp) %in% 1:2)

res <- EBASE::ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2, 
                model_file = file.path(system.file(package="EBASE"), "ebase_model.txt")) 