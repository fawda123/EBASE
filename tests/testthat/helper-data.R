library(dplyr)
library(EBASE)
library(tidyr)

# get two days of data
dat <- exdat %>%
  filter(lubridate::month(exdat$DateTimeStamp) == 6 & lubridate::day(exdat$DateTimeStamp) %in% 1:3)

res <- EBASE::ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2, 
                model_file = system.file("ebase_model.txt", package = "EBASE")) 