
# EBASE

<!-- badges: start -->
[![R-CMD-check](https://github.com/fawda123/EBASE/workflows/R-CMD-check/badge.svg)](https://github.com/fawda123/EBASE/actions)
<!-- badges: end -->

This an R package for Estuarine Bayesian Single-station Estimation method for ecosystem metabolism.  

### Installation

Install the package as follows:

``` r
# Enable the r-universe repo
options(repos = c(
    fawda123 = 'https://fawda123.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install
install.packages('EBASE')
```

### Minimal example

```
library(dplyr)
library(lubridate)
library(doParallel)

# get four days of data
dat <- exdat %>%
  filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:4)

##
# run ebase with defaults

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 2)
registerDoParallel(cl)

res <- ebase(dat, interval = 900, H = 1.85, progress = T)

stopCluster(cl)
```