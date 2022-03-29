
# EBASE

<!-- badges: start -->
[![R-CMD-check](https://github.com/fawda123/EBASE/workflows/R-CMD-check/badge.svg)](https://github.com/fawda123/EBASE/actions)
[![pkgdown](https://github.com/fawda123/EBASE/workflows/pkgdown/badge.svg)](https://github.com/fawda123/EBASE/actions)
[![codecov](https://codecov.io/gh/fawda123/EBASE/branch/main/graph/badge.svg?token=45Y7PDEHXA)](https://codecov.io/gh/fawda123/EBASE)
<!-- badges: end -->

This an R package for Estuarine BAyesian Single-station Estimation method for ecosystem metabolism.  

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

The [JAGS](https://mcmc-jags.sourceforge.io/) software must also be installed to use this package.  Follow the instructions in the link to download and install the version appropriate for your operating system.   

### Minimal example

``` r
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

res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE)

stopCluster(cl)
```