
# EBASE

<!-- badges: start -->
[![R-CMD-check](https://github.com/fawda123/EBASE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fawda123/EBASE/actions/workflows/R-CMD-check.yaml)
[![EBASE status badge](https://fawda123.r-universe.dev/badges/EBASE)](https://fawda123.r-universe.dev)
[![pkgdown](https://github.com/fawda123/EBASE/workflows/pkgdown/badge.svg)](https://github.com/fawda123/EBASE/actions)
[![Codecov test coverage](https://codecov.io/gh/fawda123/EBASE/branch/main/graph/badge.svg)](https://app.codecov.io/gh/fawda123/EBASE?branch=main)
<!-- badges: end -->

R package for Estuarine BAyesian Single-station Estimation (EBASE) method for ecosystem metabolism.

### Installation

Install the package as follows:

``` r
# Install EBASE in R:
install.packages('EBASE', repos = c('https://fawda123.r-universe.dev', 'https://cloud.r-project.org'))
```

The [JAGS](https://mcmc-jags.sourceforge.io/) software must also be installed to use this package.  Follow the instructions in the link to download and install the version appropriate for your operating system.   

### Minimal example

``` r
library(dplyr)
library(lubridate)
library(doParallel)

# get four days of data
dat <- exdat %>%
  filter(month(DateTimeStamp) == 6 & day(DateTimeStamp) %in% 1:4)

##
# run ebase with defaults

# setup parallel backend
cl <- makeCluster(2)
registerDoParallel(cl)

res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE)

stopCluster(cl)
```
