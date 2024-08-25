#' Estuarine Bayesian Single-station Estimation method for ecosystem metabolism for long time series
#' 
#' Estuarine Bayesian Single-station Estimation method for ecosystem metabolism for long time series
#' 
#' @inheritParams ebase
#' @param ncores numeric for number of cores to use for parallel processing, use \code{NULL} to suppress
#' @param quiet logical to suppress progress messages to the console
#' @param maxtry numeric for maximum number of times to retry the model if it fails
#'
#' @details \code{\link{ebase}} is run for each year in the supplied data. This facilitates running \code{\link{ebase}} on long time series by running the model sequentially on each year of data, with progress messages printed to the console if \code{quiet = FALSE}. The model run for each year will restart if it fails, up to \code{maxtry} times, after which processing continues with the next year.  The model is run in parallel using the number of cores used set by \code{ncores}. If \code{ncores = NULL}, sequential processing is used.  All other arguments are passed to \code{\link{ebase}}.
#' 
#' Similar results can be obtained by running \code{\link{ebase}} on the entire data set, but this function is useful for long time series where the model may fail for some years, e.g., when weather data may be missing for some years.
#' 
#' @import doParallel
#'
#' @return Output identical to that returned by \code{\link{ebase}}, where the results for each year are appended to the data frame as the function progresses through the years. Note that the \code{grp} column that specifies the optimization period defined by \code{ndays} is unique to each year, e.g., values will be repeated across years. 
#' 
#' @export
#'
#' @examples
#' # get one day of data
#' dat <- exdat[as.Date(exdat$DateTimeStamp, tz = 'America/Jamaica') == as.Date('2012-06-01'), ]
#' 
#' # run ebase, use more chains and iterations for a better fit, update.chains as T
#' ebase_years(dat, Z = 1.85, interval = 900, n.chains = 2, n.iter = 50, 
#'  update.chains = FALSE)
ebase_years <- function(dat, Z, interval, ndays = 1, aprior = c(4, 2), rprior = c(300, 150), bprior = c(0.251, 0.125), bmax = 0.502, nogas = FALSE, doave = TRUE, maxinterp = 43200 / interval,  n.iter = 10000, update.chains = TRUE, n.burnin = n.iter*0.5, n.chains = 3, n.thin = 10, model_file = NULL, ncores = NULL, quiet = TRUE, maxtry = 5){
  
  # check if Z is equal in length to dat if not equal to 1
  lenz <- length(Z)
  chk <- !(lenz == 1 | lenz == nrow(dat))
  if(chk){
    msg <- paste0('Supplied value for Z has length ', lenz, ', should be 1 or ', nrow(dat))
    stop(msg)
  }
  dat$Depth <- Z

  yrs <- unique(lubridate::year(dat$DateTimeStamp))
  
  out <- NULL
  for(yr in yrs){
    
    if(!quiet) cat(yr, '\t')
    
    datsub <- dat %>% 
      dplyr::filter(lubridate::year(DateTimeStamp) == yr)
    
    # variable depth for the year
    depth <- datsub$Depth
    
    # setup parallel backend
    if(!is.null(ncores)){
      cl <- parallel::makeCluster(ncores)
      registerDoParallel(cl)
    }
    
    # ebase
    res <- try(ebase(datsub, Z = depth, interval = interval, ndays = ndays, aprior = aprior, rprior = rprior,
                     bprior = bprior, bmax = bmax, nogas = nogas, doave = doave, maxinterp = maxinterp, 
                     n.iter = n.iter, update.chains = update.chains, n.burnin = n.burnin, n.chains = n.chains,
                     n.thin = n.thin, model_file = model_file), 
               silent = T)
    
    if(!is.null(ncores))
      parallel::stopCluster(cl)
    
    i <- 1
    while(inherits(res, 'try-error')){

      if(!is.null(ncores)){
        cl <- parallel::makeCluster(ncores)
        registerDoParallel(cl)
      }
      
      if(!quiet) cat('retrying...\t')
      
      # ebase
      res <- try(ebase(datsub, Z = depth, interval = interval, ndays = ndays, aprior = aprior, rprior = rprior,
                       bprior = bprior, bmax = bmax, nogas = nogas, doave = doave, maxinterp = maxinterp, 
                       n.iter = n.iter, update.chains = update.chains, n.burnin = n.burnin, n.chains = n.chains,
                       n.thin = n.thin, model_file = model_file),
                 silent = T)
      
      if(!is.null(ncores))
        parallel::stopCluster(cl)
      
      i <- i + 1
      if(i > maxtry) break()
      
    }
    if(i > maxtry){
      if(!quiet) cat('failed...\t')
      next()
    }
    
    out <- rbind(out, res)
    
  }
  
  return(out)

}