#' Estuarine Bayesian Single-station Estimation method for ecosystem metabolism
#' 
#' Estuarine Bayesian Single-station Estimation method for ecosystem metabolism
#' 
#' @param dat input data frame
#' @param H numeric as single value for water column depth (m) or vector equal in length to number of rows in \code{dat}
#' @param interval timestep interval in seconds
#' @param inits \code{NULL} or a function that returns a named list of starting values for the parameters to be estimated in the jags model, see examples
#' @param n.iter number of MCMC iterations, passed to \code{\link[R2jags]{jags}}
#' @param update.chains logical to run \code{\link{metab_update}} if chains do not converge
#' @param n.burnin number of MCMC chains to delete, passed to \code{\link[R2jags]{jags}}
#' @param n.chains number of MCMC chains to run, passed to \code{\link[R2jags]{jags}}
#' @param n.thin number of nth iterations to save for each chain, passed to \code{\link[R2jags]{jags}}
#' @param progress logical if progress saved to a txt file names 'log.txt' in the working directory
#' 
#' @import here
#' @import foreach
#'
#' @return A data frame with metabolic estimates as mmol/m3/d
#' 
#' @examples 
#' \dontrun{
#' 
#' library(dplyr)
#' library(lubridate)
#' library(doParallel)
#' 
#' # get four days of data
#' dat <- exdat %>% 
#'   filter(month(exdat$DateTimeStamp) == 6 & day(exdat$DateTimeStamp) %in% 1:4)
#' 
#' ##
#' # run ebase with defaults
#' 
#' # setup parallel backend
#' ncores <- detectCores()
#' cl <- makeCluster(ncores - 2)
#' registerDoParallel(cl)
#'   
#' res <- ebase(dat, interval = 900, H = 1.85, progress = T)
#'
#' stopCluster(cl)
#' 
#' ##
#' # run ebase with different initial starting values for the three parameters
#' 
#' inits <- function(){
#'   list(
#'     a = 0.2 / troc,
#'     r = 20 / troc,
#'     b = 0.251 / 400
#'   )
#' }
#' 
#' # setup parallel backend
#' ncores <- detectCores()
#' cl <- makeCluster(ncores - 2)
#' registerDoParallel(cl)
#'   
#' res <- ebase(dat, interval = 900, H = 1.85, progress = T, inits = inits)
#'
#' stopCluster(cl)
#' 
#' }
ebase <- function(dat, H, interval, inits = NULL, n.iter = 10000, update.chains = TRUE, n.burnin = n.iter*0.5, n.chains = 3, 
                  n.thin = 10, progress = FALSE){
  
  # prep data
  dat <- ebase_prep(dat, H)
  
  # time rate of change per day
  troc <- 86400 / interval
  
  # dates in data, get those with complete timesteps
  dts <- unique(dat$Date) %>% 
    .[table(dat$Date) == troc] 
  
  # setup log file
  strt <- Sys.time()
  
  # iterate through each date to estimate metabolism ------------------------

  # process
  output <- foreach(d = dts, .packages = c('here', 'R2jags'), .export = c('troc', 'metab_update')
                                                                         ) %dopar% {
  
    if(progress){
      sink(here('log.txt'))
      cat('Log entry time', as.character(Sys.time()), '\n')
      cat(which(d == dts), ' of ', length(dts), '\n')
      print(Sys.time() - strt)
      sink()
    }
    
    dat.sub <- dat[dat$Date == d,]
  
    # Define vectors for JAGS
    num.measurements <- nrow(dat.sub)
    DO_obs <- dat.sub$DO_obs
    PAR <- dat.sub$PAR
    DO_sat <- dat.sub$DO_sat
    sc <- dat.sub$sc
    H <- dat.sub$H
    U10 <- dat.sub$WSpd
  
    # Different random seeds
    kern <- as.integer(runif(1000,min=1,max=10000))
    iters <- sample(kern,1)
  
    # Set
    dat.list <- list("num.measurements", "troc", "DO_obs", "PAR", "DO_sat", "sc", "H", "U10")
  
    # Define monitoring variables (returned by jags)
    params <- c("ats", "bts", "gppts", "erts", "gets", "DO_mod")
  
    ## Call jags ##
    metabfit <- do.call(R2jags::jags.parallel,
                        list(data = dat.list, inits = inits, parameters.to.save = params, 
                             model.file = here::here("inst/ebase_model.txt"),
                             n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin,
                             n.thin = n.thin, n.cluster = n.chains, DIC = TRUE,
                             jags.seed = 123, digits = 5)
    )
  
    # update metab if no convergence
    metabfit <- metab_update(metabfit, update.chains, n.iter)
  
    # check final convergence
    srf <- metabfit$BUGSoutput$summary[,8]
    Rhat.test <- ifelse(any(srf > 1.1, na.rm = T) == TRUE, "Check convergence", "Fine")
  
    # insert results to table and write table
    result <- data.frame(Date=as.character(d),
                         DO_obs = dat.sub$DO_obs,
                         DO_mod = metabfit$BUGSoutput$mean$DO_mod,
                         DateTimeStamp = dat.sub$DateTimeStamp,
                         ats = c(NA, metabfit$BUGSoutput$mean$ats), # (mmol/m3/ts)/(W/m2)
                         bts = c(NA, metabfit$BUGSoutput$mean$bts), # ts/m
                         gppts = c(NA, metabfit$BUGSoutput$mean$gppts), # O2, mmol/m3/ts
                         erts = c(NA, metabfit$BUGSoutput$mean$erts), # O2, mmol/m3/ts
                         gets = c(NA, metabfit$BUGSoutput$mean$gets), # O2, mmol/m3/ts
                         dDO = c(NA, diff(metabfit$BUGSoutput$mean$DO_mod)) # O2 mmol/m3/ts
  
    )
  
    return(result)
  
  }
  
  # correct instantaneous obs to daily, g to mmol
  out <- do.call('rbind', output) %>%
    na.omit() %>%
    dplyr::mutate(
      a = ats * troc, # (mmol/m3/ts)/(W/m2) to (mmol/m3/d)/(W/m2)
      b = bts * 100 * 3600 / interval, # ts/m to hr/cm
      Pg_vol = gppts * troc, # O2 mmol/m3/ts to O2 mmol/m3/d
      Rt_vol = erts * troc, # O2 mmol/m3/ts to O2 mmol/m3/d
      D = -1 * gets * troc, #  # O2 mmol/m3/ts to O2 mmol/m3/d
      dDO = dDO * troc #  # O2 mmol/m3/ts to O2 mmol/m3/d
    ) %>%
    dplyr::select(-ats, -bts, -gppts, -erts, -gets)
  
  return(out)

}
