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
#' @param model_file \code{NULL} to use \code{\link{ebase_model}} or a path to a model text file can be used
#' 
#' @export
#' 
#' @import here
#' @import foreach
#'
#' @details Required input data are time series for dissolved oxygen (mg/L), water temperature (C), salinity (psu), total PAR (W/m2/s), and wind speed (m/s).  See the \code{\link{exdat}} example data file for a representation of the required data.  Data are typically from continuously monitored water quality and weather parameters are hourly of sub-hourly time steps.  Oxygen concentrations are converted to mmol/m3 prior to metabolic estimation. Water column depth is also required to return volumetric estimates.  This can be supplied as a single value or a vector of length equal to the number of rows in \code{dat}.
#' 
#' The metabolic estimates are based on a mass balance equation in Grace et al. 2015 with the gas exchange estimate from Wanninkhof 2004.  It is similar to that provided by the BASEmetab R package at \url{https://github.com/dgiling/BASEmetab}, with modifications to estimate different parameters. The equation optimized in the JAGS model is: 
#' 
#' \deqn{ \frac{\delta{C_d}}{\delta{t}} = [\,aPAR]\, - [\,r]\, - \frac{1}{H}\left[-bU_{10}^2\left(\frac{s_c}{600} \right)^{-0.5} \left(C_{Sat} - C_d \right )\right]}
#' 
#' Gross production is provided by \emph{aPAR}, respiration is provided by \emph{r}, and gas exchange is provided by the remainder.  The likelihood of the parameters \emph{a}, \emph{r}, and \emph{b} given the observed data are estimated from the JAGS model using prior distributions shown in the model file. At each time step, the change in oxygen concentration between time steps is calculated from the equation using model inputs and parameter guesses, and then a finite difference approximation is used to estimate modeled oxygen concentration.  The estimated concentration is also returned for comparison to observed as one measure of model performance.   
#' 
#' @return A data frame with metabolic estimates for volumetric gross production (\code{Pg_vol}, O2 mmol/m3/d), respiration (\code{Rt_vol},  O2 mmol/m3/d), and gas exchange (\code{D}, O2 mmol/m3/d).  Additional parameters estimated by the model that are returned include \code{a} and \code{b}.  The \code{a} parameter is a constant that represents the primary production per quantum of light with units of  (mmol/m3/d)(W/m2) and is used to estimate gross production (Grace et al., 2015).  The \code{b} parameter is a constant used to estimate gas exchange in hr/cm (provided as 0.251 in eqn. 4 in Wanninkhof 2014).
#' 
#' @references 
#' 
#' Grace, M.R., Giling, D.P., Hladyz, S., Caron, V., Thompson, R.M., Nally, R.M., 2015. Fast processing of diel oxygen curves: Estimating stream metabolism with BASE (BAyesian Single-station Estimation). Limnology and Oceanography: Methods 13, e10011. https://doi.org/10.1002/lom3.10011
#' 
#' Wanninkhof, R., 2014. Relationship between wind speed and gas exchange over the ocean revisited. Limnology and Oceanography: Methods 12, 351–362. https://doi.org/10.4319/lom.2014.12.351

#' @examples 
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
#' # run ebase with defaults, parallel
#' 
#' # setup parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2)
#' 
#' stopCluster(cl)
#'
#' ##
#' # run ebase with different initial starting values for the three parameters, parallel
#' 
#' inits <- function(troc = 86400 / 900){
#'   list(
#'     a = 0.2 / troc,
#'     r = 20 / troc,
#'     b = 0.251 / 400
#'   )
#' }
#' 
#' # setup parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'   
#' res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, inits = inits, n.chains = 2)
#'
#' stopCluster(cl)
ebase <- function(dat, H, interval, inits = NULL, n.iter = 10000, update.chains = TRUE, n.burnin = n.iter*0.5, n.chains = 3, 
                  n.thin = 10, progress = FALSE, model_file = NULL){
  
  # prep data
  dat <- ebase_prep(dat, H)
  
  # time rate of change per day
  troc <- 86400 / interval
  
  # dates in data, get those with complete timesteps
  dts <- unique(dat$Date) %>% 
    .[table(dat$Date) == troc] 
  
  # use model function or model file
  mod_in <- ebase_model 
  if(!is.null(model_file))
    mod_in <- model_file
    
  # setup log file
  strt <- Sys.time()
  
  # iterate through each date to estimate metabolism ------------------------

  # process
  output <- foreach(d = dts, .packages = c('here', 'R2jags', 'rjags'), .export = c('troc', 'metab_update', 'mod_in')
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
    metabfit <- do.call(jags.parallel,
                        list(data = dat.list, inits = inits, parameters.to.save = params, 
                             model.file = mod_in,
                             n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin,
                             n.thin = n.thin, n.cluster = n.chains, DIC = TRUE,
                             jags.seed = 123, digits = 5)
    )
  
    # update metab if no convergence
    metabfit <- metab_update(metabfit, update.chains, n.iter)
  
    # check final convergence
    srf <- metabfit$BUGSoutput$summary[,8]
    Rhat.test <- ifelse(any(srf > 1.1, na.rm = TRUE) == TRUE, "Check convergence", "Fine")
  
    # insert results to table and write table
    result <- data.frame(Date = as.character(d),
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
      Date = lubridate::ymd(Date), 
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
