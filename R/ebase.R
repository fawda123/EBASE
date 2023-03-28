#' Estuarine Bayesian Single-station Estimation method for ecosystem metabolism
#' 
#' Estuarine Bayesian Single-station Estimation method for ecosystem metabolism
#' 
#' @param dat input data frame
#' @param H numeric as single value for water column depth (m) or vector equal in length to number of rows in \code{dat}
#' @param interval timestep interval in seconds
#' @param ndays numeric for number of days in \code{dat} for optimizing the metabolic equation, see details
#' @param aprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{a} parameter, see details
#' @param rprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{r} parameter, see details
#' @param bprior numeric vector of length two indicating the mean and standard deviation for the prior distribution of the \emph{b} parameter, see details
#' @param bmax numeric value for the upper limit on the prior distribution for \code{bprior}, set as twice the default value of the mean
#' @param doave logical indicating if the average dissolved oxygen concentration is used as the starting value for the estimation (default), otherwise the first observation will be used if \code{FALSE}, see details
#' @param maxinterp numeric value for minimum number of continuous observations that must not be interpolated within a group defined by \code{ndays} to assign as \code{NA} in output, see details
#' @param n.iter number of MCMC iterations, passed to \code{\link[R2jags]{jags}}
#' @param update.chains logical to run \code{\link{metab_update}} if chains do not converge
#' @param n.burnin number of MCMC chains to delete, passed to \code{\link[R2jags]{jags}}
#' @param n.chains number of MCMC chains to run, passed to \code{\link[R2jags]{jags}}
#' @param n.thin number of nth iterations to save for each chain, passed to \code{\link[R2jags]{jags}}
#' @param progress logical if progress saved to a txt file named 'log.txt' in the working directory
#' @param model_file \code{NULL} to use the model file included with the package or a path to a model text file can be used
#' 
#' @export
#' 
#' @import here
#' @import foreach
#' @import rjags
#' @import R2jags
#' @importFrom dplyr %>%
#'
#' @details Required input data are time series for dissolved oxygen (mg/L), water temperature (C), salinity (psu), total PAR (W/m2/s), and wind speed (m/s).  See the \code{\link{exdat}} example data file for a representation of the required data.  Data are typically from continuously monitored water quality and weather parameters are hourly of sub-hourly time steps.  Oxygen concentrations are converted to mmol/m3 prior to metabolic estimation. Water column depth is also required.  This can be supplied as a single value or a vector of length equal to the number of rows in \code{dat}.
#' 
#' The metabolic estimates are based on a mass balance equation in Grace et al. 2015 with the gas exchange estimate from Wanninkhof 2004.  It is similar to that provided by the BASEmetab R package at \url{https://github.com/dgiling/BASEmetab}, with modifications to estimate different parameters. The equation optimized in the JAGS model is: 
#' 
#' \deqn{ H\frac{dC_d}{dt} = aPAR - r - \left[bU_{10}^2\left(\frac{Sc}{600} \right)^{-0.5} \left(C_{Sat} - C_d \right )\right]}
#' 
#' Gross production is provided by \emph{aPAR}, respiration is provided by \emph{r}, and gas exchange is provided by the remainder.  The likelihood of the parameters \emph{a}, \emph{r}, and \emph{b} given the observed data are estimated from the JAGS model using prior distributions shown in the model file. At each time step, the change in oxygen concentration between time steps is calculated from the equation using model inputs and parameter guesses, and then a finite difference approximation is used to estimate modeled oxygen concentration.  The first modeled value starts at the mean oxygen concentration for all measurements in the optimization period.  The estimated concentration at each time step is also returned for comparison to observed as one measure of model performance.   
#' 
#' The prior distributions for the \emph{a}, \emph{r}, and \emph{b} parameters are defined in the model file included with the package as normal distributions with mean and standard deviations provided by the \code{aprior}, \code{rprior}, and \code{bprior} arguments. The default values were chosen based on the ability of EBASE to reproduce known parameters from a forward metabolism model.  An additional constraint is that all the prior distributions are truncated to be positive values as required by the core metabolism equation above. The upper limit for \emph{b} is set as two times 0.251, as given in eqn. 4 in Wanninkhof 2014. Units for each parameter are (mmol/m3/d)/(W/m2) for \emph{a}, mmol/m3/d for \emph{r}, and (cm/hr)/(m2/s2) for \emph{b}. 
#' 
#' The \code{ndays} argument defines the model optimization period as the number of days that are used for optimizing the above mass balance equation.  By default, this is done each day, i.e., \code{ndays= 1} such that a loop is used that applies the model equation to observations within each day, evaluated iteratively from the first observation in a day to the last.  Individual parameter estimates for \emph{a}, \emph{r}, and \emph{b} are then returned for each day.  However, more days can be used to estimate the unknown parameters, such that the loop can be evaluated for every \code{ndays} specified by the argument.  The \code{ndays} argument will separate the input data into groups of consecutive days, where each group has a total number of days equal to \code{ndays}.  The final block may not include the complete number of days specified by \code{ndays} if the number of unique dates in the input data includes a remainder when divided by \code{ndays}, e.g., if seven days are in the input data and \code{ndays = 5}, there will be two groups where the first has five days and the second has two days. The output data from \code{ebase} includes a column that specifies the grouping that was used based on \code{ndays}.
#' 
#' Missing values in the input data are also interpolated prior to estimating metabolism.  It is the responsibility of the user to verify that these interpolated values are not wildly inaccurate.  Missing values are linearly interpolated between non-missing values at the time step specified by the value in \code{interval}.  This works well for small gaps, but can easily create inaccurate values at gaps larger than a few hours. The \code{\link{interp_plot}} function can be used to visually assess the interpolated values. Records at the start or end of the input time series that do not include a full day are also removed.  A warning is returned to the console if gaps are found or dangling records are found. 
#' 
#' The \code{maxinterp} argument specifies a minimum number of observations that must not be interpolated within groups defined by \code{ndays} that are assigned \code{NA} in the output (except \code{Date} and \code{DateTimeStamp}).  Groups with continuous rows of interpolated values with length longer than this argument are assigned \code{NA}.  The default value is half a day, i.e., 43200 seconds divided by the value in \code{interval}.
#' 
#' The \code{doave} argument can be used to define which dissolved oxygen value is used as the starting point in the Bayesian estimation for the optimization period.  The default setting (\code{doave = TRUE}) will use the average of all the dissolved oxygen values in the optimization period defined by \code{ndays}.  For example, the average of all dissolved oxygen values in each 24 hour period will be used if \code{doave = TRUE} and \code{ndays = 1}.  The first dissolved oxygen observation of the time series in the optimization period will be used as the starting point if \code{doave = F}.  In this case, the simulated dissolved oxygen time series will always start at the first observed dissolved oxygen value for each optimization period. 
#' 
#' @return A data frame with metabolic estimates for areal gross production (\code{P}, O2 mmol/m2/d), respiration (\code{R},  O2 mmol/m2/d; the \code{r} parameter), and gas exchange (\code{D}, O2 mmol/m2/d).  Additional parameters estimated by the model that are returned include \code{a} and \code{b}.  The \code{a} parameter is a constant that represents the primary production per quantum of light with units of (mmol/m3/d)/(W/m2) and is used to estimate gross production (Grace et al., 2015).  The \code{b} parameter is a constant used to estimate gas exchange in (cm/hr)/(m2/s2) (provided as 0.251 in eqn. 4 in Wanninkhof 2014).  Observed dissolved oxygen (\code{DO_obs}, mmol/m3), modeled dissolved oxygen (\code{DO_mod}, mmol/m3), and delta dissolved oxygen of the modeled results (\code{dDO}, mmol/m3/d) are also returned.  Note that delta dissolved oxygen is a daily rate.
#' 
#' 95% credible intervals for \code{a}, \code{b}, and \code{r} are also returned in the corresponding columns \code{alo}, \code{ahi}, \code{blo}, \code{bhi}, \code{Rlo}, and \code{Rhi}, for the 2.5th and 97.5th percentile estimates for each parameter, respectively.  These values indicate the interval within which there is a 95% probability that the true parameter is in this range. Note that all values for these parameters are repeated across rows, although only one estimate for each is returned based on the number of days defined by \code{ndays}. 
#' 
#' Model fit can also be assessed using the \code{converge} and \code{rsq} columns.  The values in these columns apply to each group in the \code{grp} column as specified with the \code{ndays} argument. The \code{converge} column indicates \code{"Check convergence"} or \code{"Fine"} if the JAGS estimate converged at that iteration (repeated across rows for the group).  The \code{n.chains} argument can be increased if convergence is not achieved. Similarly, the \code{rsq} column shows the r-squared values of the linear fit between the modeled and observed dissolved oxygen (repeated across rows for the group).  These values can also be viewed with \code{\link{fit_plot}}.
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
#'\dontrun{
#' ##
#' # run ebase with different prior distributions
#' 
#' # setup parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'   
#' res <- ebase(dat, interval = 900, H = 1.85, progress = TRUE, n.chains = 2, bprior = c(0.2, 0.05))
#'
#' stopCluster(cl)
#' }
ebase <- function(dat, H, interval, ndays = 1, aprior = c(0.2, 1), rprior = c(20, 50), bprior = c(0.251, 0.1), bmax = 0.502, doave = TRUE, maxinterp = 43200 / interval,  n.iter = 10000, update.chains = TRUE, n.burnin = n.iter*0.5, n.chains = 3, n.thin = 10, progress = FALSE, model_file = NULL){
  
  # prep data
  dat <- ebase_prep(dat, H = H, interval = interval, ndays = ndays)
  
  # the number of time steps for each iteration of the loop in days
  nstepd <- 86400 / interval

  # groups in data
  grps <- unique(dat$grp)
  
  # model file
  if(is.null(model_file))
    mod_in <- system.file('ebase_model.txt', package = 'EBASE')
  if(!is.null(model_file)){
    chk <- file.exists(model_file)
    if(!chk)
      stop('model_file does not exist, did you make a typo?')
    mod_in <- model_file
  } 
  
  # setup log file
  strt <- Sys.time()
  
  # iterate through each date to estimate metabolism ------------------------

  # process
  output <- foreach(i = grps, .packages = c('here', 'R2jags', 'rjags', 'dplyr'), .export = c('nstepd', 'metab_update', 'interval', 'aprior', 'rprior', 'bprior', 'bmax')
                                                                         ) %dopar% {
  
    if(progress){
      sink(here('log.txt'))
      cat('Log entry time', as.character(Sys.time()), '\n')
      cat('Group ', which(i == grps), ' of ', length(grps), '\n')
      print(Sys.time() - strt)
      sink()
    }
    
    dat.sub <- dat[dat$grp == i,]
  
    # Define vectors for JAGS
    num.measurements <- nrow(dat.sub)
    DO_obs <- dat.sub$DO_obs
    PAR <- dat.sub$PAR
    DO_sat <- dat.sub$DO_sat
    sc <- dat.sub$sc
    H <- mean(dat.sub$H)
    U10 <- dat.sub$WSpd
    
    DO_start <- DO_obs[1]
    if(doave)
      DO_start <- mean(DO_obs)
    
    # Set
    dat.list <- list("num.measurements", "nstepd", "interval", "aprior", "rprior", "bprior", "bmax", "DO_obs", "PAR", "DO_sat", "sc", "H", "U10", "DO_start")
  
    # Define monitoring variables (returned by jags)
    params <- c("ats", "bts", "gppts", "erts", "gets", "DO_mod")
  
    ## Call jags ##
    metabfit <- do.call(jags.parallel,
                        list(data = dat.list, parameters.to.save = params, 
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
  
    # credible intervals for a, b, er
    cred <- data.frame(metabfit$BUGSoutput$summary)
    cred$var <- row.names(cred)
    cred$var <- gsub('\\[|\\]|\\d+', '', cred$var)
    
    # insert results to table and write table
    result <- data.frame(
      Date = dat.sub$Date,
      grp = dat.sub$grp,
      H = H, #m
      DO_obs = dat.sub$DO_obs,
      DO_mod = metabfit$BUGSoutput$mean$DO_mod,
      DO_modlo = cred[cred$var == 'DO_mod', 'X2.5.'],
      DO_modhi = cred[cred$var == 'DO_mod', 'X97.5.'],
      DateTimeStamp = dat.sub$DateTimeStamp,
      ats = c(NA, metabfit$BUGSoutput$mean$ats), # (mmol/m3/ts)/(W/m2)
      atslo = c(NA, cred[cred$var == 'ats', 'X2.5.']),
      atshi = c(NA, cred[cred$var == 'ats', 'X97.5.']),
      bts = c(NA, metabfit$BUGSoutput$mean$bts), # (m/ts)/(m2/s2)
      btslo = c(NA, cred[cred$var == 'bts', 'X2.5.']),
      btshi = c(NA, cred[cred$var == 'bts', 'X97.5.']),
      gppts = c(NA, metabfit$BUGSoutput$mean$gppts) * mean(H), # O2, mmol/m3/ts to mmol/m2/ts
      gpptslo = c(NA, cred[cred$var == 'gppts', 'X2.5.']) * mean(H),
      gpptshi = c(NA, cred[cred$var == 'gppts', 'X97.5.']) * mean(H),
      erts = c(NA, metabfit$BUGSoutput$mean$erts) * mean(H), # O2, mmol/m3/ts to mmol/m2/ts
      ertslo = c(NA, cred[cred$var == 'erts', 'X2.5.']) * mean(H),
      ertshi = c(NA, cred[cred$var == 'erts', 'X97.5.']) * mean(H),
      gets = c(NA, metabfit$BUGSoutput$mean$gets), # O2, mmol/m2/ts
      getslo = c(NA, cred[cred$var == 'gets', 'X2.5.']),
      getshi = c(NA, cred[cred$var == 'gets', 'X97.5.']),
      dDO = c(NA, diff(metabfit$BUGSoutput$mean$DO_mod)), # O2 mmol/m3/ts
      converge = Rhat.test
    )
  
    rsq <- lm(DO_mod ~ DO_obs, data = result) %>% 
      summary() %>% 
      .$r.squared
    
    result$rsq <- rsq
    
    return(result)
  
  }

  # correct instantaneous obs to daily, g to mmol
  out <- do.call('rbind', output) %>%
    dplyr::mutate(
      Date = lubridate::ymd(Date), 
      a = ats * nstepd, # (mmol/m3/ts)/(W/m2) to (mmol/m3/d)/(W/m2)
      alo = atslo * nstepd, 
      ahi = atshi * nstepd, 
      b = bts * 100 * 3600 / interval, # (m/ts)/(m2/s2) to (cm/hr)/(m2/s2)
      blo = btslo * 100 * 3600 / interval, 
      bhi = btshi * 100 * 3600 / interval,
      P = gppts * nstepd, # O2 mmol/m2/ts to O2 mmol/m2/d
      Plo = gpptslo * nstepd,
      Phi = gpptshi * nstepd,
      R = erts * nstepd, # O2 mmol/m2/ts to O2 mmol/m2/d
      Rlo = ertslo * nstepd, 
      Rhi = ertshi * nstepd,
      D = gets * nstepd, #  # O2 mmol/m2/ts to O2 mmol/m2/d
      Dlo = getslo * nstepd, 
      Dhi = getshi * nstepd,
      dDO = dDO * nstepd #  # O2 mmol/m3/ts to O2 mmol/m3/d
    ) %>%
    dplyr::select(-ats, -atslo, -atshi, -bts, -btslo, -btshi, -gppts, -gpptslo, -gpptshi, -erts, -ertslo, -ertshi, -gets, -getslo, -getshi)
  
  out <- ebase_form(out, dat, interval, maxinterp)
  
  return(out)

}
