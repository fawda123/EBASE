#' EBASE JAGS model
#'
#' EBASE JAGS model
#' 
#' @export
ebase_model <- function(){

  # -------
  # PRIORS
  
  # set priors
  amean <- 0.2 / nstepd
  # asd <- 0.01 * amean
  # atau <- 1 / (asd * asd)
  # rmean <- 20 / nstepd
  # rsd <- 1.341 * rmean
  # rtau <- 1 / (rsd * rsd)
  bmean <- 0.251 / (100 * 3600 / interval)
  # bsd <- 1.341 * bmean
  # btau <- 1 / (bsd * bsd)
  
  a ~ dunif(0 * amean, 2 * amean)
  r ~ dnorm(0, 0.01)#T(0,)
  b ~ dunif(0.8 * bmean, 1.2 * bmean) # note that Wanninkhof reports uncertainty of this parameter at +/- 20%
  
  tau ~ dgamma(1,0.001)
  
  #--------
  
  # model
  
  # DO modelled
  zz ~ dnorm(0,1000000)
  DO_mod[1] <- DO_obs[1] + zz # make monitorable
  
  for (i in 1:(num.measurements-1)){		
    
    DO_obs[i+1] ~ dnorm(DO_mod[i+1], tau)
    DO_mod[i+1] <- DO_mod[i] 
    + a * PAR[i] 
    - r 
    - (1 / H[i]) * (-b * pow(U10[i], 2) * pow(sc[i] / 600, -0.5) * (DO_sat[i] - DO_mod[i]))
    
    # posterior predictive assessment nodes #
    ats[i] <- a 
    bts[i] <- b
    gppts[i] <- a * PAR[i]
    erts[i] <- r
    gets[i] <- (1 / H[i]) * (b * pow(U10[i], 2) * pow(sc[i] / 600, -0.5) * (DO_sat[i] - DO_mod[i]))
    
  }
  
}
