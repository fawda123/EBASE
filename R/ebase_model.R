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
  bmean <- 0.251 / (100 * 3600 / interval)

  a ~ dunif(arng[1] * amean, 2 * arng[2])
  r ~ dnorm(0, 1 / rvar)#T(0,)
  b ~ dunif(brng[1] * bmean, brng[2] * bmean) # note that Wanninkhof reports uncertainty of this parameter at +/- 20%
  
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
