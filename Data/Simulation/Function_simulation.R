# SIMULATE ODEs

## ODE system

ODE_RV_IAV <- function(t, y, parms, shift, beta_rv52=FALSE){
  
  # State variables
  ## RV
  S_rv <- y[["S_rv"]] # Susceptible
  I_rv <- y[["I_rv"]] # Infected/infectious
  R_rv <- y[["R_rv"]] # Recovered
  C_rv <- y[["C_iav"]] # Cumulated cases
  
  ## IAV
  S_iav <- y[["S_iav"]] # Susceptible
  I_iav <- y[["I_iav"]] # Infected/infectious
  R_iav <- y[["R_iav"]]# Recovered
  C_iav <- y[["C_iav"]] # Cumulated cases
  
  # Parameters
  
  mu <- parms[["mu"]]
  pop <- parms[["pop"]]
  
  # Temporal derivatives
  
  c <- parms[["c"]][floor(t-shift)+1]
  
  ## RV
  if(beta_rv52){
    dC_rv <- (1+parms[["k_rv"]]*c)*max(c(0,1+parms[["phi_iav"]]*I_iav/pop))*
      parms[["beta_rv"]][(floor(t) %% 52)+1]*S_rv*I_rv/pop
  }else{
    dC_rv <- (1+parms[["k_rv"]]*c)*max(c(0,1+parms[["phi_iav"]]*I_iav/pop))*
      parms[["b0_rv"]]*(1+parms[["a1_rv"]]*cos(4*pi*(t/52-parms[["d1_rv"]]))+
                          parms[["a2_rv"]]*cos(2*pi*(t/52-(parms[["d1_rv"]]+parms[["d2_rv"]]))))*S_rv*I_rv/pop
  }
  
  recov_rv <- parms[["gamma_rv"]]*I_rv
  waning_rv <- parms[["omega_rv"]]*R_rv
  
  dS_rv <- waning_rv - dC_rv - mu*S_rv + mu*pop
  dI_rv <- dC_rv - recov_rv - mu*I_rv
  dR_rv <- recov_rv - waning_rv - mu*R_rv
  
  ## IAV
  dC_iav <- (1+parms[["k_iav"]]*c)*max(c(0,1+parms[["phi_rv"]]*I_rv/pop))*
    parms[["b0_iav"]]*(1+parms[["a_iav"]]*cos(2*pi*(t/52-parms[["d_iav"]])))*S_iav*I_iav/pop
  
  recov_iav <- parms[["gamma_iav"]]*I_iav
  waning_iav <- parms[["omega_iav"]]*R_iav
  
  dS_iav <- waning_iav - dC_iav - mu*S_iav + mu*pop
  dI_iav <- dC_iav - recov_iav - mu*I_iav
  dR_iav <- recov_iav - waning_iav - mu*R_iav
  
  return(list(c(dS_rv, dI_rv, dR_rv, dS_iav, dI_iav, dR_iav, dC_rv, dC_iav)))
}

## Simulation main function

simulODE_RV_IAV <- function(times, y0, parms, shift=0, method='lsoda', beta_rv52=FALSE){
  
  n <- length(times)
  
  simul <- ode(times=times, y=c(y0, "C_rv"=0, "C_iav"=0), parms=parms, shift=shift, beta_rv52=beta_rv52,
               func=ODE_RV_IAV, method=method) %>% as.data.frame
  
  return(list("SIR"=data.frame("time"=times,
                               "S"=c(simul$S_rv, simul$S_iav),
                               "I"=c(simul$I_rv, simul$I_iav),
                               "R"=c(simul$R_rv, simul$R_iav),
                               "pathogen"=rep(c("RV", "IAV"), each=n),
                               "c"=parms$c),
              "cases"=data.frame("time"=times[-1],
                                 "week"=((times[-1]-1) %% 52)+1,
                                 "cases"=c(parms[["rho_rv"]]*(simul$C_rv[-1]-simul$C_rv[-n]),
                                           parms[["rho_iav"]]*(simul$C_iav[-1]-simul$C_iav[-n])),
                                 "pathogen"=rep(c("RV", "IAV"), each=n-1),
                                 "c"=parms$c[-n])))
}

## Simulation function with kick in IAV dynamics

simulODE_RV_IAV.kick <- function(times, y0, parms, n_kick=0, kick=0, beta_rv52=FALSE, method='lsoda'){
  
  # kick = % of S (IAV) moved to R
  
  n <- length(times)
  
  # Step 1
  
  simul_step1 <- ode(times=times[1:n_kick], y=c(y0, "C_rv"=0, "C_iav"=0), parms=parms, shift=0,
                     beta_rv52=beta_rv52, func=ODE_RV_IAV, method=method) %>% as.data.frame
  
  y0_kick <- simul_step1[n_kick,-1] %>% unlist
  
  simul_step1 <- simul_step1[-n_kick,]
  
  # Step 2
  
  S_to_R <- kick*y0_kick[["S_iav"]]
  y0_kick[["S_iav"]] <- y0_kick[["S_iav"]] - S_to_R
  y0_kick[["R_iav"]] <- y0_kick[["R_iav"]] + S_to_R
  
  simul_step2 <- ode(times=times[n_kick:n], y=y0_kick, parms=parms, shift=0, beta_rv52=beta_rv52,
                     func=ODE_RV_IAV, method=method) %>% as.data.frame
  
  simul <- rbind(simul_step1, simul_step2)
  
  return(list("SIR"=data.frame("time"=times,
                               "S"=c(simul$S_rv, simul$S_iav),
                               "I"=c(simul$I_rv, simul$I_iav),
                               "R"=c(simul$R_rv, simul$R_iav),
                               "pathogen"=rep(c("RV", "IAV"), each=n),
                               "c"=parms$c),
              "cases"=data.frame("time"=times[-1],
                                 "week"=((times[-1]-1) %% 52)+1,
                                 "cases"=c(parms[["rho_rv"]]*(simul$C_rv[-1]-simul$C_rv[-n]),
                                           parms[["rho_iav"]]*(simul$C_iav[-1]-simul$C_iav[-n])),
                                 "pathogen"=rep(c("RV", "IAV"), each=n-1),
                                 "c"=parms$c[-n])))
}

# SIMULATE DISCRETIZED VERSION

simulate_RV_IAV <- function(y0, parms, times, n_step=1, week, pop, c=NA, obs_RV=NA){
  
  dt <- 1/n_step
  times2 <- times[1]-1
  
  # RV
  S_rv <- y0[["S_rv"]]*pop
  I_rv <- y0[["I_rv"]]*pop
  R_rv <- (1-y0[["S_rv"]]-y0[["I_rv"]])*pop
  cases_rv <- rep(0, length(times))
  
  # IAV
  S_iav <- y0[["S_iav"]]*pop
  I_iav <- y0[["I_iav"]]*pop
  R_iav <- (1-y0[["S_iav"]]-y0[["I_iav"]])*pop
  cases_iav <- rep(0, length(times))
  
  mu <- parms[["mu"]]
  
  # RV
  b0_rv <- parms[["b0_rv"]]
  a1_rv <- parms[["a1_rv"]]
  a2_rv <- parms[["a2_rv"]]
  d1_rv <- parms[["d1_rv"]]
  d2_rv <- parms[["d2_rv"]]
  gamma_rv <- parms[["gamma_rv"]]
  omega_rv <- parms[["omega_rv"]]
  phi_rv <- parms[["phi_rv"]]
  
  # IAV
  b0_iav <- parms[["b0_iav"]]
  a <- parms[["a_iav"]]
  d <- parms[["d_iav"]]
  gamma_iav <- parms[["gamma_iav"]]
  omega_iav <- parms[["omega_iav"]]
  phi_iav <- parms[["phi_iav"]]
  
  if(any(is.na(c))){
    c <- rep(0, length(times))
    k_rv <- 0
    k_iav <- 0
  }else{
    k_rv <- parms[["k_rv"]]
    k_iav <- parms[["k_iav"]]
  }
  
  for(i in 1:length(times)){
    
    for(j in 1:n_step){
      
      t <- (i-1)*n_step+j
      times2 <- c(times2, times2[t]+dt)
      
      # RV
      h_rv <- (1+k_rv*c[i])*max(c(0,1+phi_iav*I_iav[i]/pop))*
        b0_rv*(1+a1_rv*cos(4*pi*(week[i]/52-d1_rv))+
                 a2_rv*cos(2*pi*(week[i]/52-(d1_rv+d2_rv))))*I_rv[t]/pop
      Sout_rv <- (1-exp(-(h_rv+mu)*dt))*S_rv[t]
      StoI_rv <- Sout_rv*h_rv/(h_rv+mu)
      
      Iout_rv <- (1-exp(-(gamma_rv+mu)*dt))*I_rv[t]
      ItoR_rv <- Iout_rv*gamma_rv/(gamma_rv+mu)
      
      Rout_rv <- (1-exp(-(omega_rv+mu)*dt))*R_rv[t]
      RtoS_rv <- Rout_rv*omega_rv/(omega_rv+mu)
      
      S_rv <- c(S_rv, S_rv[t] + Rout_rv - StoI_rv + Iout_rv-ItoR_rv) # S[t] + RtoS - Sout + (Sout-StoI+Iout-ItoR+Rout-RtoS)
      I_rv <- c(I_rv, I_rv[t] + StoI_rv - Iout_rv)
      R_rv <- c(R_rv, R_rv[t] + ItoR_rv - Rout_rv)
      
      cases_rv[i] <- cases_rv[i] + StoI_rv
      
      # IAV
      h_iav <- (1+k_iav*c[i])*max(c(0,1+phi_rv*I_rv[i]/pop))*
        b0_iav*(1+a_iav*cos(2*pi*(week[i]/52-d_iav)))*I_iav[t]/pop
      Sout_iav <- (1-exp(-(h_iav+mu)*dt))*S_iav[t]
      StoI_iav <- Sout_iav*h_iav/(h_iav+mu)
      
      Iout_iav <- (1-exp(-(gamma_iav+mu)*dt))*I_iav[t]
      ItoR_iav <- Iout_iav*gamma_iav/(gamma_iav+mu)
      
      Rout_iav <- (1-exp(-(omega_iav+mu)*dt))*R_iav[t]
      RtoS_iav <- Rout_iav*omega_iav/(omega_iav+mu)
      
      S_iav <- c(S_iav, S_iav[t] + Rout_iav - StoI_iav + Iout_iav-ItoR_iav)
      I_iav <- c(I_iav, I_iav[t] + StoI_iav - Iout_iav)
      R_iav <- c(R_iav, R_iav[t] + ItoR_iav - Rout_iav)
      
      cases_iav[i] <- cases_iav[i] + StoI_iav
    }
  }
  return(list("SIR"=data.frame("time"=times2,
                               "S"=c(S_rv, S_iav), "I"=c(I_rv, I_iav), "R"=c(R_rv, R_iav),
                               "pathogen"=rep(c("RV", "IAV"), each=length(times2))),
              "cases"=data.frame("time"=times,
                                 "cases"=c(parms[["rho_rv"]]*cases_rv,
                                           parms[["rho_iav"]]*cases_iav),
                                 "pathogen"=rep(c("RV", "IAV"), each=length(times)))))
}

# SMOOTH CASES

smooth_cases <- function(x, eps=1E-9){
  smooth <- c()
  n <- length(x)
  for(i in 1:n){
    smooth[i] <- (x[c(max(1,i-1):min(i+1,n))]+eps) %>% log %>% mean %>% exp
  }
  return(smooth)
}