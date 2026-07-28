data{
  // Data
  int<lower=1> n_country;                              // Number of countries
  int<lower=1> n_loc;                                  // Number of locations/regions
  int<lower=1> n_data;                                 // Total number of weeks across all locations= sum(n_week)
  array[n_loc] int<lower=1> n_week;                    // Number of weeks per location (1-number of time steps)
  array[n_data] int<lower=1, upper=n_country> country; // Country identifier (1 or 2)
  array[n_data] int loc;                               // location identifier (1-n_loc)
  array[n_data] int week;                              // Week number (1-52)
  array[n_data] real obs;                              // Observed data
  array[n_data] real obs_IAV;                          // Observed data for influenza A virus
  array[n_data] real c;                                // Changes in mobility (%)
  
  // Fixed parameters
  real pop[n_loc];      // Total population size
  real gamma;           // Recovery rate
  real mu;              // Natural death and birth rate
}

parameters{
  
  // Parameters to estimate
  
  real<lower=0, upper=1> omega; // Waning rate of immunity (same across locations)
  
  vector[n_loc] logbeta0;            // baseline for log-beta
  matrix[n_loc,52] eps_beta;         // increments for beta on the log scale
  vector<lower=0>[n_loc] sigma_beta; // scale of random-walk increments
  
  vector<lower=-1, upper=1>[n_country] phi; // Viral interaction parameter
  vector<lower=0, upper=0.01>[n_loc] rho;
  vector<lower=0, upper=-1/min(c)>[n_loc] kappa;
  
  // Initial conditions to estimate
  simplex[3] init[n_loc];
  
  // SD observation noise (log scale)
  vector<lower=0>[n_loc] sigma;
}

transformed parameters{
  
  vector[n_data+n_loc] S; // Susceptible
  vector[n_data+n_loc] I; // Infected = prevalence*N
  vector[n_data+n_loc] R; // Recovered
  vector[n_data] cases; // rescaled number of cases
  
  matrix[n_loc,52] beta; // weekly transmission rate
  
  for(i in 1:n_loc){
    vector[52] eps = to_vector(eps_beta[i])*sigma_beta[i];
    eps -= mean(eps);
    real cumsum_eps = 0;
    
    for(k in 1:52){
      cumsum_eps += eps[k];
      beta[i,k] = exp(logbeta0[i] + cumsum_eps);
    }
  }
  
  {
    int state_idx = 0;
    int obs_idx = 1;
    
    for(i in 1:n_loc){
      
      state_idx += 1;
      
      // Initial conditions
      S[state_idx] = init[i][1]*pop[i];
      I[state_idx] = init[i][2]*pop[i];
      R[state_idx] = init[i][3]*pop[i];
      
      for(t in 1:n_week[i]){
        
        real h = (1+kappa[i]*c[obs_idx])*(1+phi[country[i]]*obs_IAV[obs_idx])*beta[i,week[obs_idx]]*I[state_idx]/pop[i];
        real Sout = (1-exp(-(h+mu)))*S[state_idx];
        real StoI = Sout*h/(h+mu);
        
        real Iout = (1-exp(-(gamma+mu)))*I[state_idx];
        real ItoR = Iout*gamma/(gamma+mu);
        
        real Rout = (1-exp(-(omega+mu)))*R[state_idx];
        real RtoS = Rout*omega/(omega+mu);
        
        S[state_idx+1] = S[state_idx] + Rout - StoI + Iout-ItoR; // = S[i] + RtoS - Sout + (Sout-StoI+Iout-ItoR+Rout-RtoS)
        I[state_idx+1] = I[state_idx] + StoI - Iout;
        R[state_idx+1] = R[state_idx] + ItoR - Rout;
        cases[obs_idx] = rho[i]*StoI;
        
        state_idx += 1;
        obs_idx += 1;
      }
    }
  }
}

model{
  // Priors
  logbeta0 ~ normal(log(2.5),0.25);
	to_vector(eps_beta) ~ normal(0,1);
  sigma_beta ~ normal(0,0.2);
  kappa ~ uniform(0,-1/min(c));
  phi ~ normal(0,0.25);
  omega ~ normal(0,0.2);
  rho ~ beta(1,99);
  
  for (i in 1:n_loc){
    init[i] ~ dirichlet([35,1,4]'); # mean: S0/N=35/40=0.875, I0/N=1/40=0.025, R0/N=0.1
  }
  
  sigma ~ normal(0,0.5);
  
  // Likelihood
  for(j in 1:n_data){
    if(obs[j]>0){
      obs[j] ~ lognormal(log(cases[j]), sigma[loc[j]]);
    }
  }
}

generated quantities{
  vector[n_data] log_lik;
  for(j in 1:n_data){
    if(obs[j]>0){
      log_lik[j] = lognormal_lpdf(obs[j] | log(cases[j]), sigma[loc[j]]);
    }
  }
}
