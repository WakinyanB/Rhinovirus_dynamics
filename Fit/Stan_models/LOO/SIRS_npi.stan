data{
  // Data
  int<lower=1> n_week;  // Number of weeks (1-number of time steps)
  
  int<lower=1, upper=n_week> n_fit;
  int<lower=1, upper=n_week> fit_index[n_fit];
  
  int week[n_week];     // Week number (1-52)
  real obs[n_week];     // Observed data (including missing data to match index with week)
  real c[n_week];       // Changes in mobility (%)
  
  // Fixed parameters
  real pop;              // Total population size
  real gamma;            // Recovery rate
  real mu;               // Natural death and birth rate
  //real omega;           // Rate of waning immunity
}

parameters{
  
  // Parameters to estimate
  real logbeta0;            // baseline for log-beta
  vector[52] eps_beta;      // increments for beta on the log scale
  real<lower=0> sigma_beta; // scale of random-walk increments
  
  real<lower=0, upper=-1/min(c)> kappa;
  
  real<lower=0, upper=0.01> rho;
  real<lower=0, upper=1> omega;
  
  // Initial conditions to estimate
  real<lower=0, upper=1> S0;
  real<lower=0, upper=1-S0> I0;
  
  // Overdispersion parameter
  real<lower=0> sigma;
}

transformed parameters{
  
  vector<lower=0>[n_week+1] S; // Susceptible
  vector<lower=0>[n_week+1] I; // Infected = prevalence*N
  vector<lower=0>[n_week+1] R; // Recovered
  vector<lower=0>[n_week] cases; // rescaled number of cases
  
  vector[52] beta; // weekly transmission rate
  {
    vector[52] eps = eps_beta*sigma_beta;
    eps = eps-mean(eps);
    real cumsum_eps = 0;
    for(k in 1:52){
      cumsum_eps += eps[k];
      beta[k] = exp(logbeta0 + cumsum_eps);
    }
  }
  
  // Initial conditions
  S[1] = S0*pop;
  I[1] = I0*pop;
  R[1] = (1-S0-I0)*pop;
  
  for(i in 1:n_week){
    
    real h = (1+kappa*c[i])*beta[week[i]]*I[i]/pop;
    real Sout = (1-exp(-(h+mu)))*S[i];
    real StoI = Sout*h/(h+mu);
    
    real Iout = (1-exp(-(gamma+mu)))*I[i];
    real ItoR = Iout*gamma/(gamma+mu);
    
    real Rout = (1-exp(-(omega+mu)))*R[i];
    real RtoS = Rout*omega/(omega+mu);
    
    S[i+1] = S[i] + Rout - StoI + Iout-ItoR; // = S[i] + RtoS - Sout + (Sout-StoI+Iout-ItoR+Rout-RtoS)
    I[i+1] = I[i] + StoI - Iout;
    R[i+1] = R[i] + ItoR - Rout;
    cases[i] = rho*StoI;
  }
}

model{
  // Priors
  logbeta0 ~ normal(log(2.5),0.25);
	eps_beta ~ normal(0,1);
  sigma_beta ~ normal(0,0.2);
  kappa ~ uniform(0,-1/min(c));
  omega ~ normal(0,0.2);
  rho ~ beta(1,99);
  S0 ~ beta(7,1);
  I0 ~ beta(2,98);
  sigma ~ normal(0,0.5);
  
  // Likelihood
  for(i in fit_index){
    if(obs[i]>0){
      log(obs[i]) ~ normal(log(cases[i]), sigma);
    }
  }
}

generated quantities{
  vector[n_week] log_lik;
  for(i in 1:n_week){
    if(obs[i]>0){
      log_lik[i] = lognormal_lpdf(obs[i] | log(cases[i]), sigma);
    }
  }
}
