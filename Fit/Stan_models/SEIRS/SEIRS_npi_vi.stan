data{
  // Data
  int<lower=1> n_week;  // Number of weeks (1-number of time steps)
  int week[n_week];     // Week number (1-52)
  real obs[n_week];     // Observed data (including missing data to match index with week)
  real obs_IAV[n_week]; // Observed data for influenza A virus
  real c[n_week];       // Changes in mobility (%)
  
  // Fixed parameters
  real pop;              // Total population size
  real eta;              // E -> I transition rate
  real gamma;            // Recovery rate
  real mu;               // Natural death and birth rate
  //real omega;          // Rate of waning immunity
  int n_step;
}

parameters{
  
  // Parameters to estimate
  real logbeta0;            // baseline for log-beta
  vector[52] eps_beta;      // increments for beta on the log scale
  real<lower=0> sigma_beta; // scale of random-walk increments
  
  real<lower=0, upper=-1/min(c)> kappa;
  real<lower=-1, upper=1> phi;
  
  real<lower=0, upper=0.01> rho;
  real<lower=0, upper=1> omega;
  
  // Initial conditions to estimate
  real<lower=0, upper=1> S0;
  real<lower=0, upper=1-S0> I0;
  real<lower=0, upper=1-S0-I0> E0;
  
  // Overdispersion parameter
  real<lower=0> sigma;
}

transformed parameters{
  
  vector<lower=0>[n_week*n_step+1] S; // Susceptible
  vector<lower=0>[n_week*n_step+1] E; // Exposed
  vector<lower=0>[n_week*n_step+1] I; // Infectious
  vector<lower=0>[n_week*n_step+1] R; // Recovered
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
  E[1] = E0*pop;
  I[1] = I0*pop;
  R[1] = (1-S0-E0-I0)*pop;
  
  real dt = 1.0/n_step; // duration of each time step
  
  for(i in 1:n_week){
    
    cases[i] = 0;
    real h = (1+kappa*c[i])*(1+phi*obs_IAV[i])*beta[week[i]]/pop;
    
    for(j in 1:n_step){
      
      int t = (i-1)*n_step+j;
      
      real Sout = (1-exp(-(h*I[t]+mu)*dt))*S[t];
      real StoE = Sout*h*I[t]/(h*I[t]+mu);
      
      real Eout = (1-exp(-(eta+mu)*dt))*E[t];
      real EtoI = Eout*eta/(eta+mu);
      
      real Iout = (1-exp(-(gamma+mu)*dt))*I[t];
      real ItoR = Iout*gamma/(gamma+mu);
      
      real Rout = (1-exp(-(omega+mu)*dt))*R[t];
      real RtoS = Rout*omega/(omega+mu);
      
      S[t+1] = S[t] + Rout - StoE + Eout-EtoI + Iout-ItoR; // = S[t]+RtoS-Sout + (Sout-StoE + Eout-EtoI + Iout-ItoR + Rout-RtoS)
      E[t+1] = E[t] + StoE - Eout;
      I[t+1] = I[t] + EtoI - Iout;
      R[t+1] = R[t] + ItoR - Rout;
      
      cases[i] += rho*EtoI;
    }
  }
}

model{
  // Priors
  logbeta0 ~ normal(log(2.5),0.25);
	eps_beta ~ normal(0,1);
  sigma_beta ~ normal(0,0.2);
  kappa ~ uniform(0,-1/min(c));
  phi ~ normal(0,0.2);
  omega ~ normal(0,0.2);
  rho ~ beta(1,99);
  S0 ~ beta(7,1);
  E0 ~ beta(2,98);
  I0 ~ beta(2,98);
  sigma ~ normal(0,0.5);
  
  // Likelihood
  for(i in 1:n_week){
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
