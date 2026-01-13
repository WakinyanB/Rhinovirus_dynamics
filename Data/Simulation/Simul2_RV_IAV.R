rm(list=ls())

library(tidyverse)
library(dplyr)
library(lubridate)
library(deSolve)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data/Simulation")

source("Function_simulation.R")

## PARAMETERS & TIME

# IAV
b0_iav <- 2.5
a_iav <- 0.2
d_iav <- 0.9
gamma_iav <- 1.5 # mean = 7/1.5 = 4.7 days
omega_iav <- 1/40 # mean = 40 weeks
phi_iav <- -5
rho_iav <- 2E-3
k_iav <- 1

plot(b0_iav*(1+a_iav*cos(2*pi*(1:52)/52-d_iav))/gamma_iav, type='l')

# RV
gamma_rv <- 7/8 # mean = 8 days
omega_rv <- 1/8 # mean = 8 weeks
phi_rv <- 0
rho_rv <- 1E-3
k_rv <- 0.85

SIR0 <- read.csv2("SIR_0.csv") %>% subset(pathogen=="IAV")
parms0 <- readRDS("parms0_simul.rds")

beta0 <- parms0[["b0_rv"]]*(1+parms0[["a1_rv"]]*cos(4*pi*((1:52)/52-parms0[["d1_rv"]]))+
                              parms0[["a2_rv"]]*cos(2*pi*((1:52)/52-(parms0[["d1_rv"]]+parms0[["d2_rv"]]))))

beta1 <- beta0/(1+phi_iav*SIR0$I[2:53]/parms0[["pop"]])

plot(beta1, type='l')
lines(beta0, col='red')

mu <- 1/80/52
pop <- 5E+6

# SD of observation errors (log scale)
sigma_low <- 0.08
sigma_high <- 0.4

parms <- list("beta_rv"=beta1,
              "gamma_rv"=gamma_rv, "omega_rv"=omega_rv, "phi_rv"=phi_rv, "rho_rv"=rho_rv,
              "k_rv"=k_rv,
              "b0_iav"=b0_iav, "a_iav"=a_iav, "d_iav"=d_iav, "gamma_iav"=gamma_iav,
              "omega_iav"=omega_iav, "phi_iav"=phi_iav, "k_iav"=k_iav,
              "rho_iav"=rho_iav,
              "mu"=mu, "pop"=pop)

n_years_init <- 50 # "warm-up"
n_years1 <- 6 # pre-pandemic
n_years2 <- 4 # during and post-pandemic
n_years <- n_years1 + n_years2

## "Warm-up" to the endemic attractor

y0 <- c("S_rv"=0.99, "I_rv"=0.01, "R_rv"=0,
        "S_iav"=0.99, "I_iav"=0.01, "R_iav"=0)*pop

times <- 0:(n_years_init*52)

parms$c <- rep(0, length(times))

simul_init <- simulODE_RV_IAV(times, y0, parms, beta_rv52=TRUE)

ggplot(subset(simul_init$SIR, pathogen=="RV")) +
  geom_line(aes(x=time, y=S/pop), col="blue") +
  geom_line(aes(x=time, y=I/pop), col="red") +
  geom_line(aes(x=time, y=R/pop), col="darkgreen")

ggplot(subset(simul_init$SIR, pathogen=="IAV")) +
  geom_line(aes(x=time, y=S/pop), col="blue") +
  geom_line(aes(x=time, y=I/pop), col="red") +
  geom_line(aes(x=time, y=R/pop), col="darkgreen")

## MAIN SIMULATION

y0.2 <- (c(subset(simul_init$SIR, pathogen=="RV")[nrow(simul_init$SIR)/2,2:4],
           subset(simul_init$SIR, pathogen=="IAV")[nrow(simul_init$SIR)/2,2:4]) %>%
           unlist)

names(y0.2) <- names(y0)

times <- 0:(n_years*52)

load("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data/Google_mobility/mobility_mean_CA.RData")
npi <- mobility_mean_ca2

npi$week_ending <- ymd(npi$week_ending)
npi$time <- npi$week_ending[1] %>% ymd %>% format("%V") %>% as.integer +
  0:(nrow(npi)-1) + n_years1*52
c <- data.frame("time"=times, "c"=0)
c$c[match(npi$time, c$time)] <- npi$trend4/100
parms$c <- c$c

simul <- simulODE_RV_IAV(times=times, y0.2, parms, beta_rv52=TRUE)
SIR <- simul$SIR
cases <- simul$cases

ggpubr::ggarrange(
  ggplot(SIR, aes(x=time/52, y=S/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ggplot(SIR, aes(x=time/52, y=I/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ggplot(SIR, aes(x=time/52, y=R/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ncol=1, common.legend=TRUE, legend='right') # 6 x 7.3 

ggplot(cases) + 
  geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
  geom_vline(xintercept=1:10, lty=3) +
  geom_line(lwd=0.7, aes(x=time/52, y=cases, col=pathogen)) +
  scale_fill_gradient(low="grey60", high="white") +
  scale_x_continuous(expand=c(0,0), breaks=1:10) +
  theme_minimal() # 7.3 x 3.7

write.csv2(SIR, "SIR2.csv", row.names=FALSE)
saveRDS(subset(SIR, pathogen=="RV" & time==0), "init2_RV.rds")
saveRDS(parms, "parms2_simul.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy_sdlow <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma_low))
obs_rv$cases_noisy_sdhigh <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma_high))
obs_iav$cases_noisy_sdlow <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma_low))
obs_iav$cases_noisy_sdhigh <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma_high))

obs_iav$smooth_cases_noisy_sdlow <- smooth_cases(obs_iav$cases_noisy_sdlow)
obs_iav$smooth_cases_noisy_sdhigh <- smooth_cases(obs_iav$cases_noisy_sdhigh)

save(obs_rv, obs_iav, file="cases_2.RData")

## --------------- PERTURBATION PRE-PANDEMIC ---------------

kick <- 0.35 # % of S (IAV) moved to R
n_kick <- 1.5*52

simul_kick <- simulODE_RV_IAV.kick(times=times, y0=y0.2, parms=parms, n_kick=n_kick, kick=kick, beta_rv52=TRUE)
SIR_kick <- simul_kick$SIR
cases_kick <- simul_kick$cases

ggpubr::ggarrange(
  ggplot(SIR_kick, aes(x=time/52, y=S/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ggplot(SIR_kick, aes(x=time/52, y=I/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ggplot(SIR_kick, aes(x=time/52, y=R/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ncol=1, common.legend=TRUE, legend='right') # 6 x 7.3

ggplot(cases_kick) + 
  geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
  geom_vline(xintercept=1:10, lty=3) +
  geom_line(lwd=0.7, aes(x=time/52, y=cases, col=pathogen)) +
  scale_fill_gradient(low="grey60", high="white") +
  scale_x_continuous(expand=c(0,0), breaks=1:10) +
  theme_minimal() # 7.3 x 3.7

saveRDS(subset(SIR_kick, pathogen=="RV" & time==0), "init2_kick_RV.rds")
write.csv2(SIR_kick, "SIR_kick_2.csv", row.names=FALSE)

obs_kick_rv <- cases_kick %>% subset(pathogen=="RV")
obs_kick_iav <- cases_kick %>% subset(pathogen=="IAV")

set.seed(101)
obs_kick_rv$cases_noisy_sdlow <- obs_kick_rv$cases*exp(rnorm(nrow(obs_kick_rv), 0, sigma_low))
obs_kick_rv$cases_noisy_sdhigh <- obs_kick_rv$cases*exp(rnorm(nrow(obs_kick_rv), 0, sigma_high))
obs_kick_iav$cases_noisy_sdlow <- obs_kick_iav$cases*exp(rnorm(nrow(obs_kick_iav), 0, sigma_low))
obs_kick_iav$cases_noisy_sdhigh <- obs_kick_iav$cases*exp(rnorm(nrow(obs_kick_iav), 0, sigma_high))

obs_kick_iav$smooth_cases_noisy_sdlow <- smooth_cases(obs_kick_iav$cases_noisy_sdlow)
obs_kick_iav$smooth_cases_noisy_sdhigh <- smooth_cases(obs_kick_iav$cases_noisy_sdhigh)

save(obs_kick_rv, obs_kick_iav, file="cases_kick_2.RData")

## --------------- SHIFT OF 6 MONTHS ---------------

shift <- 52/2 # weeks

y0.2_shift <- (c(subset(simul_init$SIR, pathogen=="RV")[nrow(simul_init$SIR)/2-shift,2:4],
                 subset(simul_init$SIR, pathogen=="IAV")[nrow(simul_init$SIR)/2-shift,2:4]) %>%
                 unlist)

names(y0.2_shift) <- names(y0)

simul_shift <- simulODE_RV_IAV(y0=y0.2_shift, parms=parms, times=times+shift, shift=shift,
                               beta_rv52=TRUE, method='ode45')
SIR_shift <- simul_shift$SIR
cases_shift <- simul_shift$cases

SIR_shift$time <- SIR_shift$time-shift
cases_shift$time <- cases_shift$time-shift

ggpubr::ggarrange(
  ggplot(SIR_shift, aes(x=time/52, y=S/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ggplot(SIR_shift, aes(x=time/52, y=I/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ggplot(SIR_shift, aes(x=time/52, y=R/pop)) +
    geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
    geom_line(aes(col=pathogen)) +
    scale_fill_gradient(low="grey60", high="white") +
    scale_x_continuous(expand=c(0,0), breaks=1:10) +
    theme_minimal(),
  ncol=1, common.legend=TRUE, legend='right') # 6 x 7.3

ggplot(cases_shift) + 
  geom_rect(aes(xmin=(time-1)/52, xmax=time/52, ymin=0, ymax=+Inf, fill=c)) +
  geom_vline(xintercept=1:10, lty=3) +
  geom_line(lwd=0.7, aes(x=time/52, y=cases, col=pathogen)) +
  scale_fill_gradient(low="grey60", high="white") +
  scale_x_continuous(expand=c(0,0), breaks=1:10) +
  theme_minimal() # 7.3 x 3.7

saveRDS(subset(SIR_shift, pathogen=="RV" & time==0), "init2_shift_RV.rds")
write.csv2(SIR_shift, "SIR_shift_2.csv", row.names=FALSE)

obs_shift_rv <- cases_shift %>% subset(pathogen=="RV")
obs_shift_iav <- cases_shift %>% subset(pathogen=="IAV")

set.seed(101) 
obs_shift_rv$cases_noisy_sdlow <- obs_shift_rv$cases*exp(rnorm(nrow(obs_shift_rv), 0, sigma_low))
obs_shift_rv$cases_noisy_sdhigh <- obs_shift_rv$cases*exp(rnorm(nrow(obs_shift_rv), 0, sigma_high))
obs_shift_iav$cases_noisy_sdlow <- obs_shift_iav$cases*exp(rnorm(nrow(obs_shift_iav), 0, sigma_low))
obs_shift_iav$cases_noisy_sdhigh <- obs_shift_iav$cases*exp(rnorm(nrow(obs_shift_iav), 0, sigma_high))

obs_shift_iav$smooth_cases_noisy_sdlow <- smooth_cases(obs_shift_iav$cases_noisy_sdlow)
obs_shift_iav$smooth_cases_noisy_sdhigh <- smooth_cases(obs_shift_iav$cases_noisy_sdhigh)

save(obs_shift_rv, obs_shift_iav, file="cases_shift_2.RData")