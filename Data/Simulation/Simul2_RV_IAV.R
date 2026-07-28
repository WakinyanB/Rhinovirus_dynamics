rm(list=ls())

library(tidyverse)
library(dplyr)
library(lubridate)
library(deSolve)

setwd(".../Data_and_Codes/Data/Simulation")

source("Function_simulation.R")

## PARAMETERS & TIME

parms1 <- readRDS("parms_simul_1.rds")
SIR1 <- read.csv2("SIR_1.csv") %>% subset(pathogen=="IAV")

pop <- parms1[['pop']]

# IAV
b0_iav <- 2.4
a_iav <- 0.2
d_iav <- 0.9
gamma_iav <- 1.4 # mean = 7/1.4 = 5 days
omega_iav <- 1/40 # mean = 40 weeks
phi_iav <- -5
rho_iav <- 2E-3
k_iav <- 1

# RV
beta0 <- parms1[["b0_rv"]]*(1+parms1[["a1_rv"]]*cos(4*pi*((1:52)/52-parms1[["d1_rv"]]))+
                              parms1[["a2_rv"]]*cos(2*pi*((1:52)/52-(parms1[["d1_rv"]]+parms1[["d2_rv"]]))))

beta_rv <- beta0/(1+phi_iav*SIR1$I[2:53]/pop)

plot(beta_rv/parms1[["gamma_rv"]], ylim=c(1.2,3.2), type='l', col='blue')
lines(b0_iav*(1+a_iav*cos(2*pi*(1:52)/52-d_iav))/gamma_iav, type='l', col='red')

# SD of observation errors (log scale)
sigma <- 0.25

parms <- list("beta_rv"=beta_rv,
              "gamma_rv"=parms1[["gamma_rv"]], "omega_rv"=parms1[["omega_rv"]],
              "phi_rv"=parms1[["phi_rv"]], "rho_rv"=parms1[["rho_rv"]],
              "k_rv"=parms1[["k_rv"]],
              "b0_iav"=b0_iav, "a_iav"=a_iav, "d_iav"=d_iav, "gamma_iav"=gamma_iav,
              "omega_iav"=omega_iav, "phi_iav"=phi_iav, "k_iav"=k_iav,
              "rho_iav"=rho_iav,
              "mu"=parms1[["mu"]], "pop"=pop)

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

parms$c <- parms1$c

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

write.csv2(SIR, "SIR_2.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2.RData")

# Higher SD of observation errors (log scale)
sigma2 <- 0.5

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma2))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma2))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_SDhigh.RData")

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

write.csv2(SIR_kick, "SIR_kick_2.csv", row.names=FALSE)

obs_kick_rv <- cases_kick %>% subset(pathogen=="RV")
obs_kick_iav <- cases_kick %>% subset(pathogen=="IAV")

set.seed(101)
obs_kick_rv$cases_noisy <- obs_kick_rv$cases*exp(rnorm(nrow(obs_kick_rv), 0, sigma))
obs_kick_iav$cases_noisy <- obs_kick_iav$cases*exp(rnorm(nrow(obs_kick_iav), 0, sigma))

obs_kick_iav$smooth_cases_noisy <- smooth_cases(obs_kick_iav$cases_noisy)

save(obs_kick_rv, obs_kick_iav, file="cases_kick_2.RData")

## --------------- SHIFT OF 6 MONTHS ---------------

shift <- 52/2 # weeks

y0.2_shift <- (c(subset(simul_init$SIR, pathogen=="RV")[nrow(simul_init$SIR)/2-shift,2:4],
                 subset(simul_init$SIR, pathogen=="IAV")[nrow(simul_init$SIR)/2-shift,2:4]) %>%
                 unlist)

names(y0.2_shift) <- names(y0)

simul_shift <- simulODE_RV_IAV(y0=y0.2_shift, parms=parms, times=times+shift, shift=shift,
                               method='ode45', beta_rv52=TRUE)
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

write.csv2(SIR_shift, "SIR_shift_2.csv", row.names=FALSE)

obs_shift_rv <- cases_shift %>% subset(pathogen=="RV")
obs_shift_iav <- cases_shift %>% subset(pathogen=="IAV")

set.seed(101)
obs_shift_rv$cases_noisy <- obs_shift_rv$cases*exp(rnorm(nrow(obs_shift_rv), 0, sigma))
obs_shift_iav$cases_noisy <- obs_shift_iav$cases*exp(rnorm(nrow(obs_shift_iav), 0, sigma))

obs_shift_iav$smooth_cases_noisy <- smooth_cases(obs_shift_iav$cases_noisy)

save(obs_shift_rv, obs_shift_iav, file="cases_shift_2.RData")
