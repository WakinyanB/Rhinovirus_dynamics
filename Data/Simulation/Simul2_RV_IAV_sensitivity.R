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

# Varying some parameter values

omega_vec <- c(1/2, 1/52)
k_rv_vec <- c(0.5,1.5) 
k_iav_vec <- c(0.5,1.5)
a_iav_vec <- c(0.05,0.4)

# Global parameters and initial conditions

y0 <- c("S_rv"=0.99, "I_rv"=0.01, "R_rv"=0,
        "S_iav"=0.99, "I_iav"=0.01, "R_iav"=0)*pop

n_years_init <- 50 # "warm-up"
n_years1 <- 6 # pre-pandemic
n_years2 <- 4 # during and post-pandemic
n_years <- n_years1 + n_years2

parms <- list("beta_rv"=beta_rv,
              "gamma_rv"=parms1[["gamma_rv"]], "omega_rv"=parms1[["omega_rv"]],
              "phi_rv"=parms1[["phi_rv"]], "rho_rv"=parms1[["rho_rv"]],
              "k_rv"=parms1[["k_rv"]],
              "b0_iav"=b0_iav, "a_iav"=a_iav, "d_iav"=d_iav, "gamma_iav"=gamma_iav,
              "omega_iav"=omega_iav, "phi_iav"=phi_iav, "k_iav"=k_iav,
              "rho_iav"=rho_iav,
              "mu"=parms1[["mu"]], "pop"=pop)

# ------------- Omega ----------------------------------------------------------

# Omega[1]

parms[["omega_rv"]] <- omega_vec[1]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_omega1.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_omega1.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_omega1.RData")

# Omega[2]

parms[["omega_rv"]] <- omega_vec[2]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_omega2.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_omega2.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_omega2.RData")

# ------------- kappa_rv -------------------------------------------------------

# kappa_rv[1]

parms[["k_rv"]] <- k_rv_vec[1]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_kappa_rv1.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_kappa_rv1.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_kappa_rv1.RData")

# kappa_rv[2]

parms[["k_rv"]] <- k_rv_vec[2]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_kappa_rv2.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_kappa_rv2.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_kappa_rv2.RData")

# ------------- kappa_iav ------------------------------------------------------

# kappa_iav[1]

parms[["k_iav"]] <- k_iav_vec[1]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_kappa_iav1.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_kappa_iav1.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_kappa_iav1.RData")

# kappa_iav[2]

parms[["k_iav"]] <- k_iav_vec[2]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_kappa_iav2.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_kappa_iav2.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_kappa_iav2.RData")

# ------------- a_iav ----------------------------------------------------------

# a_iav[1]

parms[["a_iav"]] <- a_iav_vec[1]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_a_iav1.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_a_iav1.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_a_iav1.RData")

# a_iav[2]

parms[["a_iav"]] <- a_iav_vec[2]

## "Warm-up" to the endemic attractor

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

write.csv2(SIR, "SIR_2_a_iav2.csv", row.names=FALSE)
saveRDS(parms, "parms_simul_2_a_iav2.rds")

obs_rv <- cases %>% subset(pathogen=="RV")
obs_iav <- cases %>% subset(pathogen=="IAV")

set.seed(101)
obs_rv$cases_noisy <- obs_rv$cases*exp(rnorm(nrow(obs_rv), 0, sigma))
obs_iav$cases_noisy <- obs_iav$cases*exp(rnorm(nrow(obs_iav), 0, sigma))

obs_iav$smooth_cases_noisy <- smooth_cases(obs_iav$cases_noisy)

save(obs_rv, obs_iav, file="cases_2_a_iav2.RData")