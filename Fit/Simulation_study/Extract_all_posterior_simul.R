rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Fit")

######################### Fit for simulated dataset #0 #########################

## Original simulation

fit0_sdlow <- readRDS("Output/fit_RV_SIRS_simul0_sdlow.RDS")
fit0_npi_sdlow <- readRDS("Output/fit_RV_SIRS_npi_simul0_sdlow.RDS")
fit0_vi_sdlow <- readRDS("Output/fit_RV_SIRS_vi_simul0_sdlow.RDS")
fit0_npi_vi_sdlow <- readRDS("Output/fit_RV_SIRS_npi_vi_simul0_sdlow.RDS")

p0_sdlow <- fit0_sdlow %>% extract
p0_npi_sdlow <- fit0_npi_sdlow %>% extract
p0_vi_sdlow <- fit0_vi_sdlow %>% extract
p0_npi_vi_sdlow <- fit0_npi_vi_sdlow %>% extract

fit0_sdhigh <- readRDS("Output/fit_RV_SIRS_simul0_sdhigh.RDS")
fit0_npi_sdhigh <- readRDS("Output/fit_RV_SIRS_npi_simul0_sdhigh.RDS")
fit0_vi_sdhigh <- readRDS("Output/fit_RV_SIRS_vi_simul0_sdhigh.RDS")
fit0_npi_vi_sdhigh <- readRDS("Output/fit_RV_SIRS_npi_vi_simul0_sdhigh.RDS")

p0_sdhigh <- fit0_sdhigh %>% extract
p0_npi_sdhigh <- fit0_npi_sdhigh %>% extract
p0_vi_sdhigh <- fit0_vi_sdhigh %>% extract
p0_npi_vi_sdhigh <- fit0_npi_vi_sdhigh %>% extract

## Kick

fit0_vi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_vi_simul0_kick_sdlow.RDS")
fit0_npi_vi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul0_kick_sdlow.RDS")

p0_sdlow_kick <- p0_sdlow
p0_npi_sdlow_kick <- p0_npi_sdlow
p0_vi_sdlow_kick <- fit0_vi_sdlow_kick %>% extract
p0_npi_vi_sdlow_kick <- fit0_npi_vi_sdlow_kick %>% extract

fit0_vi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_vi_simul0_kick_sdhigh.RDS")
fit0_npi_vi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul0_kick_sdhigh.RDS")

p0_sdhigh_kick <- p0_sdhigh
p0_npi_sdhigh_kick <- p0_npi_sdhigh
p0_vi_sdhigh_kick <- fit0_vi_sdhigh_kick %>% extract
p0_npi_vi_sdhigh_kick <- fit0_npi_vi_sdhigh_kick %>% extract

## Shift

fit0_sdlow_shift <- readRDS("Output/fit_RV_SIRS_simul0_shift_sdlow.RDS")
fit0_npi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_npi_simul0_shift_sdlow.RDS")
fit0_vi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_vi_simul0_shift_sdlow.RDS")
fit0_npi_vi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul0_shift_sdlow.RDS")

p0_sdlow_shift <- fit0_sdlow_shift %>% extract
p0_npi_sdlow_shift <- fit0_npi_sdlow_shift %>% extract
p0_vi_sdlow_shift <- fit0_vi_sdlow_shift %>% extract
p0_npi_vi_sdlow_shift <- fit0_npi_vi_sdlow_shift %>% extract

fit0_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_simul0_shift_sdhigh.RDS")
fit0_npi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_npi_simul0_shift_sdhigh.RDS")
fit0_vi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_vi_simul0_shift_sdhigh.RDS")
fit0_npi_vi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul0_shift_sdhigh.RDS")

p0_sdhigh_shift <- fit0_sdhigh_shift %>% extract
p0_npi_sdhigh_shift <- fit0_npi_sdhigh_shift %>% extract
p0_vi_sdhigh_shift <- fit0_vi_sdhigh_shift %>% extract
p0_npi_vi_sdhigh_shift <- fit0_npi_vi_sdhigh_shift %>% extract

save(
  p0_sdlow, p0_npi_sdlow, p0_vi_sdlow, p0_npi_vi_sdlow,
  p0_sdlow_kick, p0_npi_sdlow_kick, p0_vi_sdlow_kick, p0_npi_vi_sdlow_kick,
  p0_sdlow_shift, p0_npi_sdlow_shift, p0_vi_sdlow_shift, p0_npi_vi_sdlow_shift,
  file="Simul3/Posterior_simul0_sdlow.RData")

save(p0_sdhigh, p0_npi_sdhigh, p0_vi_sdhigh, p0_npi_vi_sdhigh,
     p0_sdhigh_kick, p0_npi_sdhigh_kick, p0_vi_sdhigh_kick, p0_npi_vi_sdhigh_kick,
     p0_sdhigh_shift, p0_npi_sdhigh_shift, p0_vi_sdhigh_shift, p0_npi_vi_sdhigh_shift,
     file="Simul3/Posterior_simul0_sdhigh.RData")

######################### Fit for simulated dataset #1 #########################

## Original simulation

fit1_sdlow <- readRDS("Output/fit_RV_SIRS_simul1_sdlow.RDS")
fit1_npi_sdlow <- readRDS("Output/fit_RV_SIRS_npi_simul1_sdlow.RDS")
fit1_vi_sdlow <- readRDS("Output/fit_RV_SIRS_vi_simul1_sdlow.RDS")
fit1_npi_vi_sdlow <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_sdlow.RDS")

p1_sdlow <- fit1_sdlow %>% extract
p1_npi_sdlow <- fit1_npi_sdlow %>% extract
p1_vi_sdlow <- fit1_vi_sdlow %>% extract
p1_npi_vi_sdlow <- fit1_npi_vi_sdlow %>% extract

fit1_sdhigh <- readRDS("Output/fit_RV_SIRS_simul1_sdhigh.RDS")
fit1_npi_sdhigh <- readRDS("Output/fit_RV_SIRS_npi_simul1_sdhigh.RDS")
fit1_vi_sdhigh <- readRDS("Output/fit_RV_SIRS_vi_simul1_sdhigh.RDS")
fit1_npi_vi_sdhigh <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_sdhigh.RDS")

p1_sdhigh <- fit1_sdhigh %>% extract
p1_npi_sdhigh <- fit1_npi_sdhigh %>% extract
p1_vi_sdhigh <- fit1_vi_sdhigh %>% extract
p1_npi_vi_sdhigh <- fit1_npi_vi_sdhigh %>% extract

## Kick

fit1_sdlow_kick <- readRDS("Output/fit_RV_SIRS_simul1_kick_sdlow.RDS")
fit1_npi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_npi_simul1_kick_sdlow.RDS")
fit1_vi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_vi_simul1_kick_sdlow.RDS")
fit1_npi_vi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_kick_sdlow.RDS")

p1_sdlow_kick <- fit1_sdlow_kick %>% extract
p1_npi_sdlow_kick <- fit1_npi_sdlow_kick %>% extract
p1_vi_sdlow_kick <- fit1_vi_sdlow_kick %>% extract
p1_npi_vi_sdlow_kick <- fit1_npi_vi_sdlow_kick %>% extract

fit1_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_simul1_kick_sdhigh.RDS")
fit1_npi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_npi_simul1_kick_sdhigh.RDS")
fit1_vi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_vi_simul1_kick_sdhigh.RDS")
fit1_npi_vi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_kick_sdhigh.RDS")

p1_sdhigh_kick <- fit1_sdhigh_kick %>% extract
p1_npi_sdhigh_kick <- fit1_npi_sdhigh_kick %>% extract
p1_vi_sdhigh_kick <- fit1_vi_sdhigh_kick %>% extract
p1_npi_vi_sdhigh_kick <- fit1_npi_vi_sdhigh_kick %>% extract

## Shift

fit1_sdlow_shift <- readRDS("Output/fit_RV_SIRS_simul1_shift_sdlow.RDS")
fit1_npi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_npi_simul1_shift_sdlow.RDS")
fit1_vi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_vi_simul1_shift_sdlow.RDS")
fit1_npi_vi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_shift_sdlow.RDS")

p1_sdlow_shift <- fit1_sdlow_shift %>% extract
p1_npi_sdlow_shift <- fit1_npi_sdlow_shift %>% extract
p1_vi_sdlow_shift <- fit1_vi_sdlow_shift %>% extract
p1_npi_vi_sdlow_shift <- fit1_npi_vi_sdlow_shift %>% extract

fit1_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_simul1_shift_sdhigh.RDS")
fit1_npi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_npi_simul1_shift_sdhigh.RDS")
fit1_vi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_vi_simul1_shift_sdhigh.RDS")
fit1_npi_vi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_shift_sdhigh.RDS")

p1_sdhigh_shift <- fit1_sdhigh_shift %>% extract
p1_npi_sdhigh_shift <- fit1_npi_sdhigh_shift %>% extract
p1_vi_sdhigh_shift <- fit1_vi_sdhigh_shift %>% extract
p1_npi_vi_sdhigh_shift <- fit1_npi_vi_sdhigh_shift %>% extract

save(
  p1_sdlow, p1_npi_sdlow, p1_vi_sdlow, p1_npi_vi_sdlow,
  p1_sdlow_kick, p1_npi_sdlow_kick, p1_vi_sdlow_kick, p1_npi_vi_sdlow_kick,
  p1_sdlow_shift, p1_npi_sdlow_shift, p1_vi_sdlow_shift, p1_npi_vi_sdlow_shift,
  file="Simul3/Posterior_simul1_sdlow.RData")

save(
  p1_sdhigh, p1_npi_sdhigh, p1_vi_sdhigh, p1_npi_vi_sdhigh,
  p1_sdhigh_kick, p1_npi_sdhigh_kick, p1_vi_sdhigh_kick, p1_npi_vi_sdhigh_kick,
  p1_sdhigh_shift, p1_npi_sdhigh_shift, p1_vi_sdhigh_shift, p1_npi_vi_sdhigh_shift,
  file="Simul3/Posterior_simul1_sdhigh.RData")

######################### Fit for simulated dataset #1 v2 #########################

## Original simulation

fit1_v2_sdlow <- readRDS("Output/fit_RV_SIRS_simul1_v2_sdlow.RDS")
fit1_v2_npi_sdlow <- readRDS("Output/fit_RV_SIRS_npi_simul1_v2_sdlow.RDS")
fit1_v2_vi_sdlow <- readRDS("Output/fit_RV_SIRS_vi_simul1_v2_sdlow.RDS")
fit1_v2_npi_vi_sdlow <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_v2_sdlow.RDS")

p1_v2_sdlow <- fit1_v2_sdlow %>% extract
p1_v2_npi_sdlow <- fit1_v2_npi_sdlow %>% extract
p1_v2_vi_sdlow <- fit1_v2_vi_sdlow %>% extract
p1_v2_npi_vi_sdlow <- fit1_v2_npi_vi_sdlow %>% extract

fit1_v2_sdhigh <- readRDS("Output/fit_RV_SIRS_simul1_v2_sdhigh.RDS")
fit1_v2_npi_sdhigh <- readRDS("Output/fit_RV_SIRS_npi_simul1_v2_sdhigh.RDS")
fit1_v2_vi_sdhigh <- readRDS("Output/fit_RV_SIRS_vi_simul1_v2_sdhigh.RDS")
fit1_v2_npi_vi_sdhigh <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_v2_sdhigh.RDS")

p1_v2_sdhigh <- fit1_v2_sdhigh %>% extract
p1_v2_npi_sdhigh <- fit1_v2_npi_sdhigh %>% extract
p1_v2_vi_sdhigh <- fit1_v2_vi_sdhigh %>% extract
p1_v2_npi_vi_sdhigh <- fit1_v2_npi_vi_sdhigh %>% extract

## Kick

fit1_v2_sdlow_kick <- readRDS("Output/fit_RV_SIRS_simul1_v2_kick_sdlow.RDS")
fit1_v2_npi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_npi_simul1_v2_kick_sdlow.RDS")
fit1_v2_vi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_vi_simul1_v2_kick_sdlow.RDS")
fit1_v2_npi_vi_sdlow_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_v2_kick_sdlow.RDS")

p1_v2_sdlow_kick <- fit1_v2_sdlow_kick %>% extract
p1_v2_npi_sdlow_kick <- fit1_v2_npi_sdlow_kick %>% extract
p1_v2_vi_sdlow_kick <- fit1_v2_vi_sdlow_kick %>% extract
p1_v2_npi_vi_sdlow_kick <- fit1_v2_npi_vi_sdlow_kick %>% extract

fit1_v2_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_simul1_v2_kick_sdhigh.RDS")
fit1_v2_npi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_npi_simul1_v2_kick_sdhigh.RDS")
fit1_v2_vi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_vi_simul1_v2_kick_sdhigh.RDS")
fit1_v2_npi_vi_sdhigh_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_v2_kick_sdhigh.RDS")

p1_v2_sdhigh_kick <- fit1_v2_sdhigh_kick %>% extract
p1_v2_npi_sdhigh_kick <- fit1_v2_npi_sdhigh_kick %>% extract
p1_v2_vi_sdhigh_kick <- fit1_v2_vi_sdhigh_kick %>% extract
p1_v2_npi_vi_sdhigh_kick <- fit1_v2_npi_vi_sdhigh_kick %>% extract

## Shift

fit1_v2_sdlow_shift <- readRDS("Output/fit_RV_SIRS_simul1_v2_shift_sdlow.RDS")
fit1_v2_npi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_npi_simul1_v2_shift_sdlow.RDS")
fit1_v2_vi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_vi_simul1_v2_shift_sdlow.RDS")
fit1_v2_npi_vi_sdlow_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_v2_shift_sdlow.RDS")

p1_v2_sdlow_shift <- fit1_v2_sdlow_shift %>% extract
p1_v2_npi_sdlow_shift <- fit1_v2_npi_sdlow_shift %>% extract
p1_v2_vi_sdlow_shift <- fit1_v2_vi_sdlow_shift %>% extract
p1_v2_npi_vi_sdlow_shift <- fit1_v2_npi_vi_sdlow_shift %>% extract

fit1_v2_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_simul1_v2_shift_sdhigh.RDS")
fit1_v2_npi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_npi_simul1_v2_shift_sdhigh.RDS")
fit1_v2_vi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_vi_simul1_v2_shift_sdhigh.RDS")
fit1_v2_npi_vi_sdhigh_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_v2_shift_sdhigh.RDS")

p1_v2_sdhigh_shift <- fit1_v2_sdhigh_shift %>% extract
p1_v2_npi_sdhigh_shift <- fit1_v2_npi_sdhigh_shift %>% extract
p1_v2_vi_sdhigh_shift <- fit1_v2_vi_sdhigh_shift %>% extract
p1_v2_npi_vi_sdhigh_shift <- fit1_v2_npi_vi_sdhigh_shift %>% extract

save(
  p1_v2_sdlow, p1_v2_npi_sdlow, p1_v2_vi_sdlow, p1_v2_npi_vi_sdlow,
  p1_v2_sdlow_kick, p1_v2_npi_sdlow_kick, p1_v2_vi_sdlow_kick, p1_v2_npi_vi_sdlow_kick,
  p1_v2_sdlow_shift, p1_v2_npi_sdlow_shift, p1_v2_vi_sdlow_shift, p1_v2_npi_vi_sdlow_shift,
  file="Posterior_simul1_v2_sdlow.RData")

save(
  p1_v2_sdhigh, p1_v2_npi_sdhigh, p1_v2_vi_sdhigh, p1_v2_npi_vi_sdhigh,
  p1_v2_sdhigh_kick, p1_v2_npi_sdhigh_kick, p1_v2_vi_sdhigh_kick, p1_v2_npi_vi_sdhigh_kick,
  p1_v2_sdhigh_shift, p1_v2_npi_sdhigh_shift, p1_v2_vi_sdhigh_shift, p1_v2_npi_vi_sdhigh_shift,
  file="Posterior_simul1_v2_sdhigh.RData")