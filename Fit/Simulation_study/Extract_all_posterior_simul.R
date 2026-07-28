rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)

setwd(".../Simulation_study")

######################### Fit for simulated dataset #1 #########################

## Original simulation

fit1 <- readRDS("Output/fit_RV_SIRS_simul1.RDS")
fit1_npi <- readRDS("Output/fit_RV_SIRS_npi_simul1.RDS")
fit1_vi <- readRDS("Output/fit_RV_SIRS_vi_simul1.RDS")
fit1_npi_vi <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1.RDS")

p1 <- fit1 %>% extract
p1_npi <- fit1_npi %>% extract
p1_vi <- fit1_vi %>% extract
p1_npi_vi <- fit1_npi_vi %>% extract

## Kick

fit1_kick <- readRDS("Output/fit_RV_SIRS_simul1_kick.RDS")
fit1_npi_kick <- readRDS("Output/fit_RV_SIRS_npi_simul1_kick.RDS")
fit1_vi_kick <- readRDS("Output/fit_RV_SIRS_vi_simul1_kick.RDS")
fit1_npi_vi_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_kick.RDS")

p1_kick <- fit1_kick %>% extract
p1_npi_kick <- fit1_npi_kick %>% extract
p1_vi_kick <- fit1_vi_kick %>% extract
p1_npi_vi_kick <- fit1_npi_vi_kick %>% extract

## Shift

fit1_shift <- readRDS("Output/fit_RV_SIRS_simul1_shift.RDS")
fit1_npi_shift <- readRDS("Output/fit_RV_SIRS_npi_simul1_shift.RDS")
fit1_vi_shift <- readRDS("Output/fit_RV_SIRS_vi_simul1_shift.RDS")
fit1_npi_vi_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul1_shift.RDS")

p1_shift <- fit1_shift %>% extract
p1_npi_shift <- fit1_npi_shift %>% extract
p1_vi_shift <- fit1_vi_shift %>% extract
p1_npi_vi_shift <- fit1_npi_vi_shift %>% extract

save(
  p1, p1_npi, p1_vi, p1_npi_vi,
  p1_kick, p1_npi_kick, p1_vi_kick, p1_npi_vi_kick,
  p1_shift, p1_npi_shift, p1_vi_shift, p1_npi_vi_shift,
  file="Posterior_simul1.RData")

######################### Fit for simulated dataset #2 #########################

## Original simulation

fit2 <- readRDS("Output/fit_RV_SIRS_simul2.RDS")
fit2_npi <- readRDS("Output/fit_RV_SIRS_npi_simul2.RDS")
fit2_vi <- readRDS("Output/fit_RV_SIRS_vi_simul2.RDS")
fit2_npi_vi <- readRDS("Output/fit_RV_SIRS_npi_vi_simul2.RDS")

p2 <- fit2 %>% extract
p2_npi <- fit2_npi %>% extract
p2_vi <- fit2_vi %>% extract
p2_npi_vi <- fit2_npi_vi %>% extract

## Kick

fit2_kick <- readRDS("Output/fit_RV_SIRS_simul2_kick.RDS")
fit2_npi_kick <- readRDS("Output/fit_RV_SIRS_npi_simul2_kick.RDS")
fit2_vi_kick <- readRDS("Output/fit_RV_SIRS_vi_simul2_kick.RDS")
fit2_npi_vi_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul2_kick.RDS")

p2_kick <- fit2_kick %>% extract
p2_npi_kick <- fit2_npi_kick %>% extract
p2_vi_kick <- fit2_vi_kick %>% extract
p2_npi_vi_kick <- fit2_npi_vi_kick %>% extract

## Shift

fit2_shift <- readRDS("Output/fit_RV_SIRS_simul2_shift.RDS")
fit2_npi_shift <- readRDS("Output/fit_RV_SIRS_npi_simul2_shift.RDS")
fit2_vi_shift <- readRDS("Output/fit_RV_SIRS_vi_simul2_shift.RDS")
fit2_npi_vi_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul2_shift.RDS")

p2_shift <- fit2_shift %>% extract
p2_npi_shift <- fit2_npi_shift %>% extract
p2_vi_shift <- fit2_vi_shift %>% extract
p2_npi_vi_shift <- fit2_npi_vi_shift %>% extract

save(
  p2, p2_npi, p2_vi, p2_npi_vi,
  p2_kick, p2_npi_kick, p2_vi_kick, p2_npi_vi_kick,
  p2_shift, p2_npi_shift, p2_vi_shift, p2_npi_vi_shift,
  file="Posterior_simul2.RData")

######################### Fit for simulated dataset #3 #########################

## Original simulation

fit3 <- readRDS("Output/fit_RV_SIRS_simul3.RDS")
fit3_npi <- readRDS("Output/fit_RV_SIRS_npi_simul3.RDS")
fit3_vi <- readRDS("Output/fit_RV_SIRS_vi_simul3.RDS")
fit3_npi_vi <- readRDS("Output/fit_RV_SIRS_npi_vi_simul3.RDS")

p3 <- fit3 %>% extract
p3_npi <- fit3_npi %>% extract
p3_vi <- fit3_vi %>% extract
p3_npi_vi <- fit3_npi_vi %>% extract

## Kick

fit3_kick <- readRDS("Output/fit_RV_SIRS_simul3_kick.RDS")
fit3_npi_kick <- readRDS("Output/fit_RV_SIRS_npi_simul3_kick.RDS")
fit3_vi_kick <- readRDS("Output/fit_RV_SIRS_vi_simul3_kick.RDS")
fit3_npi_vi_kick <- readRDS("Output/fit_RV_SIRS_npi_vi_simul3_kick.RDS")

p3_kick <- fit3_kick %>% extract
p3_npi_kick <- fit3_npi_kick %>% extract
p3_vi_kick <- fit3_vi_kick %>% extract
p3_npi_vi_kick <- fit3_npi_vi_kick %>% extract

## Shift

fit3_shift <- readRDS("Output/fit_RV_SIRS_simul3_shift.RDS")
fit3_npi_shift <- readRDS("Output/fit_RV_SIRS_npi_simul3_shift.RDS")
fit3_vi_shift <- readRDS("Output/fit_RV_SIRS_vi_simul3_shift.RDS")
fit3_npi_vi_shift <- readRDS("Output/fit_RV_SIRS_npi_vi_simul3_shift.RDS")

p3_shift <- fit3_shift %>% extract
p3_npi_shift <- fit3_npi_shift %>% extract
p3_vi_shift <- fit3_vi_shift %>% extract
p3_npi_vi_shift <- fit3_npi_vi_shift %>% extract

save(
  p3, p3_npi, p3_vi, p3_npi_vi,
  p3_kick, p3_npi_kick, p3_vi_kick, p3_npi_vi_kick,
  p3_shift, p3_npi_shift, p3_vi_shift, p3_npi_vi_shift,
  file="Posterior_simul3.RData")