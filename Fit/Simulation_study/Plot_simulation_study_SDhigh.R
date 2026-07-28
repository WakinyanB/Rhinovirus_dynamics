rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)
library(cowplot)
library(scales)
library(ggpubr)

setwd(".../Data_and_Codes/")
source("Fit/Simulation_study/Functions.R")

# Load simulated data/parameters and estimated posterior distributions

parm_names <- c("beta", "kappa", "omega", "rho", "S0", "I0")

# ----------- Simulated dataset 1 ----------------------------------------------

SIR1 <- read.csv2("Data/Simulation/SIR_1.csv")
load("Data/Simulation/cases_1_SDhigh.RData")

p1 <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_simul1_SDhigh.RDS") %>% extract
p1_npi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_npi_simul1_SDhigh.RDS") %>% extract
p1_vi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_vi_simul1_SDhigh.RDS") %>% extract
p1_npi_vi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul1_SDhigh.RDS") %>% extract

parms1 <- readRDS("Data/Simulation/parms_simul_1.rds")
parms1_rv <- parms1 %>% extract_parms_rv(init=SIR1[1,])

obs1_rv <- obs_rv

# ----------- Simulated dataset 2 ----------------------------------------------

SIR2 <- read.csv2("Data/Simulation/SIR_2.csv")
load("Data/Simulation/cases_2_SDhigh.RData")

p2 <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_simul2_SDhigh.RDS") %>% extract
p2_npi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_npi_simul2_SDhigh.RDS") %>% extract
p2_vi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_vi_simul2_SDhigh.RDS") %>% extract
p2_npi_vi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_SDhigh.RDS") %>% extract

parms2 <- readRDS("Data/Simulation/parms_simul_2.rds")
parms2_rv <- parms2 %>% extract_parms_rv(init=SIR2[1,])

obs2_rv <- obs_rv

# ----------- Simulated dataset 3 ----------------------------------------------

SIR3 <- read.csv2("Data/Simulation/SIR_3.csv")
load("Data/Simulation/cases_3_SDhigh.RData")

p3 <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_simul3_SDhigh.RDS") %>% extract
p3_npi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_npi_simul3_SDhigh.RDS") %>% extract
p3_vi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_vi_simul3_SDhigh.RDS") %>% extract
p3_npi_vi <- readRDS("Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul3_SDhigh.RDS") %>% extract

parms3 <- readRDS("Data/Simulation/parms_simul_3.rds")
parms3_rv <- parms3 %>% extract_parms_rv(init=SIR3[1,])

obs3_rv <- obs_rv

# ----------- Plot -------------------------------------------------------------

# Transmission profiles for each scenario

beta1 <- parms1$b0_rv*(1+parms1$a1_rv*cos(4*pi*((1:52)/52-parms1$d1_rv))+
                         parms1$a2_rv*cos(2*pi*((1:52)/52-(parms1$d1_rv+parms1$d2_rv))))
beta2 <- parms2$beta_rv
beta3 <- parms3$b0_rv*(1+parms3$a1_rv*cos(4*pi*((1:52)/52-parms3$d1_rv))+
                         parms3$a2_rv*cos(2*pi*((1:52)/52-(parms3$d1_rv+parms3$d2_rv))))

# Scenario #1

## Main simulation
s1 <- p1 %>% summary_posterior(parm=parm_names)
s1_npi <- p1_npi %>% summary_posterior(parm=parm_names)
s1_vi <- p1_vi %>% summary_posterior(parm=parm_names)
s1_npi_vi <- p1_npi_vi %>% summary_posterior(parm=parm_names)

# Scenario #2

## Main simulation
s2 <- p2 %>% summary_posterior(parm=parm_names)
s2_npi <- p2_npi %>% summary_posterior(parm=parm_names)
s2_vi <- p2_vi %>% summary_posterior(parm=parm_names)
s2_npi_vi <- p2_npi_vi %>% summary_posterior(parm=parm_names)

# Scenario #3

## Main simulation
s3 <- p3 %>% summary_posterior(parm=parm_names)
s3_npi <- p3_npi %>% summary_posterior(parm=parm_names)
s3_vi <- p3_vi %>% summary_posterior(parm=parm_names)
s3_npi_vi <- p3_npi_vi %>% summary_posterior(parm=parm_names)


Legend <- ggplot() +
  geom_ribbon(data=data.frame(vi=c('0 (fixed)','estimated')),
              aes(x=vi, ymin=0, ymax=1, fill=vi, col=vi), alpha=0.5) +
  
  geom_line(data=data.frame(x=c(1,2), y=c(0.5,0.5), lty = ""),
            aes(x=x, y=y, linetype=lty), lwd=0.6) +
  scale_fill_manual(values=c("#FFA500", "#800080")) +
  scale_color_manual(values=c("#FFA500", "#800080")) +
  scale_linetype_manual(values="dashed") +
  labs(col=expression(paste(scriptstyle('Viral interaction parameter, '), phi, ' ')),
       fill=expression(paste(scriptstyle('Viral interaction parameter, '), phi, ' ')),
       lty=expression(scriptstyle('True value'))) +
  
  theme_minimal() +
  theme(legend.title=element_text(size=20), legend.text=element_text(size=15),
        legend.position='top', legend.key.width=unit(1,"cm")) +
  
  guides(linetype=guide_legend(order=2), colour=guide_legend(order=1), fill=guide_legend(order=1))

plot_grid(
  
  get_legend(Legend),
  ggdraw() + draw_label("A) Scenario #1 (main simulation)", size=15, x=0.05, hjust=0, fontface='bold'),
  
  plot_grid(
    Plot_phi(p1_vi, p1_npi_vi, real_max_vi=0) +
      scale_y_continuous(labels=percent_format(), limits=c(-0.75,0.75), breaks=seq(-0.75,0.75,0.25)),
    Plot_R0(s1$beta, s1_npi$beta, s1_vi$beta, s1_npi_vi$beta, parms1_rv) +
      theme(legend.position='none'),
    Plot_rel_err(s=s1$other_parms, s_npi=s1_npi$other_parms,
                 s_vi=s1_vi$other_parms, s_npi_vi=s1_npi_vi$other_parms,
                 real_parms=parms1_rv) + theme(legend.position='none'),
    ncol=3),
  
  plot_grid(
    Plot_fit(obs1_rv, p1, p1_npi, p1_vi, p1_npi_vi) + theme(legend.position='none'),
    Plot_cor(obs1_rv, p1, p1_npi, p1_vi, p1_npi_vi) + theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ggdraw() + draw_label("B) Scenario #2 (main simulation)", size=15, x=0.05, hjust=0, fontface='bold'),
  
  plot_grid(
    Plot_phi(p2_vi, p2_npi_vi, real_max_vi=parms2_rv$phi*max(subset(SIR2, pathogen=='IAV')$I)/parms2_rv$pop) +
      scale_y_continuous(labels=percent_format(), limits=c(-0.75,0.75), breaks=seq(-0.75,0.75,0.25)),
    Plot_R0(s2$beta, s2_npi$beta, s2_vi$beta, s2_npi_vi$beta, parms2_rv, custom_beta=TRUE) +
      theme(legend.position='none'),
    Plot_rel_err(s=s2$other_parms, s_npi=s2_npi$other_parms,
                 s_vi=s2_vi$other_parms, s_npi_vi=s2_npi_vi$other_parms,
                 real_parms=parms2_rv) + theme(legend.position='none'),
    ncol=3),
  
  plot_grid(
    Plot_fit(obs2_rv, p2, p2_npi, p2_vi, p2_npi_vi) + theme(legend.position='none'),
    Plot_cor(obs2_rv, p2, p2_npi, p2_vi, p2_npi_vi) + theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ggdraw() + draw_label("C) Scenario #3 (main simulation)", size=15, x=0.05, hjust=0, fontface='bold') +
    scale_y_continuous(labels=percent_format(), limits=c(-0.75,0.75), breaks=seq(-0.75,0.75,0.25)),
  
  plot_grid(
    Plot_phi(p3_vi, p3_npi_vi, real_max_vi=parms3_rv$phi*max(subset(SIR3, pathogen=='IAV')$I)/parms3_rv$pop) +
      scale_y_continuous(labels=percent_format(), limits=c(-0.75,0.75), breaks=seq(-0.75,0.75,0.25)),
    Plot_R0(s3$beta, s3_npi$beta, s3_vi$beta, s3_npi_vi$beta, parms3_rv) +
      theme(legend.position='none'),
    Plot_rel_err(s=s3$other_parms, s_npi=s3_npi$other_parms,
                 s_vi=s3_vi$other_parms, s_npi_vi=s3_npi_vi$other_parms,
                 real_parms=parms3_rv) + theme(legend.position='none'),
    ncol=3),
  
  plot_grid(
    Plot_fit(obs3_rv, p3, p3_npi, p3_vi, p3_npi_vi) + theme(legend.position='none'),
    Plot_cor(obs3_rv, p3, p3_npi, p3_vi, p3_npi_vi) + theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ncol=1, rel_heights=c(0.1, rep(c(0.1,0.45,0.45),3))) # 13 x 11
