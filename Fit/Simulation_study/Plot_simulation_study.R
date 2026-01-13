rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)
library(cowplot)
library(scales)
library(ggpubr)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV")
source("Stan_fit/Simul3/Functions.R")

parm_names <- c("beta", "kappa", "omega", "rho", "S0", "I0")

widths <- c(0.4, 0.8,1) # plot widths

# ----------- Simulated dataset 0 ----------------------------------------------

load("Stan_fit/Simul3/Posterior_simul0_sdlow.RData")
load("Stan_fit/Simul3/Posterior_simul0_sdhigh.RData")

parms0 <- readRDS("Data/Simulated_data2/parms0_simul.rds")

parms0_rv <- parms0 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init0_RV.rds"))
parms0_kick_rv <- parms0 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init0_kick_RV.rds"))
parms0_shift_rv <- parms0 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init0_shift_RV.rds"))

load("Data/Simulated_data2/cases_0.RData")
load("Data/Simulated_data2/cases_kick_0.RData")
load("Data/Simulated_data2/cases_shift_0.RData")

SIR0 <- read.csv2("Data/Simulated_data2/SIR_0.csv")
SIR0_kick <- read.csv2("Data/Simulated_data2/SIR_kick_0.csv")
SIR0_shift <- read.csv2("Data/Simulated_data2/SIR_shift_0.csv")

# Endemic attractor

s0_sdlow <- p0_sdlow %>% summary_posterior(parm=parm_names)
s0_npi_sdlow <- p0_npi_sdlow %>% summary_posterior(parm=parm_names)
s0_vi_sdlow <- p0_vi_sdlow %>% summary_posterior(parm=parm_names)
s0_npi_vi_sdlow <- p0_npi_vi_sdlow %>% summary_posterior(parm=parm_names)
s0_sdhigh <- p0_sdhigh %>% summary_posterior(parm=parm_names)
s0_npi_sdhigh <- p0_npi_sdhigh %>% summary_posterior(parm=parm_names)
s0_vi_sdhigh <- p0_vi_sdhigh %>% summary_posterior(parm=parm_names)
s0_npi_vi_sdhigh <- p0_npi_vi_sdhigh %>% summary_posterior(parm=parm_names)

estim0 <- plot_grid(
  
  Plot_phi(p0_vi_sdlow, p0_npi_vi_sdlow, p0_vi_sdhigh, p0_npi_vi_sdhigh, obs_iav, SIR0, parms0_rv),
  
  Plot_rel_err(s_sdlow=s0_sdlow$other_parms,
               s_npi_sdlow=s0_npi_sdlow$other_parms,
               s_vi_sdlow=s0_vi_sdlow$other_parms,
               s_npi_vi_sdlow=s0_npi_vi_sdlow$other_parms,
               s_sdhigh=s0_sdhigh$other_parms,
               s_npi_sdhigh=s0_npi_sdhigh$other_parms,
               s_vi_sdhigh=s0_vi_sdhigh$other_parms,
               s_npi_vi_sdhigh=s0_npi_vi_sdhigh$other_parms,
               real_parms=parms0_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s0_sdlow$beta,
          beta_npi_sdlow=s0_npi_sdlow$beta,
          beta_vi_sdlow=s0_vi_sdlow$beta,
          beta_npi_vi_sdlow=s0_npi_vi_sdlow$beta,
          beta_sdhigh=s0_sdhigh$beta,
          beta_npi_sdhigh=s0_npi_sdhigh$beta,
          beta_vi_sdhigh=s0_vi_sdhigh$beta,
          beta_npi_vi_sdhigh=s0_npi_vi_sdhigh$beta,
          real_parms=parms0_rv),
  
  ncol=3, rel_widths=widths)

# Kick in IAV dynamics

s0_sdlow_kick <- p0_sdlow_kick %>% summary_posterior(parm=parm_names)
s0_npi_sdlow_kick <- p0_npi_sdlow_kick %>% summary_posterior(parm=parm_names)
s0_vi_sdlow_kick <- p0_vi_sdlow_kick %>% summary_posterior(parm=parm_names)
s0_npi_vi_sdlow_kick <- p0_npi_vi_sdlow_kick %>% summary_posterior(parm=parm_names)
s0_sdhigh_kick <- p0_sdhigh_kick %>% summary_posterior(parm=parm_names)
s0_npi_sdhigh_kick <- p0_npi_sdhigh_kick %>% summary_posterior(parm=parm_names)
s0_vi_sdhigh_kick <- p0_vi_sdhigh_kick %>% summary_posterior(parm=parm_names)
s0_npi_vi_sdhigh_kick <- p0_npi_vi_sdhigh_kick %>% summary_posterior(parm=parm_names)

estim0_kick <- plot_grid(
  
  Plot_phi(p0_vi_sdlow_kick, p0_npi_vi_sdlow_kick, p0_vi_sdhigh_kick, p0_npi_vi_sdhigh_kick,
           obs_kick_iav, SIR0_kick, parms0_kick_rv),
  
  Plot_rel_err(s_sdlow=s0_sdlow_kick$other_parms,
               s_npi_sdlow=s0_npi_sdlow_kick$other_parms,
               s_vi_sdlow=s0_vi_sdlow_kick$other_parms,
               s_npi_vi_sdlow=s0_npi_vi_sdlow_kick$other_parms,
               s_sdhigh=s0_sdhigh_kick$other_parms,
               s_npi_sdhigh=s0_npi_sdhigh_kick$other_parms,
               s_vi_sdhigh=s0_vi_sdhigh_kick$other_parms,
               s_npi_vi_sdhigh=s0_npi_vi_sdhigh_kick$other_parms,
               real_parms=parms0_kick_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s0_sdlow_kick$beta,
          beta_npi_sdlow=s0_npi_sdlow_kick$beta,
          beta_vi_sdlow=s0_vi_sdlow_kick$beta,
          beta_npi_vi_sdlow=s0_npi_vi_sdlow_kick$beta,
          beta_sdhigh=s0_sdhigh_kick$beta,
          beta_npi_sdhigh=s0_npi_sdhigh_kick$beta,
          beta_vi_sdhigh=s0_vi_sdhigh_kick$beta,
          beta_npi_vi_sdhigh=s0_npi_vi_sdhigh_kick$beta,
          real_parms=parms0_kick_rv),
  
  ncol=3, rel_widths=widths)

# 6-month shift in NPI timing

s0_sdlow_shift <- p0_sdlow_shift %>% summary_posterior(parm=parm_names)
s0_npi_sdlow_shift <- p0_npi_sdlow_shift %>% summary_posterior(parm=parm_names)
s0_vi_sdlow_shift <- p0_vi_sdlow_shift %>% summary_posterior(parm=parm_names)
s0_npi_vi_sdlow_shift <- p0_npi_vi_sdlow_shift %>% summary_posterior(parm=parm_names)
s0_sdhigh_shift <- p0_sdhigh_shift %>% summary_posterior(parm=parm_names)
s0_npi_sdhigh_shift <- p0_npi_sdhigh_shift %>% summary_posterior(parm=parm_names)
s0_vi_sdhigh_shift <- p0_vi_sdhigh_shift %>% summary_posterior(parm=parm_names)
s0_npi_vi_sdhigh_shift <- p0_npi_vi_sdhigh_shift %>% summary_posterior(parm=parm_names)

estim0_shift <- plot_grid(
  
  Plot_phi(p0_vi_sdlow_shift, p0_npi_vi_sdlow_shift, p0_vi_sdhigh_shift, p0_npi_vi_sdhigh_shift,
           obs_shift_iav, SIR0_shift, parms0_shift_rv),
  
  Plot_rel_err(s_sdlow=s0_sdlow_shift$other_parms,
               s_npi_sdlow=s0_npi_sdlow_shift$other_parms,
               s_vi_sdlow=s0_vi_sdlow_shift$other_parms,
               s_npi_vi_sdlow=s0_npi_vi_sdlow_shift$other_parms,
               s_sdhigh=s0_sdhigh_shift$other_parms,
               s_npi_sdhigh=s0_npi_sdhigh_shift$other_parms,
               s_vi_sdhigh=s0_vi_sdhigh_shift$other_parms,
               s_npi_vi_sdhigh=s0_npi_vi_sdhigh_shift$other_parms,
               real_parms=parms0_shift_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s0_sdlow_shift$beta,
          beta_npi_sdlow=s0_npi_sdlow_shift$beta,
          beta_vi_sdlow=s0_vi_sdlow_shift$beta,
          beta_npi_vi_sdlow=s0_npi_vi_sdlow_shift$beta,
          beta_sdhigh=s0_sdhigh_shift$beta,
          beta_npi_sdhigh=s0_npi_sdhigh_shift$beta,
          beta_vi_sdhigh=s0_vi_sdhigh_shift$beta,
          beta_npi_vi_sdhigh=s0_npi_vi_sdhigh_shift$beta,
          real_parms=parms0_shift_rv),
  
  ncol=3, rel_widths=widths)

# Combining plots

plot_grid(
  ggdraw() + draw_label("A) Original simulation", size=15, hjust=0, x=0.05),
  estim0,
  ggdraw() + draw_label("B) With 6-month shift in NPI timing", size=15, hjust=0, x=0.05),
  estim0_shift,
  ggdraw() + draw_label("C) With pre-pandemic perturbation in IAV dynamics", size=15, hjust=0, x=0.05),
  estim0_kick,
  ncol=1, rel_heights=rep(c(0.1,0.9),3)
) # Landscape 12 x 10

Plot_cor(data=obs_rv, data_kick=obs_kick_rv, data_shift=obs_shift_rv,
         p_sdlow=p0_sdlow,
         p_npi_sdlow=p0_npi_sdlow,
         p_vi_sdlow=p0_vi_sdlow,
         p_npi_vi_sdlow=p0_npi_vi_sdlow,
         p_sdhigh=p0_sdhigh,
         p_npi_sdhigh=p0_npi_sdhigh,
         p_vi_sdhigh=p0_vi_sdhigh,
         p_npi_vi_sdhigh=p0_npi_vi_sdhigh,
                 
         p_sdlow_kick=p0_sdlow_kick,
         p_npi_sdlow_kick=p0_npi_sdlow_kick,
         p_vi_sdlow_kick=p0_vi_sdlow_kick,
         p_npi_vi_sdlow_kick=p0_npi_vi_sdlow_kick,
         p_sdhigh_kick=p0_sdhigh_kick,
         p_npi_sdhigh_kick=p0_npi_sdhigh_kick,
         p_vi_sdhigh_kick=p0_vi_sdhigh_kick,
         p_npi_vi_sdhigh_kick=p0_npi_vi_sdhigh_kick,
                 
         p_sdlow_shift=p0_sdlow_shift,
         p_npi_sdlow_shift=p0_npi_sdlow_shift,
         p_vi_sdlow_shift=p0_vi_sdlow_shift,
         p_npi_vi_sdlow_shift=p0_npi_vi_sdlow_shift,
         p_sdhigh_shift=p0_sdhigh_shift,
         p_npi_sdhigh_shift=p0_npi_sdhigh_shift,
         p_vi_sdhigh_shift=p0_vi_sdhigh_shift,
         p_npi_vi_sdhigh_shift=p0_npi_vi_sdhigh_shift) # Landscape 9.5 x 4.5

fit0 <- Plot_fit(obs=obs_rv,
                 p_sdlow=p0_sdlow,
                 p_npi_sdlow=p0_npi_sdlow,
                 p_vi_sdlow=p0_vi_sdlow,
                 p_npi_vi_sdlow=p0_npi_vi_sdlow,
                 p_sdhigh=p0_sdhigh,
                 p_npi_sdhigh=p0_npi_sdhigh,
                 p_vi_sdhigh=p0_vi_sdhigh,
                 p_npi_vi_sdhigh=p0_npi_vi_sdhigh)
  
fit0_kick <- Plot_fit(obs=obs_kick_rv,
                      p_sdlow=p0_sdlow_kick,
                      p_npi_sdlow=p0_npi_sdlow_kick,
                      p_vi_sdlow=p0_vi_sdlow_kick,
                      p_npi_vi_sdlow=p0_npi_vi_sdlow_kick,
                      p_sdhigh=p0_sdhigh_kick,
                      p_npi_sdhigh=p0_npi_sdhigh_kick,
                      p_vi_sdhigh=p0_vi_sdhigh_kick,
                      p_npi_vi_sdhigh=p0_npi_vi_sdhigh_kick)

fit0_shift <- Plot_fit(obs=obs_shift_rv,
                       p_sdlow=p0_sdlow_shift,
                       p_npi_sdlow=p0_npi_sdlow_shift,
                       p_vi_sdlow=p0_vi_sdlow_shift,
                       p_npi_vi_sdlow=p0_npi_vi_sdlow_shift,
                       p_sdhigh=p0_sdhigh_shift,
                       p_npi_sdhigh=p0_npi_sdhigh_shift,
                       p_vi_sdhigh=p0_vi_sdhigh_shift,
                       p_npi_vi_sdhigh=p0_npi_vi_sdhigh_shift,
                       shift=-0.5)

# ggarrange(fit0 + ggtitle("A)"),
#           fit0_shift + ggtitle("B) 6-month shift in NPIs timing"),
#           fit0_kick + ggtitle("C) Perturbation in IAV dynamics"),
#           ncol=1, common.legend=TRUE, legend="right") # Landscape 8 x 7

cor0 <- Plot_cor(data=obs_rv, data_kick=obs_kick_rv, data_shift=obs_shift_rv,
                 p_sdlow=p0_sdlow,
                 p_npi_sdlow=p0_npi_sdlow,
                 p_vi_sdlow=p0_vi_sdlow,
                 p_npi_vi_sdlow=p0_npi_vi_sdlow,
                 p_sdhigh=p0_sdhigh,
                 p_npi_sdhigh=p0_npi_sdhigh,
                 p_vi_sdhigh=p0_vi_sdhigh,
                 p_npi_vi_sdhigh=p0_npi_vi_sdhigh,
                 
                 p_sdlow_kick=p0_sdlow_kick,
                 p_npi_sdlow_kick=p0_npi_sdlow_kick,
                 p_vi_sdlow_kick=p0_vi_sdlow_kick,
                 p_npi_vi_sdlow_kick=p0_npi_vi_sdlow_kick,
                 p_sdhigh_kick=p0_sdhigh_kick,
                 p_npi_sdhigh_kick=p0_npi_sdhigh_kick,
                 p_vi_sdhigh_kick=p0_vi_sdhigh_kick,
                 p_npi_vi_sdhigh_kick=p0_npi_vi_sdhigh_kick,
                 
                 p_sdlow_shift=p0_sdlow_shift,
                 p_npi_sdlow_shift=p0_npi_sdlow_shift,
                 p_vi_sdlow_shift=p0_vi_sdlow_shift,
                 p_npi_vi_sdlow_shift=p0_npi_vi_sdlow_shift,
                 p_sdhigh_shift=p0_sdhigh_shift,
                 p_npi_sdhigh_shift=p0_npi_sdhigh_shift,
                 p_vi_sdhigh_shift=p0_vi_sdhigh_shift,
                 p_npi_vi_sdhigh_shift=p0_npi_vi_sdhigh_shift,
                 simple=TRUE) # Landscape 9.5 x 4.5

plot_grid(
  ggarrange(fit0 +
              ggtitle("A) Original simulation") +
              theme(axis.title.x=element_blank(),
                    plot.title=element_text(size=12)),
            fit0_shift + ggtitle("B) With 6-month shift in NPI timing") +
              theme(axis.title.x=element_blank(),
                    plot.title=element_text(size=12)),
            fit0_kick +
              ggtitle("C) With pre-pandemic perturbation in IAV dynamics") +
              theme(plot.title=element_text(size=12)),
            ncol=1, legend="none"),
  cor0 + theme(legend.position='top'),
  labels=c("", "D)"), label_fontface='plain', rel_widths=c(0.6,0.4)) # landscape: 11 x 7

# ----------- Simulated dataset 1 ----------------------------------------------

rm(list=setdiff(ls(), c("parm_names", "widths", lsf.str())))

load("Stan_fit/Simul3/Posterior_simul1_sdlow.RData")
load("Stan_fit/Simul3/Posterior_simul1_sdhigh.RData")

parms1 <- readRDS("Data/Simulated_data2/parms1_simul.rds")
load("Data/Simulated_data2/cases_1.RData")

parms1_rv <- parms1 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init1_RV.rds"))
parms1_kick_rv <- parms1 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init1_kick_RV.rds"))
parms1_shift_rv <- parms1 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init1_shift_RV.rds"))

load("Data/Simulated_data2/cases_1.RData")
load("Data/Simulated_data2/cases_kick_1.RData")
load("Data/Simulated_data2/cases_shift_1.RData")

SIR1 <- read.csv2("Data/Simulated_data2/SIR_1.csv")
SIR1_kick <- read.csv2("Data/Simulated_data2/SIR_kick_1.csv")
SIR1_shift <- read.csv2("Data/Simulated_data2/SIR_shift_1.csv")

# Endemic attractor

s1_sdlow <- p1_sdlow %>% summary_posterior(parm=parm_names)
s1_npi_sdlow <- p1_npi_sdlow %>% summary_posterior(parm=parm_names)
s1_vi_sdlow <- p1_vi_sdlow %>% summary_posterior(parm=parm_names)
s1_npi_vi_sdlow <- p1_npi_vi_sdlow %>% summary_posterior(parm=parm_names)
s1_sdhigh <- p1_sdhigh %>% summary_posterior(parm=parm_names)
s1_npi_sdhigh <- p1_npi_sdhigh %>% summary_posterior(parm=parm_names)
s1_vi_sdhigh <- p1_vi_sdhigh %>% summary_posterior(parm=parm_names)
s1_npi_vi_sdhigh <- p1_npi_vi_sdhigh %>% summary_posterior(parm=parm_names)

estim1 <- plot_grid(
  
  Plot_phi(p1_vi_sdlow, p1_npi_vi_sdlow, p1_vi_sdhigh, p1_npi_vi_sdhigh, obs_iav, SIR1, parms1_rv),
  
  Plot_rel_err(s_sdlow=s1_sdlow$other_parms,
               s_npi_sdlow=s1_npi_sdlow$other_parms,
               s_vi_sdlow=s1_vi_sdlow$other_parms,
               s_npi_vi_sdlow=s1_npi_vi_sdlow$other_parms,
               s_sdhigh=s1_sdhigh$other_parms,
               s_npi_sdhigh=s1_npi_sdhigh$other_parms,
               s_vi_sdhigh=s1_vi_sdhigh$other_parms,
               s_npi_vi_sdhigh=s1_npi_vi_sdhigh$other_parms,
               real_parms=parms1_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s1_sdlow$beta,
          beta_npi_sdlow=s1_npi_sdlow$beta,
          beta_vi_sdlow=s1_vi_sdlow$beta,
          beta_npi_vi_sdlow=s1_npi_vi_sdlow$beta,
          beta_sdhigh=s1_sdhigh$beta,
          beta_npi_sdhigh=s1_npi_sdhigh$beta,
          beta_vi_sdhigh=s1_vi_sdhigh$beta,
          beta_npi_vi_sdhigh=s1_npi_vi_sdhigh$beta,
          real_parms=parms1_rv),
  
  ncol=3, rel_widths=widths)

# Kick in IAV dynamics

s1_sdlow_kick <- p1_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_npi_sdlow_kick <- p1_npi_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_vi_sdlow_kick <- p1_vi_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_npi_vi_sdlow_kick <- p1_npi_vi_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_sdhigh_kick <- p1_sdhigh_kick %>% summary_posterior(parm=parm_names)
s1_npi_sdhigh_kick <- p1_npi_sdhigh_kick %>% summary_posterior(parm=parm_names)
s1_vi_sdhigh_kick <- p1_vi_sdhigh_kick %>% summary_posterior(parm=parm_names)
s1_npi_vi_sdhigh_kick <- p1_npi_vi_sdhigh_kick %>% summary_posterior(parm=parm_names)

estim1_kick <- plot_grid(
  
  Plot_phi(p1_vi_sdlow_kick, p1_npi_vi_sdlow_kick, p1_vi_sdhigh_kick, p1_npi_vi_sdhigh_kick,
           obs_kick_iav, SIR1_kick, parms1_kick_rv),
  
  Plot_rel_err(s_sdlow=s1_sdlow_kick$other_parms,
               s_npi_sdlow=s1_npi_sdlow_kick$other_parms,
               s_vi_sdlow=s1_vi_sdlow_kick$other_parms,
               s_npi_vi_sdlow=s1_npi_vi_sdlow_kick$other_parms,
               s_sdhigh=s1_sdhigh_kick$other_parms,
               s_npi_sdhigh=s1_npi_sdhigh_kick$other_parms,
               s_vi_sdhigh=s1_vi_sdhigh_kick$other_parms,
               s_npi_vi_sdhigh=s1_npi_vi_sdhigh_kick$other_parms,
               real_parms=parms1_kick_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s1_sdlow_kick$beta,
          beta_npi_sdlow=s1_npi_sdlow_kick$beta,
          beta_vi_sdlow=s1_vi_sdlow_kick$beta,
          beta_npi_vi_sdlow=s1_npi_vi_sdlow_kick$beta,
          beta_sdhigh=s1_sdhigh_kick$beta,
          beta_npi_sdhigh=s1_npi_sdhigh_kick$beta,
          beta_vi_sdhigh=s1_vi_sdhigh_kick$beta,
          beta_npi_vi_sdhigh=s1_npi_vi_sdhigh_kick$beta,
          real_parms=parms1_kick_rv),
  
  ncol=3, rel_widths=widths)

# 6-month shift in NPI timing

s1_sdlow_shift <- p1_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_npi_sdlow_shift <- p1_npi_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_vi_sdlow_shift <- p1_vi_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_npi_vi_sdlow_shift <- p1_npi_vi_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_sdhigh_shift <- p1_sdhigh_shift %>% summary_posterior(parm=parm_names)
s1_npi_sdhigh_shift <- p1_npi_sdhigh_shift %>% summary_posterior(parm=parm_names)
s1_vi_sdhigh_shift <- p1_vi_sdhigh_shift %>% summary_posterior(parm=parm_names)
s1_npi_vi_sdhigh_shift <- p1_npi_vi_sdhigh_shift %>% summary_posterior(parm=parm_names)

estim1_shift <- plot_grid(
  
  Plot_phi(p1_vi_sdlow_shift, p1_npi_vi_sdlow_shift, p1_vi_sdhigh_shift, p1_npi_vi_sdhigh_shift,
           obs_shift_iav, SIR1_shift, parms1_shift_rv),
  
  Plot_rel_err(s_sdlow=s1_sdlow_shift$other_parms,
               s_npi_sdlow=s1_npi_sdlow_shift$other_parms,
               s_vi_sdlow=s1_vi_sdlow_shift$other_parms,
               s_npi_vi_sdlow=s1_npi_vi_sdlow_shift$other_parms,
               s_sdhigh=s1_sdhigh_shift$other_parms,
               s_npi_sdhigh=s1_npi_sdhigh_shift$other_parms,
               s_vi_sdhigh=s1_vi_sdhigh_shift$other_parms,
               s_npi_vi_sdhigh=s1_npi_vi_sdhigh_shift$other_parms,
               real_parms=parms1_shift_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s1_sdlow_shift$beta,
          beta_npi_sdlow=s1_npi_sdlow_shift$beta,
          beta_vi_sdlow=s1_vi_sdlow_shift$beta,
          beta_npi_vi_sdlow=s1_npi_vi_sdlow_shift$beta,
          beta_sdhigh=s1_sdhigh_shift$beta,
          beta_npi_sdhigh=s1_npi_sdhigh_shift$beta,
          beta_vi_sdhigh=s1_vi_sdhigh_shift$beta,
          beta_npi_vi_sdhigh=s1_npi_vi_sdhigh_shift$beta,
          real_parms=parms1_shift_rv),
  
  ncol=3, rel_widths=widths)

# Combining plots

plot_grid(
  ggdraw() + draw_label("A) Original simulation", size=15, hjust=0, x=0.05),
  estim1,
  ggdraw() + draw_label("B) With 6-month shift in NPI timing", size=15, hjust=0, x=0.05),
  estim1_shift,
  ggdraw() + draw_label("C) With pre-pandemic perturbation in IAV dynamics", size=15, hjust=0, x=0.05),
  estim1_kick,
  ncol=1, rel_heights=rep(c(0.1,0.9),3)
) # Landscape 12 x 10

Plot_cor(data=obs_rv, data_kick=obs_kick_rv, data_shift=obs_shift_rv,
         p_sdlow=p1_sdlow,
         p_npi_sdlow=p1_npi_sdlow,
         p_vi_sdlow=p1_vi_sdlow,
         p_npi_vi_sdlow=p1_npi_vi_sdlow,
         p_sdhigh=p1_sdhigh,
         p_npi_sdhigh=p1_npi_sdhigh,
         p_vi_sdhigh=p1_vi_sdhigh,
         p_npi_vi_sdhigh=p1_npi_vi_sdhigh,
         
         p_sdlow_kick=p1_sdlow_kick,
         p_npi_sdlow_kick=p1_npi_sdlow_kick,
         p_vi_sdlow_kick=p1_vi_sdlow_kick,
         p_npi_vi_sdlow_kick=p1_npi_vi_sdlow_kick,
         p_sdhigh_kick=p1_sdhigh_kick,
         p_npi_sdhigh_kick=p1_npi_sdhigh_kick,
         p_vi_sdhigh_kick=p1_vi_sdhigh_kick,
         p_npi_vi_sdhigh_kick=p1_npi_vi_sdhigh_kick,
         
         p_sdlow_shift=p1_sdlow_shift,
         p_npi_sdlow_shift=p1_npi_sdlow_shift,
         p_vi_sdlow_shift=p1_vi_sdlow_shift,
         p_npi_vi_sdlow_shift=p1_npi_vi_sdlow_shift,
         p_sdhigh_shift=p1_sdhigh_shift,
         p_npi_sdhigh_shift=p1_npi_sdhigh_shift,
         p_vi_sdhigh_shift=p1_vi_sdhigh_shift,
         p_npi_vi_sdhigh_shift=p1_npi_vi_sdhigh_shift) # Landscape 9 x 4

fit1 <- Plot_fit(obs=obs_rv,
                 p_sdlow=p1_sdlow,
                 p_npi_sdlow=p1_npi_sdlow,
                 p_vi_sdlow=p1_vi_sdlow,
                 p_npi_vi_sdlow=p1_npi_vi_sdlow,
                 p_sdhigh=p1_sdhigh,
                 p_npi_sdhigh=p1_npi_sdhigh,
                 p_vi_sdhigh=p1_vi_sdhigh,
                 p_npi_vi_sdhigh=p1_npi_vi_sdhigh)

fit1_kick <- Plot_fit(obs=obs_kick_rv,
                      p_sdlow=p1_sdlow_kick,
                      p_npi_sdlow=p1_npi_sdlow_kick,
                      p_vi_sdlow=p1_vi_sdlow_kick,
                      p_npi_vi_sdlow=p1_npi_vi_sdlow_kick,
                      p_sdhigh=p1_sdhigh_kick,
                      p_npi_sdhigh=p1_npi_sdhigh_kick,
                      p_vi_sdhigh=p1_vi_sdhigh_kick,
                      p_npi_vi_sdhigh=p1_npi_vi_sdhigh_kick)

fit1_shift <- Plot_fit(obs=obs_shift_rv,
                       p_sdlow=p1_sdlow_shift,
                       p_npi_sdlow=p1_npi_sdlow_shift,
                       p_vi_sdlow=p1_vi_sdlow_shift,
                       p_npi_vi_sdlow=p1_npi_vi_sdlow_shift,
                       p_sdhigh=p1_sdhigh_shift,
                       p_npi_sdhigh=p1_npi_sdhigh_shift,
                       p_vi_sdhigh=p1_vi_sdhigh_shift,
                       p_npi_vi_sdhigh=p1_npi_vi_sdhigh_shift)

# ggarrange(fit1 + ggtitle("A)"),
#           fit1_shift + ggtitle("B) With 6-month shift in NPI timing"),
#           fit1_kick + ggtitle("C) With pre-pandemic perturbation in IAV dynamics"),
#           ncol=1, common.legend=TRUE, legend="right") # Landscape 8 x 7

cor1 <- Plot_cor(data=obs_rv, data_kick=obs_kick_rv, data_shift=obs_shift_rv,
                 p_sdlow=p1_sdlow,
                 p_npi_sdlow=p1_npi_sdlow,
                 p_vi_sdlow=p1_vi_sdlow,
                 p_npi_vi_sdlow=p1_npi_vi_sdlow,
                 p_sdhigh=p1_sdhigh,
                 p_npi_sdhigh=p1_npi_sdhigh,
                 p_vi_sdhigh=p1_vi_sdhigh,
                 p_npi_vi_sdhigh=p1_npi_vi_sdhigh,
                 
                 p_sdlow_kick=p1_sdlow_kick,
                 p_npi_sdlow_kick=p1_npi_sdlow_kick,
                 p_vi_sdlow_kick=p1_vi_sdlow_kick,
                 p_npi_vi_sdlow_kick=p1_npi_vi_sdlow_kick,
                 p_sdhigh_kick=p1_sdhigh_kick,
                 p_npi_sdhigh_kick=p1_npi_sdhigh_kick,
                 p_vi_sdhigh_kick=p1_vi_sdhigh_kick,
                 p_npi_vi_sdhigh_kick=p1_npi_vi_sdhigh_kick,
                 
                 p_sdlow_shift=p1_sdlow_shift,
                 p_npi_sdlow_shift=p1_npi_sdlow_shift,
                 p_vi_sdlow_shift=p1_vi_sdlow_shift,
                 p_npi_vi_sdlow_shift=p1_npi_vi_sdlow_shift,
                 p_sdhigh_shift=p1_sdhigh_shift,
                 p_npi_sdhigh_shift=p1_npi_sdhigh_shift,
                 p_vi_sdhigh_shift=p1_vi_sdhigh_shift,
                 p_npi_vi_sdhigh_shift=p1_npi_vi_sdhigh_shift,
                 simple=TRUE) # Landscape 9 x 4

plot_grid(
  ggarrange(fit1 +
              ggtitle("A) Original simulation") +
              theme(axis.title.x=element_blank(),
                    plot.title=element_text(size=12)),
            fit1_shift + ggtitle("B) With 6-month shift in NPI timing") +
              theme(axis.title.x=element_blank(),
                    plot.title=element_text(size=12)),
            fit1_kick +
              ggtitle("C) With pre-pandemic perturbation in IAV dynamics") +
              theme(plot.title=element_text(size=12)),
            ncol=1, legend="none"),
  cor1 + theme(legend.position='top'),
  labels=c("", "D)"), label_fontface='plain', rel_widths=c(0.6,0.4)) # 11 x 7

# ----------- Simulated dataset 1 v2 ----------------------------------------------

rm(list=setdiff(ls(), c("parm_names", "widths", lsf.str())))

load("Stan_fit/Simul3/Posterior_Simul1_v2_sdlow.RData")
load("Stan_fit/Simul3/Posterior_Simul1_v2_sdhigh.RData")

parms1_v2 <- readRDS("Data/Simulated_data2/parms1_v2_simul.rds")

parms1_v2_rv <- parms1_v2 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init1_v2_RV.rds"))
parms1_v2_kick_rv <- parms1_v2 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init1_v2_kick_RV.rds"))
parms1_v2_shift_rv <- parms1_v2 %>% extract_parms_rv(init=readRDS("Data/Simulated_data2/init1_v2_shift_RV.rds"))

load("Data/Simulated_data2/cases_1_v2.RData")
load("Data/Simulated_data2/cases_kick_1_v2.RData")
load("Data/Simulated_data2/cases_shift_1_v2.RData")

SIR1_v2 <- read.csv2("Data/Simulated_data2/SIR1_v2.csv")
SIR1_v2_kick <- read.csv2("Data/Simulated_data2/SIR_kick_1_v2.csv")
SIR1_v2_shift <- read.csv2("Data/Simulated_data2/SIR_shift_1_v2.csv")

# Endemic attractor

s1_v2_sdlow <- p1_v2_sdlow %>% summary_posterior(parm=parm_names)
s1_v2_npi_sdlow <- p1_v2_npi_sdlow %>% summary_posterior(parm=parm_names)
s1_v2_vi_sdlow <- p1_v2_vi_sdlow %>% summary_posterior(parm=parm_names)
s1_v2_npi_vi_sdlow <- p1_v2_npi_vi_sdlow %>% summary_posterior(parm=parm_names)
s1_v2_sdhigh <- p1_v2_sdhigh %>% summary_posterior(parm=parm_names)
s1_v2_npi_sdhigh <- p1_v2_npi_sdhigh %>% summary_posterior(parm=parm_names)
s1_v2_vi_sdhigh <- p1_v2_vi_sdhigh %>% summary_posterior(parm=parm_names)
s1_v2_npi_vi_sdhigh <- p1_v2_npi_vi_sdhigh %>% summary_posterior(parm=parm_names)

estim1_v2 <- plot_grid(
  
  Plot_phi(p1_v2_vi_sdlow, p1_v2_npi_vi_sdlow, p1_v2_vi_sdhigh, p1_v2_npi_vi_sdhigh, obs_iav, SIR1_v2, parms1_v2_rv),
  
  Plot_rel_err(s_sdlow=s1_v2_sdlow$other_parms,
               s_npi_sdlow=s1_v2_npi_sdlow$other_parms,
               s_vi_sdlow=s1_v2_vi_sdlow$other_parms,
               s_npi_vi_sdlow=s1_v2_npi_vi_sdlow$other_parms,
               s_sdhigh=s1_v2_sdhigh$other_parms,
               s_npi_sdhigh=s1_v2_npi_sdhigh$other_parms,
               s_vi_sdhigh=s1_v2_vi_sdhigh$other_parms,
               s_npi_vi_sdhigh=s1_v2_npi_vi_sdhigh$other_parms,
               real_parms=parms1_v2_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s1_v2_sdlow$beta,
          beta_npi_sdlow=s1_v2_npi_sdlow$beta,
          beta_vi_sdlow=s1_v2_vi_sdlow$beta,
          beta_npi_vi_sdlow=s1_v2_npi_vi_sdlow$beta,
          beta_sdhigh=s1_v2_sdhigh$beta,
          beta_npi_sdhigh=s1_v2_npi_sdhigh$beta,
          beta_vi_sdhigh=s1_v2_vi_sdhigh$beta,
          beta_npi_vi_sdhigh=s1_v2_npi_vi_sdhigh$beta,
          real_parms=parms1_v2_rv, custom_beta =TRUE),
  
  ncol=3, rel_widths=widths)

# Kick in IAV dynamics

s1_v2_sdlow_kick <- p1_v2_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_v2_npi_sdlow_kick <- p1_v2_npi_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_v2_vi_sdlow_kick <- p1_v2_vi_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_v2_npi_vi_sdlow_kick <- p1_v2_npi_vi_sdlow_kick %>% summary_posterior(parm=parm_names)
s1_v2_sdhigh_kick <- p1_v2_sdhigh_kick %>% summary_posterior(parm=parm_names)
s1_v2_npi_sdhigh_kick <- p1_v2_npi_sdhigh_kick %>% summary_posterior(parm=parm_names)
s1_v2_vi_sdhigh_kick <- p1_v2_vi_sdhigh_kick %>% summary_posterior(parm=parm_names)
s1_v2_npi_vi_sdhigh_kick <- p1_v2_npi_vi_sdhigh_kick %>% summary_posterior(parm=parm_names)

estim1_v2_kick <- plot_grid(
  
  Plot_phi(p1_v2_vi_sdlow_kick, p1_v2_npi_vi_sdlow_kick, p1_v2_vi_sdhigh_kick, p1_v2_npi_vi_sdhigh_kick,
           obs_kick_iav, SIR1_v2_kick, parms1_v2_kick_rv),
  
  Plot_rel_err(s_sdlow=s1_v2_sdlow_kick$other_parms,
               s_npi_sdlow=s1_v2_npi_sdlow_kick$other_parms,
               s_vi_sdlow=s1_v2_vi_sdlow_kick$other_parms,
               s_npi_vi_sdlow=s1_v2_npi_vi_sdlow_kick$other_parms,
               s_sdhigh=s1_v2_sdhigh_kick$other_parms,
               s_npi_sdhigh=s1_v2_npi_sdhigh_kick$other_parms,
               s_vi_sdhigh=s1_v2_vi_sdhigh_kick$other_parms,
               s_npi_vi_sdhigh=s1_v2_npi_vi_sdhigh_kick$other_parms,
               real_parms=parms1_v2_kick_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s1_v2_sdlow_kick$beta,
          beta_npi_sdlow=s1_v2_npi_sdlow_kick$beta,
          beta_vi_sdlow=s1_v2_vi_sdlow_kick$beta,
          beta_npi_vi_sdlow=s1_v2_npi_vi_sdlow_kick$beta,
          beta_sdhigh=s1_v2_sdhigh_kick$beta,
          beta_npi_sdhigh=s1_v2_npi_sdhigh_kick$beta,
          beta_vi_sdhigh=s1_v2_vi_sdhigh_kick$beta,
          beta_npi_vi_sdhigh=s1_v2_npi_vi_sdhigh_kick$beta,
          real_parms=parms1_v2_kick_rv, custom_beta=TRUE),
  
  ncol=3, rel_widths=widths)

# 6-month shift in NPI timing

s1_v2_sdlow_shift <- p1_v2_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_v2_npi_sdlow_shift <- p1_v2_npi_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_v2_vi_sdlow_shift <- p1_v2_vi_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_v2_npi_vi_sdlow_shift <- p1_v2_npi_vi_sdlow_shift %>% summary_posterior(parm=parm_names)
s1_v2_sdhigh_shift <- p1_v2_sdhigh_shift %>% summary_posterior(parm=parm_names)
s1_v2_npi_sdhigh_shift <- p1_v2_npi_sdhigh_shift %>% summary_posterior(parm=parm_names)
s1_v2_vi_sdhigh_shift <- p1_v2_vi_sdhigh_shift %>% summary_posterior(parm=parm_names)
s1_v2_npi_vi_sdhigh_shift <- p1_v2_npi_vi_sdhigh_shift %>% summary_posterior(parm=parm_names)

estim1_v2_shift <- plot_grid(
  
  Plot_phi(p1_v2_vi_sdlow_shift, p1_v2_npi_vi_sdlow_shift, p1_v2_vi_sdhigh_shift, p1_v2_npi_vi_sdhigh_shift,
           obs_shift_iav, SIR1_v2_shift, parms1_v2_shift_rv),
  
  Plot_rel_err(s_sdlow=s1_v2_sdlow_shift$other_parms,
               s_npi_sdlow=s1_v2_npi_sdlow_shift$other_parms,
               s_vi_sdlow=s1_v2_vi_sdlow_shift$other_parms,
               s_npi_vi_sdlow=s1_v2_npi_vi_sdlow_shift$other_parms,
               s_sdhigh=s1_v2_sdhigh_shift$other_parms,
               s_npi_sdhigh=s1_v2_npi_sdhigh_shift$other_parms,
               s_vi_sdhigh=s1_v2_vi_sdhigh_shift$other_parms,
               s_npi_vi_sdhigh=s1_v2_npi_vi_sdhigh_shift$other_parms,
               real_parms=parms1_v2_shift_rv) +
    theme(legend.position='none'),
  
  Plot_R0(beta_sdlow=s1_v2_sdlow_shift$beta,
          beta_npi_sdlow=s1_v2_npi_sdlow_shift$beta,
          beta_vi_sdlow=s1_v2_vi_sdlow_shift$beta,
          beta_npi_vi_sdlow=s1_v2_npi_vi_sdlow_shift$beta,
          beta_sdhigh=s1_v2_sdhigh_shift$beta,
          beta_npi_sdhigh=s1_v2_npi_sdhigh_shift$beta,
          beta_vi_sdhigh=s1_v2_vi_sdhigh_shift$beta,
          beta_npi_vi_sdhigh=s1_v2_npi_vi_sdhigh_shift$beta,
          real_parms=parms1_v2_shift_rv, custom_beta=TRUE),
  
  ncol=3, rel_widths=widths)

# Combining plots

plot_grid(
  ggdraw() + draw_label("A)", size=15, hjust=0, x=0.05),
  estim1_v2,
  ggdraw() + draw_label("B) With 6-month shift in NPI timing", size=15, hjust=0, x=0.05),
  estim1_v2_shift,
  ggdraw() + draw_label("C) With pre-pandemic perturbation in IAV dynamics", size=15, hjust=0, x=0.05),
  estim1_v2_kick,
  ncol=1, rel_heights=rep(c(0.1,0.9),3)
) # Landscape 12 x 10

fit1_v2 <- Plot_fit(obs=obs_rv,
                 p_sdlow=p1_v2_sdlow,
                 p_npi_sdlow=p1_v2_npi_sdlow,
                 p_vi_sdlow=p1_v2_vi_sdlow,
                 p_npi_vi_sdlow=p1_v2_npi_vi_sdlow,
                 p_sdhigh=p1_v2_sdhigh,
                 p_npi_sdhigh=p1_v2_npi_sdhigh,
                 p_vi_sdhigh=p1_v2_vi_sdhigh,
                 p_npi_vi_sdhigh=p1_v2_npi_vi_sdhigh)

fit1_v2_kick <- Plot_fit(obs=obs_kick_rv,
                      p_sdlow=p1_v2_sdlow_kick,
                      p_npi_sdlow=p1_v2_npi_sdlow_kick,
                      p_vi_sdlow=p1_v2_vi_sdlow_kick,
                      p_npi_vi_sdlow=p1_v2_npi_vi_sdlow_kick,
                      p_sdhigh=p1_v2_sdhigh_kick,
                      p_npi_sdhigh=p1_v2_npi_sdhigh_kick,
                      p_vi_sdhigh=p1_v2_vi_sdhigh_kick,
                      p_npi_vi_sdhigh=p1_v2_npi_vi_sdhigh_kick)

fit1_v2_shift <- Plot_fit(obs=obs_shift_rv,
                       p_sdlow=p1_v2_sdlow_shift,
                       p_npi_sdlow=p1_v2_npi_sdlow_shift,
                       p_vi_sdlow=p1_v2_vi_sdlow_shift,
                       p_npi_vi_sdlow=p1_v2_npi_vi_sdlow_shift,
                       p_sdhigh=p1_v2_sdhigh_shift,
                       p_npi_sdhigh=p1_v2_npi_sdhigh_shift,
                       p_vi_sdhigh=p1_v2_vi_sdhigh_shift,
                       p_npi_vi_sdhigh=p1_v2_npi_vi_sdhigh_shift)

# ggarrange(fit1_v2 + ggtitle("A)"),
#           fit1_v2_shift + ggtitle("B) With 6-month shift in NPI timing"),
#           fit1_v2_kick + ggtitle("C) With pre-pandemic perturbation in IAV dynamics"),
#           ncol=1, common.legend=TRUE, legend="right") # Landscape 8 x 7

cor1_v2 <- Plot_cor(data=obs_rv, data_kick=obs_kick_rv, data_shift=obs_shift_rv,
         p_sdlow=p1_v2_sdlow,
         p_npi_sdlow=p1_v2_npi_sdlow,
         p_vi_sdlow=p1_v2_vi_sdlow,
         p_npi_vi_sdlow=p1_v2_npi_vi_sdlow,
         p_sdhigh=p1_v2_sdhigh,
         p_npi_sdhigh=p1_v2_npi_sdhigh,
         p_vi_sdhigh=p1_v2_vi_sdhigh,
         p_npi_vi_sdhigh=p1_v2_npi_vi_sdhigh,
         
         p_sdlow_kick=p1_v2_sdlow_kick,
         p_npi_sdlow_kick=p1_v2_npi_sdlow_kick,
         p_vi_sdlow_kick=p1_v2_vi_sdlow_kick,
         p_npi_vi_sdlow_kick=p1_v2_npi_vi_sdlow_kick,
         p_sdhigh_kick=p1_v2_sdhigh_kick,
         p_npi_sdhigh_kick=p1_v2_npi_sdhigh_kick,
         p_vi_sdhigh_kick=p1_v2_vi_sdhigh_kick,
         p_npi_vi_sdhigh_kick=p1_v2_npi_vi_sdhigh_kick,
         
         p_sdlow_shift=p1_v2_sdlow_shift,
         p_npi_sdlow_shift=p1_v2_npi_sdlow_shift,
         p_vi_sdlow_shift=p1_v2_vi_sdlow_shift,
         p_npi_vi_sdlow_shift=p1_v2_npi_vi_sdlow_shift,
         p_sdhigh_shift=p1_v2_sdhigh_shift,
         p_npi_sdhigh_shift=p1_v2_npi_sdhigh_shift,
         p_vi_sdhigh_shift=p1_v2_vi_sdhigh_shift,
         p_npi_vi_sdhigh_shift=p1_v2_npi_vi_sdhigh_shift,
         simple=TRUE) # Landscape 9.5 x 4.5

plot_grid(
  ggarrange(fit1_v2 + ggtitle("A) Original simulation") + theme(axis.title.x=element_blank()),
            fit1_v2_shift + ggtitle("B) With 6-month shift in NPI timing") + theme(axis.title.x=element_blank()),
            fit1_v2_kick + ggtitle("C) With pre-pandemic perturbation in IAV dynamics"),
            ncol=1, legend="none"),
  cor1_v2 + theme(legend.position='top'),
  labels=c("", "D)"), label_fontface='plain', rel_widths=c(0.6,0.4)) # 11 x 7
