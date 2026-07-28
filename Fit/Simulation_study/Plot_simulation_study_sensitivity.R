rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)
library(cowplot)
library(scales)

setwd('.../Data_and_Codes')

source('Fit/Functions.R')

parms <- readRDS('Data/Simulation/parms_simul_2.rds')

pop <- parms[['pop']]

default_values <- c('phi'=parms[['phi_iav']], 'kappa'=parms[['k_rv']],
                    'Omega'=1/parms[['omega_rv']], 'rho'=parms[['rho_rv']])

beta <- cbind('Week'=1:52, 'beta'=parms[['beta_rv']]/(parms[['gamma_rv']]+parms[['mu']]))

parms <- c('phi', 'kappa', 'Omega', 'rho', 'S0', 'I0')

p <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2.RDS') %>% extract
SIR <- read.csv2('Data/Simulation/SIR_2.csv')

true_value <- function(parm_to_change=NULL, SIR){
  
  res <- default_values
  res[names(parm_to_change)] <- parm_to_change
  res['phi'] <- res['phi']*max(subset(SIR, pathogen=='IAV')$I)/pop
  
  return(c(res, (subset(SIR, pathogen=='RV' & time==0)[c('S','I')]/pop) %>% setNames(c('S0', 'I0'))))
}

summary_posterior_list <- function(posterior){
  return(posterior[c('phi', 'kappa', 'omega', 'rho', 'S0', 'I0')] %>% lapply(function(X){
    return(X %>% as.vector %>% compute_median_95CI)}) %>%
      bind_rows)
}

beta_to_R0 <- function(beta, gamma=log(1/(1-7/8)), mu=1/80/52, dt=1){
  beta[,-1] <- beta[,-1]*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu
  return(beta)
}

pd <- position_dodge(width=0.7)

x_labels <- c(phi="hat(phi)", kappa="hat(kappa)", Omega="hat(Omega)", rho="hat(rho)", S0="hat(S)[0]", I0="hat(I)[0]")

# Omega

Omega <- c(default_values[['Omega']], 2, 52)

p_omega1 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_omega1.RDS') %>% extract
p_omega2 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_omega2.RDS') %>% extract

SIR_omega1 <- read.csv2('Data/Simulation/SIR_2_omega1.csv')
SIR_omega2 <- read.csv2('Data/Simulation/SIR_2_omega2.csv')

estim_Omega <- rbind(
  data.frame('id'=Omega[1], parm=parms, summary_posterior_list(p)),
  data.frame('id'=Omega[2], parm=parms, summary_posterior_list(p_omega1)),
  data.frame('id'=Omega[3], parm=parms, summary_posterior_list(p_omega2))
  )

estim_Omega[estim_Omega$parm=='Omega',-(1:2)] <- estim_Omega[estim_Omega$parm=='Omega',-(1:2)] %>% rate_to_duration

true_value_Omega <- rbind(
  data.frame('id'=Omega[1], t(true_value(SIR=SIR))),
  data.frame('id'=Omega[2], t(true_value(parm_to_change=c('Omega'=Omega[2]), SIR=SIR_omega1))),
  data.frame('id'=Omega[3], t(true_value(parm_to_change=c('Omega'=Omega[3]), SIR=SIR_omega2)))
)

estim_Omega$true_value <- NA

for(i in 1:nrow(estim_Omega)){
  estim_Omega$true_value[i] <- true_value_Omega[[match(estim_Omega$id[i], true_value_Omega$id),
                                                 match(estim_Omega$parm[i], colnames(true_value_Omega))]]
}

colors <- c("#BCBDDC", "#807DBA", "#3F007D")

(Fig_Omega <- plot_grid(
  
  rbind(
    data.frame('id'=Omega[1], p %>% summary_beta %>% beta_to_R0),
    data.frame('id'=Omega[2], p_omega1 %>% summary_beta %>% beta_to_R0),
    data.frame('id'=Omega[3], p_omega2 %>% summary_beta %>% beta_to_R0)
    ) %>%
    ggplot(aes(x=Week)) +
    geom_line(aes(y=median, col=as.factor(id))) +
    geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=as.factor(id)), alpha=0.2) +
    geom_line(data=beta, aes(y=beta), lty='dashed', lwd=0.2) +
    labs(y="Basic reproduction number\n", fill=expression(paste(Omega,' (weeks)')),
         col=expression(paste(Omega,' (weeks)'))) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme_test() +
    theme(axis.title.y=element_text(size=10), legend.position='left'),
  
  estim_Omega %>%
    mutate(parm=factor(parm, levels=parms)) %>%
    
    ggplot(aes(x=parm, col=as.factor(id))) +
    geom_hline(yintercept=0, lty="dashed", linewidth=0.3) +
    labs(y="Relative error", col=expression(paste(Omega,' (weeks)'))) +
    geom_segment(aes(y=CI_lower/true_value-1, yend=CI_upper/true_value-1), position=pd) +
    geom_point(aes(y=median/true_value-1), position=pd) +
    scale_x_discrete(labels=function(x){parse(text=x_labels[x])}) +
    scale_y_continuous(labels=percent_format(), breaks=seq(-1,1,0.5), limits=c(-1.5,1.5)) +
    scale_color_manual(values=colors) +
    theme_test() +
    theme(axis.title.x=element_blank(), legend.position='none'),
  
  ncol=2, rel_widths=c(0.55,0.45)))

# k_rv

k_rv <- c(default_values[['kappa']], 0.5, 1.5)

p_k_rv1 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_kappa_rv1.RDS') %>% extract
p_k_rv2 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_kappa_rv2.RDS') %>% extract

SIR_k_rv1 <- read.csv2('Data/Simulation/SIR_2_kappa_rv1.csv')
SIR_k_rv2 <- read.csv2('Data/Simulation/SIR_2_kappa_rv2.csv')

estim_k_rv <- rbind(
  data.frame('id'=k_rv[1], parm=parms, summary_posterior_list(p)),
  data.frame('id'=k_rv[2], parm=parms, summary_posterior_list(p_k_rv1)),
  data.frame('id'=k_rv[3], parm=parms, summary_posterior_list(p_k_rv2))
)

estim_k_rv[estim_k_rv$parm=='Omega',-(1:2)] <- estim_k_rv[estim_k_rv$parm=='Omega',-(1:2)] %>% rate_to_duration

true_value_k_rv <- rbind(
  data.frame('id'=k_rv[1], t(true_value(SIR=SIR))),
  data.frame('id'=k_rv[2], t(true_value(parm_to_change=c('kappa'=k_rv[2]), SIR=SIR_k_rv1))),
  data.frame('id'=k_rv[3], t(true_value(parm_to_change=c('kappa'=k_rv[3]), SIR=SIR_k_rv2)))
)

estim_k_rv$true_value <- NA

for(i in 1:nrow(estim_k_rv)){
  estim_k_rv$true_value[i] <- true_value_k_rv[[match(estim_k_rv$id[i], true_value_k_rv$id),
                                               match(estim_k_rv$parm[i], colnames(true_value_k_rv))]]
}

colors <- c("#9ECAE1", "#4292C6", "#08306B")

(Fig_k_rv <- plot_grid(
  
  rbind(
    data.frame('id'=k_rv[1], p %>% summary_beta %>% beta_to_R0),
    data.frame('id'=k_rv[2], p_k_rv1 %>% summary_beta %>% beta_to_R0),
    data.frame('id'=k_rv[3], p_k_rv2 %>% summary_beta %>% beta_to_R0)
    ) %>%
    ggplot(aes(x=Week)) +
    geom_line(aes(y=median, col=as.factor(id))) +
    geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=as.factor(id)), alpha=0.2) +
    geom_line(data=beta, aes(y=beta), lty='dashed', lwd=0.2) +
    labs(y="Basic reproduction number\n", fill=expression(kappa), col=expression(kappa)) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme_test() +
    theme(axis.title.y=element_text(size=10), legend.position='left'),
  
  estim_k_rv %>%
    mutate(parm=factor(parm, levels=parms)) %>%
    
    ggplot(aes(x=parm, col=as.factor(id))) +
    geom_hline(yintercept=0, lty="dashed", linewidth=0.3) +
    labs(y="Relative error", col=expression(kappa)) +
    geom_segment(aes(y=CI_lower/true_value-1, yend=CI_upper/true_value-1), position=pd) +
    geom_point(aes(y=median/true_value-1), position=pd) +
    scale_x_discrete(labels=function(x){parse(text=x_labels[x])}) +
    scale_y_continuous(labels=percent_format(), breaks=seq(-1,1,0.5), limits=c(-1,1)) +
    scale_color_manual(values=colors) +
    theme_test() +
    theme(axis.title.x=element_blank(), legend.position='none'),
  
  ncol=2, rel_widths=c(0.55,0.45)))

# k_iav

k_iav <- c(1, 0.5, 1.5)

p_k_iav1 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_kappa_iav1.RDS') %>% extract
p_k_iav2 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_kappa_iav2.RDS') %>% extract

SIR_k_iav1 <- read.csv2('Data/Simulation/SIR_2_kappa_iav1.csv')
SIR_k_iav2 <- read.csv2('Data/Simulation/SIR_2_kappa_iav2.csv')

estim_k_iav <- rbind(
  data.frame('id'=k_iav[1], parm=parms, summary_posterior_list(p)),
  data.frame('id'=k_iav[2], parm=parms, summary_posterior_list(p_k_iav1)),
  data.frame('id'=k_iav[3], parm=parms, summary_posterior_list(p_k_iav2))
)

estim_k_iav[estim_k_iav$parm=='Omega',-(1:2)] <- estim_k_iav[estim_k_iav$parm=='Omega',-(1:2)] %>% rate_to_duration

true_value_k_iav <- rbind(
  data.frame('id'=k_iav[1], t(true_value(SIR=SIR))),
  data.frame('id'=k_iav[2], t(true_value(SIR=SIR_k_iav1))),
  data.frame('id'=k_iav[3], t(true_value(SIR=SIR_k_iav2)))
)

estim_k_iav$true_value <- NA

for(i in 1:nrow(estim_k_iav)){
  estim_k_iav$true_value[i] <- true_value_k_iav[[match(estim_k_iav$id[i], true_value_k_iav$id),
                                                 match(estim_k_iav$parm[i], colnames(true_value_k_iav))]]
}

colors <- c("#A1D99B", "#41AB5D", "#00441B")

(Fig_k_iav <- plot_grid(
  
  rbind(
    data.frame('id'=k_iav[1], p %>% summary_beta %>% beta_to_R0),
    data.frame('id'=k_iav[2], p_k_iav1 %>% summary_beta %>% beta_to_R0),
    data.frame('id'=k_iav[3], p_k_iav2 %>% summary_beta %>% beta_to_R0)
    ) %>%
    ggplot(aes(x=Week)) +
    geom_line(aes(y=median, col=as.factor(id))) +
    geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=as.factor(id)), alpha=0.2) +
    geom_line(data=beta, aes(y=beta), lty='dashed', lwd=0.2) +
    labs(y="Basic reproduction number\n", fill=expression(kappa[IAV]), col=expression(kappa[IAV])) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme_test() +
    theme(axis.title.y=element_text(size=10), legend.position='left'),
  
  estim_k_iav %>%
    mutate(parm=factor(parm, levels=parms)) %>%
    
    ggplot(aes(x=parm, col=as.factor(id))) +
    geom_hline(yintercept=0, lty="dashed", linewidth=0.3) +
    labs(y="Relative error", col=expression(kappa[IAV])) +
    geom_segment(aes(y=CI_lower/true_value-1, yend=CI_upper/true_value-1), position=pd) +
    geom_point(aes(y=median/true_value-1), position=pd) +
    scale_x_discrete(labels=function(x){parse(text=x_labels[x])}) +
    scale_y_continuous(labels=percent_format(), breaks=seq(-1,1,0.5), limits=c(-1,1)) +
    scale_color_manual(values=colors) +
    theme_test() +
    theme(axis.title.x=element_blank(), legend.position='none'),
  
  ncol=2, rel_widths=c(0.55,0.45)))

# a_iav

a_iav <- c(0.2, 0.05, 0.4)

p_a_iav1 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_a_iav1.RDS') %>% extract
p_a_iav2 <- readRDS('Fit/Simulation_study/Output/fit_RV_SIRS_npi_vi_simul2_a_iav2.RDS') %>% extract

SIR_a_iav1 <- read.csv2('Data/Simulation/SIR_2_a_iav1.csv')
SIR_a_iav2 <- read.csv2('Data/Simulation/SIR_2_a_iav2.csv')

estim_a_iav <- rbind(
  data.frame('id'=a_iav[1], parm=parms, summary_posterior_list(p)),
  data.frame('id'=a_iav[2], parm=parms, summary_posterior_list(p_a_iav1)),
  data.frame('id'=a_iav[3], parm=parms, summary_posterior_list(p_a_iav2))
)

estim_a_iav[estim_a_iav$parm=='Omega',-(1:2)] <- estim_a_iav[estim_a_iav$parm=='Omega',-(1:2)] %>% rate_to_duration

true_value_a_iav <- rbind(
  data.frame('id'=a_iav[1], t(true_value(SIR=SIR))),
  data.frame('id'=a_iav[2], t(true_value(SIR=SIR_a_iav1))),
  data.frame('id'=a_iav[3], t(true_value(SIR=SIR_a_iav2)))
)

estim_a_iav$true_value <- NA

for(i in 1:nrow(estim_a_iav)){
  estim_a_iav$true_value[i] <- true_value_a_iav[[match(estim_a_iav$id[i], true_value_a_iav$id),
                                                 match(estim_a_iav$parm[i], colnames(true_value_a_iav))]]
}

colors <- c("#FCBBA1", "#EF3B2C", "#67000D")

(Fig_a_iav <- plot_grid(
  
  rbind(
    data.frame('id'=a_iav[1], p %>% summary_beta %>% beta_to_R0),
    data.frame('id'=a_iav[2], p_a_iav1 %>% summary_beta %>% beta_to_R0),
    data.frame('id'=a_iav[3], p_a_iav2 %>% summary_beta %>% beta_to_R0)
  ) %>%
    ggplot(aes(x=Week)) +
    geom_line(aes(y=median, col=as.factor(id))) +
    geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=as.factor(id)), alpha=0.2) +
    geom_line(data=beta, aes(y=beta), lty='dashed', lwd=0.2) +
    labs(y="Basic reproduction number\n", fill=expression(a[IAV]), col=expression(a[IAV])) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme_test() +
    theme(axis.title.y=element_text(size=10), legend.position='left'),
  
  estim_a_iav %>%
    mutate(parm=factor(parm, levels=parms)) %>%
    
    ggplot(aes(x=parm, col=as.factor(id))) +
    geom_hline(yintercept=0, lty="dashed", linewidth=0.3) +
    labs(y="Relative error", col=expression(a[IAV])) +
    geom_segment(aes(y=CI_lower/true_value-1, yend=CI_upper/true_value-1), position=pd) +
    geom_point(aes(y=median/true_value-1), position=pd) +
    scale_x_discrete(labels=function(x){parse(text=x_labels[x])}) +
    scale_y_continuous(labels=percent_format(), breaks=seq(-1,1,0.5), limits=c(-1,1)) +
    scale_color_manual(values=colors) +
    theme_test() +
    theme(axis.title.x=element_blank(), legend.position='none'),
  
  ncol=2, rel_widths=c(0.55,0.45)))

# Final plot

plot_grid(Fig_Omega, Fig_k_rv, Fig_k_iav, Fig_a_iav, ncol=1, labels=LETTERS[1:4], align='hv')
ggsave('simulation_sensitivity.pdf', width=7, height=9)
