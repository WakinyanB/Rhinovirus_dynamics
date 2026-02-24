rm(list=ls())

library(tidyverse)
library(plyr)
library(scales)
library(rstan)

setwd('C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/')

x_labs <- (1:2) %>% factor(labels=c('Known', 'Unknown'))

dt=1
mu=1/80/52
gamma=2

# ------------------ Scenario 2 ------------------------------------------------

load("Data/Simulation_v2/cases_2.RData")
SIR <- read.csv2("Data/Simulation_v2/SIR_2.csv")
parms <- readRDS("Data/Simulation_v2/parms_simul_2.rds")

p2 <- readRDS('Fit/Simulation_study2/Output/fit_RV_SIRS_npi_vi_simul2.RDS') %>% extract # knowing NPIs shapes
p2_estim_all_npi <- readRDS('Fit/Simulation_study2/Output/fit_RV_SIRS_npi_vi_simul2_npi_estim.RDS') %>% extract # Estimating all NPIs

phi2 <- rbind(data.frame(id=x_labs[1], phi=p2$phi),
              data.frame(id=x_labs[2], phi=p2_estim_all_npi$phi))

(Fig_phi2 <- ggplot(phi2, aes(x=id, y=phi, fill=id)) +
    geom_hline(yintercept=0, linewidth=0.6, col="grey70") +
    geom_hline(yintercept=parms$phi_iav*max(subset(SIR, pathogen=='IAV')$I)/parms$pop, linewidth=0.3, lty='dashed') +
    labs(x='NPI shape', y="Max. change in\nforce of infection\ndue to viral interaction") +
    geom_violin(trim=FALSE, col=NA, alpha=0.4) +
    geom_segment(data=data.frame(id=x_labs,
                                 CI95_lower=quantile(p2$phi, probs=c(0.025,0.975)),
                                 CI95_upper=quantile(p2_estim_all_npi$phi, probs=c(0.025,0.975))),
                 aes(y=CI95_lower, yend=CI95_upper), lwd=0.3) +
    geom_point(data=data.frame(id=x_labs, m=c(median(p2$phi),
                                           median(p2_estim_all_npi$phi))), aes(y=m),
               pch=21, size=2, stroke=0.1) +
    scale_y_continuous(labels=percent_format(), limits=c(-0.5,0.2), breaks=seq(-0.5,0.2,0.2)) +
    theme_bw() +
    theme(legend.position='none', axis.title.y=element_text(size=10)))
  
obs_rv <- p2_estim_all_npi$cases %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t %>% cbind(obs_rv,.)
obs_rv$year <- obs_rv$time/52

npi_period <- range(obs_rv$year[which(obs_rv$c!=0)])

(Fig_fit2 <- ggplot(obs_rv, aes(x=year)) +
  labs(x='Time (years)', y='RV detections\n', title='Scenario #2') +
  geom_rect(aes(xmin=npi_period[1], xmax=npi_period[2], ymin=-Inf, ymax=+Inf), fill='grey85') +
  geom_line(aes(y=`50%`), col='#03A3A8') +
  geom_ribbon(aes(ymin=`2.5%`, ymax=`97.5%`), fill='#00BFC4', alpha=0.3) +
  geom_point(aes(y=cases_noisy), size=0.4) +
  scale_x_continuous(breaks=0:10, expand=c(0,0)) +
  theme_test() + theme(plot.title=element_text(face='bold', size=13)))

beta2 <- rbind(
  cbind(estim_all_npi=x_labs[1],
        data.frame('week'=1:52, t(apply(p2$beta, 2, quantile, probs=c(0.025,0.5,0.975))))),
  cbind(estim_all_npi=x_labs[2],
        data.frame('week'=1:52, t(apply(p2_estim_all_npi$beta, 2, quantile, probs=c(0.025,0.5,0.975)))))
  )

colnames(beta2)[3:5] <- c('CI95_lower', 'median', 'CI95_upper') 

(Fig_beta2 <- ggplot(beta2, aes(x=week)) +
  labs(x='Week', y='Basic repro-\nduction number', col='NPI shape', fill='NPI shape') +
  geom_ribbon(aes(ymin=CI95_lower*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu,
                  ymax=CI95_upper*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu,
                  fill=estim_all_npi),
              alpha=0.25, colour=NA) +
  geom_line(aes(y=median*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu, col=estim_all_npi)) +
  geom_line(data=data.frame("week"=1:52, "R0"=parms$beta_rv/(parms$gamma_rv+parms$mu)), aes(y=R0), lty='dashed', lwd=0.2) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(1,5.5), breaks=seq(1,6,1)) +
  theme_test())

# ------------------ Scenario 3 ------------------------------------------------

load("Data/Simulation_v2/cases_3.RData")
SIR <- read.csv2("Data/Simulation_v2/SIR_3.csv")
parms <- readRDS("Data/Simulation_v2/parms_simul_3.rds")

p3 <- readRDS('Fit/Simulation_study2/Output/fit_RV_SIRS_npi_vi_simul3.RDS') %>% extract # knowing NPIs shapes
p3_estim_all_npi <- readRDS('Fit/Simulation_study2/Output/fit_RV_SIRS_npi_vi_simul3_npi_estim.RDS') %>% extract # Estimating all NPIs

phi3 <- rbind(data.frame(id=x_labs[1], phi=p3$phi),
              data.frame(id=x_labs[2], phi=p3_estim_all_npi$phi))

(Fig_phi3 <- ggplot(phi3, aes(x=id, y=phi, fill=id)) +
    geom_hline(yintercept=0, linewidth=0.6, col="grey70") +
    geom_hline(yintercept=parms$phi_iav*max(subset(SIR, pathogen=='IAV')$I)/parms$pop, linewidth=0.3, lty='dashed') +
    labs(x='NPI shape', y="Max. change\nin force of infection\ndue to viral interaction") +
    geom_violin(trim=FALSE, col=NA, alpha=0.4) +
    geom_segment(data=data.frame(id=x_labs,
                                 CI95_lower=quantile(p3$phi, probs=c(0.025,0.975)),
                                 CI95_upper=quantile(p3_estim_all_npi$phi, probs=c(0.025,0.975))),
                 aes(y=CI95_lower, yend=CI95_upper), lwd=0.3) +
    geom_point(data=data.frame(id=x_labs, m=c(median(p3$phi),
                                              median(p3_estim_all_npi$phi))), aes(y=m),
               pch=21, size=2, stroke=0.1) +
    scale_y_continuous(labels=percent_format(), limits=c(-0.5,0.2), breaks=seq(-0.5,0.2,0.2)) +
    theme_bw() +
    theme(legend.position='none', axis.title.y=element_text(size=10)))

obs_rv <- p3_estim_all_npi$cases %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t %>% cbind(obs_rv,.)
obs_rv$year <- obs_rv$time/52

npi_period <- range(obs_rv$year[which(obs_rv$c!=0)])

(Fig_fit3 <- ggplot(obs_rv, aes(x=year)) +
    labs(x='Time (years)', y='RV detections\n', title='Scenario #3') +
    geom_rect(aes(xmin=npi_period[1], xmax=npi_period[2], ymin=-Inf, ymax=+Inf), fill='grey85') +
    geom_line(aes(y=`50%`), col='#03A3A8') +
    geom_ribbon(aes(ymin=`2.5%`, ymax=`97.5%`), fill='#00BFC4', alpha=0.3) +
    geom_point(aes(y=cases_noisy), size=0.4) +
    scale_x_continuous(breaks=0:10, expand=c(0,0)) +
    theme_test()+ theme(plot.title=element_text(face='bold', size=13)))

beta3 <- rbind(
  cbind(estim_all_npi=x_labs[1],
        data.frame('week'=1:52, t(apply(p3$beta, 2, quantile, probs=c(0.025,0.5,0.975))))),
  cbind(estim_all_npi=x_labs[2],
        data.frame('week'=1:52, t(apply(p3_estim_all_npi$beta, 2, quantile, probs=c(0.025,0.5,0.975)))))
)

colnames(beta3)[3:5] <- c('CI95_lower', 'median', 'CI95_upper') 

seasonal_beta3 <- parms$b0_rv*(1+parms$a1_rv*cos(4*pi*((1:52)/52-parms$d1_rv))+
                                 parms$a2_rv*cos(2*pi*((1:52)/52-(parms$d1_rv+parms$d2_rv))))

(Fig_beta3 <- ggplot(beta3, aes(x=week)) +
    labs(x='Week', y='Basic repro-\nduction number', col='NPI shape', fill='NPI shape') +
    geom_ribbon(aes(ymin=CI95_lower*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu,
                    ymax=CI95_upper*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu,
                    fill=estim_all_npi),
                alpha=0.25, colour=NA) +
    geom_line(aes(y=median*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu, col=estim_all_npi)) +
    geom_line(data=data.frame("week"=1:52, "R0"=seasonal_beta3/(parms$gamma_rv+parms$mu)), aes(y=R0), lty='dashed', lwd=0.2) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(1,5.5), breaks=seq(1,6,1)) +
    theme_test())

# ------------------ Combine plots ---------------------------------------------

plot_grid(
  plot_grid(Fig_fit2, plot_grid(Fig_phi2, Fig_beta2, align='h', labels=c('B','C'), ncol=2), labels='A', ncol=1),
  plot_grid(Fig_fit3, plot_grid(Fig_phi3, Fig_beta3, align='h', labels=c('E','F'), ncol=2), labels='D', ncol=1),
  ncol=1
) # 9 x 7
