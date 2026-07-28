rm(list=ls())

library(tidyverse)
library(plyr)
library(lubridate)
library(cowplot)
library(ggpubr)
library(rstan)

setwd('.../Data_and_Codes')

source("Fit/Functions.R")

load("Data/USA/Data_USA.RData")
load("Data/Canada/data_canada_province.RData")

regions <- c(paste('HHS', 2:10), 'British Columbia', 'Ontario', 'Prairies')

colnames(data_at)[2] <- 'location'
colnames(data_bc)[2] <- 'location'
colnames(data_on)[2] <- 'location'
colnames(data_pr)[2] <- 'location'

n_week <- c(nrow(data_hhs2), nrow(data_hhs3), nrow(data_hhs4),
            nrow(data_hhs5), nrow(data_hhs6), nrow(data_hhs7), nrow(data_hhs8),
            nrow(data_hhs9), nrow(data_hhs10), nrow(data_bc),
            nrow(data_on), nrow(data_pr))

n_loc <- length(n_week)

gamma <- 2 # approx 8 days for recovery
mu <- 1/80/52 # natural mortality

pop_us <- read.csv("Data/USA/State_2020_pop.csv")
pop_ca <- read.csv("Data/Canada/Canada_pop_size.csv")

pop_HHS <- pop_us %>% ddply(~HHS_region, function(X){
  return(c('pop_2020'=sum(X$pop_2020)))
})

fit <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_all_locations_v3_2.RDS")

check_hmc_diagnostics(fit)
get_num_divergent(fit)
get_num_max_treedepth(fit)
get_low_bfmi_chains(fit)
get_bfmi(fit)

p <- extract(fit)

i2 <- cumsum(n_week)
i1 <- c(1, i2[-length(i2)]+1)

obs_fit <- rbind(
  
  cbind(data_hhs2[,colnames(data_hhs2) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[1]:i2[1]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs3[,colnames(data_hhs3) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[2]:i2[2]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs4[,colnames(data_hhs4) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[3]:i2[3]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs5[,colnames(data_hhs5) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[4]:i2[4]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs6[,colnames(data_hhs6) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[5]:i2[5]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs7[,colnames(data_hhs7) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[6]:i2[6]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs8[,colnames(data_hhs8) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[7]:i2[7]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs9[,colnames(data_hhs9) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[8]:i2[8]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_hhs10[,colnames(data_hhs10) %in% c('location', 'date', 'RV_scaled_cases')],
        p$cases[,i1[9]:i2[9]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  
  cbind(data_bc[,colnames(data_bc) %in% c('location','date', 'RV_scaled_cases')],
        p$cases[,i1[10]:i2[10]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_on[,colnames(data_on) %in% c('location','date', 'RV_scaled_cases')],
        p$cases[,i1[11]:i2[11]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t),
  cbind(data_pr[,colnames(data_pr) %in% c('location','date', 'RV_scaled_cases')],
        p$cases[,i1[12]:i2[12]] %>% apply(2, quantile, probs=c(0.025,0.5,0.975)) %>% t)
)

obs_fit$location <- gsub('BC', 'British Columbia', obs_fit$location)
obs_fit$location <- gsub('ON', 'Ontario', obs_fit$location)
obs_fit$location <- gsub('Pr', 'Prairies', obs_fit$location)

(Fig_fit <- obs_fit %>%
    mutate(location=factor(location, levels=regions)) %>%
    ggplot(aes(x=date)) +
    facet_wrap(~location, ncol=2, scales='free_y') +
    labs(y='Detections\n') +
    geom_rect(aes(xmin=ymd('2020-02-15'), xmax=ymd('2022-10-15'),
                  ymin=-Inf, ymax=+Inf), fill='grey85') +
    geom_vline(xintercept=ymd(paste0(2014:2025, "-01-01")), lty='dotted', col="grey40", lwd=0.3) +
    geom_point(aes(y=RV_scaled_cases), pch=21, stroke=0.005, size=1, alpha=0.8) +
    geom_line(aes(y=`50%`), lwd=0.4, col='blue') +
    geom_ribbon(aes(ymin=`2.5%`, ymax=`97.5%`), fill='#2EACFF', alpha=0.25) +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.y=element_text(size=7),
          strip.background = element_blank()))

phi_estim <- rbind(data.frame('country'='US', 'phi'=p$phi[,1]),
                   data.frame('country'='Canada', 'phi'=p$phi[,2])) %>%
  mutate(country=factor(country, levels=c('Canada', 'US')))

phi_quantiles <- compute_median_95CI(p$phi) %>%
  cbind(country=c('US', 'Canada')) %>%
  mutate(country=factor(country, levels=c('Canada', 'US')))

(Fig_phi <- ggplot(phi_estim, aes(x=phi, y=country)) +
    labs(x="\nMax. change in transmission due to viral interaction\n") +
    geom_vline(xintercept=0, lwd=0.2, lty='dashed') +
    geom_violin(fill="#800080", col=NA, alpha=0.2) +
    geom_segment(data=phi_quantiles, aes(x=CI_lower, xend=CI_upper), lwd=0.2) +
    geom_point(data=phi_quantiles, aes(x=median), pch=21, fill="#800080", cex=2) +
    scale_x_continuous(labels=scales::percent, limits=c(-0.75,0.75), breaks=seq(-0.75,0.75,0.25)) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    theme_bw() +
    theme(axis.title.x=element_text(size=9),
          axis.title.y=element_blank(), legend.position='none'))

beta_quantiles <- p$beta %>% apply(c(2,3), quantile, probs=c(0.025, 0.5, 0.975))

R0_factor <- (1-exp(-mu))/(1-exp(-(gamma+mu)))/mu

(Fig_R0 <- expand_grid(week=1:52, location=regions) %>%
    mutate(
      location=factor(location, levels=regions),
      CI_lower=as.vector(beta_quantiles[1,,]),
      median=as.vector(beta_quantiles[2,,]),
      CI_upper=as.vector(beta_quantiles[3,,])
    ) %>%
    ggplot(aes(x=week)) +
    facet_wrap(~location) +
    labs(x='Week', y='Basic reproduction number') +
    geom_ribbon(aes(ymin=CI_lower*R0_factor, ymax=CI_upper*R0_factor),
                fill="#EF9AA2", col=NA, alpha=0.55) +
    geom_line(aes(y=median*R0_factor), col="#E81C2C") +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_y_continuous(expand=c(0,0)) +
    theme_test() +
    theme(axis.text.x=element_text(size=7),
          axis.title.y.left=element_text(margin=margin(r=10, unit="pt")),
          axis.title.y.right=element_text(margin=margin(l=10, unit="pt"))))

Omega <- p$omega %>% rate_to_duration
Omega_quantiles <- Omega %>% as.vector %>% compute_median_95CI

dens <- density(Omega)
dens_df <- data.frame(x=dens$x, y=dens$y) %>%
  subset(x>=Omega_quantiles[['CI_lower']] & x<=Omega_quantiles[['CI_upper']])

(Fig_Omega <- ggplot(data=data.frame(x=Omega), aes(x=x)) +
    geom_area(data=dens_df, aes(y=y), fill='#808000', alpha=0.25) +
    annotate(geom='segment', x=Omega_quantiles[['median']], y=0,
             yend=dens_df$y[which.min(abs(dens_df$x-Omega_quantiles[["median"]]))], col='#808000', linewidth=0.7) +
    labs(x=expression(paste(hat(Omega), ", mean duration of immune protection (weeks)")),
         y='Density\n') +
    geom_density() +
    scale_x_continuous(limits=c(5,8), breaks=5:8) +
    scale_y_continuous(expand=c(0,0)) +
    theme_classic())

plot_grid(
  Fig_fit,
  plot_grid(
    plot_grid(Fig_phi, Fig_Omega, ncol=1, rel_heights=c(0.55,0.45), labels=LETTERS[2:3]),
    Fig_R0,
    ncol=2, rel_widths=c(0.45,0.55), labels=c(NA, 'D')),
  ncol=1, rel_heights=c(0.62,0.38), labels='A')

ggsave('Results_fit_combined.pdf', width=9, height=11)
