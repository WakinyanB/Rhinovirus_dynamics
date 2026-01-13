rm(list=ls())

library(tidyverse)
library(plyr)
library(lubridate)
library(scales)
library(cowplot)
library(ggpubr)
library(ggnewscale)
library(rstan)
library(bridgesampling)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes")

source("Fit/Functions.R")

load("Data/USA/Data_USA.RData")
load("Data/Canada/data_canada_province.RData")

US_names <- c("US (national)", paste('HHS', 2:10))
CA_names <- c("Canada (national)", "British Columbia", "Ontario", "Prairies")

# Without viral interaction

## US

p_us <- readRDS("Fit/Output/fit_RV_SIRS_npi_us.RDS") %>% extract
#p_hhs1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs1.RDS") %>% extract
p_hhs2 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs2.RDS") %>% extract
p_hhs3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs3.RDS") %>% extract
p_hhs4 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs4.RDS") %>% extract
p_hhs5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs5.RDS") %>% extract
p_hhs6 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs6.RDS") %>% extract
p_hhs7 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs7.RDS") %>% extract
p_hhs8 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs8.RDS") %>% extract
p_hhs9 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs9.RDS") %>% extract
p_hhs10 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs10.RDS") %>% extract

## Canada

p_ca <- readRDS("Fit/Output/fit_RV_SIRS_npi_ca.RDS") %>% extract
# p_at <- readRDS("Fit/Output/fit_RV_SIRS_npi_at.RDS") %>% extract
p_bc <- readRDS("Fit/Output/fit_RV_SIRS_npi_bc.RDS") %>% extract
p_on <- readRDS("Fit/Output/fit_RV_SIRS_npi_on.RDS") %>% extract
p_pr <- readRDS("Fit/Output/fit_RV_SIRS_npi_pr.RDS") %>% extract

# With viral interaction (lag0)

## US

p_us_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us.RDS") %>% extract
# p_hhs1_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1.RDS") %>% extract
p_hhs2_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2.RDS") %>% extract
p_hhs3_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3.RDS") %>% extract
p_hhs4_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4.RDS") %>% extract
p_hhs5_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5.RDS") %>% extract
p_hhs6_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6.RDS") %>% extract
p_hhs7_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7.RDS") %>% extract
p_hhs8_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8.RDS") %>% extract
p_hhs9_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9.RDS") %>% extract
p_hhs10_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10.RDS") %>% extract

## Canada

p_ca_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca.RDS") %>% extract
# p_at_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at.RDS") %>% extract
p_bc_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc.RDS") %>% extract
p_on_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on.RDS") %>% extract
p_pr_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr.RDS") %>% extract

# With viral interaction (lag1)

## US

p_us_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_lag1.RDS") %>% extract
# p_hhs1_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_lag1.RDS") %>% extract
p_hhs2_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag1.RDS") %>% extract
p_hhs3_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag1.RDS") %>% extract
p_hhs4_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag1.RDS") %>% extract
p_hhs5_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag1.RDS") %>% extract
p_hhs6_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_lag1.RDS") %>% extract
p_hhs7_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag1.RDS") %>% extract
p_hhs8_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_lag1.RDS") %>% extract
p_hhs9_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_lag1.RDS") %>% extract
p_hhs10_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag1.RDS") %>% extract

## Canada

p_ca_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_lag1.RDS") %>% extract
# p_at_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_lag1.RDS") %>% extract
p_bc_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag1.RDS") %>% extract
p_on_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_lag1.RDS") %>% extract
p_pr_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_lag1.RDS") %>% extract

# With viral interaction (lag3)

## US

p_us_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_lag3.RDS") %>% extract
# p_hhs1_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_lag3.RDS") %>% extract
p_hhs2_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag3.RDS") %>% extract
p_hhs3_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag3.RDS") %>% extract
p_hhs4_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag3.RDS") %>% extract
p_hhs5_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag3.RDS") %>% extract
p_hhs6_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_lag3.RDS") %>% extract
p_hhs7_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag3.RDS") %>% extract
p_hhs8_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_lag3.RDS") %>% extract
p_hhs9_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_lag3.RDS") %>% extract
p_hhs10_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag3.RDS") %>% extract

## Canada

p_ca_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_lag3.RDS") %>% extract
# p_at_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_lag3.RDS") %>% extract
p_bc_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag3.RDS") %>% extract
p_on_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_lag3.RDS") %>% extract
p_pr_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_lag3.RDS") %>% extract

# With viral interaction (lag5)

## US

p_us_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_lag5.RDS") %>% extract
# p_hhs1_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_lag5.RDS") %>% extract
p_hhs2_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag5.RDS") %>% extract
p_hhs3_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag5.RDS") %>% extract
p_hhs4_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag5.RDS") %>% extract
p_hhs5_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag5.RDS") %>% extract
p_hhs6_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_lag5.RDS") %>% extract
p_hhs7_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag5.RDS") %>% extract
p_hhs8_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_lag5.RDS") %>% extract
p_hhs9_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_lag5.RDS") %>% extract
p_hhs10_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag5.RDS") %>% extract

## Canada

p_ca_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_lag5.RDS") %>% extract
# p_at_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_lag5.RDS") %>% extract
p_bc_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag5.RDS") %>% extract
p_on_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_lag5.RDS") %>% extract
p_pr_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_lag5.RDS") %>% extract

probs <- c(0.5,0.025,0.975)

phi_estim <- rbind(
  
  data.frame(country="US", region="US (national)", lag="0", p_us_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="US (national)", lag="1", p_us_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="US (national)", lag="3", p_us_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="US (national)", lag="5", p_us_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 2", lag="0", p_hhs2_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 2", lag="1", p_hhs2_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 2", lag="3", p_hhs2_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 2", lag="5", p_hhs2_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 3", lag="0", p_hhs3_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 3", lag="1", p_hhs3_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 3", lag="3", p_hhs3_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 3", lag="5", p_hhs3_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 4", lag="0", p_hhs4_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 4", lag="1", p_hhs4_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 4", lag="3", p_hhs4_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 4", lag="5", p_hhs4_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 5", lag="0", p_hhs5_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 5", lag="1", p_hhs5_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 5", lag="3", p_hhs5_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 5", lag="5", p_hhs5_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 6", lag="0", p_hhs6_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 6", lag="1", p_hhs6_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 6", lag="3", p_hhs6_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 6", lag="5", p_hhs6_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 7", lag="0", p_hhs7_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 7", lag="1", p_hhs7_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 7", lag="3", p_hhs7_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 7", lag="5", p_hhs7_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 8", lag="0", p_hhs8_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 8", lag="1", p_hhs8_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 8", lag="3", p_hhs8_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 8", lag="5", p_hhs8_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 9", lag="0", p_hhs9_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 9", lag="1", p_hhs9_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 9", lag="3", p_hhs9_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 9", lag="5", p_hhs9_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="US", region="HHS 10", lag="0", p_hhs10_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 10", lag="1", p_hhs10_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 10", lag="3", p_hhs10_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="US", region="HHS 10", lag="5", p_hhs10_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="CA", region="Canada (national)", lag="0", p_ca_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Canada (national)", lag="1", p_ca_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Canada (national)", lag="3", p_ca_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Canada (national)", lag="5", p_ca_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="CA", region="British Columbia", lag="0", p_bc_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="British Columbia", lag="1", p_bc_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="British Columbia", lag="3", p_bc_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="British Columbia", lag="5", p_bc_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="CA", region="Ontario", lag="0", p_on_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Ontario", lag="1", p_on_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Ontario", lag="3", p_on_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Ontario", lag="5", p_on_lag5$phi %>% quantile(probs) %>% t),
  
  data.frame(country="CA", region="Prairies", lag="0", p_pr_lag0$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Prairies", lag="1", p_pr_lag1$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Prairies", lag="3", p_pr_lag3$phi %>% quantile(probs) %>% t),
  data.frame(country="CA", region="Prairies", lag="5", p_pr_lag5$phi %>% quantile(probs) %>% t)
)

colnames(phi_estim)[4:6] <- c('median', 'CI_lower', 'CI_upper')

phi_estim$region <- phi_estim$region %>% factor(levels=rev(c(US_names, CA_names)))
phi_estim$lag <- phi_estim$lag %>% factor(levels=c(5,3,1,0))

pd <- position_dodge(width=0.6)

x_lim <- c(-0.21,0.3)
colors <- c('pink', '#8255A1', '#1F007F', 'black')

plot_grid(
  phi_estim %>%
    subset(country=="US") %>%
    ggplot(aes(y=region)) +
    geom_vline(xintercept=0, lwd=0.2, lty='dashed') +
    labs(col=expression(paste(italic('l')~' (lag)'))) +
    geom_segment(aes(x=CI_lower, xend=CI_upper, col=lag), position=pd, lwd=0.2) +
    geom_point(aes(x=median, col=lag), cex=0.75, position=pd) +
    scale_x_continuous(limits=x_lim, labels=scales::percent) +
    scale_color_manual(values=colors) +
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_text(size=8), legend.position=c(0.85,0.8)) +
    guides(color=guide_legend(reverse=TRUE)),

  phi_estim %>%
    subset(country=="CA") %>%
    ggplot(aes(y=region)) +
    geom_vline(xintercept=0, lwd=0.2, lty='dashed') +
    labs(x="\nMax. change in transmission due to viral interaction") +
    geom_segment(aes(x=CI_lower, xend=CI_upper, col=lag), position=pd, lwd=0.2) +
    geom_point(aes(x=median, col=lag), cex=0.75, position=pd) +
    scale_x_continuous(limits=x_lim, labels=scales::percent) +
    scale_color_manual(values=colors) +
    theme_bw() +
    theme(axis.title.x=element_text(size=8), axis.text.x=element_text(size=7),
          axis.title.y=element_blank(), axis.text.y=element_text(size=8),
          legend.position='none'),
  
  ncol=1, rel_heights=c(0.65,0.35), align='v') # 6 x 4

vi_factor <- list(
  'levels'=c('phi=0', paste('lag', c(0,1,3,5))),
  'labels'=c(bquote(phi==0), lapply(c(0,1,3,5), function(l){bquote(hat(phi)~'(lag='*.(l)*')')}))
)

loglik_us <- rbind(
  cbind('location'='US (national)',
        rbind(
          data.frame(vi='phi=0', p_us %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_us_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_us_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_us_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_us_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 2',
        rbind(
          data.frame(vi='phi=0', p_hhs2 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs2_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs2_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs2_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs2_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 3',
        rbind(
          data.frame(vi='phi=0', p_hhs3 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs3_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs3_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs3_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs3_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 4',
        rbind(
          data.frame(vi='phi=0', p_hhs4 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs4_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs4_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs4_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs4_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 5',
        rbind(
          data.frame(vi='phi=0', p_hhs5 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs5_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs5_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs5_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs5_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 6',
        rbind(
          data.frame(vi='phi=0', p_hhs6 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs6_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs6_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs6_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs6_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 7',
        rbind(
          data.frame(vi='phi=0', p_hhs7 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs7_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs7_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs7_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs7_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 8',
        rbind(
          data.frame(vi='phi=0', p_hhs8 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs8_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs8_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs8_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs8_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 9',
        rbind(
          data.frame(vi='phi=0', p_hhs9 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs9_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs9_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs9_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs9_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='HHS 10',
        rbind(
          data.frame(vi='phi=0', p_hhs10 %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_hhs10_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_hhs10_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_hhs10_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_hhs10_lag5 %>% summary_loglik %>% t)
        ))
  ) %>%
  mutate(location=factor(location, levels=US_names),
         vi=factor(vi, levels=vi_factor$levels))

ggplot(loglik_us, aes(x=vi)) +
  facet_wrap(~location, ncol=2, scales='free_y') +
  labs(y='Log-likelihood\n') +
  geom_segment(aes(y=CI_lower, yend=CI_upper), lwd=0.1) +
  geom_point(aes(y=median), cex=0.8) +
  scale_x_discrete(labels=vi_factor$labels) +
  theme_test() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8, angle=30, hjust=1),
        axis.text.y=element_text(size=7)) # landscape: 7 x 6

loglik_ca <- rbind(
  cbind('location'='Canada (national)',
        rbind(
          data.frame(vi='phi=0', p_ca %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_ca_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_ca_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_ca_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_ca_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='British Columbia',
        rbind(
          data.frame(vi='phi=0', p_bc %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_bc_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_bc_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_bc_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_bc_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='Ontario',
        rbind(
          data.frame(vi='phi=0', p_on %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_on_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_on_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_on_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_on_lag5 %>% summary_loglik %>% t)
        )),
  cbind('location'='Prairies',
        rbind(
          data.frame(vi='phi=0', p_pr %>% summary_loglik %>% t),
          data.frame(vi='lag 0', p_pr_lag0 %>% summary_loglik %>% t),
          data.frame(vi='lag 1', p_pr_lag1 %>% summary_loglik %>% t),
          data.frame(vi='lag 3', p_pr_lag3 %>% summary_loglik %>% t),
          data.frame(vi='lag 5', p_pr_lag5 %>% summary_loglik %>% t)
        ))
  ) %>%
  mutate(location=factor(location, levels=CA_names),
         vi=factor(vi, levels=vi_factor$levels))

ggplot(loglik_ca, aes(x=vi)) +
  facet_wrap(~location, ncol=2, scales='free_y') +
  labs(y='Log-likelihood\n') +
  geom_segment(aes(y=CI_lower, yend=CI_upper), lwd=0.1) +
  geom_point(aes(y=median), cex=0.8) +
  scale_x_discrete(labels=vi_factor$labels) +
  theme_test() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8, angle=30, hjust=1),
        axis.text.y=element_text(size=7)) # landscape: 5 x 6

p_list_CA <- list("CA"=p_ca, "BC"=p_bc, "ON"=p_on, "PR"=p_pr)

p_list_US <- list("US"=p_us,
                  #"HHS1"=p_hhs1,
                  "HHS2"=p_hhs2, "HHS3"=p_hhs3, "HHS4"=p_hhs4,
                  "HHS5"=p_hhs5, "HHS6"=p_hhs6, "HHS7"=p_hhs7, "HHS8"=p_hhs8,
                  "HHS9"=p_hhs9, "HHS10"=p_hhs10)

theme_CA <- theme(axis.title.x=element_text(size=12),
                  axis.title.y=element_blank(), axis.text.y=element_text(size=10),
                  legend.position='none')

theme_US <- theme(axis.title.x=element_text(size=12),
                  axis.title.y=element_blank(), axis.text.y=element_text(size=6.5),
                  legend.position='none')

colors_CA <- c("#DA5E06", "#E82789", "#6A65AE", "#109A70")

plot_grid(
  
  plot_distribution(p_list=p_list_CA, parm="omega", colors=colors_CA, duration=TRUE,
                    xlab=expression(paste(hat(Omega), " (weeks)"))) +
    scale_x_continuous(breaks=seq(2,16,2)) +
    theme(axis.title.x=element_text(size=10),
          axis.title.y=element_blank(), axis.text.y=element_text(size=10),
          legend.position='none'),
  plot_distribution(p_list=p_list_US, parm="omega", duration=TRUE, scale=4,
                    xlab=expression(paste(hat(Omega), " (weeks)"))) +
    scale_x_continuous(breaks=seq(5,25,5), limits=c(3,30)) +
    theme(axis.title.x=element_text(size=10),
          axis.title.y=element_blank(), axis.text.y=element_text(size=7),
          legend.position='none'),
  
  plot_distribution(p_list=p_list_CA, parm="kappa", colors=colors_CA, xlab=expression(hat(kappa))) +
    scale_x_continuous(breaks=seq(0.4,1,0.1)) + theme_CA,
  plot_distribution(p_list=p_list_US, parm="kappa", xlab=expression(hat(kappa)), scale=6) +
    scale_x_continuous(breaks=seq(0.6,1.8,0.2)) + theme_US,
  
  plot_distribution(p_list=p_list_CA, parm="rho", colors=colors_CA, xlab=expression(hat(rho))) +
    scale_x_continuous(breaks=seq(0,9e-4,3e-4)) + theme_CA,
  plot_distribution(p_list=p_list_US, parm="rho", xlab=expression(hat(rho)), scale=6) +
    scale_x_continuous(breaks=seq(0,8e-4,2e-4)) + theme_US,
  
  plot_distribution(p_list=p_list_CA, parm="S0", colors=colors_CA, xlab=expression(hat(S)[0]/N)) +
    scale_x_continuous(breaks=seq(0.3,1,0.1), limits=c(0.25,1)) + theme_CA,
  plot_distribution(p_list=p_list_US, parm="S0", xlab=expression(hat(S)[0]/N), scale=6) +
    scale_x_continuous(breaks=seq(0.3,1,0.1), limits=c(0.25,1)) + theme_US,
  
  plot_distribution(p_list=p_list_CA, parm="I0", colors=colors_CA, xlab=expression(hat(I)[0]/N)) +
    scale_x_continuous(breaks=seq(0,0.06,0.02), limits=c(0,0.06)) + theme_CA,
  plot_distribution(p_list=p_list_US, parm="I0", xlab=expression(hat(I)[0]/N), scale=6) +
    scale_x_continuous(breaks=seq(0,0.12,0.02), limits=c(0,0.135)) + theme_US,
  
  ncol=2, labels=LETTERS[1:10]
) # 10 x 9

beta_US <- rbind(
  cbind("location"="US (national)", summary_beta(p_us)),
  #cbind("location"="HHS 1", summary_beta(p_hhs1)),
  cbind("location"="HHS 2", summary_beta(p_hhs2)),
  cbind("location"="HHS 3", summary_beta(p_hhs3)),
  cbind("location"="HHS 4", summary_beta(p_hhs4)),
  cbind("location"="HHS 5", summary_beta(p_hhs5)),
  cbind("location"="HHS 6", summary_beta(p_hhs6)),
  cbind("location"="HHS 7", summary_beta(p_hhs7)),
  cbind("location"="HHS 8", summary_beta(p_hhs8)),
  cbind("location"="HHS 9", summary_beta(p_hhs9)),
  cbind("location"="HHS 10", summary_beta(p_hhs10))) %>%
  mutate(location=factor(location, levels=US_names))

beta_CA <- rbind(
  cbind("location"="Canada (national)", summary_beta(p_ca)),
  cbind("location"="British Columbia", summary_beta(p_bc)),
  cbind("location"="Ontario", summary_beta(p_on)),
  cbind("location"="Prairies", summary_beta(p_pr))) %>%
  mutate(location=factor(location, levels=CA_names))

plot_grid(
  Plot_beta(beta_US, ylim=c(0.5,4.6), ncol=5),
  Plot_beta(beta_CA, ylim=c(0.8,2.1), ncol=2) +
    theme(axis.title.y.left=element_text(margin=margin(r=10, l=40, unit="pt")),
          axis.title.y.right=element_text(margin=margin(r=40, l=10, unit="pt"))),
  labels=LETTERS[1:2], rel_heights=c(0.55,0.45), ncol=1) # 8 x 7

# Fitted values

## US

fit_us <- summary_fit(p_us, data_us)
fit_hhs2 <- summary_fit(p_hhs2, data_hhs2)
fit_hhs3 <- summary_fit(p_hhs3, data_hhs3)
fit_hhs4 <- summary_fit(p_hhs4, data_hhs4)
fit_hhs5 <- summary_fit(p_hhs5, data_hhs5)
fit_hhs6 <- summary_fit(p_hhs6, data_hhs6)
fit_hhs7 <- summary_fit(p_hhs7, data_hhs7)
fit_hhs8 <- summary_fit(p_hhs8, data_hhs8)
fit_hhs9 <- summary_fit(p_hhs9, data_hhs9)
fit_hhs10 <- summary_fit(p_hhs10, data_hhs10)

SI_us <- summary_SI(p_us, data_us)
SI_hhs2 <- summary_SI(p_hhs2, data_hhs2)
SI_hhs3 <- summary_SI(p_hhs3, data_hhs3)
SI_hhs4 <- summary_SI(p_hhs4, data_hhs4)
SI_hhs5 <- summary_SI(p_hhs5, data_hhs5)
SI_hhs6 <- summary_SI(p_hhs6, data_hhs6)
SI_hhs7 <- summary_SI(p_hhs7, data_hhs7)
SI_hhs8 <- summary_SI(p_hhs8, data_hhs8)
SI_hhs9 <- summary_SI(p_hhs9, data_hhs9)
SI_hhs10 <- summary_SI(p_hhs10, data_hhs10)

## Canada

fit_ca <- summary_fit(p_ca, data_ca)
fit_bc <- summary_fit(p_bc, data_bc)
fit_on <- summary_fit(p_on, data_on)
fit_pr <- summary_fit(p_pr, data_pr)

SI_ca <- summary_SI(p_ca, data_ca)
SI_bc <- summary_SI(p_bc, data_bc)
SI_on <- summary_SI(p_on, data_on)
SI_pr <- summary_SI(p_pr, data_pr)

## Plots

plot_grid(
  plot_SI_fit(SI_hhs2, fit_hhs2, location='HHS 2'),
  plot_SI_fit(SI_hhs3, fit_hhs3, location='HHS 3'),
  plot_SI_fit(SI_hhs4, fit_hhs4, location='HHS 4'),
  plot_SI_fit(SI_hhs5, fit_hhs5, location='HHS 5'),
  plot_SI_fit(SI_hhs6, fit_hhs6, location='HHS 6'),
  plot_SI_fit(SI_hhs7, fit_hhs7, location='HHS 7'),
  plot_SI_fit(SI_hhs8, fit_hhs8, location='HHS 8'),
  plot_SI_fit(SI_hhs9, fit_hhs9, location='HHS 9'),
  plot_SI_fit(SI_hhs10, fit_hhs10, location='HHS 10'),
  nrow=3, ncol=3
) # landscape: 13 x 10

S_lim <- c(0.4,1)
I_lim <- c(0,0.15)
I_breaks <- seq(0,0.15,0.05)

plot_grid(
  plot_SI_fit(SI_ca, fit_ca, location='Canada (national)', S_lim=S_lim, I_lim=I_lim, I_breaks=I_breaks),
  plot_SI_fit(SI_bc, fit_bc, location='British Columbia', S_lim=S_lim, I_lim=I_lim, I_breaks=I_breaks),
  plot_SI_fit(SI_on, fit_on, location='Ontario', S_lim=S_lim, I_lim=I_lim, I_breaks=I_breaks),
  plot_SI_fit(SI_pr, fit_pr, location='Prairies', S_lim=S_lim, I_lim=I_lim, I_breaks=I_breaks),
  nrow=2, ncol=2
) # landscape: 9 x 7

Re_US <- rbind(
  Summary_Re(p_us, data_us) %>% cbind('location'='US (national)',.),
  Summary_Re(p_hhs2, data_hhs2) %>% cbind('location'='HHS 2',.),
  Summary_Re(p_hhs3, data_hhs3) %>% cbind('location'='HHS 3',.),
  Summary_Re(p_hhs4, data_hhs4) %>% cbind('location'='HHS 4',.),
  Summary_Re(p_hhs5, data_hhs5) %>% cbind('location'='HHS 5',.),
  Summary_Re(p_hhs6, data_hhs6) %>% cbind('location'='HHS 6',.),
  Summary_Re(p_hhs7, data_hhs7) %>% cbind('location'='HHS 7',.),
  Summary_Re(p_hhs8, data_hhs8) %>% cbind('location'='HHS 8',.),
  Summary_Re(p_hhs9, data_hhs9) %>% cbind('location'='HHS 9',.),
  Summary_Re(p_hhs10, data_hhs10) %>% cbind('location'='HHS 10',.)) %>%
  mutate(location=factor(location, levels=US_names))

Re_US %>%
  mutate(CI_lower=pmax(CI_lower,0.4),
         CI_upper=pmin(CI_upper,1.7)) %>%
  ggplot(aes(x=date)) +
  facet_wrap(~location, ncol=2) +
  labs(y="Effective reproduction number\n", col="mean\nchange\nin mobility",
       fill="mean\nchange\nin mobility") +
  geom_rect(aes(xmin=date-1, xmax=date, ymin=-Inf, ymax=+Inf, fill=c, col=c)) +
  geom_vline(xintercept=ymd(paste0(2014:2025, "-01-01")),
             lty='dotted', col="grey40", lwd=0.2) +
  geom_hline(yintercept=1, lwd=0.2, col='red') +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.3) +
  geom_line(aes(y=median), lwd=0.15) +
  scale_x_date(expand=c(0,0), breaks=ymd(paste0(seq(2014,2024,2), '-01-01')),
               date_labels="%Y") +
  scale_y_continuous(expand=c(0,0), limits=c(0.4,1.7), breaks=seq(0.6,1.6,0.2)) +
  scale_fill_gradient2(low="#2C792D", mid="white", high="#90529C",
                       midpoint=0, na.value=NA, label=scales::percent) +
  scale_color_gradient2(low="#2C792D", mid="white", high="#90529C",
                        midpoint=0, na.value=NA, label=scales::percent) +
  theme_test() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8),
        legend.title=element_text(size=8), legend.text=element_text(size=7)) # 9 x 7

Re_CA <- rbind(
  Summary_Re(p_ca, data_ca) %>% cbind('location'='Canada (national)',.),
  Summary_Re(p_bc, data_bc) %>% cbind('location'='British Columbia',.),
  Summary_Re(p_on, data_on) %>% cbind('location'='Ontario',.),
  Summary_Re(p_pr, data_pr) %>% cbind('location'='Prairies',.)) %>%
  mutate(location=factor(location, levels=CA_names))

Re_CA %>%
  ggplot(aes(x=date)) +
  facet_wrap(~location, ncol=2) +
  labs(y="Effective reproduction number\n", col="mean\nchange\nin mobility",
       fill="mean\nchange\nin mobility") +
  geom_rect(aes(xmin=date-1, xmax=date, ymin=-Inf, ymax=+Inf, fill=c, col=c)) +
  geom_vline(xintercept=ymd(paste0(2014:2025, "-01-01")),
             lty='dotted', col="grey40", lwd=0.3) +
  geom_hline(yintercept=1, lwd=0.2, col='red') +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.3) +
  geom_line(aes(y=median), lwd=0.15) +
  scale_x_date(expand=c(0,0), breaks=ymd(paste0(seq(2014,2024,2), '-01-01')),
               date_labels="%Y") +
  scale_y_continuous(expand=c(0,0), limits=c(0.57,1.81), breaks=seq(0.6,1.6,0.2)) +
  scale_fill_gradient2(low="#2C792D", mid="white", high="#90529C",
                       midpoint=0, na.value=NA, label=scales::percent) +
  scale_color_gradient2(low="#2C792D", mid="white", high="#90529C",
                        midpoint=0, na.value=NA, label=scales::percent) +
  theme_test() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8),
        legend.title=element_text(size=8), legend.text=element_text(size=7)) # 7 x 5

plot_grid(
  
  plot_SI_fit2(
    SI=rbind(cbind('vi'='0', summary_SI(p_us, data_us)),
             cbind('vi'='1', summary_SI(p_us_lag0, data_us))),
    fit=rbind(cbind('vi'='0', summary_fit(p_us, data_us)),
              cbind('vi'='1', summary_fit(p_us_lag0, data_us))),
    Re=rbind(cbind('vi'='0', Summary_Re(p_us, data_us)),
             cbind('vi'='1', Summary_Re(p_us_lag0, data_us, vi=TRUE))),
    location="US"),
  
  plot_SI_fit2(
    SI=rbind(cbind('vi'='0', summary_SI(p_ca, data_ca)),
             cbind('vi'='1', summary_SI(p_ca_lag0, data_ca))),
    fit=rbind(cbind('vi'='0', summary_fit(p_ca, data_ca)),
              cbind('vi'='1', summary_fit(p_ca_lag0, data_ca))),
    Re=rbind(cbind('vi'='0', Summary_Re(p_ca, data_ca)),
             cbind('vi'='1', Summary_Re(p_ca_lag0, data_ca, vi=TRUE))),
    location="Canada"),
  
  labels=LETTERS[1:2], ncol=2) # landscape: 9 x 7