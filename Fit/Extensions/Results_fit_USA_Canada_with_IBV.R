rm(list=ls())

library(tidyverse)
library(plyr)
library(lubridate)
library(cowplot)
library(rstan)

setwd(".../Data_and_Codes")

source("Fit/Functions.R")

load("Data/USA/Data_USA.RData")
load("Data/Canada/data_canada_province.RData")

US_names <- c("US", paste0('HHS', 2:10))
CA_names <- c("CA", "BC", "ON", "PR")

## US

# With IAV

p_us <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us.RDS") %>% extract
# p_hhs1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1.RDS") %>% extract
p_hhs2 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2.RDS") %>% extract
# p_hhs3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3.RDS") %>% extract
p_hhs4 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4.RDS") %>% extract
p_hhs5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5.RDS") %>% extract
p_hhs6 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6.RDS") %>% extract
p_hhs7 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7.RDS") %>% extract
p_hhs8 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8.RDS") %>% extract
p_hhs9 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9.RDS") %>% extract
p_hhs10 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10.RDS") %>% extract

# With IAV + IBV

p_us_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_with_IBV.RDS") %>% extract
# p_hhs1_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_with_IBV.RDS") %>% extract
p_hhs2_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_with_IBV.RDS") %>% extract
# p_hhs3_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_with_IBV.RDS") %>% extract
p_hhs4_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_with_IBV.RDS") %>% extract
p_hhs5_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_with_IBV.RDS") %>% extract
p_hhs6_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_with_IBV.RDS") %>% extract
p_hhs7_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_with_IBV.RDS") %>% extract
p_hhs8_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_with_IBV.RDS") %>% extract
p_hhs9_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_with_IBV.RDS") %>% extract
p_hhs10_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_with_IBV.RDS") %>% extract

## Canada

# With IAV

p_ca <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca.RDS") %>% extract
# p_at <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at.RDS") %>% extract
p_bc <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc.RDS") %>% extract
p_on <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on.RDS") %>% extract
p_pr <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr.RDS") %>% extract

# With IAV + IBV

p_ca_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_with_IBV.RDS") %>% extract
# p_at_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_with_IBV.RDS") %>% extract
p_bc_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_with_IBV.RDS") %>% extract
p_on_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_with_IBV.RDS") %>% extract
p_pr_with_IBV <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_with_IBV.RDS") %>% extract

probs <- c(0.5,0.025,0.975)

phi_estim <- rbind(
  
  data.frame(country='US', region='US', type='IAV', phi=p_us$phi),
  data.frame(country='US', region='US', type='IAV + IBV', phi=p_us_with_IBV$phi),
  
  # data.frame(country='US', region='HHS1', type='IAV', phi=p_hhs1$phi),
  # data.frame(country='US', region='HHS1', type='IAV + IBV', phi=p_hhs1_with_IBV$phi),
  
  data.frame(country='US', region='HHS2', type='IAV', phi=p_hhs2$phi),
  data.frame(country='US', region='HHS2', type='IAV + IBV', phi=p_hhs2_with_IBV$phi),
  
  # data.frame(country='US', region='HHS3', type='IAV', phi=p_hhs3$phi),
  # data.frame(country='US', region='HHS3', type='IAV + IBV', phi=p_hhs3_with_IBV$phi),
  
  data.frame(country='US', region='HHS4', type='IAV', phi=p_hhs4$phi),
  data.frame(country='US', region='HHS4', type='IAV + IBV', phi=p_hhs4_with_IBV$phi),
  
  data.frame(country='US', region='HHS5', type='IAV', phi=p_hhs5$phi),
  data.frame(country='US', region='HHS5', type='IAV + IBV', phi=p_hhs5_with_IBV$phi),
  
  data.frame(country='US', region='HHS6', type='IAV', phi=p_hhs6$phi),
  data.frame(country='US', region='HHS6', type='IAV + IBV', phi=p_hhs6_with_IBV$phi),
  
  data.frame(country='US', region='HHS7', type='IAV', phi=p_hhs7$phi),
  data.frame(country='US', region='HHS7', type='IAV + IBV', phi=p_hhs7_with_IBV$phi),
  
  data.frame(country='US', region='HHS8', type='IAV', phi=p_hhs8$phi),
  data.frame(country='US', region='HHS8', type='IAV + IBV', phi=p_hhs8_with_IBV$phi),
  
  data.frame(country='US', region='HHS9', type='IAV', phi=p_hhs9$phi),
  data.frame(country='US', region='HHS9', type='IAV + IBV', phi=p_hhs9_with_IBV$phi),
  
  data.frame(country='US', region='HHS10', type='IAV', phi=p_hhs10$phi),
  data.frame(country='US', region='HHS10', type='IAV + IBV', phi=p_hhs10_with_IBV$phi),
  
  data.frame(country='Canada', region='CA', type='IAV', phi=p_ca$phi),
  data.frame(country='Canada', region='CA', type='IAV + IBV', phi=p_ca_with_IBV$phi),
  
  # data.frame(country='Canada', region='AT', type='IAV', phi=p_at$phi),
  # data.frame(country='Canada', region='AT', type='IAV + IBV', phi=p_at_with_IBV$phi),
  
  data.frame(country='Canada', region='BC', type='IAV', phi=p_bc$phi),
  data.frame(country='Canada', region='BC', type='IAV + IBV', phi=p_bc_with_IBV$phi),
  
  data.frame(country='Canada', region='ON', type='IAV', phi=p_on$phi),
  data.frame(country='Canada', region='ON', type='IAV + IBV', phi=p_on_with_IBV$phi),
  
  data.frame(country='Canada', region='PR', type='IAV', phi=p_pr$phi),
  data.frame(country='Canada', region='PR', type='IAV + IBV', phi=p_pr_with_IBV$phi)
  ) %>%
  mutate(region=factor(region, levels=c(US_names, CA_names)))

pd <- position_dodge(width=0.5)
colors <-  c('#ED3325', '#009FC3')

plot_grid(
  phi_estim %>%
    subset(country=='US') %>%
    ggplot(aes(x=region, y=phi, fill=type)) +
    geom_violin(position=pd, alpha=0.3, color=NA) +
    geom_boxplot(position=pd, width=0.25, lwd=0.2, outlier.shape=NA) +
    labs(title='US', y=expression(hat(phi))) +
    scale_y_continuous(limits=c(-0.3,0.4), breaks=seq(-0.2,0.4,0.1)) +
    scale_fill_manual(values=colors) +
    theme_test() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(size=7),
          legend.title=element_blank(), legend.direction='horizontal',
          legend.position=c(0.25,0.85)),
  
  phi_estim %>%
    subset(country=='Canada') %>%
    ggplot(aes(x=region, y=phi, fill=type)) +
    geom_violin(position=pd, alpha=0.3, color=NA) +
    geom_boxplot(position=pd, width=0.25, lwd=0.2, outlier.shape=NA) +
    labs(title='Canada', y=expression(hat(phi))) +
    scale_y_continuous(limits=c(-0.3,0.4), breaks=seq(-0.2,0.4,0.1)) +
    scale_fill_manual(values=colors) +
    theme_test() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position='none'),
  
  ncol=2, align='h', rel_widths=c(0.7,0.3)) # 7 x 3
