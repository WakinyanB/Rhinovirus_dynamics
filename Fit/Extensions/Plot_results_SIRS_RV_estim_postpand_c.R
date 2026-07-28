rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)

setwd('.../Data_and_Codes/Fit/')

source("Functions.R")

US_names <- c("US", paste0('HHS', 2:10))
CA_names <- c("CA", "BC", "ON", "PR")

colors_CA <- c("#DA5E06", "#E82789", "#6A65AE", "#109A70")

Plot_beta2 <- function(beta, ylim, ncol, dt=1, mu=1/80/52, gamma=2){
  return(
    beta %>%
      ggplot(aes(x=Week)) +
      facet_wrap(~location, ncol=ncol) +
      labs(y=expression(paste(hat(beta), ", seasonal transmission rate (",week^{-1},")")),
           col=expression(eta), fill=col=expression(eta)) +
      geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=eta), col=NA, alpha=0.25) +
      geom_line(aes(y=median, col=eta)) +
      scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
      scale_y_continuous(expand=c(0,0), limits=ylim,
                         sec.axis=sec_axis(
                           transform=~.*(1-exp(-mu*dt))/(1-exp(-(gamma+mu)*dt))/mu,
                           name=expression(paste(hat(R)[0], ", basic reproduction number")))) +
      theme_test() +
      theme(axis.title.y.left=element_text(margin=margin(r=10, unit="pt")),
            axis.title.y.right=element_text(margin=margin(l=10, unit="pt")))
  )
}

# Main posteriors

## US

p_us <- readRDS('Output/fit_RV_SIRS_npi_vi_us.RDS') %>% extract
p_hhs2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs2.RDS') %>% extract
p_hhs3 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs3.RDS') %>% extract
p_hhs4 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs4.RDS') %>% extract
p_hhs5 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs5.RDS') %>% extract
p_hhs6 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs6.RDS') %>% extract
p_hhs7 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs7.RDS') %>% extract
p_hhs8 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs8.RDS') %>% extract
p_hhs9 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs9.RDS') %>% extract
p_hhs10 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs10.RDS') %>% extract

## Canada

p_ca <- readRDS('Output/fit_RV_SIRS_npi_vi_ca.RDS') %>% extract
p_bc <- readRDS('Output/fit_RV_SIRS_npi_vi_bc.RDS') %>% extract
p_on <- readRDS('Output/fit_RV_SIRS_npi_vi_on.RDS') %>% extract
p_pr <- readRDS('Output/fit_RV_SIRS_npi_vi_pr.RDS') %>% extract

# Posteriors with potential post-pandemic shift in behavioral norm

## US

p_us.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_us_estim_postpand_c.RDS') %>% extract
p_hhs2.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs2_estim_postpand_c.RDS') %>% extract
p_hhs3.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs3_estim_postpand_c.RDS') %>% extract
p_hhs4.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs4_estim_postpand_c.RDS') %>% extract
p_hhs5.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs5_estim_postpand_c.RDS') %>% extract
p_hhs6.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs6_estim_postpand_c.RDS') %>% extract
p_hhs7.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs7_estim_postpand_c.RDS') %>% extract
p_hhs8.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs8_estim_postpand_c.RDS') %>% extract
p_hhs9.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs9_estim_postpand_c.RDS') %>% extract
p_hhs10.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_hhs10_estim_postpand_c.RDS') %>% extract

## Canada

p_ca.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_ca_estim_postpand_c.RDS') %>% extract
p_bc.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_bc_estim_postpand_c.RDS') %>% extract
p_on.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_on_estim_postpand_c.RDS') %>% extract
p_pr.2 <- readRDS('Output/fit_RV_SIRS_npi_vi_pr_estim_postpand_c.RDS') %>% extract

# Convert to data frames

## Max. change in transmission due to viral interaction

phi_estim <- rbind(
  
  data.frame(country='US', region='US', eta='1 (same as pre-pandemic baseline)', phi=p_us$phi),
  data.frame(country='US', region='US', eta='estimated', phi=p_us.2$phi),
  data.frame(country='US', region='HHS2', eta='1 (same as pre-pandemic baseline)', phi=p_hhs2$phi),
  data.frame(country='US', region='HHS2', eta='estimated', phi=p_hhs2.2$phi),
  data.frame(country='US', region='HHS3', eta='1 (same as pre-pandemic baseline)', phi=p_hhs3$phi),
  data.frame(country='US', region='HHS3', eta='estimated', phi=p_hhs3.2$phi),
  data.frame(country='US', region='HHS4', eta='1 (same as pre-pandemic baseline)', phi=p_hhs4$phi),
  data.frame(country='US', region='HHS4', eta='estimated', phi=p_hhs4.2$phi),
  data.frame(country='US', region='HHS5', eta='1 (same as pre-pandemic baseline)', phi=p_hhs5$phi),
  data.frame(country='US', region='HHS5', eta='estimated', phi=p_hhs5.2$phi),
  data.frame(country='US', region='HHS6', eta='1 (same as pre-pandemic baseline)', phi=p_hhs6$phi),
  data.frame(country='US', region='HHS6', eta='estimated', phi=p_hhs6.2$phi),
  data.frame(country='US', region='HHS7', eta='1 (same as pre-pandemic baseline)', phi=p_hhs7$phi),
  data.frame(country='US', region='HHS7', eta='estimated', phi=p_hhs7.2$phi),
  data.frame(country='US', region='HHS8', eta='1 (same as pre-pandemic baseline)', phi=p_hhs8$phi),
  data.frame(country='US', region='HHS8', eta='estimated', phi=p_hhs8.2$phi),
  data.frame(country='US', region='HHS9', eta='1 (same as pre-pandemic baseline)', phi=p_hhs9$phi),
  data.frame(country='US', region='HHS9', eta='estimated', phi=p_hhs9.2$phi),
  data.frame(country='US', region='HHS10', eta='1 (same as pre-pandemic baseline)', phi=p_hhs10$phi),
  data.frame(country='US', region='HHS10', eta='estimated', phi=p_hhs10.2$phi),
  
  data.frame(country='Canada', region='CA', eta='1 (same as pre-pandemic baseline)', phi=p_ca$phi),
  data.frame(country='Canada', region='CA', eta='estimated', phi=p_ca.2$phi),
  data.frame(country='Canada', region='BC', eta='1 (same as pre-pandemic baseline)', phi=p_bc$phi),
  data.frame(country='Canada', region='BC', eta='estimated', phi=p_bc.2$phi),
  data.frame(country='Canada', region='ON', eta='1 (same as pre-pandemic baseline)', phi=p_on$phi),
  data.frame(country='Canada', region='ON', eta='estimated', phi=p_on.2$phi),
  data.frame(country='Canada', region='PR', eta='1 (same as pre-pandemic baseline)', phi=p_pr$phi),
  data.frame(country='Canada', region='PR', eta='estimated', phi=p_pr.2$phi))

# Mean duration of immune protection

Omega_estim <- rbind(
  
  data.frame(country='US', region='US', eta='1 (same as pre-pandemic baseline)', Omega=p_us$omega),
  data.frame(country='US', region='US', eta='estimated', Omega=p_us.2$omega),
  data.frame(country='US', region='HHS2', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs2$omega),
  data.frame(country='US', region='HHS2', eta='estimated', Omega=p_hhs2.2$omega),
  data.frame(country='US', region='HHS3', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs3$omega),
  data.frame(country='US', region='HHS3', eta='estimated', Omega=p_hhs3.2$omega),
  data.frame(country='US', region='HHS4', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs4$omega),
  data.frame(country='US', region='HHS4', eta='estimated', Omega=p_hhs4.2$omega),
  data.frame(country='US', region='HHS5', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs5$omega),
  data.frame(country='US', region='HHS5', eta='estimated', Omega=p_hhs5.2$omega),
  data.frame(country='US', region='HHS6', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs6$omega),
  data.frame(country='US', region='HHS6', eta='estimated', Omega=p_hhs6.2$omega),
  data.frame(country='US', region='HHS7', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs7$omega),
  data.frame(country='US', region='HHS7', eta='estimated', Omega=p_hhs7.2$omega),
  data.frame(country='US', region='HHS8', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs8$omega),
  data.frame(country='US', region='HHS8', eta='estimated', Omega=p_hhs8.2$omega),
  data.frame(country='US', region='HHS9', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs9$omega),
  data.frame(country='US', region='HHS9', eta='estimated', Omega=p_hhs9.2$omega),
  data.frame(country='US', region='HHS10', eta='1 (same as pre-pandemic baseline)', Omega=p_hhs10$omega),
  data.frame(country='US', region='HHS10', eta='estimated', Omega=p_hhs10.2$omega),
  
  data.frame(country='Canada', region='CA', eta='1 (same as pre-pandemic baseline)', Omega=p_ca$omega),
  data.frame(country='Canada', region='CA', eta='estimated', Omega=p_ca.2$omega),
  data.frame(country='Canada', region='BC', eta='1 (same as pre-pandemic baseline)', Omega=p_bc$omega),
  data.frame(country='Canada', region='BC', eta='estimated', Omega=p_bc.2$omega),
  data.frame(country='Canada', region='ON', eta='1 (same as pre-pandemic baseline)', Omega=p_on$omega),
  data.frame(country='Canada', region='ON', eta='estimated', Omega=p_on.2$omega),
  data.frame(country='Canada', region='PR', eta='1 (same as pre-pandemic baseline)', Omega=p_pr$omega),
  data.frame(country='Canada', region='PR', eta='estimated', Omega=p_pr.2$omega)) %>%
  mutate(Omega=rate_to_duration(Omega))

phi_estim$region <- factor(phi_estim$region, levels=c(US_names, CA_names))
Omega_estim$region <- factor(Omega_estim$region, levels=c(US_names, CA_names))

# Seasonal transmission rate

## US

beta_US <- rbind(
  cbind("location"="US", eta='1 (same as pre-pandemic baseline)', summary_beta(p_us)),
  cbind("location"="US", eta='estimated', summary_beta(p_us.2)),
  cbind("location"="HHS2", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs2)),
  cbind("location"="HHS2", eta='estimated', summary_beta(p_hhs2.2)),
  cbind("location"="HHS3", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs3)),
  cbind("location"="HHS3", eta='estimated', summary_beta(p_hhs3.2)),
  cbind("location"="HHS4", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs4)),
  cbind("location"="HHS4", eta='estimated', summary_beta(p_hhs4.2)),
  cbind("location"="HHS5", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs5)),
  cbind("location"="HHS5", eta='estimated', summary_beta(p_hhs5.2)),
  cbind("location"="HHS6", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs6)),
  cbind("location"="HHS6", eta='estimated', summary_beta(p_hhs6.2)),
  cbind("location"="HHS7", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs7)),
  cbind("location"="HHS7", eta='estimated', summary_beta(p_hhs7.2)),
  cbind("location"="HHS8", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs8)),
  cbind("location"="HHS8", eta='estimated', summary_beta(p_hhs8.2)),
  cbind("location"="HHS9", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs9)),
  cbind("location"="HHS9", eta='estimated', summary_beta(p_hhs9.2)),
  cbind("location"="HHS10", eta='1 (same as pre-pandemic baseline)', summary_beta(p_hhs10)),
  cbind("location"="HHS10", eta='estimated', summary_beta(p_hhs10.2))) %>%
  mutate(location=factor(location, levels=US_names))

## Canada

beta_CA <- rbind(
  cbind("location"="CA", eta='1 (same as pre-pandemic baseline)', summary_beta(p_ca)),
  cbind("location"="CA", eta='estimated', summary_beta(p_ca.2)),
  cbind("location"="BC", eta='1 (same as pre-pandemic baseline)', summary_beta(p_bc)),
  cbind("location"="BC", eta='estimated', summary_beta(p_bc.2)),
  cbind("location"="ON", eta='1 (same as pre-pandemic baseline)', summary_beta(p_on)),
  cbind("location"="ON", eta='estimated', summary_beta(p_on.2)),
  cbind("location"="PR", eta='1 (same as pre-pandemic baseline)', summary_beta(p_pr)),
  cbind("location"="PR", eta='estimated', summary_beta(p_pr.2))) %>%
  mutate(location=factor(location, levels=CA_names))

# Plot

pd <- position_dodge(width=0.5)
colors <- c('#1f77b4', '#ff7f0e')

plot_grid(
  plot_grid(
    plot_distribution(p_list=list("US"=p_us.2, "HHS2"=p_hhs2.2, "HHS3"=p_hhs3.2, "HHS4"=p_hhs4.2,
                                  "HHS5"=p_hhs5.2, "HHS6"=p_hhs6.2, "HHS7"=p_hhs7.2, "HHS8"=p_hhs8.2,
                                  "HHS9"=p_hhs9.2, "HHS10"=p_hhs10.2), scale=3,
                      parm='eta', xlab=expression(paste(hat(eta), ", post-pandemic transmission multiplier"))) +
      geom_vline(xintercept=1, lwd=0.4, lty='dashed', col='grey50') +
      scale_x_continuous(limits=c(0.8,1.2)) +
      scale_fill_manual(values=rep(colors[2],10)) +
      theme(axis.title.x=element_text(size=10),
            axis.title.y=element_blank(), axis.text.y=element_text(size=8),
            legend.position='none') + ggtitle('US'),
    
    plot_distribution(list("CA"=p_ca.2, "BC"=p_bc.2, "ON"=p_on.2, "PR"=p_pr.2), scale=2,
                      parm='eta', xlab=expression(paste(hat(eta), ", post-pandemic transmission multiplier"))) +
      geom_vline(xintercept=1, lwd=0.4, lty='dashed', col='grey50') +
      scale_x_continuous(limits=c(0.8,1.2)) +
      scale_fill_manual(values=rep(colors[2],4)) +
      theme(axis.title.x=element_text(size=10),
            axis.title.y=element_blank(), axis.text.y=element_text(size=10),
            legend.position='none') + ggtitle('Canada'),
    align='hv', labels=LETTERS[1:2]),
  
    plot_grid(
      
      phi_estim %>%
        subset(country=='US') %>%
        ggplot(aes(x=region, y=phi, fill=eta)) +
        geom_violin(position=pd, alpha=0.3, color=NA) +
        geom_boxplot(position=pd, width=0.25, lwd=0.3, outlier.shape=NA) +
        labs(title='US', y=expression(hat(phi)), fill=expression(eta)) +
        scale_y_continuous(limits=c(-0.3,0.4), breaks=seq(-0.2,0.4,0.1)) +
        scale_fill_manual(values=colors) +
        theme_test() +
        theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8.5),
              legend.position=c(0.25,0.8)),
    
      phi_estim %>%
        subset(country=='Canada') %>%
        ggplot(aes(x=region, y=phi, fill=eta)) +
        geom_violin(position=pd, alpha=0.3, color=NA) +
        geom_boxplot(position=pd, width=0.25, lwd=0.3, outlier.shape=NA) +
        labs(title='Canada', y=expression(hat(phi)), fill=expression(eta)) +
        scale_y_continuous(limits=c(-0.3,0.4), breaks=seq(-0.2,0.4,0.1)) +
        scale_fill_manual(values=colors) +
        theme_test() +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position='none'),
    
      Omega_estim %>%
        subset(country=='US') %>%
        ggplot(aes(x=region, y=Omega, fill=eta)) +
        geom_violin(position=pd, alpha=0.3, color=NA) +
        geom_boxplot(position=pd, width=0.25, lwd=0.3, outlier.shape=NA) +
        labs(y=expression(hat(Omega)), fill=expression(eta)) +
        scale_y_continuous(limits=c(0, 30), breaks=seq(5,30,5)) +
        scale_fill_manual(values=colors) +
        theme_test() +
        theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8.5),
              legend.position='none'),
    
      Omega_estim %>%
        subset(country=='Canada') %>%
        ggplot(aes(x=region, y=Omega, fill=eta)) +
        geom_violin(position=pd, alpha=0.3, color=NA) +
        geom_boxplot(position=pd, width=0.25, lwd=0.3, outlier.shape=NA) +
        labs(y=expression(hat(Omega)), fill=expression(eta)) +
        scale_y_continuous(limits=c(0, 30), breaks=seq(5,30,5)) +
        scale_fill_manual(values=colors) +
        theme_test() +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position='none'),
    
    rel_widths=c(0.7,0.3), rel_heights=c(0.4,0.3), align='v', labels=LETTERS[3:6]),
  
  plot_grid(
    Plot_beta2(beta_US, ylim=c(0.5,5.3), ncol=5) +
      scale_color_manual(values=colors) +
      scale_fill_manual(values=colors) +
      theme(axis.text.x=element_text(size=7),
            axis.title.y.right=element_blank(), legend.position='none') +
      theme(),
    Plot_beta2(beta_CA, ylim=c(0.5,2.1), ncol=2) +
      scale_color_manual(values=colors) +
      scale_fill_manual(values=colors) +
      theme(axis.text.x=element_text(size=7),
            axis.title.y.left=element_blank(), legend.position='none'),
    
    rel_widths=c(0.6,0.4), labels=LETTERS[7:8]),
  
  ncol=1, rel_heights=c(0.25,0.45,0.3)) # 11 x 9
