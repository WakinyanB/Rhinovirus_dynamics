rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)
library(cowplot)
library(scales)
library(ggpubr)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/")
source("Fit/Simulation_study2/Functions.R")

# Load simulated data/parameters and estimated posterior distributions

parm_names <- c("beta", "kappa", "omega", "rho", "S0", "I0")

# ----------- Simulated dataset 1 ----------------------------------------------

load("Fit/Simulation_study2/Posterior_simul1.RData")

SIR1 <- read.csv2("Data/Simulation_v2/SIR_1.csv")
SIR1_kick <- read.csv2("Data/Simulation_v2/SIR_kick_1.csv")
SIR1_shift <- read.csv2("Data/Simulation_v2/SIR_shift_1.csv")

parms1 <- readRDS("Data/Simulation_v2/parms_simul_1.rds")
parms1_rv <- parms1 %>% extract_parms_rv(init=SIR1[1,])
parms1_kick_rv <- parms1 %>% extract_parms_rv(init=SIR1_kick[1,])
parms1_shift_rv <- parms1 %>% extract_parms_rv(init=SIR1_shift[1,])

load("Data/Simulation_v2/cases_1.RData")
load("Data/Simulation_v2/cases_kick_1.RData")
load("Data/Simulation_v2/cases_shift_1.RData")

obs1_rv <- obs_rv
obs1_shift_rv <- obs_shift_rv
obs1_kick_rv <- obs_kick_rv

# ----------- Simulated dataset 2 ----------------------------------------------

load("Fit/Simulation_study2/Posterior_simul2.RData")

SIR2 <- read.csv2("Data/Simulation_v2/SIR_2.csv")
SIR2_kick <- read.csv2("Data/Simulation_v2/SIR_kick_2.csv")
SIR2_shift <- read.csv2("Data/Simulation_v2/SIR_shift_2.csv")

parms2 <- readRDS("Data/Simulation_v2/parms_simul_2.rds")
parms2_rv <- parms2 %>% extract_parms_rv(init=SIR2[1,])
parms2_kick_rv <- parms2 %>% extract_parms_rv(init=SIR2_kick[1,])
parms2_shift_rv <- parms2 %>% extract_parms_rv(init=SIR2_shift[1,])

load("Data/Simulation_v2/cases_2.RData")
load("Data/Simulation_v2/cases_kick_2.RData")
load("Data/Simulation_v2/cases_shift_2.RData")

obs2_rv <- obs_rv
obs2_shift_rv <- obs_shift_rv
obs2_kick_rv <- obs_kick_rv

# ----------- Simulated dataset 3 ----------------------------------------------

load("Fit/Simulation_study2/Posterior_simul3.RData")

SIR3 <- read.csv2("Data/Simulation_v2/SIR_3.csv")
SIR3_kick <- read.csv2("Data/Simulation_v2/SIR_kick_3.csv")
SIR3_shift <- read.csv2("Data/Simulation_v2/SIR_shift_3.csv")

parms3 <- readRDS("Data/Simulation_v2/parms_simul_3.rds")
parms3_rv <- parms3 %>% extract_parms_rv(init=SIR3[1,])
parms3_kick_rv <- parms3 %>% extract_parms_rv(init=SIR3_kick[1,])
parms3_shift_rv <- parms3 %>% extract_parms_rv(init=SIR3_shift[1,])

load("Data/Simulation_v2/cases_3.RData")
load("Data/Simulation_v2/cases_kick_3.RData")
load("Data/Simulation_v2/cases_shift_3.RData")

obs3_rv <- obs_rv
obs3_shift_rv <- obs_shift_rv
obs3_kick_rv <- obs_kick_rv

# Plot transmission profiles for each scenario

beta1 <- parms1$b0_rv*(1+parms1$a1_rv*cos(4*pi*((1:52)/52-parms1$d1_rv))+
                         parms1$a2_rv*cos(2*pi*((1:52)/52-(parms1$d1_rv+parms1$d2_rv))))
beta2 <- parms2$beta_rv
beta3 <- parms3$b0_rv*(1+parms3$a1_rv*cos(4*pi*((1:52)/52-parms3$d1_rv))+
                         parms3$a2_rv*cos(2*pi*((1:52)/52-(parms3$d1_rv+parms3$d2_rv))))

(Fig_beta <- plot_grid(
  data.frame("Week"=rep(1:52,5),
             "scenario"=c(rep("Scenario #1\n(RV and IAV independent)",52),
                          rep(paste0("Scenario #", 2:3), each=104)),
             "Interaction"=rep(c(0,0,1,0,1), each=52),
             "beta"=c(beta1,
                      beta2,
                      beta2*(1+parms2$phi_iav*subset(SIR2, pathogen=="IAV")$I[2:53]/parms2$pop),
                      beta3,
                      beta3*(1+parms3$phi_iav*subset(SIR3, pathogen=="IAV")$I[2:53]/parms3$pop))) %>%
    mutate(Interaction=as.factor(Interaction)) %>%
    ggplot(aes(x=Week, y=beta)) +
    facet_wrap(~scenario) +
    geom_line(aes(col=Interaction, lty=Interaction, linewidth=Interaction)) +
    labs(y="RV seasonal\ntransmission rate (/week)\n") +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_y_continuous(expand=c(0,0), limits=c(1.15,2.6)) +
    scale_color_manual(values=c('#0D4ABA', "#3B7BF1"),
                       labels=c("No IAV interference", "With IAV interference\n(endemic attractor)")) +
    scale_linetype_manual(values=c('solid','dashed'),
                          labels=c("No IAV interference", "With IAV interference\n(endemic attractor)")) +
    scale_linewidth_manual(values=c(0.7,0.45)) +
    geom_text(data=data.frame(Week=5, beta=1.75, scenario = "Scenario #1\n(RV and IAV independent)"),
              aes(x=Week, y=beta, label=expression(beta(t))), col='#0D4ABA', hjust=0,
              size=5, inherit.aes=FALSE) +
    theme_test() +
    theme(legend.position=c(0.97,0.75), legend.justification='right', 
          legend.box.background=element_rect(color="black", linewidth=0.2),
          legend.box.margin=margin(5,5,5,5),
          legend.title=element_text(margin=margin(b=3), size=9),
          legend.spacing.y=unit(6,"pt"),
          legend.key.width=unit(2,"lines"),
          legend.text=element_text(size=7.5)) +
    guides(linewidth='none'), # 7.5 x 3.5
  
  data.frame("Week"=1:52, "beta"=parms2$b0_iav*(1+parms2$a_iav*cos(2*pi*(1:52)/52-parms2$d_iav))) %>%
    ggplot(aes(x=Week, y=beta)) +
    labs(y="IAV seasonal\ntransmission rate (/week)\n") +
    geom_line(col="#EE6251", linewidth=0.7) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_y_continuous(expand=c(0,0), limits=c(1.75,3.15)) +
    annotate(geom='text', label=expression(beta[IAV](t)), col="#EE6251", x=5, y=3, size=5, hjust=0) +
    theme_test() +
    theme(axis.title.y=element_text(size=10), plot.margin=margin(t=32,r=5,b=5,l=5)),
  
  ncol=2, rel_widths=c(2.5,1), labels=LETTERS[1:2])) # 10.5 x 3.5

# Scenario #1

## Main simulation
s1 <- p1 %>% summary_posterior(parm=parm_names)
s1_npi <- p1_npi %>% summary_posterior(parm=parm_names)
s1_vi <- p1_vi %>% summary_posterior(parm=parm_names)
s1_npi_vi <- p1_npi_vi %>% summary_posterior(parm=parm_names)

## NPI shift
s1_shift <- p1_shift %>% summary_posterior(parm=parm_names)
s1_npi_shift <- p1_npi_shift %>% summary_posterior(parm=parm_names)
s1_vi_shift <- p1_vi_shift %>% summary_posterior(parm=parm_names)
s1_npi_vi_shift <- p1_npi_vi_shift %>% summary_posterior(parm=parm_names)

# IAV kick
s1_kick <- p1_kick %>% summary_posterior(parm=parm_names)
s1_npi_kick <- p1_npi_kick %>% summary_posterior(parm=parm_names)
s1_vi_kick <- p1_vi_kick %>% summary_posterior(parm=parm_names)
s1_npi_vi_kick <- p1_npi_vi_kick %>% summary_posterior(parm=parm_names)

# Scenario #2

## Main simulation
s2 <- p2 %>% summary_posterior(parm=parm_names)
s2_npi <- p2_npi %>% summary_posterior(parm=parm_names)
s2_vi <- p2_vi %>% summary_posterior(parm=parm_names)
s2_npi_vi <- p2_npi_vi %>% summary_posterior(parm=parm_names)

## NPI shift
s2_shift <- p2_shift %>% summary_posterior(parm=parm_names)
s2_npi_shift <- p2_npi_shift %>% summary_posterior(parm=parm_names)
s2_vi_shift <- p2_vi_shift %>% summary_posterior(parm=parm_names)
s2_npi_vi_shift <- p2_npi_vi_shift %>% summary_posterior(parm=parm_names)

# IAV kick
s2_kick <- p2_kick %>% summary_posterior(parm=parm_names)
s2_npi_kick <- p2_npi_kick %>% summary_posterior(parm=parm_names)
s2_vi_kick <- p2_vi_kick %>% summary_posterior(parm=parm_names)
s2_npi_vi_kick <- p2_npi_vi_kick %>% summary_posterior(parm=parm_names)

# Scenario #3

## Main simulation
s3 <- p3 %>% summary_posterior(parm=parm_names)
s3_npi <- p3_npi %>% summary_posterior(parm=parm_names)
s3_vi <- p3_vi %>% summary_posterior(parm=parm_names)
s3_npi_vi <- p3_npi_vi %>% summary_posterior(parm=parm_names)

## NPI shift
s3_shift <- p3_shift %>% summary_posterior(parm=parm_names)
s3_npi_shift <- p3_npi_shift %>% summary_posterior(parm=parm_names)
s3_vi_shift <- p3_vi_shift %>% summary_posterior(parm=parm_names)
s3_npi_vi_shift <- p3_npi_vi_shift %>% summary_posterior(parm=parm_names)

# IAV kick
s3_kick <- p3_kick %>% summary_posterior(parm=parm_names)
s3_npi_kick <- p3_npi_kick %>% summary_posterior(parm=parm_names)
s3_vi_kick <- p3_vi_kick %>% summary_posterior(parm=parm_names)
s3_npi_vi_kick <- p3_npi_vi_kick %>% summary_posterior(parm=parm_names)

rel_error_vi <- function(estim, parm=NA, SIR=NA, interaction=TRUE){
  
  if(interaction){
    x_real <- parm$phi*max(subset(SIR, pathogen=='IAV')$I)/parm$pop
    return((median(estim)-x_real)/(1+x_real))
  }else{
    return(median(estim))
  }
}

rel_error_beta <- function(p, beta_real, week){
  return(apply(p$beta[,week],2,median)/beta_real[week]-1)
}

Heatmap_rel_error <- function(df, lim_col=0.5, value_size=3){
  return(
    ggplot(df, aes(x=type, y=id, fill=abs_dist)) +
      geom_tile(color="white") +
      geom_text(aes(label=round(abs_dist*100, 0), col=text_col), size=value_size) +
      facet_grid(~npi, switch="y") +
      labs(y="Scenario\n", x=NULL, fill="Relative error of\nestimated median (%)") +
      scale_fill_gradientn(colours=RColorBrewer::brewer.pal(11, "BrBG"),
                           values=scales::rescale(c(-lim_col,0,lim_col)),
                           limits=c(-lim_col,lim_col),
                           oob=scales::squish,
                           breaks=lim_col*c(-0.75,0,0.75),
                           labels=c('Underestimation', '', 'Overestimation'),
                           guide=guide_colorbar(ticks=FALSE, draw.ulim=FALSE,
                                                draw.llim=FALSE, frame.col=NA)) +
      scale_color_identity() +
      theme_minimal() +
      theme(axis.text.x=element_text(size=8, angle=30, hjust=1),
            panel.grid=element_blank(),
            strip.placement="outside") +
      guides(col='none')
    )
}

vi_rel_err <- rbind(
  cbind(id='#1',
        rbind(data.frame(type='main', npi='0', abs_dist=rel_error_vi(p1_vi$phi, interaction=FALSE)),
              data.frame(type='main', npi='1', abs_dist=rel_error_vi(p1_npi_vi$phi, interaction=FALSE)),
              data.frame(type='shift', npi='0', abs_dist=rel_error_vi(p1_vi_shift$phi, interaction=FALSE)),
              data.frame(type='shift', npi='1', abs_dist=rel_error_vi(p1_npi_vi_shift$phi, interaction=FALSE)),
              data.frame(type='kick', npi='0', abs_dist=rel_error_vi(p1_vi_kick$phi, interaction=FALSE)),
              data.frame(type='kick', npi='1', abs_dist=rel_error_vi(p1_npi_vi_kick$phi, interaction=FALSE)))),
  cbind(id='#2',
        rbind(data.frame(type='main', npi='0', abs_dist=rel_error_vi(p2_vi$phi, parm=parms2_rv, SIR=SIR2)),
              data.frame(type='main', npi='1', abs_dist=rel_error_vi(p2_npi_vi$phi, parm=parms2_rv, SIR=SIR2)),
              data.frame(type='shift', npi='0', abs_dist=rel_error_vi(p2_vi_shift$phi, parm=parms2_shift_rv, SIR=SIR2_shift)),
              data.frame(type='shift', npi='1', abs_dist=rel_error_vi(p2_npi_vi_shift$phi, parm=parms2_shift_rv, SIR=SIR2_shift)),
              data.frame(type='kick', npi='0', abs_dist=rel_error_vi(p2_vi_kick$phi, parm=parms2_kick_rv, SIR=SIR2_kick)),
              data.frame(type='kick', npi='1', abs_dist=rel_error_vi(p2_npi_vi_kick$phi, parm=parms2_kick_rv, SIR=SIR2_kick)))),
  cbind(id='#3',
        rbind(data.frame(type='main', npi='0', abs_dist=rel_error_vi(p3_vi$phi, parm=parms3_rv, SIR=SIR3)),
              data.frame(type='main', npi='1', abs_dist=rel_error_vi(p3_npi_vi$phi, parm=parms3_rv, SIR=SIR3)),
              data.frame(type='shift', npi='0', abs_dist=rel_error_vi(p3_vi_shift$phi, parm=parms3_shift_rv, SIR=SIR3_shift)),
              data.frame(type='shift', npi='1', abs_dist=rel_error_vi(p3_npi_vi_shift$phi, parm=parms3_shift_rv, SIR=SIR3_shift)),
              data.frame(type='kick', npi='0', abs_dist=rel_error_vi(p3_vi_kick$phi, parm=parms3_kick_rv, SIR=SIR3_kick)),
              data.frame(type='kick', npi='1', abs_dist=rel_error_vi(p3_npi_vi_kick$phi, parm=parms3_kick_rv, SIR=SIR3_kick))))
  ) %>%
  mutate(id=factor(id, levels=c('#3', '#2', '#1')),
         type=factor(type, levels=c('main', 'shift', 'kick'),
                     labels=c('Main simulation', 'With NPI shift', 'With IAV kick')),
         npi=factor(npi, levels=c('0', '1'), labels=c('Using prepandemic data', 'Including (post-)pandemic data')),
         text_col=ifelse(abs(abs_dist)>0.3, 'white', 'black'))

k <- c(30,52) # weeks

beta_rel_error <- rbind(
  # Scenario 1
  ## Main simulation
  data.frame(id='#1', type='main', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p1, beta1, k)),
  data.frame(id='#1', type='main', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p1_npi, beta1, k)),
  data.frame(id='#1', type='main', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p1_vi, beta1, k)),
  data.frame(id='#1', type='main', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p1_npi_vi, beta1, k)),
  ## NPI shift
  data.frame(id='#1', type='shift', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p1_shift, beta1, k)),
  data.frame(id='#1', type='shift', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p1_npi_shift, beta1, k)),
  data.frame(id='#1', type='shift', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p1_vi_shift, beta1, k)),
  data.frame(id='#1', type='shift', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p1_npi_vi_shift, beta1, k)),
  # IAV kick
  data.frame(id='#1', type='kick', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p1_kick, beta1, k)),
  data.frame(id='#1', type='kick', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p1_npi_kick, beta1, k)),
  data.frame(id='#1', type='kick', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p1_vi_kick, beta1, k)),
  data.frame(id='#1', type='kick', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p1_npi_vi_kick, beta1, k)),
  
  # Scenario 2
  ## Main simulation
  data.frame(id='#2', type='main', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p2, beta2, k)),
  data.frame(id='#2', type='main', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p2_npi, beta2, k)),
  data.frame(id='#2', type='main', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p2_vi, beta2, k)),
  data.frame(id='#2', type='main', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p2_npi_vi, beta2, k)),
  ## NPI shift
  data.frame(id='#2', type='shift', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p2_shift, beta2, k)),
  data.frame(id='#2', type='shift', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p2_npi_shift, beta2, k)),
  data.frame(id='#2', type='shift', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p2_vi_shift, beta2, k)),
  data.frame(id='#2', type='shift', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p2_npi_vi_shift, beta2, k)),
  ## IAV kick
  data.frame(id='#2', type='kick', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p2_kick, beta2, k)),
  data.frame(id='#2', type='kick', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p2_npi_kick, beta2, k)),
  data.frame(id='#2', type='kick', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p2_vi_kick, beta2, k)),
  data.frame(id='#2', type='kick', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p2_npi_vi_kick, beta2, k)),
  
  # Scenario 3
  ## Main simulation
  data.frame(id='#3', type='main', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p3, beta3, k)),
  data.frame(id='#3', type='main', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p3_npi, beta3, k)),
  data.frame(id='#3', type='main', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p3_vi, beta3, k)),
  data.frame(id='#3', type='main', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p3_npi_vi, beta3, k)),
  ## NPI shift
  data.frame(id='#3', type='shift', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p3_shift, beta3, k)),
  data.frame(id='#3', type='shift', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p3_npi_shift, beta3, k)),
  data.frame(id='#3', type='shift', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p3_vi_shift, beta3, k)),
  data.frame(id='#3', type='shift', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p3_npi_vi_shift, beta3, k)),
  ## IAV kick
  data.frame(id='#3', type='kick', npi='0', vi='0', week=k, abs_dist=rel_error_beta(p3_kick, beta3, k)),
  data.frame(id='#3', type='kick', npi='1', vi='0', week=k, abs_dist=rel_error_beta(p3_npi_kick, beta3, k)),
  data.frame(id='#3', type='kick', npi='0', vi='1', week=k, abs_dist=rel_error_beta(p3_vi_kick, beta3, k)),
  data.frame(id='#3', type='kick', npi='1', vi='1', week=k, abs_dist=rel_error_beta(p3_npi_vi_kick, beta3, k))
  ) %>%
  mutate(id=factor(id, levels=c('#3', '#2', '#1')),
         type=factor(type, levels=c('main', 'shift', 'kick'),
                     labels=c('Main simulation', 'With NPI shift', 'With IAV kick')),
         npi=factor(npi, levels=c('0', '1'), labels=c('Using prepandemic data', 'Including (post-)pandemic data')),
         text_col=ifelse(abs(abs_dist)>0.3, 'white', 'black'))

# plot_grid(
#   
#   Fig_beta,
#   
#   plot_grid(
#     
#     ggdraw() + draw_label(expression('Max. change-fold in force of infection due to viral interaction 1 +'~hat(phi)), size=15, x=0.05, hjust=0),
#     
#     Heatmap_rel_error(vi_rel_err, value_size=4) +
#       theme(axis.text.x=element_text(size=8, angle=0, hjust=0.5),
#             plot.margin=margin(t=5, r=80, b=5, l=80)),
#     
#     ggdraw() + draw_label(expression('RV seasonal transmission estimator'~hat(beta)(t)), size=15, x=0.05, hjust=0),
#     
#     ggarrange(
#       
#       Heatmap_rel_error(subset(beta_rel_error, week==k[1] & vi=='1'), lim_col=0.35) +
#         labs(title='Week 30 (IAV trough)') +
#         theme(plot.title=element_text(size=11)),
#       
#       Heatmap_rel_error(subset(beta_rel_error, week==k[2] & vi=='1'), lim_col=0.35) +
#         labs(title='Week 52 (IAV peak)') +
#         theme(plot.title=element_text(size=11, hjust=0.5),
#               axis.title.y=element_blank(), axis.text.y=element_blank()),
#       
#       widths=c(0.55,0.45), legend='none'),
#     
#     rel_heights=c(0.15,0.75,0.15,1), ncol=1, labels=c('C','','D','')),
#   
#   ncol=1, rel_heights=c(0.25,0.75)) # 11 x 10.5

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

  Fig_beta,
  
  plot_grid(
    
    ggdraw() + draw_label('Scenario #1', size=13),
    ggdraw() + draw_label('Scenario #2', size=13),
    ggdraw() + draw_label('Scenario #3', size=13),
    
    Plot_phi(p1_vi, p1_npi_vi, real_max_vi=0),
    Plot_phi(p2_vi, p2_npi_vi, real_max_vi=parms2_rv$phi*max(subset(SIR2, pathogen=='IAV')$I)/parms2_rv$pop) +
      theme(axis.title.y=element_blank()),
    Plot_phi(p3_vi, p3_npi_vi, real_max_vi=parms3_rv$phi*max(subset(SIR3, pathogen=='IAV')$I)/parms3_rv$pop) +
      theme(axis.title.y=element_blank()),
    
    Plot_R0(s1$beta, s1_npi$beta, s1_vi$beta, s1_npi_vi$beta, parms1_rv) + ylab('Basic reproduction number\n') +
      theme(legend.position='none'),
    Plot_R0(s2$beta, s2_npi$beta, s2_vi$beta, s2_npi_vi$beta, parms2_rv, custom_beta=TRUE) +
      theme(legend.position='none', axis.title.y=element_blank()),
    Plot_R0(s3$beta, s3_npi$beta, s3_vi$beta, s3_npi_vi$beta, parms3_rv) +
      scale_y_continuous(breaks=seq(1,3.5,0.5)) +
      theme(legend.position='none', axis.title.y=element_blank()),
    
    labels=c(rep('',3), LETTERS[3:8]), label_y=1.1, rel_heights=c(0.05,0.5,0.5)),
  
  get_legend(Legend + theme(legend.title=element_text(size=15), legend.text=element_text(size=10))),
  
  rel_heights=c(0.35,0.6,0.05), ncol=1) # 10 x 8.5

plot_grid(
  
  ggdraw() + draw_label('Max. change-fold in force of infection due to viral interaction', size=15, x=0.01, hjust=0),
  
  Heatmap_rel_error(vi_rel_err, value_size=4) +
    theme(axis.text.x=element_text(size=8, angle=0, hjust=0.5),
          plot.margin=margin(t=5, r=55, b=5, l=55)),
  
  ggdraw() + draw_label(expression('RV seasonal transmission'~hat(beta)(t)), size=15, x=0.01, hjust=0),
  
  ggarrange(
    Heatmap_rel_error(subset(beta_rel_error, week==k[1] & vi=='1'), lim_col=0.35) +
      labs(title='Week 30 (IAV trough)') +
      theme(plot.title=element_text(size=11)),
    
    Heatmap_rel_error(subset(beta_rel_error, week==k[2] & vi=='1'), lim_col=0.35) +
      labs(title='Week 52 (IAV peak)') +
      theme(plot.title=element_text(size=11, hjust=0.5),
            axis.title.y=element_blank(), axis.text.y=element_blank()),
    
    widths=c(0.55,0.45), labels=LETTERS[2:3], legend='none'),
  
  rel_heights=c(0.15,0.75,0.15,1), ncol=1, labels=c('','A','')) # 9 x 7.5

# Scenario #1

plot_grid(
  
  get_legend(Legend),
  
  ggdraw() + draw_label("A) Main simulation", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p1_vi, p1_npi_vi, real_max_vi=0),
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
  
  ggdraw() + draw_label("B) With 6-month shift in NPIs", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p1_vi_shift, p1_npi_vi_shift, real_max_vi=0),
    Plot_R0(s1_shift$beta, s1_npi_shift$beta, s1_vi_shift$beta, s1_npi_vi$beta, parms1_shift_rv) +
      theme(legend.position='none'),
    Plot_rel_err(s=s1_shift$other_parms, s_npi=s1_npi_shift$other_parms,
                 s_vi=s1_vi_shift$other_parms, s_npi_vi=s1_npi_vi_shift$other_parms,
                 real_parms=parms1_shift_rv) + theme(legend.position='none'),
    ncol=3),
  plot_grid(
    Plot_fit(obs1_shift_rv, p1_shift, p1_npi_shift, p1_vi_shift, p1_npi_vi_shift, shift=-0.5) +
      theme(legend.position='none'),
    Plot_cor(obs1_shift_rv, p1_shift, p1_npi_shift, p1_vi_shift, p1_npi_vi_shift) +
      theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ggdraw() + draw_label("C) With prepandemic perturbation in IAV dynamics",
                        size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p1_vi_kick, p1_npi_vi_kick, real_max_vi=0),
    Plot_R0(s1_kick$beta, s1_npi_kick$beta, s1_vi_kick$beta, s1_npi_vi_kick$beta, parms1_shift_rv) +
      theme(legend.position='none'),
    Plot_rel_err(s=s1_kick$other_parms, s_npi=s1_npi_kick$other_parms,
                 s_vi=s1_vi_kick$other_parms, s_npi_vi=s1_npi_vi_kick$other_parms,
                 real_parms=parms1_shift_rv) + theme(legend.position='none'),
    ncol=3),
  plot_grid(
    Plot_fit(obs1_kick_rv, p1_kick, p1_npi_kick, p1_vi_kick, p1_npi_vi_kick) +
      theme(legend.position='none'),
    Plot_cor(obs1_kick_rv, p1_kick, p1_npi_kick, p1_vi_kick, p1_npi_vi_kick) +
      theme(legend.position='none'),
    rel_widths=c(2,1)),
  
    ncol=1, rel_heights=c(0.075, rep(c(0.1,0.45,0.45),3))) # 13 x 11

# Scenario #2

plot_grid(
  
  get_legend(Legend),
  
  ggdraw() + draw_label("A) Main simulation", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p2_vi, p2_npi_vi, real_max_vi=parms2_rv$phi*max(subset(SIR2, pathogen=='IAV')$I)/parms2_rv$pop),
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
  
  ggdraw() + draw_label("B) With 6-month shift in NPIs", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p2_vi_shift, p2_npi_vi_shift, real_max_vi=parms2_shift_rv$phi*max(subset(SIR2_shift, pathogen=='IAV')$I)/parms2_shift_rv$pop),
    Plot_R0(s2_shift$beta, s2_npi_shift$beta, s2_vi_shift$beta, s2_npi_vi$beta, parms2_shift_rv, custom_beta=TRUE) +
      theme(legend.position='none'),
    Plot_rel_err(s=s2_shift$other_parms, s_npi=s2_npi_shift$other_parms,
                 s_vi=s2_vi_shift$other_parms, s_npi_vi=s2_npi_vi_shift$other_parms,
                 real_parms=parms2_shift_rv) + theme(legend.position='none'),
    ncol=3),
  plot_grid(
    Plot_fit(obs2_shift_rv, p2_shift, p2_npi_shift, p2_vi_shift, p2_npi_vi_shift, shift=-0.5) +
      theme(legend.position='none'),
    Plot_cor(obs2_shift_rv, p2_shift, p2_npi_shift, p2_vi_shift, p2_npi_vi_shift) +
      theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ggdraw() + draw_label("C) With prepandemic perturbation in IAV dynamics",
                      size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p2_vi_kick, p2_npi_vi_kick, real_max_vi=parms2_kick_rv$phi*max(subset(SIR2_kick, pathogen=='IAV')$I)/parms2_kick_rv$pop),
    Plot_R0(s2_kick$beta, s2_npi_kick$beta, s2_vi_kick$beta, s2_npi_vi_kick$beta, parms2_kick_rv, custom_beta=TRUE) +
      theme(legend.position='none'),
    Plot_rel_err(s=s2_kick$other_parms, s_npi=s2_npi_kick$other_parms,
                 s_vi=s2_vi_kick$other_parms, s_npi_vi=s2_npi_vi_kick$other_parms,
                 real_parms=parms2_kick_rv) + theme(legend.position='none'),
    ncol=3),
  plot_grid(
    Plot_fit(obs2_kick_rv, p2_kick, p2_npi_kick, p2_vi_kick, p2_npi_vi_kick) +
      theme(legend.position='none'),
    Plot_cor(obs2_kick_rv, p2_kick, p2_npi_kick, p2_vi_kick, p2_npi_vi_kick) +
      theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ncol=1, rel_heights=c(0.075, rep(c(0.1,0.45,0.45),3))) # 13 x 11

# Scenario #3

plot_grid(
  
  get_legend(Legend),
  
  ggdraw() + draw_label("A) Main simulation", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p3_vi, p3_npi_vi, real_max_vi=parms3_rv$phi*max(subset(SIR3, pathogen=='IAV')$I)/parms3_rv$pop),
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
  
  ggdraw() + draw_label("B) With 6-month shift in NPIs", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p3_vi_shift, p3_npi_vi_shift, real_max_vi=parms3_shift_rv$phi*max(subset(SIR3_shift, pathogen=='IAV')$I)/parms3_shift_rv$pop),
    Plot_R0(s3_shift$beta, s3_npi_shift$beta, s3_vi_shift$beta, s3_npi_vi$beta, parms3_shift_rv) +
      theme(legend.position='none'),
    Plot_rel_err(s=s3_shift$other_parms, s_npi=s3_npi_shift$other_parms,
                 s_vi=s3_vi_shift$other_parms, s_npi_vi=s3_npi_vi_shift$other_parms,
                 real_parms=parms3_shift_rv) + theme(legend.position='none'),
    ncol=3),
  plot_grid(
    Plot_fit(obs3_shift_rv, p3_shift, p3_npi_shift, p3_vi_shift, p3_npi_vi_shift, shift=-0.5) +
      theme(legend.position='none'),
    Plot_cor(obs3_shift_rv, p3_shift, p3_npi_shift, p3_vi_shift, p3_npi_vi_shift) +
      theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ggdraw() + draw_label("C) With prepandemic perturbation in IAV dynamics",
                      size=15, x=0.05, hjust=0, fontface='bold'),
  plot_grid(
    Plot_phi(p3_vi_kick, p3_npi_vi_kick, real_max_vi=parms3_kick_rv$phi*max(subset(SIR3_kick, pathogen=='IAV')$I)/parms3_kick_rv$pop),
    Plot_R0(s3_kick$beta, s3_npi_kick$beta, s3_vi_kick$beta, s3_npi_vi_kick$beta, parms3_kick_rv) +
      theme(legend.position='none'),
    Plot_rel_err(s=s3_kick$other_parms, s_npi=s3_npi_kick$other_parms,
                 s_vi=s3_vi_kick$other_parms, s_npi_vi=s3_npi_vi_kick$other_parms,
                 real_parms=parms3_kick_rv) + theme(legend.position='none'),
    ncol=3),
  plot_grid(
    Plot_fit(obs3_kick_rv, p3_kick, p3_npi_kick, p3_vi_kick, p3_npi_vi_kick) +
      theme(legend.position='none'),
    Plot_cor(obs3_kick_rv, p3_kick, p3_npi_kick, p3_vi_kick, p3_npi_vi_kick) +
      theme(legend.position='none'),
    rel_widths=c(2,1)),
  
  ncol=1, rel_heights=c(0.075,rep(c(0.1,0.45,0.45),3))) # 13 x 11
