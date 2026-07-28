rm(list=ls())

library(tidyverse)
library(plyr)
library(lubridate)
library(cowplot)
library(ggpubr)
library(rstan)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes")

# Import data

load("Data/USA/Data_USA.RData")
load("Data/Canada/data_canada_province.RData")

# Import posterior distributions

p_US_SIRS <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us.RDS") %>% extract
p_US_SEIRS <- readRDS("Fit/Output/fit_RV_SEIRS_npi_vi_us.RDS") %>% extract

p_CA_SIRS <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca.RDS") %>% extract
p_CA_SEIRS <- readRDS("Fit/Output/fit_RV_SEIRS_npi_vi_ca.RDS") %>% extract

summary_fit <- function(p, data, model){
  
  return(p$cases %>%
           apply(2, quantile, probs=c(0.5,0.025,0.975)) %>% t %>%
           data.frame(model, data$date, data$RV_scaled_cases, .) %>%
           setNames(c("model", "date", "obs", "median","CI95_lower","CI95_upper")))
}

fit_US <- rbind(summary_fit(p_US_SIRS, data_us, model="SIRS"),
                summary_fit(p_US_SEIRS, data_us, model="SEIRS")) %>%
  mutate(model=factor(model, levels=c("SIRS", "SEIRS")))

fit_CA <- rbind(summary_fit(p_CA_SIRS, data_ca, model="SIRS"),
                summary_fit(p_CA_SEIRS, data_ca, model="SEIRS")) %>%
  mutate(model=factor(model, levels=c("SIRS", "SEIRS")))

colors <- c("blue", "green")
fill <- c("#9EC3FF", "#9EFFA7")
years <- ymd(paste0(2014:2025, "-01-01"))
xlim <- range(c(data_us$date, data_ca$date))

npi_period <- geom_rect(aes(xmin=ymd('2020-02-15'), xmax=ymd('2022-10-15'),
                            ymin=-Inf, ymax=+Inf), fill='grey85')

vlines <- geom_vline(xintercept=ymd(paste0(2014:2025, "-01-01")),
                     lty='dotted', col="grey40", lwd=0.3)



plot_grid(
  
  ggplot(fit_US, aes(x=date)) +
    npi_period + vlines +
    geom_point(aes(y=obs), cex=0.5) +
    geom_ribbon(aes(ymin=CI95_lower, ymax=CI95_upper, fill=model), alpha=0.5) +
    geom_line(aes(x=date, y=median, col=model, lty=model), linewidth=0.5) +
    labs(title="US", y='RV/EV rescaled detections\n') +
    scale_x_date(expand=c(0,0), limits=xlim, breaks=years, date_labels="%Y") +
    scale_y_continuous(expand=c(0,0)) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=fill) +
    theme_test() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9),
          legend.position='none'),
  
  ggplot(fit_CA, aes(x=date)) +
    npi_period + vlines +
    geom_point(aes(y=obs), cex=0.5) +
    geom_ribbon(aes(ymin=CI95_lower, ymax=CI95_upper, fill=model), alpha=0.5) +
    geom_line(aes(x=date, y=median, col=model, lty=model), linewidth=0.5) +
    labs(title="Canada", y='RV/EV rescaled detections\n') +
    scale_x_date(expand=c(0,0), limits=xlim, breaks=years, date_labels="%Y") +
    scale_y_continuous(expand=c(0,0)) + 
    scale_color_manual(values=colors) +
    scale_fill_manual(values=fill) +
    theme_test() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9),
          legend.title=element_blank(), legend.position=c(0.93,0.8)),
  
  ncol=1, labels=LETTERS[1:2]
) # landscape: 7 x 5

p_US_SEIRS$beta %>% apply(2, quantile, probs=0.5) %>%
  plot(type='l', col='darkgreen', lty='dashed', ylim=c(1.2,2))
lines(p_US_SIRS$beta %>% apply(2, quantile, probs=0.5), col='blue')

p_US_SIRS$phi %>% quantile(probs=c(0.5,0.025,0.975))
p_US_SEIRS$phi %>% quantile(probs=c(0.5,0.025,0.975))

p_US_SIRS$kappa %>% quantile(probs=c(0.5,0.025,0.975))
p_US_SEIRS$kappa %>% quantile(probs=c(0.5,0.025,0.975))

(1/(1-exp(-p_US_SIRS$omega))) %>% quantile(probs=c(0.5,0.025,0.975))
(0.25/(1-exp(-p_US_SEIRS$omega*0.25))) %>% quantile(probs=c(0.5,0.025,0.975))

p_US_SIRS$rho %>% quantile(probs=c(0.5,0.025,0.975))
p_US_SEIRS$rho %>% quantile(probs=c(0.5,0.025,0.975))
