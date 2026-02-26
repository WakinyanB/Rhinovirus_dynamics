rm(list=ls())

library(tidyverse)
library(plyr)
library(cowplot)
library(scales)
library(ggpubr)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data/Simulation_v2")

load("cases_1.RData")
obs1_rv <- obs_rv
obs1_iav <- obs_iav
load("cases_kick_1.RData")
obs1_kick_rv <- obs_kick_rv
obs1_kick_iav <- obs_kick_iav
load("cases_shift_1.RData")
obs1_shift_rv <- obs_shift_rv
obs1_shift_iav <- obs_shift_iav

load("cases_2.RData")
obs2_rv <- obs_rv
obs2_iav <- obs_iav
load("cases_kick_2.RData")
obs2_kick_rv <- obs_kick_rv
obs2_kick_iav <- obs_kick_iav
load("cases_shift_2.RData")
obs2_shift_rv <- obs_shift_rv
obs2_shift_iav <- obs_shift_iav

load("cases_2_no_vi.RData")
obs2_no_vi_rv <- obs_rv
obs2_no_vi_iav <- obs_iav
load("cases_kick_2_no_vi.RData")
obs2_no_vi_kick_rv <- obs_kick_rv
obs2_no_vi_kick_iav <- obs_kick_iav
load("cases_shift_2_no_vi.RData")
obs2_no_vi_shift_rv <- obs_shift_rv
obs2_no_vi_shift_iav <- obs_shift_iav

obs2_no_vi_rv$pathogen <- "RV (no interaction)"
obs2_no_vi_kick_rv$pathogen <- "RV (no interaction)"
obs2_no_vi_shift_rv$pathogen <- "RV (no interaction)"
obs2_no_vi_rv$cases_noisy <- NA
obs2_no_vi_kick_rv$cases_noisy <- NA
obs2_no_vi_shift_rv$cases_noisy <- NA

load("cases_3.RData")
obs3_rv <- obs_rv
obs3_iav <- obs_iav
load("cases_kick_3.RData")
obs3_kick_rv <- obs_kick_rv
obs3_kick_iav <- obs_kick_iav
load("cases_shift_3.RData")
obs3_shift_rv <- obs_shift_rv
obs3_shift_iav <- obs_shift_iav

load("cases_3_no_vi.RData")
obs3_no_vi_rv <- obs_rv
obs3_no_vi_iav <- obs_iav
load("cases_kick_3_no_vi.RData")
obs3_no_vi_kick_rv <- obs_kick_rv
obs3_no_vi_kick_iav <- obs_kick_iav
load("cases_shift_3_no_vi.RData")
obs3_no_vi_shift_rv <- obs_shift_rv
obs3_no_vi_shift_iav <- obs_shift_iav

obs3_no_vi_rv$pathogen <- "RV (no interaction)"
obs3_no_vi_kick_rv$pathogen <- "RV (no interaction)"
obs3_no_vi_shift_rv$pathogen <- "RV (no interaction)"
obs3_no_vi_rv$cases_noisy <- NA
obs3_no_vi_kick_rv$cases_noisy <- NA
obs3_no_vi_shift_rv$cases_noisy <- NA

data <- rbind(
  
  cbind(id='#1', type='main', obs1_rv),
  cbind(id='#1', type='main', obs1_iav[,1:6]),
  cbind(id='#1', type='shift', obs1_shift_rv),
  cbind(id='#1', type='shift', obs1_shift_iav[,1:6]),
  cbind(id='#1', type='kick', obs1_kick_rv),
  cbind(id='#1', type='kick', obs1_kick_iav[,1:6]),
  
  cbind(id='#2', type='main', obs2_rv),
  cbind(id='#2', type='main', obs2_iav[,1:6]),
  cbind(id='#2', type='shift', obs2_shift_rv),
  cbind(id='#2', type='shift', obs2_shift_iav[,1:6]),
  cbind(id='#2', type='kick', obs2_kick_rv),
  cbind(id='#2', type='kick', obs2_kick_iav[,1:6]),
  
  cbind(id='#2', type='main', obs2_no_vi_rv),
  cbind(id='#2', type='shift', obs2_no_vi_shift_rv),
  cbind(id='#2', type='kick', obs2_no_vi_kick_rv),
  
  cbind(id='#3', type='main', obs3_rv),
  cbind(id='#3', type='main', obs3_iav[,1:6]),
  cbind(id='#3', type='shift', obs3_shift_rv),
  cbind(id='#3', type='shift', obs3_shift_iav[,1:6]),
  cbind(id='#3', type='kick', obs3_kick_rv),
  cbind(id='#3', type='kick', obs3_kick_iav[,1:6]),
  
  cbind(id='#3', type='main', obs3_no_vi_rv),
  cbind(id='#3', type='shift', obs3_no_vi_shift_rv),
  cbind(id='#3', type='kick', obs3_no_vi_kick_rv)
  )

shift <- -0.5
type_levels <- c('main', 'shift', 'kick')
type_labels <- c('Main simulation', 'With 6-month shift in NPI timing', 'With pre-pandemic IAV perturbation')
path_levels <- c("IAV", "RV (no interaction)", "RV")

data$time <- data$time/52
data$time[data$type=='shift'] <- data$time[data$type=='shift'] + shift

data$type <- factor(data$type, levels=type_levels, labels=type_labels)
data$pathogen <- factor(data$pathogen, levels=path_levels)
data$c <- ifelse(data$c==0, yes=NA, no=data$c)

tmax_prepandemic <- data.frame(type=factor(type_levels, levels=type_levels, labels=type_labels),
                               tmax=c(6, 6+shift, 6))
data %>%
  ggplot(aes(x=time, y=cases)) +
  facet_grid(id~type) +
  geom_rect(aes(xmin=time-1/52, xmax=time, ymin=-Inf, ymax=+Inf, fill=c)) +
  geom_vline(xintercept=0:10, lty='dotted', col="grey40", lwd=0.3) +
  geom_vline(data=tmax_prepandemic, aes(xintercept=tmax)) +
  geom_line(aes(col=pathogen), lwd=0.4) +
  labs(x="Time (years)\n", y="Detections", col="Pathogen",
       fill="mean change\nin mobility") +
  scale_x_continuous(expand=c(0,0), breaks=0:9) +
  scale_y_continuous(expand=c(0.015,0)) +
  scale_color_manual(values=c('#EE6251','#91B5F7','#0D4ABA')) +
  scale_fill_gradient2(low="#2C792D", mid="white", high="#90529C",
                       midpoint=0, na.value=NA, label=scales::percent) +
  theme_test() +
  theme(title=element_text(size=10)) # 9.5 x 4.5
