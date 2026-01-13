rm(list=ls())

library(tidyverse)
library(plyr)

path <- "C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data/"

setwd(path)

load("Google_mobility/mobility_mean_CA.RData")

data_ca <- read.csv("Canada/data_canada_resp.csv", header=TRUE)[,-1] %>% subset(province!="QC")

data <- merge.data.frame(x=subset(data_ca, type=="RV"|type=="RV/EV"),
                         y=subset(data_ca, type=="Flu A"), by=c("date", "province"))

all(data$week.x==data$week.y)

colnames(data)[which(colnames(data)=="week.x")] <- "week"
colnames(data)[which(colnames(data)=="positive.x")] <- "RV_positive"
colnames(data)[which(colnames(data)=="positive.y")] <- "IAV_positive"
colnames(data)[which(colnames(data)=="tests.x")] <- "RV_test"
colnames(data)[which(colnames(data)=="tests.y")] <- "IAV_test"

data <- data[,-c(grep("\\.x", colnames(data)), grep("\\.y", colnames(data)))]

data$date <- data$date %>% ymd

data$RV_test_rolling_avg <- NA
data$IAV_test_rolling_avg <- NA
data$RV_testing_factor <- NA
data$IAV_testing_factor <- NA

mean_tests <- data %>%
  ddply(~province, function(X){return(c("RV"=mean(X$RV_test), "IAV"=mean(X$IAV_test)))})

for(i in 1:nrow(data)){
  
  X <- subset(data, province == data$province[i] &
                    date >= data$date[i] %m-% months(6) &
                    date <= data$date[i] %m+% months(6))
  
  data$RV_test_rolling_avg[i] <- mean(X$RV_test)
  data$IAV_test_rolling_avg[i] <- mean(X$IAV_test)
  
  data$RV_testing_factor[i] <- mean_tests$RV[match(unique(X$province), mean_tests$province)]/
                               data$RV_test_rolling_avg[i]
  data$IAV_testing_factor[i] <- mean_tests$IAV[match(unique(X$province), mean_tests$province)]/
                                data$IAV_test_rolling_avg[i]
}

data$RV_scaled_cases <- (data$RV_positive/100)*data$RV_test*data$RV_testing_factor
data$IAV_scaled_cases <- (data$IAV_positive/100)*data$IAV_test*data$IAV_testing_factor

ggplot(data, aes(x=date)) +
  facet_wrap(~province, scales="free_y") +
  geom_line(aes(y=(RV_positive/100)*RV_test), col="grey") +
  geom_line(aes(y=RV_scaled_cases))

ggplot(data, aes(x=date)) +
  facet_wrap(~province, scales="free_y") +
  geom_line(aes(y=(IAV_positive/100)*IAV_test), col="grey") +
  geom_line(aes(y=IAV_scaled_cases))

sum_IAV_cases_lag <- function(data, lag=0){
  return(
    (1:nrow(data)) %>% sapply(function(i){
      return(sum(data$IAV_scaled_cases[data$province==data$province[i] &
                                         data$date>=data$date[i]-7*lag &
                                         data$date<=data$date[i]]))
    })
  )
}

data$IAV_scaled_cases_lag1 <- sum_IAV_cases_lag(data, lag=1)
data$IAV_scaled_cases_lag3 <- sum_IAV_cases_lag(data, lag=3)
data$IAV_scaled_cases_lag5 <- sum_IAV_cases_lag(data, lag=5)

ggplot(data, aes(x=date)) +
  facet_wrap(~province, ncol=3, scales="free_y") +
  geom_line(aes(y=IAV_scaled_cases)) +
  geom_line(aes(y=IAV_scaled_cases_lag1), col='red') +
  geom_line(aes(y=IAV_scaled_cases_lag3), col='darkgreen') +
  geom_line(aes(y=IAV_scaled_cases_lag5), col='orange')

data_ca <- data %>% subset(province=="Canada")
data_at <- data %>% subset(province=="At")
data_bc <- data %>% subset(province=="BC")
data_on <- data %>% subset(province=="ON")
data_pr <- data %>% subset(province=="Pr")

data_ca$c <- 0
data_at$c <- 0
data_bc$c <- 0
data_on$c <- 0
data_pr$c <- 0

data_ca$c[match(mobility_mean_ca2$week_ending, data_ca$date)] <- mobility_mean_ca2$trend4/100
data_at$c[match(mobility_mean_at2$week_ending, data_at$date)] <- mobility_mean_at2$trend4/100
data_bc$c[match(mobility_mean_bc2$week_ending, data_bc$date)] <- mobility_mean_bc2$trend4/100
data_on$c[match(mobility_mean_on2$week_ending, data_on$date)] <- mobility_mean_on2$trend4/100
data_pr$c[match(mobility_mean_pr2$week_ending, data_pr$date)] <- mobility_mean_pr2$trend4/100

# save(data_ca, data_at, data_bc, data_on, data_pr, file="Data/Canada/data_canada_province.RData")