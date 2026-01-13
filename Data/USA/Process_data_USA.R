rm(list=ls())

library(tidyverse)
library(plyr)
library(readxl)
library(lubridate)
library(ISOweek)
library(ggpubr)

setwd('C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data')

years <- ymd(paste0(2014:2025,"01-01"))
locations <- c("National", paste("HHS", 1:10))

# RV

## 2010-2020

RV.1 <- read_xlsx("USA/NVERSS/NVERSS_data_final_22Jul2020.xlsx", sheet=2)[,1:5]
colnames(RV.1)[c(1,5)] <- c("date", "location")
RV.1$date <- ymd(RV.1$date)
RV.1$Rhinopos <- as.numeric(RV.1$Rhinopos)
RV.1$location <- paste("HHS", RV.1$location)
RV.1 <- RV.1[-which(RV.1$Rhinotest==134119),] # Delete this suspicious line because 134119 tests is far too high

RV.1 <- RV.1 %>% ddply(~date+location, function(X){ # sum over all test types
  return(c("test"=sum(X$Rhinotest), "positive"=sum(X$Rhinopos)))
})

RV.1 <- RV.1 %>% rbind(
  ddply(RV.1, ~date, function(X){
    return(apply(X[,3:4],2,sum))
  }) %>% cbind("location"="National")
)

## 2019-2025

RV.2 <- rbind(
  transform(read.csv("USA/NVERSS/detection_national_rv-ev.csv", header=TRUE),
            date=mdy(date),
            location="National"),
  transform(read_xlsx("USA/NVERSS/NREVSS_RV_data_2020_2025_HHS.xlsx"),
            date=ymd(date),
            location=paste("HHS", location))
)

RV.2$test <- RV.2$negative+RV.2$positive

## Combine the two datasets

RV <- rbind(
  RV.1[, colnames(RV.1) %in% c("date", "location", "test", "positive")],
  RV.2[RV.2$date > max(RV.1$date), colnames(RV.2) %in% c("date", "location", "test", "positive")]
)

RV$location <- RV$location %>% factor(levels=locations)

# IAV

col_names <- c("REGION", "YEAR", "WEEK", "TOTAL.SPECIMENS", "TOTAL.A")

IAV <- rbind(
  read.csv("USA/FluView/ICL_NREVSS_Combined_prior_to_2015_16.csv", header=TRUE)[,col_names],
  read.csv("USA/FluView/ICL_NREVSS_Combined_prior_to_2015_16_National.csv", header=TRUE)[,col_names],
  read.csv("USA/FluView/ICL_NREVSS_Clinical_Labs.csv", header=TRUE)[,col_names],
  read.csv("USA/FluView/ICL_NREVSS_Clinical_Labs_National.csv", header=TRUE)[,col_names]
) %>%
  mutate(date=ISOweek2date(sprintf("%d-W%02d-6", YEAR, WEEK))) # 6=Saturday

colnames(IAV) <- c("location", "year", "week", "test", "positive", "date")

IAV$location <- IAV$location %>% gsub("Region", "HHS",.)
IAV$location <- IAV$location %>% gsub("X", "National",.)
IAV$location <- factor(IAV$location, levels=locations)

# Final dataset

colnames(RV) <- c("date", "location", "RV_test", "RV_positive")
colnames(IAV) <- c("location", "year", "week", "IAV_test", "IAV_positive", "date")

data <- IAV[,c("location", "date", "week", "IAV_test", "IAV_positive")] %>% merge(RV)

data$RV_test_rolling_avg <- NA
data$IAV_test_rolling_avg <- NA
data$RV_testing_factor <- NA
data$IAV_testing_factor <- NA

mean_test <- data %>%
  ddply(~location, function(X){return(c("RV"=mean(X$RV_test), "IAV"=mean(X$IAV_test)))})

for(i in 1:nrow(data)){
  
  X <- subset(data, location == data$location[i] &
                date >= data$date[i] %m-% months(6) &
                date <= data$date[i] %m+% months(6))
  
  data$RV_test_rolling_avg[i] <- mean(X$RV_test)
  data$IAV_test_rolling_avg[i] <- mean(X$IAV_test)
  
  data$RV_testing_factor[i] <- mean_test$RV[match(unique(X$location), mean_test$location)]/data$RV_test_rolling_avg[i]
  data$IAV_testing_factor[i] <- mean_test$IAV[match(unique(X$location), mean_test$location)]/data$IAV_test_rolling_avg[i]
}

data$RV_scaled_cases <- data$RV_positive*data$RV_testing_factor
data$IAV_scaled_cases <- data$IAV_positive*data$IAV_testing_factor

data %>%
  ggplot(aes(x=date, y=RV_positive)) +
  facet_wrap(~location, ncol=3, scales="free_y") +
  geom_vline(xintercept=years, lwd=0.5, lty='dotted') +
  geom_line(col="grey50") +
  geom_line(aes(y=RV_scaled_cases), col="red") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9))

data %>%
  ggplot(aes(x=date, y=IAV_test)) +
  facet_wrap(~location, ncol=3, scales="free_y") +
  geom_vline(xintercept=years, lwd=0.5, lty='dotted') +
  geom_line() +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9))

data %>%
  ggplot(aes(x=date, y=IAV_positive)) +
  facet_wrap(~location, ncol=3, scales="free_y") +
  geom_vline(xintercept=years, lwd=0.5, lty='dotted') +
  geom_line(col="grey50") +
  geom_line(aes(y=IAV_scaled_cases), col="red") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9))

data <- data[order(data$date),]

sum_IAV_cases_lag <- function(data, lag=0){
  return(
    (1:nrow(data)) %>% sapply(function(i){
      return(sum(data$IAV_scaled_cases[data$location==data$location[i] &
                                         data$date>=data$date[i]-7*lag &
                                         data$date<=data$date[i]]))
      })
  )
}

data$IAV_scaled_cases_lag1 <- sum_IAV_cases_lag(data, lag=1)
data$IAV_scaled_cases_lag3 <- sum_IAV_cases_lag(data, lag=3)
data$IAV_scaled_cases_lag5 <- sum_IAV_cases_lag(data, lag=5)

data <- data %>% subset(date >= ymd("2014-01-01"))

ggplot(data, aes(x=date)) +
  facet_wrap(~location, ncol=3, scales="free_y") +
  geom_line(aes(y=IAV_scaled_cases)) +
  geom_line(aes(y=IAV_scaled_cases_lag1), col='red') +
  geom_line(aes(y=IAV_scaled_cases_lag3), col='darkgreen') +
  geom_line(aes(y=IAV_scaled_cases_lag5), col='orange')

data_us <- data %>% subset(location=="National")
data_hhs1 <- data %>% subset(location=="HHS 1")
data_hhs2 <- data %>% subset(location=="HHS 2")
data_hhs3 <- data %>% subset(location=="HHS 3")
data_hhs4 <- data %>% subset(location=="HHS 4")
data_hhs5 <- data %>% subset(location=="HHS 5")
data_hhs6 <- data %>% subset(location=="HHS 6")
data_hhs7 <- data %>% subset(location=="HHS 7")
data_hhs8 <- data %>% subset(location=="HHS 8")
data_hhs9 <- data %>% subset(location=="HHS 9")
data_hhs10 <- data %>% subset(location=="HHS 10")

# Google mobility data

load("Google_mobility/mobility_mean_US.RData")

data_us$c <- 0
data_hhs1$c <- 0
data_hhs2$c <- 0
data_hhs3$c <- 0
data_hhs4$c <- 0
data_hhs5$c <- 0
data_hhs6$c <- 0
data_hhs7$c <- 0
data_hhs8$c <- 0
data_hhs9$c <- 0
data_hhs10$c <- 0

data_us$c[match(mobility_mean_us$week_ending, data_us$date)] <- mobility_mean_us$trend4/100
data_hhs1$c[match(mobility_mean_hhs1$week_ending, data_hhs1$date)] <- mobility_mean_hhs1$trend4/100
data_hhs2$c[match(mobility_mean_hhs2$week_ending, data_hhs2$date)] <- mobility_mean_hhs2$trend4/100
data_hhs3$c[match(mobility_mean_hhs3$week_ending, data_hhs3$date)] <- mobility_mean_hhs3$trend4/100
data_hhs4$c[match(mobility_mean_hhs4$week_ending, data_hhs4$date)] <- mobility_mean_hhs4$trend4/100
data_hhs5$c[match(mobility_mean_hhs5$week_ending, data_hhs5$date)] <- mobility_mean_hhs5$trend4/100
data_hhs6$c[match(mobility_mean_hhs6$week_ending, data_hhs6$date)] <- mobility_mean_hhs6$trend4/100
data_hhs7$c[match(mobility_mean_hhs7$week_ending, data_hhs7$date)] <- mobility_mean_hhs7$trend4/100
data_hhs8$c[match(mobility_mean_hhs8$week_ending, data_hhs8$date)] <- mobility_mean_hhs8$trend4/100
data_hhs9$c[match(mobility_mean_hhs9$week_ending, data_hhs9$date)] <- mobility_mean_hhs9$trend4/100
data_hhs10$c[match(mobility_mean_hhs10$week_ending, data_hhs10$date)] <- mobility_mean_hhs10$trend4/100

# save(data_us, data_hhs1, data_hhs2, data_hhs3, data_hhs4, data_hhs5,
#      data_hhs6, data_hhs7, data_hhs8, data_hhs9, data_hhs10, file="Data_USA.RData")