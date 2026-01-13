rm(list=ls())

library(tidyverse)
library(plyr)
library(lubridate)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data/")

pop_size <- read.csv("Canada/Canada_pop_size.csv")

categories <- c("retail_and_recreation", "grocery_and_pharmacy", "transit_stations", "workplaces")

mobility <- rbind(read.csv("Google_mobility/2020_CA_Region_Mobility_Report.csv", header=TRUE),
                  read.csv("Google_mobility/2021_CA_Region_Mobility_Report.csv", header=TRUE),
                  read.csv("Google_mobility/2022_CA_Region_Mobility_Report.csv", header=TRUE))

mobility <- mobility %>% subset(sub_region_2=="")

mobility <- mobility[,c("sub_region_1", "date", categories) %>% sapply(function(x){
  return(grep(x, colnames(mobility)))
  })]

mobility$sub_region_1[mobility$sub_region_1==""] <- "Canada"

colnames(mobility) <- gsub("_percent_change_from_baseline", "", colnames(mobility))
colnames(mobility)[1] <- "province"

mobility <- mobility[mobility$province %in% c("Canada", "Alberta", "British Columbia", "Manitoba",
                                              "New Brunswick", "Newfoundland and Labrador", "Nova Scotia",
                                              "Ontario", "Prince Edward Island", "Saskatchewan"),]

mobility$date <- ymd(mobility$date)
mobility$week_ending <- ceiling_date(mobility$date,'week')-1

mobility_mean <- mobility %>% ddply(~province+week_ending, function(X){
  return(X[,colnames(X) %in% c("retail_and_recreation", "grocery_and_pharmacy",
                               "transit_stations", "workplaces")] %>% apply(2, mean, na.rm=TRUE))
  })

mobility_mean$trend4 <- mobility_mean[,!colnames(mobility_mean) %in% c("province", "week_ending")] %>%
  apply(1, mean, na.rm=TRUE)

mobility_mean_ca2 <- mobility_mean %>% subset(province=="Canada")
mobility_mean_bc2 <- mobility_mean %>% subset(province=="British Columbia")
mobility_mean_on2 <- mobility_mean %>% subset(province=="Ontario")

mobility_mean_pr2 <- mobility_mean %>% subset(province=="Alberta" |
                                              province=="Manitoba" |
                                              province=="Saskatchewan") %>%
  ddply(~week_ending, function(X){
    return(
      X[,!colnames(X) %in% c("province", "week_ending")] %>%
        apply(2, weighted.mean, w=pop_size$pop_2021[match(X$province, pop_size$Province)])
      )}
    ) %>% cbind("province"="Prairies",.)

mobility_mean_at2 <- mobility_mean %>% subset(province=="New Brunswick" |
                                              province=="Newfoundland and Labrador" |
                                              province=="Nova Scotia" |
                                              province=="Prince Edward Island") %>%
  ddply(~week_ending, function(X){
    return(
      X[,!colnames(X) %in% c("province", "week_ending")] %>%
        apply(2, weighted.mean, w=pop_size$pop_2021[match(X$province, pop_size$Province)])
    )}
  ) %>% cbind("province"="Atlantic",.)

# write.csv(mobility_mean_ca2, "Google_mobility/mobility_mean_ca2.csv", row.names=FALSE)
# write.csv(mobility_mean_at2, "Google_mobility/mobility_mean_at2.csv", row.names=FALSE)
# write.csv(mobility_mean_bc2, "Google_mobility/mobility_mean_bc2.csv", row.names=FALSE)
# write.csv(mobility_mean_on2, "Google_mobility/mobility_mean_on2.csv", row.names=FALSE)
# write.csv(mobility_mean_pr2, "Google_mobility/mobility_mean_pr2.csv", row.names=FALSE)

# save(mobility_mean_ca2, mobility_mean_at2, mobility_mean_bc2, mobility_mean_on2, mobility_mean_pr2,
#      file="Google_mobility/mobility_mean_CA.RData")