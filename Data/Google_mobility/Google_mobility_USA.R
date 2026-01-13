rm(list=ls())

library(tidyverse)
library(plyr)
library(lubridate)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/Data/")

pop_size <- read.csv("USA/State_2020_pop.csv", header=TRUE)
pop_size <- pop_size %>% rbind(data.frame("State"="US",
                                          "HHS_region"="National",
                                          "pop_2020"=sum(pop_size$pop_2020)))

categories <- c("retail_and_recreation", "grocery_and_pharmacy", "transit_stations", "workplaces")

mobility <- rbind(read.csv("Google_mobility/2020_US_Region_Mobility_Report.csv", header=TRUE),
                  read.csv("Google_mobility/2021_US_Region_Mobility_Report.csv", header=TRUE),
                  read.csv("Google_mobility/2022_US_Region_Mobility_Report.csv", header=TRUE))

mobility <- mobility %>% subset(sub_region_2=="")

mobility <- mobility[,c("sub_region_1", "date", categories) %>% sapply(function(x){
  return(grep(x, colnames(mobility)))
  })]

mobility$sub_region_1[mobility$sub_region_1==""] <- "US"

colnames(mobility) <- gsub("_percent_change_from_baseline", "", colnames(mobility))
colnames(mobility)[1] <- "location"

mobility$date <- ymd(mobility$date)
mobility$week_ending <- ceiling_date(mobility$date,'week')-1

mobility_mean <- mobility %>% ddply(~location+week_ending, function(X){
  return(X[,colnames(X) %in% c("retail_and_recreation", "grocery_and_pharmacy",
                               "transit_stations", "workplaces")] %>% apply(2, mean, na.rm=TRUE))
  })

mobility_mean$trend4 <- mobility_mean[,!colnames(mobility_mean) %in% c("location", "week_ending")] %>%
  apply(1, mean, na.rm=TRUE)

mobility_mean$HHS_region <- pop_size$HHS_region[match(mobility_mean$location, pop_size$State)]

mobility_mean_HHS <- mobility_mean %>% subset(location!="US") %>%
  ddply(~HHS_region+week_ending, function(X){
    return(
      X[,!colnames(X) %in% c("location", "week_ending", "HHS_region")] %>%
        apply(2, weighted.mean, w=pop_size$pop_2020[match(X$location, pop_size$State)])
    )
})

mobility_mean_us <- mobility_mean %>% subset(location=="US")
mobility_mean_hhs1 <- mobility_mean_HHS %>% subset(HHS_region=="1")
mobility_mean_hhs2 <- mobility_mean_HHS %>% subset(HHS_region=="2")
mobility_mean_hhs3 <- mobility_mean_HHS %>% subset(HHS_region=="3")
mobility_mean_hhs4 <- mobility_mean_HHS %>% subset(HHS_region=="4")
mobility_mean_hhs5 <- mobility_mean_HHS %>% subset(HHS_region=="5")
mobility_mean_hhs6 <- mobility_mean_HHS %>% subset(HHS_region=="6")
mobility_mean_hhs7 <- mobility_mean_HHS %>% subset(HHS_region=="7")
mobility_mean_hhs8 <- mobility_mean_HHS %>% subset(HHS_region=="8")
mobility_mean_hhs9 <- mobility_mean_HHS %>% subset(HHS_region=="9")
mobility_mean_hhs10 <- mobility_mean_HHS %>% subset(HHS_region=="10")

# mobility_mean_us %>% write.csv("Google_mobility/mobility_mean_us.csv", row.names=FALSE)
# mobility_mean_hhs1 %>% write.csv("Google_mobility/mobility_mean_hhs1.csv", row.names=FALSE)
# mobility_mean_hhs2 %>% write.csv("Data/Google_mobility/mobility_mean_hhs2.csv", row.names=FALSE)
# mobility_mean_hhs3 %>% write.csv("Data/Google_mobility/mobility_mean_hhs3.csv", row.names=FALSE)
# mobility_mean_hhs4 %>% write.csv("Data/Google_mobility/mobility_mean_hhs4.csv", row.names=FALSE)
# mobility_mean_hhs5 %>% write.csv("Data/Google_mobility/mobility_mean_hhs5.csv", row.names=FALSE)
# mobility_mean_hhs6 %>% write.csv("Data/Google_mobility/mobility_mean_hhs6.csv", row.names=FALSE)
# mobility_mean_hhs7 %>% write.csv("Data/Google_mobility/mobility_mean_hhs7.csv", row.names=FALSE)
# mobility_mean_hhs8 %>% write.csv("Data/Google_mobility/mobility_mean_hhs8.csv", row.names=FALSE)
# mobility_mean_hhs9 %>% write.csv("Data/Google_mobility/mobility_mean_hhs9.csv", row.names=FALSE)
# mobility_mean_hhs10 %>% write.csv("Data/Google_mobility/mobility_mean_hhs10.csv", row.names=FALSE)

# save(mobility_mean_us,
#      mobility_mean_hhs1, mobility_mean_hhs2, mobility_mean_hhs3, mobility_mean_hhs4, mobility_mean_hhs5,
#      mobility_mean_hhs6, mobility_mean_hhs7, mobility_mean_hhs8, mobility_mean_hhs9, mobility_mean_hhs10,
#      file="Google_mobility/mobility_mean_US.RData")