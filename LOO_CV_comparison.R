rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)
library(loo)
library(cowplot)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes")

load("Data/USA/Data_USA.RData")
load("Data/Canada/data_canada_province.RData")

US_names <- c("US (national)", paste('HHS', 2:10))
CA_names <- c("Canada (national)", "British Columbia", "Ontario", "Prairies")

# Without viral interaction

## US

fit_us <- readRDS("Fit/Output/fit_RV_SIRS_npi_us.RDS")
#fit_hhs1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs1.RDS")
fit_hhs2 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs2.RDS")
fit_hhs3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs3.RDS")
fit_hhs4 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs4.RDS")
fit_hhs5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs5.RDS")
fit_hhs6 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs6.RDS")
fit_hhs7 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs7.RDS")
fit_hhs8 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs8.RDS")
fit_hhs9 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs9.RDS")
fit_hhs10 <- readRDS("Fit/Output/fit_RV_SIRS_npi_hhs10.RDS")

## Canada

fit_ca <- readRDS("Fit/Output/fit_RV_SIRS_npi_ca.RDS")
# fit_at <- readRDS("Fit/Output/fit_RV_SIRS_npi_at.RDS")
fit_bc <- readRDS("Fit/Output/fit_RV_SIRS_npi_bc.RDS")
fit_on <- readRDS("Fit/Output/fit_RV_SIRS_npi_on.RDS")
fit_pr <- readRDS("Fit/Output/fit_RV_SIRS_npi_pr.RDS")

# With viral interaction (lag0)

## US

fit_us_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us.RDS")
# fit_hhs1_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1.RDS")
fit_hhs2_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2.RDS")
fit_hhs3_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3.RDS")
fit_hhs4_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4.RDS")
fit_hhs5_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5.RDS")
fit_hhs6_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6.RDS")
fit_hhs7_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7.RDS")
fit_hhs8_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8.RDS")
fit_hhs9_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9.RDS")
fit_hhs10_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10.RDS")

## Canada

fit_ca_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca.RDS")
# fit_at_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at.RDS")
fit_bc_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc.RDS")
fit_on_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on.RDS")
fit_pr_lag0 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr.RDS")

# With viral interaction (lag1)

## US

fit_us_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_lag1.RDS")
# fit_hhs1_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_lag1.RDS")
fit_hhs2_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag1.RDS")
fit_hhs3_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag1.RDS")
fit_hhs4_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag1.RDS")
fit_hhs5_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag1.RDS")
fit_hhs6_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_lag1.RDS")
fit_hhs7_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag1.RDS")
fit_hhs8_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_lag1.RDS")
fit_hhs9_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_lag1.RDS")
fit_hhs10_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag1.RDS")

## Canada

fit_ca_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_lag1.RDS")
# fit_at_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_lag1.RDS")
fit_bc_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag1.RDS")
fit_on_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_lag1.RDS")
fit_pr_lag1 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_lag1.RDS")

# With viral interaction (lag3)

## US

fit_us_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_lag3.RDS")
# fit_hhs1_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_lag3.RDS")
fit_hhs2_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag3.RDS")
fit_hhs3_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag3.RDS")
fit_hhs4_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag3.RDS")
fit_hhs5_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag3.RDS")
fit_hhs6_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_lag3.RDS")
fit_hhs7_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag3.RDS")
fit_hhs8_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_lag3.RDS")
fit_hhs9_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_lag3.RDS")
fit_hhs10_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag3.RDS")

## Canada

fit_ca_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_lag3.RDS")
# fit_at_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_lag3.RDS")
fit_bc_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag3.RDS")
fit_on_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_lag3.RDS")
fit_pr_lag3 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_lag3.RDS")

# With viral interaction (lag5)

## US

fit_us_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_lag5.RDS")
# fit_hhs1_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs1_lag5.RDS")
fit_hhs2_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag5.RDS")
fit_hhs3_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag5.RDS")
fit_hhs4_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag5.RDS")
fit_hhs5_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag5.RDS")
fit_hhs6_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs6_lag5.RDS")
fit_hhs7_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag5.RDS")
fit_hhs8_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs8_lag5.RDS")
fit_hhs9_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs9_lag5.RDS")
fit_hhs10_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag5.RDS")

## Canada

fit_ca_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca_lag5.RDS")
# fit_at_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_at_lag5.RDS")
fit_bc_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag5.RDS")
fit_on_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_on_lag5.RDS")
fit_pr_lag5 <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_pr_lag5.RDS")

# Model comparisons (LOO)

extract_loo_compare <- function(loo_model1, loo_model2){
  comp <- loo_compare(loo_model1, loo_model2)
  if(rownames(comp)[1]=='model2'){
    comp[2,1] <- -comp[2,1] # to always return elpd_model2-elpd_model1
  }
  return(t(comp[2,1:2]))
}

ELPD_US <- data.frame()

## US

loo_us <- loo::loo(extract_log_lik(fit_us, merge_chains=FALSE))
loo_us_lag0 <- loo::loo(extract_log_lik(fit_us_lag0, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 332
loo_us_lag1 <- loo::loo(extract_log_lik(fit_us_lag1, merge_chains=FALSE))
loo_us_lag3 <- loo::loo(extract_log_lik(fit_us_lag3, merge_chains=FALSE))
loo_us_lag5 <- loo::loo(extract_log_lik(fit_us_lag5, merge_chains=FALSE))

pareto_k_table(loo_us_lag0)
index <- which(loo_us_lag0$diagnostics$pareto_k>0.7)
loo_us_lag0$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='US', lag=0, extract_loo_compare(loo_us, loo_us_lag0)),
  data.frame(location='US', lag=1, extract_loo_compare(loo_us, loo_us_lag1)),
  data.frame(location='US', lag=3, extract_loo_compare(loo_us, loo_us_lag3)),
  data.frame(location='US', lag=5, extract_loo_compare(loo_us, loo_us_lag5))
)

## HHS2
loo_hhs2 <- loo::loo(extract_log_lik(fit_hhs2, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 4 373
loo_hhs2_lag0 <- loo::loo(extract_log_lik(fit_hhs2_lag0, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) #4 373
loo_hhs2_lag1 <- loo::loo(extract_log_lik(fit_hhs2_lag1, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 4 6
loo_hhs2_lag3 <- loo::loo(extract_log_lik(fit_hhs2_lag3, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 4
loo_hhs2_lag5 <- loo::loo(extract_log_lik(fit_hhs2_lag5, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 4

pareto_k_table(loo_hhs2)
index <- which(loo_hhs2$diagnostics$pareto_k>0.7)
loo_hhs2$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs2_lag0)
index <- which(loo_hhs2_lag0$diagnostics$pareto_k>0.7)
loo_hhs2_lag0$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs2_lag1)
index <- which(loo_hhs2_lag1$diagnostics$pareto_k>0.7)
loo_hhs2_lag1$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag1_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs2_lag3)
index <- which(loo_hhs2_lag3$diagnostics$pareto_k>0.7)
loo_hhs2_lag3$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag3_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs2_lag5)
index <- which(loo_hhs2_lag5$diagnostics$pareto_k>0.7)
loo_hhs2_lag5$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs2_lag5_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS2', lag=0, extract_loo_compare(loo_hhs2, loo_hhs2_lag0)),
  data.frame(location='HHS2', lag=1, extract_loo_compare(loo_hhs2, loo_hhs2_lag1)),
  data.frame(location='HHS2', lag=3, extract_loo_compare(loo_hhs2, loo_hhs2_lag3)),
  data.frame(location='HHS2', lag=5, extract_loo_compare(loo_hhs2, loo_hhs2_lag5))
)

## HHS3
loo_hhs3 <- loo::loo(extract_log_lik(fit_hhs3, merge_chains=FALSE))
loo_hhs3_lag0 <- loo::loo(extract_log_lik(fit_hhs3_lag0, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 6
loo_hhs3_lag1 <- loo::loo(extract_log_lik(fit_hhs3_lag1, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 6
loo_hhs3_lag3 <- loo::loo(extract_log_lik(fit_hhs3_lag3, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 6
loo_hhs3_lag5 <- loo::loo(extract_log_lik(fit_hhs3_lag5, merge_chains=FALSE))

pareto_k_table(loo_hhs3_lag0)
index <- which(loo_hhs3_lag0$diagnostics$pareto_k>0.7)
loo_hhs3_lag0$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs3_lag1)
index <- which(loo_hhs3_lag1$diagnostics$pareto_k>0.7)
loo_hhs3_lag1$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag1_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs3_lag3)
index <- which(loo_hhs3_lag3$diagnostics$pareto_k>0.7)
loo_hhs3_lag3$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs3_lag3_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS3', lag=0, extract_loo_compare(loo_hhs3, loo_hhs3_lag0)),
  data.frame(location='HHS3', lag=1, extract_loo_compare(loo_hhs3, loo_hhs3_lag1)),
  data.frame(location='HHS3', lag=3, extract_loo_compare(loo_hhs3, loo_hhs3_lag3)),
  data.frame(location='HHS3', lag=5, extract_loo_compare(loo_hhs3, loo_hhs3_lag5))
)

## HHS4
loo_hhs4 <- loo::loo(extract_log_lik(fit_hhs4, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 2
loo_hhs4_lag0 <- loo::loo(extract_log_lik(fit_hhs4_lag0, merge_chains=FALSE))
loo_hhs4_lag1 <- loo::loo(extract_log_lik(fit_hhs4_lag1, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 2
loo_hhs4_lag3 <- loo::loo(extract_log_lik(fit_hhs4_lag3, merge_chains=FALSE))
loo_hhs4_lag5 <- loo::loo(extract_log_lik(fit_hhs4_lag5, merge_chains=FALSE))

pareto_k_table(loo_hhs4)
index <- which(loo_hhs4$diagnostics$pareto_k>0.7)
loo_hhs4$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_hhs4_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs4_lag1)
index <- which(loo_hhs4_lag1$diagnostics$pareto_k>0.7)
loo_hhs4_lag1$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs4_lag1_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS4', lag=0, extract_loo_compare(loo_hhs4, loo_hhs4_lag0)),
  data.frame(location='HHS4', lag=1, extract_loo_compare(loo_hhs4, loo_hhs4_lag1)),
  data.frame(location='HHS4', lag=3, extract_loo_compare(loo_hhs4, loo_hhs4_lag3)),
  data.frame(location='HHS4', lag=5, extract_loo_compare(loo_hhs4, loo_hhs4_lag5))
)

## HHS5
loo_hhs5 <- loo::loo(extract_log_lik(fit_hhs5, merge_chains=FALSE))
loo_hhs5_lag0 <- loo::loo(extract_log_lik(fit_hhs5_lag0, merge_chains=FALSE))
loo_hhs5_lag1 <- loo::loo(extract_log_lik(fit_hhs5_lag1, merge_chains=FALSE))
loo_hhs5_lag3 <- loo::loo(extract_log_lik(fit_hhs5_lag3, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 342 343 344
loo_hhs5_lag5 <- loo::loo(extract_log_lik(fit_hhs5_lag5, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 342 343

pareto_k_table(loo_hhs5_lag3)
index <- which(loo_hhs5_lag3$diagnostics$pareto_k>0.7)
loo_hhs5_lag3$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag3_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs5_lag5)
index <- which(loo_hhs5_lag5$diagnostics$pareto_k>0.7)
loo_hhs5_lag5$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs5_lag5_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS5', lag=0, extract_loo_compare(loo_hhs5, loo_hhs5_lag0)),
  data.frame(location='HHS5', lag=1, extract_loo_compare(loo_hhs5, loo_hhs5_lag1)),
  data.frame(location='HHS5', lag=3, extract_loo_compare(loo_hhs5, loo_hhs5_lag3)),
  data.frame(location='HHS5', lag=5, extract_loo_compare(loo_hhs5, loo_hhs5_lag5))
)

## HHS6
loo_hhs6 <- loo::loo(extract_log_lik(fit_hhs6, merge_chains=FALSE))
loo_hhs6_lag0 <- loo::loo(extract_log_lik(fit_hhs6_lag0, merge_chains=FALSE))
loo_hhs6_lag1 <- loo::loo(extract_log_lik(fit_hhs6_lag1, merge_chains=FALSE))
loo_hhs6_lag3 <- loo::loo(extract_log_lik(fit_hhs6_lag3, merge_chains=FALSE))
loo_hhs6_lag5 <- loo::loo(extract_log_lik(fit_hhs6_lag5, merge_chains=FALSE))

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS6', lag=0, extract_loo_compare(loo_hhs6, loo_hhs6_lag0)),
  data.frame(location='HHS6', lag=1, extract_loo_compare(loo_hhs6, loo_hhs6_lag1)),
  data.frame(location='HHS6', lag=3, extract_loo_compare(loo_hhs6, loo_hhs6_lag3)),
  data.frame(location='HHS6', lag=5, extract_loo_compare(loo_hhs6, loo_hhs6_lag5))
)

## HHS7
loo_hhs7 <- loo::loo(extract_log_lik(fit_hhs7, merge_chains=FALSE))
loo_hhs7_lag0 <- loo::loo(extract_log_lik(fit_hhs7_lag0, merge_chains=FALSE))
loo_hhs7_lag1 <- loo::loo(extract_log_lik(fit_hhs7_lag1, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 1
loo_hhs7_lag3 <- loo::loo(extract_log_lik(fit_hhs7_lag3, merge_chains=FALSE))
loo_hhs7_lag5 <- loo::loo(extract_log_lik(fit_hhs7_lag5, merge_chains=FALSE))

pareto_k_table(loo_hhs7_lag1)
index <- which(loo_hhs7_lag1$diagnostics$pareto_k>0.7)
loo_hhs7_lag1$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs7_lag1_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS7', lag=0, extract_loo_compare(loo_hhs7, loo_hhs7_lag0)),
  data.frame(location='HHS7', lag=1, extract_loo_compare(loo_hhs7, loo_hhs7_lag1)),
  data.frame(location='HHS7', lag=3, extract_loo_compare(loo_hhs7, loo_hhs7_lag3)),
  data.frame(location='HHS7', lag=5, extract_loo_compare(loo_hhs7, loo_hhs7_lag5))
)

## HHS8
loo_hhs8 <- loo::loo(extract_log_lik(fit_hhs8, merge_chains=FALSE))
loo_hhs8_lag0 <- loo::loo(extract_log_lik(fit_hhs8_lag0, merge_chains=FALSE))
loo_hhs8_lag1 <- loo::loo(extract_log_lik(fit_hhs8_lag1, merge_chains=FALSE))
loo_hhs8_lag3 <- loo::loo(extract_log_lik(fit_hhs8_lag3, merge_chains=FALSE))
loo_hhs8_lag5 <- loo::loo(extract_log_lik(fit_hhs8_lag5, merge_chains=FALSE))

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS8', lag=0, extract_loo_compare(loo_hhs8, loo_hhs8_lag0)),
  data.frame(location='HHS8', lag=1, extract_loo_compare(loo_hhs8, loo_hhs8_lag1)),
  data.frame(location='HHS8', lag=3, extract_loo_compare(loo_hhs8, loo_hhs8_lag3)),
  data.frame(location='HHS8', lag=5, extract_loo_compare(loo_hhs8, loo_hhs8_lag5))
)

## HHS9
loo_hhs9 <- loo::loo(extract_log_lik(fit_hhs9, merge_chains=FALSE))
loo_hhs9_lag0 <- loo::loo(extract_log_lik(fit_hhs9_lag0, merge_chains=FALSE))
loo_hhs9_lag1 <- loo::loo(extract_log_lik(fit_hhs9_lag1, merge_chains=FALSE))
loo_hhs9_lag3 <- loo::loo(extract_log_lik(fit_hhs9_lag3, merge_chains=FALSE))
loo_hhs9_lag5 <- loo::loo(extract_log_lik(fit_hhs9_lag5, merge_chains=FALSE))

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS9', lag=0, extract_loo_compare(loo_hhs9, loo_hhs9_lag0)),
  data.frame(location='HHS9', lag=1, extract_loo_compare(loo_hhs9, loo_hhs9_lag1)),
  data.frame(location='HHS9', lag=3, extract_loo_compare(loo_hhs9, loo_hhs9_lag3)),
  data.frame(location='HHS9', lag=5, extract_loo_compare(loo_hhs9, loo_hhs9_lag5))
)

## HHS10
loo_hhs10 <- loo::loo(extract_log_lik(fit_hhs10, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 335
loo_hhs10_lag0 <- loo::loo(extract_log_lik(fit_hhs10_lag0, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 335
loo_hhs10_lag1 <- loo::loo(extract_log_lik(fit_hhs10_lag1, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 335
loo_hhs10_lag3 <- loo::loo(extract_log_lik(fit_hhs10_lag3, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 335
loo_hhs10_lag5 <- loo::loo(extract_log_lik(fit_hhs10_lag5, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 335

pareto_k_table(loo_hhs10)
index <- which(loo_hhs10$diagnostics$pareto_k>0.7)
loo_hhs10$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_hhs10_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs10_lag0)
index <- which(loo_hhs10_lag0$diagnostics$pareto_k>0.7)
loo_hhs10_lag0$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs10_lag1)
index <- which(loo_hhs10_lag1$diagnostics$pareto_k>0.7)
loo_hhs10_lag1$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag1_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs10_lag3)
index <- which(loo_hhs10_lag3$diagnostics$pareto_k>0.7)
loo_hhs10_lag3$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag3_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_hhs10_lag5)
index <- which(loo_hhs10_lag5$diagnostics$pareto_k>0.7)
loo_hhs10_lag5$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_hhs10_lag5_rm.RDS"))[,index] %>% mean

ELPD_US <- rbind(
  ELPD_US,
  data.frame(location='HHS10', lag=0, extract_loo_compare(loo_hhs10, loo_hhs10_lag0)),
  data.frame(location='HHS10', lag=1, extract_loo_compare(loo_hhs10, loo_hhs10_lag1)),
  data.frame(location='HHS10', lag=3, extract_loo_compare(loo_hhs10, loo_hhs10_lag3)),
  data.frame(location='HHS10', lag=5, extract_loo_compare(loo_hhs10, loo_hhs10_lag5))
)

write.csv2(ELPD_US, "ELPD_US.csv", row.names=FALSE)

## Canada

ELPD_CA <- data.frame()

loo_ca <- loo::loo(extract_log_lik(fit_ca, merge_chains=FALSE))
loo_ca_lag0 <- loo::loo(extract_log_lik(fit_ca_lag0, merge_chains=FALSE))
loo_ca_lag1 <- loo::loo(extract_log_lik(fit_ca_lag1, merge_chains=FALSE))
loo_ca_lag3 <- loo::loo(extract_log_lik(fit_ca_lag3, merge_chains=FALSE))
loo_ca_lag5 <- loo::loo(extract_log_lik(fit_ca_lag5, merge_chains=FALSE))

ELPD_CA <- rbind(
  ELPD_CA,
  data.frame(location='CA', lag=0, extract_loo_compare(loo_ca, loo_ca_lag0)),
  data.frame(location='CA', lag=1, extract_loo_compare(loo_ca, loo_ca_lag1)),
  data.frame(location='CA', lag=3, extract_loo_compare(loo_ca, loo_ca_lag3)),
  data.frame(location='CA', lag=5, extract_loo_compare(loo_ca, loo_ca_lag5))
)

## British Columbia
loo_bc <- loo::loo(extract_log_lik(fit_bc, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 138 147 204
loo_bc_lag0 <- loo::loo(extract_log_lik(fit_bc_lag0, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 138 147 204
loo_bc_lag1 <- loo::loo(extract_log_lik(fit_bc_lag1, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 138 147 204
loo_bc_lag3 <- loo::loo(extract_log_lik(fit_bc_lag3, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 138 147 204
loo_bc_lag5 <- loo::loo(extract_log_lik(fit_bc_lag5, merge_chains=FALSE), moment_match=TRUE, reloo=TRUE) # 138 147 204

pareto_k_table(loo_bc)
index <- which(loo_bc$diagnostics$pareto_k>0.7)
loo_bc$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_bc_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_bc_lag0)
index <- which(loo_bc_lag0$diagnostics$pareto_k>0.7)
loo_bc_lag0$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_bc_lag1)
index <- which(loo_bc_lag1$diagnostics$pareto_k>0.7)
loo_bc_lag5$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag1_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_bc_lag3)
index <- which(loo_bc_lag3$diagnostics$pareto_k>0.7)
loo_bc_lag5$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag3_rm.RDS"))[,index] %>% mean

pareto_k_table(loo_bc_lag5)
index <- which(loo_bc_lag5$diagnostics$pareto_k>0.7)
loo_bc_lag5$pointwise[index, "elpd_loo"] <- extract_log_lik(readRDS("Fit/Output/fit_RV_SIRS_npi_vi_bc_lag5_rm.RDS"))[,index] %>% mean

ELPD_CA <- rbind(
  ELPD_CA,
  data.frame(location='BC', lag=0, extract_loo_compare(loo_bc, loo_bc_lag0)),
  data.frame(location='BC', lag=1, extract_loo_compare(loo_bc, loo_bc_lag1)),
  data.frame(location='BC', lag=3, extract_loo_compare(loo_bc, loo_bc_lag3)),
  data.frame(location='BC', lag=5, extract_loo_compare(loo_bc, loo_bc_lag5))
)

## Ontario
loo_on <- loo::loo(extract_log_lik(fit_on, merge_chains=FALSE))
loo_on_lag0 <- loo::loo(extract_log_lik(fit_on_lag0, merge_chains=FALSE))
loo_on_lag1 <- loo::loo(extract_log_lik(fit_on_lag1, merge_chains=FALSE))
loo_on_lag3 <- loo::loo(extract_log_lik(fit_on_lag3, merge_chains=FALSE))
loo_on_lag5 <- loo::loo(extract_log_lik(fit_on_lag5, merge_chains=FALSE))

ELPD_CA <- rbind(
  ELPD_CA,
  data.frame(location='ON', lag=0, extract_loo_compare(loo_on, loo_on_lag0)),
  data.frame(location='ON', lag=1, extract_loo_compare(loo_on, loo_on_lag1)),
  data.frame(location='ON', lag=3, extract_loo_compare(loo_on, loo_on_lag3)),
  data.frame(location='ON', lag=5, extract_loo_compare(loo_on, loo_on_lag5))
)

## Prairies
loo_pr <- loo::loo(extract_log_lik(fit_pr, merge_chains=FALSE))
loo_pr_lag0 <- loo::loo(extract_log_lik(fit_pr_lag0, merge_chains=FALSE))
loo_pr_lag1 <- loo::loo(extract_log_lik(fit_pr_lag1, merge_chains=FALSE))
loo_pr_lag3 <- loo::loo(extract_log_lik(fit_pr_lag3, merge_chains=FALSE))
loo_pr_lag5 <- loo::loo(extract_log_lik(fit_pr_lag5, merge_chains=FALSE))

ELPD_CA <- rbind(
  ELPD_CA,
  data.frame(location='PR', lag=0, extract_loo_compare(loo_pr, loo_pr_lag0)),
  data.frame(location='PR', lag=1, extract_loo_compare(loo_pr, loo_pr_lag1)),
  data.frame(location='PR', lag=3, extract_loo_compare(loo_pr, loo_pr_lag3)),
  data.frame(location='PR', lag=5, extract_loo_compare(loo_pr, loo_pr_lag5))
)

write.csv2(ELPD_CA, "ELPD_CA.csv", row.names=FALSE)

ELPD_US$location <- factor(ELPD_US$location, levels=c('US', paste0('HHS',1:10)),
                           labels=c('US (national)', paste('HHS',1:10)))
ELPD_CA$location <- factor(ELPD_CA$location, levels=c('CA', 'BC', 'ON', 'PR'),
                           labels=c('Canada (national)', 'British Columbia', 'Ontario', 'Prairies'))

plot_grid(
  
  ggplot(ELPD_US, aes(x=as.factor(lag), y=elpd_diff/se_diff)) +
    facet_wrap(~location, ncol=3) +
    geom_hline(yintercept=0, lwd=0.1) +
    geom_bar(stat="identity", col='black', fill='black', lwd=0.5, width=0.4) +
    labs(y=expression(paste(Delta,'ELPD / ','SE(',Delta,'ELPD)'))) +
    scale_y_continuous(breaks=seq(-6,6,2), limits=c(-6,6)) +
    theme_test() +
    theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=8),
          axis.title.x=element_blank()),
  
  ggplot(ELPD_CA, aes(x=as.factor(lag), y=elpd_diff/se_diff)) +
    facet_wrap(~location) +
    geom_hline(yintercept=0, lwd=0.1) +
    geom_bar(stat="identity", col='black', fill='black', lwd=0.5, width=0.4) +
    labs(x='\nLag in IAV incidence (weeks)', y=expression(paste(Delta,'ELPD / ','SE(',Delta,'ELPD)'))) +
    scale_y_continuous(breaks=seq(-6,6,2), limits=c(-6,6)) +
    theme_test() +
    theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=8),
          plot.margin=unit(c(0,1.75,0,1.75), "cm")),
  
  ncol=1, rel_heights=c(0.6,0.4), labels=LETTERS[1:2]) # 8 x 5


plot_grid(
  
  ggplot(ELPD_US, aes(x=as.factor(lag), y=elpd_diff/se_diff)) +
    facet_wrap(~location, ncol=3) +
    geom_hline(yintercept=0, lwd=0.1) +
    geom_bar(stat="identity", col='black', fill='black', lwd=0.5, width=0.4) +
    labs(x='Lag in IAV incidence (weeks)', y=expression(paste(Delta,'ELPD / ','SE(',Delta,'ELPD)'))) +
    scale_y_continuous(breaks=seq(-6,6,2), limits=c(-6,6)) +
    theme_test() +
    theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=8)),
  
  ggplot(ELPD_CA, aes(x=as.factor(lag), y=elpd_diff/se_diff)) +
    facet_wrap(~location) +
    geom_hline(yintercept=0, lwd=0.1) +
    geom_bar(stat="identity", col='black', fill='black', lwd=0.5, width=0.4) +
    labs(x='\nLag in IAV incidence (weeks)') +
    scale_y_continuous(breaks=seq(-6,6,2), limits=c(-6,6)) +
    theme_test() +
    theme(axis.title.y=element_blank(), axis.text.y=element_text(size=8),
          plot.margin=unit(c(0.2,0.2,2.65,0.3), "cm")),
  
  ncol=2, rel_widths=c(0.6,0.4), labels=LETTERS[1:2]) # 8 x 6
