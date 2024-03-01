

# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
#library(flepicommon)
library(gghighlight)


### IMPORT INITIAL DATA -----------------------------------

# source data functions
source("source/data_setup_source.R")

opt <- list()
opt$gt_data_source <- "hhs_hosp"
opt$delphi_api_key <- "04e7369e1541a"
opt$gt_data_path <- "data/nj_covid_hosp.parquet"
opt$gt_NJ_total_hosp_data_path <- "data/NJ_total_hosp.parquet"


# only need to run this if want to update data
opt$gt_data_path <- "data/nj_covid_hosp.parquet"
opt$gt_NJ_total_hosp_data_path <- "data/NJ_total_hosp.parquet"

incidH_data <- arrow::read_parquet(opt$gt_data_path)
nj_TotalH_data <- arrow::read_parquet(opt$gt_NJ_total_hosp_data_path)

# ~ COVID-19 --------------------------------------------------------------

covid_incidH_data <- incidH_data %>%
  filter(pathogen == "COVID-19") %>%
  filter(!is.na(incidH) & incidH>0)

# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------

# create functions for sampling hospitalization duration 
 # covidhosp_stay_funct <- function(n) {
 #   rpois(n = n, lambda = LOS)
 # }
 # 
 # burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct){
 #   lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH), function(x = X) (0:(x-1)) + date))))
 # }

# ~ Functions for Empirical and Ensemble data --------------------------------------
create_hosp_dates <- function(data){
  data_burden <- list()
  
  for (i in 1:nrow(data)){
    
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates = 
                    burden_est_funct(incidH = data$incidH[i], 
                                     date = data$date[i], 
                                     hospstayfunct = covidhosp_stay_funct
                    )
                  
      )
  }
  return(data_burden)
}

create_curr_hosp <- function(data_burden, LOS){
  new_data_burden <- data_burden %>%
    bind_rows() %>%
    as_tibble() %>%
    dplyr::select(-admit_date, -incidH) %>% # should LOS get dropped? 
    group_by_all() %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()
  return(new_data_burden)
}

# CLEAN DATA FOR MERGE ----------------------------------- 
clean_observed <- function(observed){
  observed <- observed %>% 
    rename(date = hosp_dates,
           total_hosp_forecast = curr_hosp) %>% 
    group_by(pathogen, los, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week
    summarize(total_hosp_forecast = sum(total_hosp_forecast)) %>% 
    select(pathogen, week, los, total_hosp_forecast)
  
  return(observed)
}

clean_expected <- function(expected){
  
  expected <- expected %>% 
    group_by(state, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week using format()
    mutate(week = as.Date(week)) %>% 
    summarize(total_hosp = sum(total_hosp))  
  
  return(expected)
}

optimize_los <- function(LOS, data, observed){
  
  covidhosp_stay_funct <- function(n, LOS) {
    rpois(n = n, lambda = LOS)
  }

  burden_est_funct <- function(incidH, date, LOS, hospstayfunct = covidhosp_stay_funct){
    lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, LOS=LOS), function(x = X) (0:(x-1)) + date))))
  }
  
  expected_list <- create_hosp_dates(data)
  expected <- create_curr_hosp(data_burden = expected_list, LOS = LOS)
  
  observed <- clean_observed(observed)
  expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "week") %>% 
    select(state, week, pathogen, los, total_hosp, total_hosp_forecast) %>% 
    mutate(absolute_difference = abs(total_hosp - total_hosp_forecast)) %>% 
    summarize(sum_absolute_difference = sum(absolute_difference))
    
    return(combined$sum_absolute_difference)
  
}


outcome <- optimize_los(LOS = LOS, data = incidH_data, observed = covid_incidH_data)

los_range <- c(1,15)
# tol (accuracy)  is the default value (approx. 0.0001)
los_min <- optimize(optimize_funct, los_range, lower = min(los_range), upper = max(los_range),
                    maximum = FALSE)



