

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
opt$gt_NJ_total_hosp_data_path <- "data/NJ_currently_hospitalized_covid19_patients.parquet" #updated file name to new parquet folder

# only need to run this if want to update data
opt$gt_data_path <- "data/nj_covid_hosp.parquet"
opt$gt_data_path_states <- "data/pull_empirical_incidH_state_data.parquet"

incidH_data <- arrow::read_parquet(opt$gt_data_path)
incidH_data_states <- arrow::read_parquet(opt$gt_data_path)
nj_TotalH_data <- arrow::read_parquet(opt$gt_NJ_total_hosp_data_path)

# ~ COVID-19 --------------------------------------------------------------

covid_incidH_data <- incidH_data %>%
  filter(pathogen == "COVID-19") %>%
  filter(!is.na(incidH) & incidH>0) # is there a reason we don't want to include 0's (just one day) 

# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------

# create functions for sampling hospitalization duration 
covidhosp_stay_funct <- function(n) {
   rpois(n = n, lambda = 5)
 }

burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct){
   lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH), function(x = X) (0:(x-1)) + date))))
 }

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

create_curr_hosp <- function(data_burden){
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
    group_by(state, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week using format()
    mutate(week = as.Date(week)) %>% 
    summarize(total_hosp = sum(total_hosp)) %>% 
  
  return(observed)

}


clean_expected <- function(expected){
  expected <- expected %>% 
    rename(date = hosp_dates,
           total_hosp_forecast = curr_hosp) %>% 
    group_by(pathogen, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # rm los from group_by
    mutate(week = as.Date(week)) %>% 
    select(pathogen, week, total_hosp_forecast) # may need to keep state here when adding in more states 
  
  return(expected)
}

## checking everything runs outside of function -------

# expected_list <- create_hosp_dates(data = covid_incidH_data)
# expected_hosp <- create_curr_hosp(data_burden = expected_list)
# 
# observed <- clean_observed(observed = nj_TotalH_data)
# expected <- clean_expected(expected = expected_hosp)
# 
# combined <- inner_join(observed, expected, by = "week") %>% 
#   select(state, week, pathogen, total_hosp, total_hosp_forecast) %>% 
#   mutate(absolute_difference = abs(total_hosp - total_hosp_forecast)) %>% 
#   filter(!is.na(absolute_difference)) %>% 
#   summarize(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 

optimize_los <- function(data, observed){
  
  # covidhosp_stay_funct <- function(n, LOS) {
  #   rpois(n = n, lambda = LOS)
  # }
  # 
  # burden_est_funct <- function(incidH, date, LOS, hospstayfunct = covidhosp_stay_funct){
  #   lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, LOS=LOS), function(x = X) (0:(x-1)) + date))))
  # }
  # 
  expected_list <- create_hosp_dates(data)
  expected <- create_curr_hosp(data_burden = expected_list)
  
  observed <- clean_observed(observed)
  expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "week") %>% 
    select(state, week, pathogen, total_hosp, total_hosp_forecast) %>% 
    mutate(absolute_difference = abs(total_hosp - total_hosp_forecast)) %>% 
    filter(!is.na(absolute_difference)) %>% 
    summarize(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 
    
    return(combined$sum_absolute_difference)
  
}

# check optimize function returns one single outcome for optimize function 
outcome <- optimize_los(data = covid_incidH_data, observed = nj_TotalH_data)

#abs_dif <- optimize_los(LOS = LOS, data = incidH_data, observed = covid_incidH_data)

los_range <- c(1,15)
# tol (accuracy)  is the default value (approx. 0.0001)
los_min <- optimize(optimize_los, los_range, data = covid_incidH_data, observed = nj_TotalH_data, 
                    lower = min(los_range), upper = max(los_range), maximum = FALSE)



