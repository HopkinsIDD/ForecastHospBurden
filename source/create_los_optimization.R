

# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
#library(flepicommon)
library(gghighlight)
library(arrow)


### IMPORT INITIAL DATA -----------------------------------

# source data functions
source("source/data_setup_source.R")

opt <- list()
opt$gt_data_source <- "hhs_hosp"
opt$delphi_api_key <- "04e7369e1541a"
#opt$gt_NJ_total_hosp_data_path <- "data/currently_hosp_covid19_by_state_parquet/NJ_currently_hospitalized_covid19_patients.parquet" #updated file name to new parquet folder

# Incidence Data  -----------------------------------
# only need to run this if want to update data
#opt$gt_data_path <- "data/nj_covid_hosp.parquet" #running for multiple states going fwd 
opt$gt_data_path_incidH_states <- "data/pull_empirical_incidH_state_data.parquet"
opt$gt_data_path_HHS_states <- "data/currently_hosp_covid_data_daily/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_Subset.parquet"

#incidH_data <- arrow::read_parquet(opt$gt_data_path) #running for multiple states going fwd 
covid_incidH_data_states <- arrow::read_parquet(opt$gt_data_path_incidH_states) %>%   
  filter(pathogen == "COVID-19") %>%
  filter(!is.na(incidH)) %>% 
  #filter(!is.na(incidH) & incidH>0) %>% # is there a reason we don't want to include 0's (just one day) 
  rename(state = source) %>% 
  dplyr::select(pathogen, state, date, incidH)
  


covid_HHS_data_states <- arrow::read_parquet(opt$gt_data_path_HHS_states) %>% 
  mutate(total_hosp = inpatient_beds_used_covid,
         incidH_confirmed_suspected = previous_day_admission_adult_covid_confirmed + previous_day_admission_adult_covid_suspected +
           previous_day_admission_pediatric_covid_confirmed + previous_day_admission_pediatric_covid_suspected,
         incidH_confirmed = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed) %>% 
  arrange(state, date) %>% 
  mutate(incidH_confirmed_suspected_LAG = lag(incidH_confirmed_suspected),
         incidH_confirmed_LAG = lag(incidH_confirmed)) %>% 
  dplyr::select(state, date, total_hosp, incidH_confirmed_suspected, incidH_confirmed,
                incidH_confirmed_suspected_LAG, incidH_confirmed_LAG)

# LAG NOT WORKING 
# create loop to lag by one day for each state and then put back together as one dataset 
#nj_TotalH_data <- arrow::read_parquet(opt$gt_NJ_total_hosp_data_path) 

####### COMPARE INCIDH DATA SOURCES -----------

covid_incidH_data_states_AUG <- covid_incidH_data_states %>%
  filter(between(date, as.Date('2020-08-01'), Sys.Date())) %>% 
  rename(incidh_COVIDCast = incidH)

incidH_compare <- inner_join(covid_HHS_data_states, covid_incidH_data_states_AUG, by = c("date" = "date", "state" = "state"))

# Read in Hospitalization data for each state -----------------------------------

create_totalH_df <- function(state){
  states_list <- unique(covid_HHS_data_states$state)
  
  for (state in states_list) {
    
    state_data <- covid_HHS_data_states[covid_HHS_data_states$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_totalHosp_data_", state), state_data, envir = .GlobalEnv)
  }
}

create_totalH_df(state)

# archive weekly data import 
# directory <- "data/currently_hosp_covid19_by_state_parquet/"
# states <- c("NJ", "NY", "PA", "MD")  # Add more states when needed 
# 
# read_totalHosp <- function(states, directory){
#   for (state in states) {
# 
#     file_path <- paste0(directory, state, "_currently_hospitalized_covid19_patients.parquet")
#     
#     state_data <- arrow::read_parquet(file_path)
#     
#     # put new df in global env 
#     assign(paste0("covid_totalHosp_data_", state), state_data, envir = .GlobalEnv)
#   }
#   
# }
# 
# read_totalHosp(states, directory)

# Create df of incidH data for each state  ------------------------------------

create_incidH_df <- function(state){
  states_list <- unique(covid_incidH_data_states$state)
  
  for (state in states_list) {
    
    state_data <- covid_incidH_data_states[covid_incidH_data_states$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_incidH_data_", state), state_data, envir = .GlobalEnv)
  }
}

create_incidH_df(state)


# ~ COVID-19 --------------------------------------------------------------

# covid_incidH_data <- incidH_data %>%
#   filter(pathogen == "COVID-19") %>%
#   filter(!is.na(incidH) & incidH>0) # is there a reason we don't want to include 0's (just one day) 
# 
# covid_incidH_data_states <- incidH_data_states %>%
#   filter(pathogen == "COVID-19") %>%
#   filter(!is.na(incidH) & incidH>0) %>% # is there a reason we don't want to include 0's (just one day) 
#   rename(state = source)

covid_incidH_data_states %>%
  ggplot(aes(x = date, y = incidH, color = state)) + 
  geom_line() 

covid_HHS_data_states %>%
  ggplot(aes(x = date, y = total_hosp, color = state)) + 
  geom_line() 

# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------

# create functions for sampling hospitalization duration 
covidhosp_stay_funct <- function(n, los = 5) {
   rpois(n = n, lambda = los) 
 }

burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct, los = 5){
    lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, los = los), function(x = X) (0:(x-1)) + date))))
}


# ~ Functions for Empirical and Ensemble data --------------------------------------
create_hosp_dates <- function(data, los = 5){
  data_burden <- list()
  
  for (i in 1:nrow(data)){
    
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates = 
                    burden_est_funct(incidH = data$incidH[i], 
                                     date = data$date[i], 
                                     hospstayfunct = covidhosp_stay_funct,
                                     los = los
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

# CLEAN DATA FOR MERGE (WEEKLY ARCHIVE) ----------------------------------- 

clean_expected <- function(expected){
  expected <- expected %>%
    rename(date = hosp_dates,
           total_hosp_estimate = curr_hosp) %>%
    select(pathogen, date, total_hosp_estimate) 

  return(expected)
}

# CLEAN DATA FOR MERGE (WEEKLY ARCHIVE) ----------------------------------- 
# clean_observed <- function(observed){
#   observed <- observed %>% 
#     group_by(state, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week using format()
#     mutate(week = as.Date(week)) %>% 
#     summarize(total_hosp = sum(total_hosp)) %>% 
#   
#   return(observed)
# 
# }

# 
# clean_expected <- function(expected){
#   expected <- expected %>% 
#     rename(date = hosp_dates,
#            total_hosp_estimate = curr_hosp) %>% 
#     group_by(pathogen, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # rm los from group_by
#     mutate(week = as.Date(week)) %>% 
#     select(pathogen, week, total_hosp_estimate) # may need to keep state here when adding in more states 
#   
#   return(expected)
# }

## checking everything runs outside of function ------- --------------

# expected_list <- create_hosp_dates(data = covid_incidH_data)
# expected_hosp <- create_curr_hosp(data_burden = expected_list)
# 
# observed <- clean_observed(observed = nj_TotalH_data)
# expected <- clean_expected(expected = expected_hosp)
# 
# combined <- inner_join(observed, expected, by = "week") %>% 
#   select(state, week, pathogen, total_hosp, total_hosp_estimate) %>% 
#   mutate(absolute_difference = abs(total_hosp - total_hosp_estimate)) %>% 
#   filter(!is.na(absolute_difference)) %>% 
#   summarize(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 

# Create function to be read into optimization ----------------------------

optimize_los <- function(los, data, observed){
  
  # covidhosp_stay_funct <- function(n, LOS) {
  #   rpois(n = n, lambda = LOS)
  # }
  # 
  # burden_est_funct <- function(incidH, date, LOS, hospstayfunct = covidhosp_stay_funct){
  #   lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, LOS=LOS), function(x = X) (0:(x-1)) + date))))
  # }
  # 
  expected_list <- create_hosp_dates(data, los = los)
  expected <- create_curr_hosp(data_burden = expected_list)
  
  # observed <- clean_observed(observed)
  expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "date") %>% 
    dplyr::select(state, date, pathogen, total_hosp, total_hosp_estimate) %>% 
    mutate(absolute_difference = abs(total_hosp - total_hosp_estimate)) %>% 
    filter(!is.na(absolute_difference)) %>% 
    summarize(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 
    
    return(combined$sum_absolute_difference)
  
}

####### check optimize function ------------------------------------
# returns one single outcome for optimize function 
#outcome <- optimize_los(los = 5, data = covid_incidH_data, observed = clean_observed(nj_TotalH_data))

#abs_dif <- optimize_los(LOS = LOS, data = incidH_data, observed = covid_incidH_data)

los_range <- c(3,7)
# tol (accuracy)  is the default value (approx. 0.0001)
los_min <- optimize(optimize_los, los_range, data = covid_incidH_data_NJ, observed = covid_totalHosp_data_NJ, 
                    maximum = FALSE)

#outcome <- optimize_los(los = 6.8, data = covid_incidH_data, observed = clean_observed(nj_TotalH_data))


# Loop to get optimized value for each state ------------------------------------

# create optimization for each state 
for (state in states_list) {
  
  state_data <- get(paste0("covid_incidH_data_", state))
  
  # Run the optimization
  los_range <- c(3,7)
  # tol (accuracy)  is the default value (approx. 0.0001)
  los_min <- optimize(optimize_los, los_range, data = state_data, observed = total_hosp_data, 
                      maximum = FALSE)
  
  print(los_min)
}

# Create optimization for each state Loop to get create total hosp df with optimized LOS  for each state ------------------------------------

# create optimization for each state 

create_optimization <- function(optimize_los){
  states_list <- unique(covid_incidH_data_states$state)
  los_opt_by_state <- list()
  
  for (state in states_list) {
    data = get(paste0("covid_incidH_data_", state))
    observed = get(paste0("covid_totalHosp_data_", state))
    
    
    los_range <- c(3,15)
    # tol (accuracy)  is the default value (approx. 0.0001)
    los_min <- optimize(optimize_los, los_range, data = get(paste0("covid_incidH_data_", state)), 
                        observed = get(paste0("covid_totalHosp_data_", state)), 
                        maximum = FALSE)

    state_df <- data.frame(state = state, 
                           optimized_los = los_min$minimum, 
                           objective = los_min$objective)
    los_opt_by_state[[state]] <- state_df
  }
  
  los_opt_by_state <- do.call(rbind, los_opt_by_state)
  
  assign("los_opt_by_state", los_opt_by_state, envir = .GlobalEnv)
  
}
create_optimization(optimize_los)


## Create final datasets for estimated burden with optimized LOS -----------------------------------
# joined by estimate total Hosp and census total Hosp 
# one df for all states 

create_optimize_totalHosp_data <- function(los_opt_by_state = los_opt_by_state){
  states_list <- unique(covid_incidH_data_states$state)
  combined_list <- list()
  
  for (state in states_list) {
    data = get(paste0("covid_incidH_data_", state))
    observed = get(paste0("covid_totalHosp_data_", state))
    
    expected_list <- create_hosp_dates(data, los = los_opt_by_state[los_opt_by_state$state == state, "optimized_los"])
    expected <- create_curr_hosp(data_burden = expected_list)
    
    #observed <- clean_observed(observed)
    expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "date") %>% 
    dplyr::select(state, date, pathogen, total_hosp, total_hosp_estimate) %>% 
    mutate(absolute_difference = abs(total_hosp - total_hosp_estimate),
           difference = total_hosp - total_hosp_estimate)
  
  combined_list[[state]] <- combined
  }
  
  combined_df <- do.call(rbind, combined_list)
  
  return(combined_df)
  
}

covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(los_opt_by_state = los_opt_by_state)

write_parquet(covid_joined_totalHosp_state_data, "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily.parquet")
