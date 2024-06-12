

# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
library(gghighlight)
library(arrow)
library(Hmisc)


### IMPORT INITIAL DATA -----------------------------------

opt <- list()
opt$gt_data_path_HHS_states <- "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"

# read in initial data, lead incident cases by one day 
# define hosp burden and incident hospitalizations

covid_HHS_data_states <- arrow::read_parquet(opt$gt_data_path_HHS_states) %>% 
  mutate(total_hosp = total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid,
         incidH_prior_day = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as_date(date)) %>% 
  arrange(state, date) %>% 
  dplyr::select(state, date, total_hosp, incidH_prior_day)

# add labels to new variables 
attr(covid_HHS_data_states[["total_hosp"]], "label") <- "total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid"
attr(covid_HHS_data_states[["incidH_prior_day"]], "label") <- "previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed"

# Create lag of one day for incidH for each state ------- 

create_incidH_lag <- function(state_data){
  states_list <- unique(state_data$state)
  lagged_dfs <- list()
  
  for (state in states_list) {
    state_data_state <- state_data[state_data$state == state, ]
    state_data_state <- state_data_state %>% 
      mutate(incidH = lead(incidH_prior_day)) %>%  
      filter(date < max(date))  # remove most recent date to account for lag 
    
    
    lagged_dfs[[state]] <- state_data_state #store lags in a list 
  }
  
  merged_df <- do.call(bind_rows, lagged_dfs) #combine into one df 
  
  return(merged_df)
}

# update covid_HHS_data_states so incidH has lagged values
covid_HHS_data_states_lag <- create_incidH_lag(covid_HHS_data_states) 
head(covid_HHS_data_states_lag)
attr(covid_HHS_data_states_lag[["incidH"]], "label") <- "lead(previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed)"
attr(covid_HHS_data_states_lag[["total_hosp"]], "label") <- "total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid"
attr(covid_HHS_data_states_lag[["incidH_prior_day"]], "label") <- "previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed"

# create file for table 1 of incident data 
write_parquet(covid_HHS_data_states_lag, "data/US_wide_data/State_incidH_table1/covid_HHS_data_states_lag.parquet")

# Read in Hospitalization data for each state -----------------------------------

create_totalH_df <- function(data, state){
  states_list <- unique(data$state)
  
  for (state in states_list) {
    
    state_data <- data[data$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_totalHosp_data_", state), state_data, envir = .GlobalEnv)
  }
}

create_totalH_df(data = covid_HHS_data_states_lag, state)


# Create df of incidH data for each state  ------------------------------------

create_incidH_df <- function(data, state){
  states_list <- unique(data$state)
  
  for (state in states_list) {
    
    state_data <- data[data$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_incidH_data_", state), state_data, envir = .GlobalEnv)
  }
}

create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, incidH_prior_day), state)


# ~ COVID-19 --------------------------------------------------------------

covid_HHS_data_states_lag %>%
  ggplot(aes(x = date, y = incidH, color = state)) + 
  geom_line() 

covid_HHS_data_states_lag %>%
  ggplot(aes(x = date, y = total_hosp, color = state)) + 
  geom_line() 

covid_incidH_data_MD %>%
  ggplot(aes(x = date, y = incidH, color = state)) + 
  geom_line() 

covid_totalHosp_data_MD %>%
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
    rename(total_hosp_estimate = curr_hosp,
           date = hosp_dates) %>%
    select(date, total_hosp_estimate) 
  
  return(expected)
}

# CLEAN DATA FOR MERGE (WEEKLY ARCHIVE) ----------------------------------- 

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
  
  expected_list <- create_hosp_dates(data, los = los)
  expected <- create_curr_hosp(data_burden = expected_list)
  
  expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "date") %>% 
    dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
    mutate(absolute_difference = abs(total_hosp - total_hosp_estimate)) %>% 
    filter(!is.na(absolute_difference)) %>% 
    summarize(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 
  
  return(combined$sum_absolute_difference)
  
}

####### check optimize function ------------------------------------
# returns one single outcome for optimize function 
#outcome <- optimize_los(los = 5, data = covid_incidH_data, observed = clean_observed(nj_TotalH_data))

#abs_dif <- optimize_los(LOS = LOS, data = incidH_data, observed = covid_incidH_data)

## los_range <- c(3,7)
# tol (accuracy)  is the default value (approx. 0.0001)
## los_min <- optimize(optimize_los, los_range, data = covid_incidH_data_NJ, observed = covid_totalHosp_data_NJ, 
#maximum = FALSE)

#outcome <- optimize_los(los = 6.8, data = covid_incidH_data, observed = clean_observed(nj_TotalH_data))


# Loop to get optimized value for each state ------------------------------------

# create optimization for each state 

create_optimization <- function(parent_data, optimize_los){
  states_list <- unique(parent_data$state)
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
create_optimization(parent_data = covid_HHS_data_states, optimize_los)


## Create final datasets for estimated burden with optimized LOS -----------------------------------
# joined by estimate total Hosp and census total Hosp 
# one df for all states 

create_optimize_totalHosp_data <- function(parent_data, los_opt_by_state = los_opt_by_state){
  states_list <- unique(parent_data$state)
  combined_list <- list()
  
  for (state in states_list) {
    data = get(paste0("covid_incidH_data_", state))
    observed = get(paste0("covid_totalHosp_data_", state))
    
    expected_list <- create_hosp_dates(data, los = los_opt_by_state[los_opt_by_state$state == state, "optimized_los"])
    expected <- create_curr_hosp(data_burden = expected_list)
    
    #observed <- clean_observed(observed)
    expected <- clean_expected(expected)
    
    combined <- inner_join(observed, expected, by = "date") %>% 
      dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
      mutate(absolute_difference = abs(total_hosp - total_hosp_estimate),
             difference = total_hosp - total_hosp_estimate,
             relative_difference = total_hosp_estimate/total_hosp)
    
    combined_list[[state]] <- combined
  }
  
  combined_df <- do.call(rbind, combined_list)
  
  return(combined_df)
  
}

# Write Final files ----------------
covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(parent_data = covid_HHS_data_states_lag, los_opt_by_state = los_opt_by_state)

covid_HHS_data <- arrow::read_parquet(opt$gt_data_path_HHS_states) %>% 
  select(state, date, inpatient_beds, `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
         `previous_day_admission_adult_covid_confirmed_30-39`, `previous_day_admission_adult_covid_confirmed_40-49`, `previous_day_admission_adult_covid_confirmed_50-59`,               
         `previous_day_admission_adult_covid_confirmed_60-69`, `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, `previous_day_admission_adult_covid_confirmed_unknown`,
         `previous_day_admission_pediatric_covid_confirmed_0_4`, `previous_day_admission_pediatric_covid_confirmed_12_17`, `previous_day_admission_pediatric_covid_confirmed_5_11`,
         `previous_day_admission_pediatric_covid_confirmed_unknown`, `previous_day_admission_pediatric_covid_confirmed`, `previous_day_admission_adult_covid_confirmed`)


covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state, by = "state")

covid_joined_totalHosp_state_data_los_demographic <- left_join(covid_joined_totalHosp_state_data_los, covid_HHS_data, c("state", "date"))
write_parquet(covid_joined_totalHosp_state_data_los_demographic, "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_04142024.parquet")
