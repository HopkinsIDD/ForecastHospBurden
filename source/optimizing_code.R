# setup ------------------------
library("microbenchmark")
library("ggplot2movies")
library("profvis")
library("Rcpp")

# source
# https://csgillespie.github.io/efficientR/performance.html

# Code profiling --------------------------
# Rprof() is not user friendly. 
# For this reason we recommend using the profvis package for profiling your R code. 
# profvis provides an interactive graphical interface for visualising code profiling data data from Rprof().

profvis({
  library(dplyr)
  library(tidyr)
  library(tidycensus)
  library(tidyverse)
  library(readr)
  library(lubridate)
  library(gghighlight)
  library(arrow)
  library(Hmisc)
  library("parallel")
  
  create_incidH_lag <- function(state_data){
    #states_list <- unique(state_data$state)
    states_list <- c("MD", "MA")
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
  
  # Create dataframes for each state with Hospital burden data -----------------------------------
  # output: dfs for each unique state w/ hospital burden (totalHosp) data (to be used during optimization)
  
  create_totalH_df <- function(data, state){
    states_list <- unique(data$state)
    
    for (state in states_list) {
      
      state_data <- data[data$state == state, ]
      
      # put new df in global env 
      assign(paste0("covid_totalHosp_data_", state), state_data, envir = .GlobalEnv)
    }
  }
  
  # Create dataframes for each state with incident hospitalizations  ------------------------------------
  # output: dfs for each unique state w/ incident admissions (incidH) data (to be used during optimization)
  create_incidH_df <- function(data, state){
    states_list <- unique(data$state)
    
    for (state in states_list) {
      
      state_data <- data[data$state == state, ]
      
      # put new df in global env 
      assign(paste0("covid_incidH_data_", state), state_data, envir = .GlobalEnv)
    }
  }
  
  # BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------
  
  # create functions for sampling hospitalization duration 
  covidhosp_stay_funct <- function(n, los = 5) {
    rpois(n = n, lambda = los) 
  }
  
  
  burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct, los = 5){
    cl = makeCluster(4)
    lubridate::as_date(sort(unlist(parSapply(cl, X = hospstayfunct(n = incidH, los = los), function(x = X) (0:(x-1)) + date)), method = "radix"))
    on.exit(stopCluster(cl))
    }
  #method = "radix" speeds up sort a little
  #parSapply
  
  
  
  # ~ Functions for Empirical data --------------------------------------
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
      dplyr::select(-admit_date, -incidH) %>% 
      group_by_all() %>%
      summarise(curr_hosp = length(hosp_dates)) %>%
      ungroup()
    return(new_data_burden)
  }
  
  # CLEAN DATA FOR MERGE  ----------------------------------- 
  # notes: rename estimated hospitalizations columns, this could be removed by renaming above vars?? 
  clean_expected <- function(expected){
    expected <- expected %>%
      rename(total_hosp_estimate = curr_hosp,
             date = hosp_dates) %>%
      select(date, total_hosp_estimate) 
    
    return(expected)
  }
  
  # Create function that returns LOS value to be optimized on ----------------------------
  # notes: output returns absolute difference between observed (totalHosp) and expected (total_hosp_estimate) estimates of hosp burden
  # fed into create_optimization function below 
  
  optimize_los <- function(los, data, observed){
    
    expected_list <- create_hosp_dates(data, los = los)
    expected <- create_curr_hosp(data_burden = expected_list)
    
    expected <- clean_expected(expected)
    
    combined <- inner_join(observed, expected, by = "date") %>% 
      dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
      mutate(absolute_difference = abs(total_hosp - total_hosp_estimate)) %>% 
      filter(!is.na(absolute_difference)) %>% 
      summarise(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 
    
    return(combined$sum_absolute_difference)
    
  }
  
  # Estimate LOS value for each state using optimization ------------------------------------
  # notes: optimization function, searches for lowest absolute difference across range of 3-15 avg LOS  
  # output df with state, estimated LOS, and error 
  
  create_optimization <- function(parent_data, optimize_los){
    states_list <- unique(parent_data$state)
    los_opt_by_state <- list()
    
    for (state in states_list) {
      print(state) #for tracking progress
      
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
  
  
  opt <- list()
  opt$gt_data_path <- "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
  
  # read in data, define total_hosp, incidH_prior_day
  covid_HHS_data_states <- arrow::read_parquet(opt$gt_data_path) %>% 
    mutate(total_hosp = total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid,
           incidH_prior_day = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
           date = as_date(date)) %>% 
    arrange(state, date) %>% 
    dplyr::select(state, date, total_hosp, incidH_prior_day)
  
  # add labels to new variables 
  attr(covid_HHS_data_states[["total_hosp"]], "label") <- "total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid"
  attr(covid_HHS_data_states[["incidH_prior_day"]], "label") <- "previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed"
  
  # SETUP FILES FOR EACH STATE TO ESTIMATE HOSPITAL BURDEN -----------------------------------
  
  # Lag incident hospitalizations 
  ## incidH_prior_day values are moved back one day and add to new column incidH
  covid_HHS_data_states_lag <- create_incidH_lag(covid_HHS_data_states) 
  
  # add labels 
  attr(covid_HHS_data_states_lag[["incidH"]], "label") <- "lead(previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed)"
  attr(covid_HHS_data_states_lag[["total_hosp"]], "label") <- "total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid"
  attr(covid_HHS_data_states_lag[["incidH_prior_day"]], "label") <- "previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed"
  
  # create dataset for US across time
  ## add all the incident/totalHosp on a given day across all states and territories
  covid_HHS_data_USA_lag <- covid_HHS_data_states_lag %>% 
    group_by(date) %>% 
    summarise(total_hosp = sum(total_hosp),
              incidH_prior_day = sum(incidH_prior_day),
              incidH = sum(incidH)) %>% 
    mutate(state = "USA")
  
  # Stack datasets
  ## add rows of covid_HHS_data_USA_lag df to covid_HHS_data_states_lag
  
  covid_HHS_data_states_lag <- bind_rows(covid_HHS_data_states_lag, covid_HHS_data_USA_lag)
  
  # create file with reported incident data for Table 1 
  # only run when need to update 
  #write_parquet(covid_HHS_data_states_lag, "data/US_wide_data/State_incidH_table1/covid_HHS_data_states_lag.parquet")
  
  # Create dataframes for each state with Hospital burden data 
  create_totalH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-incidH, -incidH_prior_day), state) 
  
  # Create dataframes for each state with incident hospitalization data
  create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, -incidH_prior_day), state)
  
  
  create_optimization(parent_data = covid_HHS_data_states_lag, optimize_los) # note: parent data just for getting list of all states
  
})

# parallel processing -----------------------
library(parallel)
no_of_cores = detectCores()
