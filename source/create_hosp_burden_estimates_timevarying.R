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

# source data functions
source("source/data_setup_source.R")

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


# Check incidH and totalHosp master file and USA dataframes --------------------------------------------------------------

covid_HHS_data_states_lag %>%
  ggplot(aes(x = date, y = incidH, color = state)) + 
  geom_line() 

covid_HHS_data_states_lag %>%
  ggplot(aes(x = date, y = total_hosp, color = state)) + 
  geom_line() 

covid_incidH_data_USA %>%
  ggplot(aes(x = date, y = incidH, color = state)) + 
  geom_line() 

covid_totalHosp_data_USA %>%
  ggplot(aes(x = date, y = total_hosp, color = state)) + 
  geom_line() 


# Estimate LOS value for each state using optimization --------------

create_optimization_timevarying <- function(parent_data, optimize_los){
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

# Define a custom function to calculate season
get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  }
}

# Add year and season columns
covid_HHS_data_states_lagtemp <- covid_HHS_data_states_lag %>%
  mutate(
    year = year(date),
    season = sapply(date, get_season),
    # Adjust the year for Winter to span two calendar years
    adjusted_year = ifelse(season == "Winter" & month(date) == 12, year + 1, year),
    year_szn = paste0(adjusted_year, "_", season)
  ) %>%
  select(-adjusted_year)

create_incidH_df_year_szn <- function(data) {
  states_list <- unique(data$state)
  year_szn_list <- unique(data$year_szn)
  
  for (state in states_list) {
    for (year_szn in year_szn_list) {
      
      # Filter data for the current state and year_szn
      state_year_szn_data <- data[data$state == state & 
                                    data$year_szn == year_szn, ]
      
      # Ensure there's data to process
      if (nrow(state_year_szn_data) > 0) {
        # Assign the filtered data to a dynamically created variable name
        assign(
          paste0("covid_incidH_data_", state, "_", year_szn),
          state_year_szn_data,
          envir = .GlobalEnv
        )
      }
    }
  }
}

create_totalH_df_year_szn <- function(data) {
  states_list <- unique(data$state)
  year_szn_list <- unique(data$year_szn)
  
  for (state in states_list) {
    for (year_szn in year_szn_list) {
      
      # Filter data for the current state and year_szn
      state_year_szn_data <- data[data$state == state & 
                                    data$year_szn == year_szn, ]
      
      # Ensure there's data to process
      if (nrow(state_year_szn_data) > 0) {
        # Assign the filtered data to a dynamically created variable name
        assign(
          paste0("covid_totalHosp_data_", state, "_", year_szn),
          state_year_szn_data,
          envir = .GlobalEnv
        )
      }
    }
  }
}

# Create dataframes for each state with Hospital burden data 
create_totalH_df_year_szn(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day)) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df_year_szn(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day))


create_optimization_timevarying <- function(parent_data, optimize_los) {
  states_list <- unique(parent_data$state)


  los_opt_by_state_season <- list()
  
  for (state in states_list) {
    print(state) # for tracking progress
    state_data <- parent_data %>% filter(state == state)
    year_szn_list <- unique(state_data$year_szn)
    
    for (year_szn in year_szn_list) {
        # Filter data for the current state, year, and season
        filtered_data <- state_data %>% 
          filter(year_szn == year_szn)
        
        # Ensure there's data to process
        if (nrow(filtered_data) > 0) {
          data = get(paste0("covid_incidH_data_", state, "_", year_szn))
          observed = get(paste0("covid_totalHosp_data_", state, "_", year_szn))
          
          los_range <- c(3, 15)
          # tol (accuracy) is the default value (approx. 0.0001)
          los_min <- optimize(optimize_los, los_range, 
                              data = data, 
                              observed = observed, 
                              maximum = FALSE)
          
          state_season_df <- data.frame(
            state = state, 
            year_szn = year_szn,
            optimized_los = los_min$minimum, 
            objective = los_min$objective
          )
          los_opt_by_state_season[[paste(state, year_szn, sep = "_")]] <- state_season_df
        }
      
    }
  }
  
  los_opt_by_state_season <- do.call(rbind, los_opt_by_state_season)
  
  assign("los_opt_by_state_season", los_opt_by_state_season, envir = .GlobalEnv)
  
  print("Optimization completed!")
}

create_optimization_timevarying <- function(parent_data, optimize_los) {
  states_list <- unique(parent_data$state)
  los_opt_by_state_season <- list()
  
  for (state in states_list) {
    print(state) # for tracking progress
    
    # Filter data for the current state
    state_data <- parent_data %>% filter(state == state)
    year_szn_list <- unique(state_data$year_szn)
    
    for (year_szn in year_szn_list) {
      # Filter data for the current state and year_szn
      filtered_data <- state_data %>% filter(year_szn == year_szn)
      
      # Ensure there's data to process
      if (nrow(filtered_data) > 0) {
        # Construct dynamic variable names
        dynamic_incidH_name <- paste0("covid_incidH_data_", state, "_", year_szn)
        dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state, "_", year_szn)
        
        # Check if the dynamic objects exist before proceeding
        if (exists(dynamic_incidH_name, envir = .GlobalEnv) && exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
          data <- get(dynamic_incidH_name)
          observed <- get(dynamic_totalHosp_name)
          
          # Perform optimization
          los_range <- c(3, 15)
          los_min <- optimize(optimize_los, los_range, 
                              data = data, 
                              observed = observed, 
                              maximum = FALSE)
          
          # Create a results dataframe
          state_season_df <- data.frame(
            state = state, 
            year_szn = year_szn,
            optimized_los = los_min$minimum, 
            objective = los_min$objective
          )
          los_opt_by_state_season[[paste(state, year_szn, sep = "_")]] <- state_season_df
        } else {
          print(paste("Dynamic objects not found for:", dynamic_incidH_name, "or", dynamic_totalHosp_name))
        }
      }
    }
  }
  
  # Combine list of dataframes into a single dataframe
  los_opt_by_state_season <- do.call(rbind, los_opt_by_state_season)
  
  # Save the result to the global environment
  assign("los_opt_by_state_season", los_opt_by_state_season, envir = .GlobalEnv)
  
  print("Optimization completed!")
}


covid_HHS_data_states_lagtemp_sample <- covid_HHS_data_states_lagtemp %>% 
  filter(state %in% c("AS"))

create_optimization_timevarying(parent_data = covid_HHS_data_states_lagtemp, optimize_los) 

# this take a long time to run, prints states in alphabetical in console to check progress 
create_optimization(parent_data = covid_HHS_data_states_lag, optimize_los) # note: parent data just for getting list of all states
#los_opt_by_state <- arrow::read_parquet("data/US_wide_data/LOS_Optimized_by_AllStates_USA.parquet") # if don't want to run, load file directly 

# update only when want to overwrite file 
#write_parquet(los_opt_by_state, "data/US_wide_data/LOS_Optimized_by_AllStates_USA.parquet")
write_csv(los_opt_by_state_season, "data/US_wide_data/LOS_Optimized_by_AllStates_USA_time_varying.csv")

# Create hospitalization burden estimates using LOS values from optimization ---------
los_opt_by_state_season <- read_csv("data/US_wide_data/LOS_Optimized_by_AllStates_USA_time_varying.csv")

# runs faster than above function (fewer outputs) 
create_optimize_totalHosp_data_timevarying <- function(parent_data, los_opt_by_state_season) {
  states_list <- unique(parent_data$state)
  combined_list <- list()
  
  for (state_abbv in states_list) {
    print(state_abbv) # Progress tracking
    #state_data <- covid_HHS_data_states_lagtemp %>% filter(state == "MD")
    state_data <- parent_data %>% filter(state == state_abbv)
    year_szn_list <- unique(state_data$year_szn)
    
    # state_abbv <- "MD"
    # year_szn_item <- year_szn_list[2]
    for (year_szn_item in year_szn_list) {
      print(paste("Processing:", state_abbv, year_szn_item)) # Progress tracking
      
      # Dynamic variable names for the state and season
      dynamic_incidH_name <- paste0("covid_incidH_data_", state_abbv, "_", year_szn_item)
      dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv, "_", year_szn_item)
      
      # Check if the required data exists
      if (exists(dynamic_incidH_name, envir = .GlobalEnv) && exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
        data <- get(dynamic_incidH_name)
        observed <- get(dynamic_totalHosp_name)
        
        # Retrieve optimized LOS for the specific state and season
        los_value <- los_opt_by_state_season %>% 
          filter(state == state_abbv & year_szn == year_szn_item) %>%
          pull(optimized_los)
        
        if (length(los_value) == 1) { # Ensure a valid LOS value is available
          # Generate expected hospitalization data
          expected_list <- create_hosp_dates(data, los = los_value)
          expected <- create_curr_hosp(data_burden = expected_list)
          expected <- clean_expected(expected)
          
          # Combine observed and expected data
          combined <- inner_join(observed, expected, by = "date") %>% 
            dplyr::select(state, date, year_szn, total_hosp, total_hosp_estimate) %>% 
            mutate(
              absolute_difference = abs(total_hosp - total_hosp_estimate),
              difference = total_hosp - total_hosp_estimate,
              relative_difference = total_hosp_estimate / total_hosp
            )
          
          combined_list[[paste(state_abbv, year_szn_item, sep = "_")]] <- combined
        } else {
          print(paste("No LOS value for:", state_abbv, year_szn_item))
        }
      } else {
        print(paste("Data not found for:", dynamic_incidH_name, "or", dynamic_totalHosp_name))
      }
    }
  }
  
  # Combine the results into a single dataframe
  combined_df <- do.call(rbind, combined_list)
  return(combined_df)
}

optimized_data <- create_optimize_totalHosp_data_timevarying(
  parent_data = covid_HHS_data_states_lagtemp, 
  los_opt_by_state_season = los_opt_by_state_season
)

covid_joined_totalHosp_state_data <- optimized_data

#covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(parent_data = covid_HHS_data_states_lag, los_opt_by_state = los_opt_by_state)

# Write Final files for analysis ----------------
covid_HHS_data <- arrow::read_parquet(opt$gt_data_path) %>% 
  select(state, date, inpatient_beds, `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
         `previous_day_admission_adult_covid_confirmed_30-39`, `previous_day_admission_adult_covid_confirmed_40-49`, `previous_day_admission_adult_covid_confirmed_50-59`,               
         `previous_day_admission_adult_covid_confirmed_60-69`, `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, `previous_day_admission_adult_covid_confirmed_unknown`,
         `previous_day_admission_pediatric_covid_confirmed_0_4`, `previous_day_admission_pediatric_covid_confirmed_12_17`, `previous_day_admission_pediatric_covid_confirmed_5_11`,
         `previous_day_admission_pediatric_covid_confirmed_unknown`, `previous_day_admission_pediatric_covid_confirmed`, `previous_day_admission_adult_covid_confirmed`)

covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state, by = "state")

covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state_season, by = c("state", "year_szn"))

covid_joined_totalHosp_state_data_los_demographic <- left_join(covid_joined_totalHosp_state_data_los, covid_HHS_data, c("state", "date"))
write_parquet(covid_joined_totalHosp_state_data_los_demographic, "data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_TIMEVARYING.parquet")

