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
  mutate(state = "US")

# Stack datasets
## add rows of covid_HHS_data_USA_lag df to covid_HHS_data_states_lag

#covid_HHS_data_states_lag <- bind_rows(covid_HHS_data_states_lag, covid_HHS_data_USA_lag)

# Create Data for Figures US Only (uncomment above if want all states)--------
# filter date to 23/24 resp virus szn
covid_HHS_data_states_lag <- covid_HHS_data_USA_lag %>% 
  filter(date >= "2023-08-01") %>% 
  mutate(epiweek = epiweek(date))
  
# create file with reported incident data for Table 1 
# only run when need to update 
#write_parquet(covid_HHS_data_states_lag, "data/US_wide_data/State_incidH_table1/covid_HHS_data_states_lag.parquet")

# Create dataframes for each state with Hospital burden data 
create_totalH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-incidH, -incidH_prior_day), state) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, -incidH_prior_day), state)

# Estimate LOS value for each state using optimization --------------

# Define a custom function to calculate season
# get_season <- function(date) {
#   month <- month(date)
#   if (month %in% c(12, 1, 2)) {
#     return("Winter")
#   } else if (month %in% c(3, 4, 5)) {
#     return("Spring")
#   } else if (month %in% c(6, 7, 8)) {
#     return("Summer")
#   } else if (month %in% c(9, 10, 11)) {
#     return("Fall")
#   }
# }

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

forecast_hosp <- read_parquet("data/covidHubUtils_forecastData/forecast_hosp.parquet") %>% 
  select(abbreviation, target_end_date, forecast_date, horizon) 

# Create dataframes for each state with Hospital burden data 
create_totalH_df_unique_date(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day)) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df_unique_date(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day))

create_optimization_timevarying_unique_dates(parent_data = covid_HHS_data_states_lagtemp, optimize_los) 

write_csv(los_opt_by_state_unique_date, "data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_Daily_TimeVaryingLOS_90day.csv")

# Create dataframes for each state with Hospital burden data 
create_totalH_df_epiweek(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day)) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df_epiweek(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day))

create_optimization_timevarying_epiweek(parent_data = covid_HHS_data_states_lagtemp, optimize_los) 

write_csv(los_opt_by_state_forecast_date, "data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_EpiWeek_TimeVaryingLOS_90day.csv")
 
# one LOS value for entire time period 

# Get LOS estimates for LOS Generated Each EpiWeek from past 90 days ------
create_incidH_df <- function(data, state){
  states_list <- unique(data$state)
  
  for (state in states_list) {
    
    state_data <- data[data$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_incidH_data_", state), state_data, envir = .GlobalEnv)
  }
}

create_totalH_df <- function(data, state){
  states_list <- unique(data$state)
  
  for (state in states_list) {
    
    state_data <- data[data$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_totalHosp_data_", state), state_data, envir = .GlobalEnv)
  }
}

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

create_totalH_df(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day)) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day))

create_optimization(parent_data = covid_HHS_data_states_lagtemp, optimize_los) 

write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_EpiWeek_One_LOS.csv")
