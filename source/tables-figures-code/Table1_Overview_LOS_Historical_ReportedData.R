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
 # filter(date >= "2023-08-01") %>% 
  mutate(epiweek = epiweek(date))
  
  
# create file with reported incident data for Table 1 
# only run when need to update 
#write_parquet(covid_HHS_data_states_lag, "data/US_wide_data/State_incidH_table1/covid_HHS_data_states_lag.parquet")

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
    year_szn = paste0(adjusted_year, "_", season),
    respiratory_season =
      if_else( month(date) >= 8,         # Aug (8)–Dec (12) → current year is season start
               paste(year(date),  year(date) + 1, sep = "-"),
               paste(year(date) - 1, year(date), sep = "-") )
  ) %>%
  select(-adjusted_year) %>% 
  mutate(
    resp_year_start = if_else(month(date) >= 8,
                              year(date),
                              year(date) - 1),
    respiratory_season = paste(resp_year_start,
                               resp_year_start + 1,
                               sep = "-"),
    ## make it an ordered factor so graphs & tables stay in season order
    respiratory_season = factor(
      respiratory_season,
      levels = paste(2020:2023, 2021:2024, sep = "-"),
      ordered = TRUE)
  ) %>%
  select(-resp_year_start)  

# Get LOS estimates for LOS Generated Each EpiWeek from past 90 days ------
create_incidH_df_by_factor <- function(data, factor_col) {
  states_list <- unique(data$state)
  #forecast_date_list <- unique(forecast_data$forecast_date)
  
  for (abbv in states_list) {
    state_data <- data %>% filter(state == abbv)
    factor_list <- unique(state_data[[factor_col]])

    for (selected_factor in factor_list) {

      # Filter data for the current state and year_szn
      state_factor_filter <- data %>% 
        filter(state == abbv &
                 .data[[factor_col]]  == selected_factor)
      
      # Ensure there's data to process
      if (nrow(state_factor_filter) > 0) {
        # Assign the filtered data to a dynamically created variable name
        assign(
          paste0("covid_incidH_data_", abbv, "_", selected_factor),
          state_factor_filter,
          envir = .GlobalEnv
        )
      }
    }
  }
}
create_totalH_df_by_factor <- function(data, factor_col) {
  states_list <- unique(data$state)
  #forecast_date_list <- unique(forecast_data$forecast_date)
  
  for (abbv in states_list) {
    state_data <- data %>% filter(state == abbv)
    factor_list <- unique(state_data[[factor_col]])
    
    for (selected_factor in factor_list) {
      
      # Filter data for the current state and year_szn
      state_factor_filter <- data %>% 
        filter(state == abbv &
                 .data[[factor_col]]  == selected_factor)
      # Ensure there's data to process
      if (nrow(state_factor_filter) > 0) {
        # Assign the filtered data to a dynamically created variable name
        assign(
          paste0("covid_totalHosp_data_", abbv, "_", selected_factor),
          state_factor_filter,
          envir = .GlobalEnv
        )
      }
    }
  }
}


# pt 2
create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "season") 

create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "season")

create_optimization_timevarying_by_factor(parent_data = covid_HHS_data_states_lagtemp, optimize_los, factor_col = "season") 

write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_Szn.csv")

# pt 3
create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "respiratory_season") 

# Create dataframes for each state with incident hospitalization data
create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "respiratory_season")

create_optimization_timevarying_by_factor(parent_data = covid_HHS_data_states_lagtemp, optimize_los, factor_col = "respiratory_season") 

write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_Waves.csv")

# Create dataframes for each state with incident hospitalization data

create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "year_szn") 

create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "year_szn")

create_optimization_timevarying_by_factor(parent_data = covid_HHS_data_states_lagtemp, optimize_los, factor_col = "year_szn") 

write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn.csv")
