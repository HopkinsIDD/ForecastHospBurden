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

forecast_hosp <- read_parquet("data/covidHubUtils_forecastData/forecast_hosp.parquet")

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

# Create dataframes for each state with Hospital burden data 
create_totalH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-incidH, -incidH_prior_day), state) 

# Create dataframes for each state with incident hospitalization data
# create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, -incidH_prior_day), state)

# Read in optimized LOS values for each state and season -----------------------------------

los_opt_by_state_season <- read_csv("data/US_wide_data/LOS_Optimized_by_AllStates_USA_time_varying.csv")

# create prior year season for each state, so forecast values uses LOS estimates from the prior season 

los_opt_by_state_season_prior <- los_opt_by_state_season %>% 
  mutate(
    year_szn = paste0(as.numeric(substr(year_szn, 1, 4)) + 1, 
                      "_", 
                      sub(".*_", "", year_szn))
  ) %>%
  rename(optimized_los_prior = optimized_los) 



# Create hospitalization burden estimates using LOS values from optimization ---------

forecast_hosp <- read_parquet("data/covidHubUtils_forecastData/forecast_hosp.parquet") %>% 
  mutate(horizon = as.numeric(horizon))
# runs faster than above function (fewer outputs) 

forecast_hosp <- forecast_hosp %>% 
  mutate(quantile = ifelse(is.na(quantile), 0.0, quantile),
         year = year(target_end_date),
         season = sapply(target_end_date, get_season),
         adjusted_year = ifelse(season == "Winter" & month(target_end_date) == 12, year + 1, year),
         year_szn = paste0(adjusted_year, "_", season)
  ) %>%
  select(-adjusted_year) %>% 
  left_join(los_opt_by_state_season_prior, by = c("abbreviation" = "state", "year_szn" = "year_szn")) %>% 
  left_join(los_opt_by_state_season, by = c("abbreviation" = "state", "year_szn" = "year_szn")) 

forecast_hosp_23_24 <- forecast_hosp %>% 
  filter(target_end_date >= "2022-09-01" & target_end_date <= "2024-04-30",
         location != "US",
         #location_name == "Maryland",
         #quantile %in% c(0.500),
         type == "point",
         horizon <= 7,
         ) %>% #c(0.025, 0.100, 0.25, 0.500, 0.75, 0.900, 0.975))
  mutate(incidH = value,
         date = target_end_date,
         los_difference = optimized_los - optimized_los_prior,
  )
  
forecast_hosp_23_24_quantiles <- forecast_hosp %>% 
  filter(target_end_date >= "2022-09-01" & target_end_date <= "2024-04-30",
         location != "US",
         #location_name == "Maryland",
         quantile %in% c(0.025, 0.100, 0.25, 0.500, 0.75, 0.900, 0.975),
         #type == "point",
         horizon <= 7,
  ) %>% 
  mutate(incidH = value,
         date = target_end_date,
         los_difference = optimized_los - optimized_los_prior,
  )

optimized_data_all_quantiles <- create_optimize_totalHosp_data_timevarying_forecast(
  parent_data = forecast_hosp_23_24_quantiles) 


covid_joined_totalHosp_state_data <- optimized_data_all_quantiles %>% mutate(
  year = year(date),
  season = sapply(date, get_season),
  adjusted_year = ifelse(season == "Winter" & month(date) == 12, year + 1, year),
  year_szn = paste0(adjusted_year, "_", season)
)

#covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(parent_data = covid_HHS_data_states_lag, los_opt_by_state = los_opt_by_state)

# Write Final files for analysis ----------------
covid_HHS_data <- arrow::read_parquet(opt$gt_data_path) %>% 
  select(state, date, inpatient_beds, `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
         `previous_day_admission_adult_covid_confirmed_30-39`, `previous_day_admission_adult_covid_confirmed_40-49`, `previous_day_admission_adult_covid_confirmed_50-59`,               
         `previous_day_admission_adult_covid_confirmed_60-69`, `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, `previous_day_admission_adult_covid_confirmed_unknown`,
         `previous_day_admission_pediatric_covid_confirmed_0_4`, `previous_day_admission_pediatric_covid_confirmed_12_17`, `previous_day_admission_pediatric_covid_confirmed_5_11`,
         `previous_day_admission_pediatric_covid_confirmed_unknown`, `previous_day_admission_pediatric_covid_confirmed`, `previous_day_admission_adult_covid_confirmed`)

covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state, by = "state")

covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state_season_prior, by = c("state", "year_szn"))

covid_joined_totalHosp_state_data_los_demographic <- left_join(covid_joined_totalHosp_state_data_los, covid_HHS_data, c("state", "date"))
write_parquet(covid_joined_totalHosp_state_data_los_demographic, "data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_TIMEVARYING_FORECAST_QUANTILES.parquet")

