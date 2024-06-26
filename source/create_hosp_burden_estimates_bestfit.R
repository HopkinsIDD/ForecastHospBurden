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

# this take a long time to run, prints states in alphabetical in console to check progress 
create_optimization(parent_data = covid_HHS_data_states_lag, optimize_los) # note: parent data just for getting list of all states
#los_opt_by_state <- arrow::read_parquet("data/US_wide_data/LOS_Optimized_by_AllStates_USA.parquet") # if don't want to run, load file directly 

# update only when want to overwrite file 
#write_parquet(los_opt_by_state, "data/US_wide_data/LOS_Optimized_by_AllStates_USA.parquet")
#write_csv(los_opt_by_state, "data/US_wide_data/LOS_Optimized_by_AllStates_USA.csv")

# Create hospitalization burden estimates using LOS values from optimization ---------

# runs faster than above function (fewer outputs) 
covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(parent_data = covid_HHS_data_states_lag, los_opt_by_state = los_opt_by_state)

# Write Final files for analysis ----------------
covid_HHS_data <- arrow::read_parquet(opt$gt_data_path) %>% 
  select(state, date, inpatient_beds, `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
         `previous_day_admission_adult_covid_confirmed_30-39`, `previous_day_admission_adult_covid_confirmed_40-49`, `previous_day_admission_adult_covid_confirmed_50-59`,               
         `previous_day_admission_adult_covid_confirmed_60-69`, `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, `previous_day_admission_adult_covid_confirmed_unknown`,
         `previous_day_admission_pediatric_covid_confirmed_0_4`, `previous_day_admission_pediatric_covid_confirmed_12_17`, `previous_day_admission_pediatric_covid_confirmed_5_11`,
         `previous_day_admission_pediatric_covid_confirmed_unknown`, `previous_day_admission_pediatric_covid_confirmed`, `previous_day_admission_adult_covid_confirmed`)

covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state, by = "state")

covid_joined_totalHosp_state_data_los_demographic <- left_join(covid_joined_totalHosp_state_data_los, covid_HHS_data, c("state", "date"))
write_parquet(covid_joined_totalHosp_state_data_los_demographic, "data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_06242024.parquet")

