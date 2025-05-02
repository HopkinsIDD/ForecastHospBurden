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

# covid_HHS_data_states_lag <- bind_rows(covid_HHS_data_states_lag, covid_HHS_data_USA_lag)

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
    season = sapply(date, get_season))

# Create hospitalization burden estimates using LOS values from optimization ---------

los_opt_USA_epiweek <- read_csv("data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_EpiWeek_TimeVaryingLOS_90day.csv") %>% 
  mutate(epiweek = epiweek(unique_date)) 

los_opt_USA_uniquedates <- read_csv("data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_Daily_TimeVaryingLOS_90day.csv")

los_opt_USA_basic <- read_csv("data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_Basic_LOS.csv")

covid_HHS_data_USA_LOS_epiweek <- covid_HHS_data_states_lagtemp %>% 
  left_join(los_opt_USA_epiweek, by = c("state" = "state", "epiweek" = "epiweek")) 

covid_HHS_data_USA_LOS_uniquedates <- covid_HHS_data_states_lagtemp %>% 
  left_join(los_opt_USA_uniquedates, by = c("state" = "state", "date" = "unique_date")) %>% 
  filter(date != min(date))

covid_HHS_data_USA_LOS_basic <- covid_HHS_data_states_lagtemp %>% 
  left_join(los_opt_USA_basic, by = c("state" = "state")) 

# create HospBurden estimates using LOS generated for each epiweek
hosp_burden_estimates_USA_epiweek <- create_optimize_totalHosp_data_timevarying(
  parent_data = covid_HHS_data_USA_LOS_epiweek, 
  los_opt_by_state_season = los_opt_USA_epiweek
)

# create HospBurden estimates using LOS generated for each day
hosp_burden_estimates_USA_uniquedates <- create_optimize_totalHosp_data_timevarying(
  parent_data = covid_HHS_data_USA_LOS_uniquedates, 
  los_opt_by_state_season = los_opt_USA_uniquedates
) 

# create HospBurden estimates using basic LOS (one for whole time period)
hosp_burden_estimates_USA_basic <- create_optimize_totalHosp_data_timevarying(
  parent_data = covid_HHS_data_USA_LOS_basic, 
  los_opt_by_state_season = los_opt_USA_basic
) 

# Write Final files for analysis ----------------

totalHosp_estimates_USA_LOS_epiweek <- 
  hosp_burden_estimates_USA_epiweek %>% 
  mutate(
  year = year(date),
  season = sapply(date, get_season),
  epiweek = epiweek(date)) %>% 
  left_join(los_opt_USA_epiweek, by = c("state" = "state", "epiweek" = "epiweek")) 

write_parquet(totalHosp_estimates_USA_LOS_epiweek, "data/tables-figures-data/historical-hospburden-estimates-reported-data/Fig1_USA_HospBurdenEst_LOS_EpiWeek2324.parquet")

totalHosp_estimates_USA_LOS_uniquedates <- 
  hosp_burden_estimates_USA_uniquedates %>% 
  mutate(
    year = year(date),
    season = sapply(date, get_season)) %>% 
  left_join(los_opt_USA_uniquedates, by = c("state" = "state", "date" = "unique_date")) 

write_parquet(totalHosp_estimates_USA_LOS_uniquedates, "data/tables-figures-data/historical-hospburden-estimates-reported-data/Fig1_USA_HospBurdenEst_LOS_UniqueDates2324.parquet")

totalHosp_estimates_USA_LOS_basic <- 
  hosp_burden_estimates_USA_basic %>% 
  mutate(
    year = year(date),
    season = sapply(date, get_season),
    epiweek = epiweek(date)) %>% 
  left_join(los_opt_USA_epiweek, by = c("state" = "state", "epiweek" = "epiweek")) 

write_parquet(totalHosp_estimates_USA_LOS_basic, "data/tables-figures-data/historical-hospburden-estimates-reported-data/Fig1_USA_HospBurdenEst_LOS_Basic2324.parquet")
