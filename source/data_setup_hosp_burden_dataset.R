# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
library(gghighlight)



# source data functions

opt <- list()

opt$gt_NJ_total_hosp_data_path <- "data/NJ_total_hosp.parquet"
opt$gt_NJ_IH_empirical_data_path <- "data/hosp_burden/NJ_covid_empirical_weekly_data_burden.parquet"
opt$gt_NJ_IH_ensemble_data_path <- "data/hosp_burden/NJ_covid_ensemble_weekly_data_burden.parquet"


# LOAD DATA ---------------------------------------------------------------

# only need to run this if want to update data
NJ_total_hosp <- arrow::read_parquet(opt$gt_NJ_total_hosp_data_path)

empirical_IH_NJ_total_hosp <- arrow::read_parquet(opt$gt_NJ_IH_empirical_data_path)

ensemble_IH_NJ_total_hosp <- arrow::read_parquet(opt$gt_NJ_IH_ensemble_data_path)

# CLEAN & REFORMAT DATA ---------------------------------------------------------------
empirical_IH_NJ_total_hosp <- empirical_IH_NJ_total_hosp %>% 
  rename(date = hosp_dates,
        total_hosp_forecast = curr_hosp) %>% 
  select(pathogen, date, total_hosp_forecast)

# test <- ensemble_IH_NJ_total_hosp %>% 
#   group_by(scenario_id, type_id, week = format(hosp_dates, "%Y-%U")) %>%  # Group by week using format()
#   summarize(sum_value = sum(curr_hosp))     # Calculate total sum for each week

# NJ_total_hosp1 <- NJ_total_hosp %>% 
#   group_by(week = format(date, "%Y-%U")) %>%  # Group by week using format()
#   summarize(sum_value = sum(total_hosp))  
  
NJ_total_hosp_weekly <- NJ_total_hosp %>% 
  group_by(state, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week using format()
  summarize(total_hosp = sum(total_hosp))  

ensemble_IH_NJ_total_hosp_weekly <- ensemble_IH_NJ_total_hosp %>% 
  group_by(scenario_id, type_id, week = format(hosp_dates - as.numeric(format(hosp_dates, "%w")) + 1, "%Y-%m-%d")) %>%
  summarize(total_hosp_forecast = sum(curr_hosp))     # Calculate total sum for each week


# CREATE DATASETS ---------------------------------------------------------------
empirical_forecast <- inner_join(NJ_total_hosp, empirical_IH_NJ_total_hosp, by = "date") %>% 
  select(state, date, pathogen, total_hosp, total_hosp_forecast)

ensemble_forecast <-  inner_join(NJ_total_hosp_weekly, ensemble_IH_NJ_total_hosp_weekly, by = "week") %>% 
  mutate(pathogen = 'COVID-19') %>% 
  select(state, week, pathogen, scenario_id, type_id, total_hosp, total_hosp_forecast)

# CREATE OUTCOME VARIABLE DATASETS ---------------------------------------------------------------

empirical_forecast <- empirical_forecast %>% 
  mutate(dif = total_hosp_forecast - total_hosp)

test <- ensemble_forecast %>% 
  pivot_wider(names_from = type_id, 
              values_from = total_hosp_forecast) %>% 
  mutate(interval_95 = if_else(total_hosp >= `0.5` & total_hosp <= `0.975`, 1, 0))
