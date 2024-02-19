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
  select(pathogen, date, los, total_hosp_forecast)

  
NJ_total_hosp_weekly <- NJ_total_hosp %>% 
  group_by(state, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week using format()
  summarize(total_hosp = sum(total_hosp))  

ensemble_IH_NJ_total_hosp_weekly <- ensemble_IH_NJ_total_hosp %>% 
  group_by(scenario_id, type_id, los, week = format(hosp_dates - as.numeric(format(hosp_dates, "%w")) + 1, "%Y-%m-%d")) %>%
  summarize(total_hosp_forecast = sum(curr_hosp))     # Calculate total sum for each week


# CREATE DATASETS ---------------------------------------------------------------
empirical_forecast <- inner_join(NJ_total_hosp, empirical_IH_NJ_total_hosp, by = "date") %>% 
  select(state, date, pathogen, los, total_hosp, total_hosp_forecast)

ensemble_forecast <-  inner_join(NJ_total_hosp_weekly, ensemble_IH_NJ_total_hosp_weekly, by = "week") %>% 
  mutate(pathogen = 'COVID-19',
         week = as.Date(week)) %>% 
  select(state, week, pathogen, scenario_id, type_id, los, total_hosp, total_hosp_forecast)

# CREATE OUTCOME VARIABLE DATASETS ---------------------------------------------------------------

empirical_forecast <- empirical_forecast %>% 
  mutate(dif = total_hosp_forecast - total_hosp)

ensemble_forecast_interval <- ensemble_forecast %>% 
  pivot_wider(names_from = type_id, 
              values_from = total_hosp_forecast) %>% 
  mutate(CI_99 = as.numeric(if_else(total_hosp >= `0.01` & total_hosp <= `0.99`, 1, 0)),
         CI_95 = as.numeric(if_else(total_hosp >= `0.025` & total_hosp <= `0.975`, 1, 0)),
         CI_90 = as.numeric(if_else(total_hosp >= `0.05` & total_hosp <= `0.95`, 1, 0)),
         CI_80 = as.numeric(if_else(total_hosp >= `0.1` & total_hosp <= `0.9`, 1, 0)),
         CI_60 = as.numeric(if_else(total_hosp >= `0.2` & total_hosp <= `0.8`, 1, 0)),
         CI_40 = as.numeric(if_else(total_hosp >= `0.3` & total_hosp <= `0.7`, 1, 0)),
         CI_20 = as.numeric(if_else(total_hosp >= `0.4` & total_hosp <= `0.6`, 1, 0)),
         CI_10 = as.numeric(if_else(total_hosp >= `0.45` & total_hosp <= `0.55`, 1, 0))
         ) %>% 
  pivot_longer(cols = c(CI_99, CI_95, CI_90, CI_80, CI_60, CI_40, CI_20, CI_10),
               names_to = "incidH_CI", values_to = "hosp_forecast_interval") %>% 
  select(state, week, pathogen, scenario_id, incidH_CI, los, total_hosp, hosp_forecast_interval) 


### EDA 

### Ensemble IH Forecast 

ggplot(data = ensemble_forecast_interval) +
  geom_bar(mapping = aes(x = los)) +
  facet_wrap(~scenario_id)


ggplot(data = ensemble_forecast_interval) +
  geom_bar(mapping = aes(x = incidH_CI)) +
  facet_wrap(~scenario_id)

### 
plot1 <- NJ_total_hosp %>% 
  ggplot(aes(x = date, y = total_hosp)) +
  geom_line(color = "blue") +
  labs(title = "Census Hosp Burden")

plot2 <- ensemble_IH_NJ_total_hosp %>% 
  mutate(date = hosp_dates) %>% 
  ggplot(aes(x = date, y = curr_hosp)) +
  geom_line(aes(color = as.factor(los))) +
  labs(title = "Ensemble IH Forecast")

# Combine the plots
combined_plot <- plot1 + plot2

# Print the combined plot
print(combined_plot)



# The response of interest:
### does the ensemble forecast CI contain the true hosp burden value
### 0 = no; 1 = yes 

# Key predictor: scenario_id, incidH_CI, LOS 


### Empirical IH  
# The response of interest:
### the difference between the projected hosp burden estimates using empirical IH data vs. actual hosp buden data  

# Key predictor: LOS 

empirical_forecast %>%   
  ggplot(aes(x = date, y = total_hosp_forecast)) +
  geom_line(aes(color = as.factor(los))) +
  geom_line(aes(y = total_hosp)) +
  ggtitle("IH Empirical Forecast vs Hosp Burden by Length of Stay")



