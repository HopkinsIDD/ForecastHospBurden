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
opt$gt_IH_census <- "data/nj_covid_hosp.parquet"


# LOAD DATA ---------------------------------------------------------------

# only need to run this if want to update data
NJ_total_hosp <- arrow::read_parquet(opt$gt_NJ_total_hosp_data_path)

NJ_IH_hosp <- arrow::read_parquet(opt$gt_IH_census)

empirical_IH_NJ_total_hosp <- arrow::read_parquet(opt$gt_NJ_IH_empirical_data_path)

ensemble_IH_NJ_total_hosp <- arrow::read_parquet(opt$gt_NJ_IH_ensemble_data_path)



# CLEAN & REFORMAT DATA ---------------------------------------------------------------
empirical_IH_NJ_total_hosp_weekly <- empirical_IH_NJ_total_hosp %>% 
  rename(date = hosp_dates,
         total_hosp_forecast = curr_hosp) %>% 
  group_by(pathogen, los, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week
  summarize(total_hosp_forecast = sum(total_hosp_forecast)) %>% 
  select(pathogen, week, los, total_hosp_forecast)
  
NJ_total_hosp_weekly <- NJ_total_hosp %>% 
  group_by(state, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week using format()
  mutate(week = as.Date(week)) %>% 
  summarize(total_hosp = sum(total_hosp))  

ensemble_IH_NJ_total_hosp_weekly <- ensemble_IH_NJ_total_hosp %>% 
  group_by(scenario_id, type_id, los, week = format(hosp_dates - as.numeric(format(hosp_dates, "%w")) + 1, "%Y-%m-%d")) %>%
  summarize(total_hosp_forecast = sum(curr_hosp))     # Calculate total sum for each week


# CREATE DATASETS ---------------------------------------------------------------
#empirical_forecast <- inner_join(NJ_total_hosp, empirical_IH_NJ_total_hosp_weekly, by = "date") %>% 
#  select(state, date, pathogen, los, total_hosp, total_hosp_forecast)

empirical_forecast_weekly <- inner_join(NJ_total_hosp_weekly, empirical_IH_NJ_total_hosp_weekly, by = "week") %>% 
  select(state, week, pathogen, los, total_hosp, total_hosp_forecast)

ensemble_forecast <-  inner_join(NJ_total_hosp_weekly, ensemble_IH_NJ_total_hosp_weekly, by = "week") %>% 
  mutate(pathogen = 'COVID-19',
         week = as.Date(week)) %>% 
  select(state, week, pathogen, scenario_id, type_id, los, total_hosp, total_hosp_forecast)

# CREATE OUTCOME VARIABLE DATASETS ---------------------------------------------------------------

empirical_forecast_weekly <- empirical_forecast_weekly %>% 
  mutate(dif = total_hosp_forecast - total_hosp,
         ratio = total_hosp_forecast/total_hosp,
         week = as.Date(week))

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


empirical_forecast_weekly %>%   
  ggplot(aes(x = week, y = total_hosp_forecast)) +
  geom_line(aes(color = as.factor(los))) +
  geom_line(aes(y = total_hosp)) +
  labs(x="Week", y="Total Hospitalizations", color = "LOS") + 
  ggtitle("IH Empirical Forecast vs Hosp Burden by Length of Stay")

empirical_forecast_weekly %>%   
  ggplot(aes(x = week)) +
  geom_line(aes(y = total_hosp_forecast, color = as.factor(los))) +
  geom_line(aes(y = total_hosp, color = "Observed")) +
  labs(x = "Week", y = "Total Hospitalizations", color = "Legend") + 
  ggtitle("IH Empirical Forecast vs Hosp Burden by Length of Stay") +
  scale_color_manual(values = c("black", "red", "blue"), 
                     breaks = c("Observed", "1", "2"), 
                     labels = c("Observed", "1", "2"))
empirical_forecast_weekly %>%   
  ggplot(aes(x = week)) +
  geom_line(aes(y = total_hosp_forecast, color = as.factor(los))) +
  geom_line(aes(y = total_hosp, color = "Observed")) +
  labs(x = "Week", y = "Total Hospitalizations", color = "Legend") + 
  ggtitle("IH Empirical Forecast vs Hosp Burden by Length of Stay") +
  scale_color_manual(values = c("black", "red", "blue", "green", "orange", "purple", "yellow", "cyan", "magenta", "brown", "pink"), 
                     breaks = c("Observed", as.character(unique(empirical_forecast_weekly$los))),
                     labels = c("Observed", as.character(unique(empirical_forecast_weekly$los))))


empirical_forecast_weekly %>%   
  mutate(week = as.Date(week)) %>% 
  filter(year(week) == 2024) %>% 
  filter(los == 5 | los == 14) %>% 
  ggplot(aes(x = week, y = total_hosp_forecast)) +
  geom_line(aes(color = as.factor(los))) +
  geom_line(aes(y = total_hosp)) +
  labs(x="Week", y="Total Hospitalizations", color = "LOS") + 
  ggtitle("IH Empirical Forecast vs Hosp Burden by Length of Stay")

empirical_forecast_weekly %>%   
  mutate(week = as.Date(week)) %>% 
  filter(los == 5 | los == 14) %>% 
  ggplot(aes(x = week, y = total_hosp_forecast)) +
  geom_line(aes(color = as.factor(los))) +
  geom_line(aes(y = total_hosp)) +
  labs(x="Week", y="Total Hospitalizations", color = "LOS") + 
  ggtitle("IH Empirical Forecast vs Hosp Burden by Length of Stay")



## look at LOS affect on total_hosp_forecast 
empirical_forecast_weekly %>% 
  select(week, los, total_hosp_forecast) %>% 
  group_by(los) %>% 
  summarise(average_hosp = mean(total_hosp_forecast))


avg_forecast <- empirical_forecast_weekly %>%
  group_by(los) %>%
  summarize(avg_total_hosp_forecast = mean(total_hosp_forecast, na.rm = TRUE))


#### NEW SET OF DATA FOR LOS = 5 and LOS = 14 

NJ_total_hosp_5 <- arrow::read_parquet("data/hosp_burden/NJ_covid_empirical_weekly_data_burden_LOS_5.parquet", as_data_frame = TRUE)
NJ_total_hosp_14 <- arrow::read_parquet("data/hosp_burden/NJ_covid_empirical_weekly_data_burden_LOS_14.parquet", as_data_frame = TRUE)
NJ_total_hosp_1 <- arrow::read_parquet("data/hosp_burden/NJ_covid_empirical_weekly_data_burden_LOS_1.parquet", as_data_frame = TRUE)

# CLEAN AND MERGE DATASETS ---------------------------------------------------------------

NJ_total_hosp_5 <- NJ_total_hosp_5 %>% 
  mutate(los = 5)
NJ_total_hosp_14 <- NJ_total_hosp_14 %>% 
  mutate(los = 14)
NJ_total_hosp_1 <- NJ_total_hosp_1 %>% 
  mutate(los = 1)

NJ_total_hosp_5_14 <- bind_rows(NJ_total_hosp_5, NJ_total_hosp_14, NJ_total_hosp_1)
# empirical_forecast_weekly_5_14 <- inner_join(NJ_total_hosp_weekly, NJ_total_hosp_5_14, by = "week") %>% 
#   select(state, week, pathogen, los, total_hosp, total_hosp_forecast)

#make data weekly 
NJ_total_hosp_5_14_weekly <- NJ_total_hosp_5_14 %>% 
  rename(date = hosp_dates,
         total_hosp_forecast = curr_hosp) %>% 
  group_by(pathogen, los, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week
  mutate(week = as.Date(week)) %>% 
  summarize(total_hosp_forecast = sum(total_hosp_forecast)) %>% 
  select(pathogen, week, los, total_hosp_forecast)

NJ_total_hosp_5_14_weekly_2 <- NJ_total_hosp_5_14 %>% 
  rename(date = hosp_dates,
         total_hosp_forecast = curr_hosp) %>% 
  group_by(pathogen, los, week = format(date - as.numeric(format(date, "%w")) + 1, "%Y-%m-%d")) %>%  # Group by week
  mutate(week = as.Date(week)) %>% 
  select(pathogen, week, los, total_hosp_forecast)

# CREATE DATASETS ---------------------------------------------------------------
#empirical_forecast <- inner_join(NJ_total_hosp, empirical_IH_NJ_total_hosp_weekly, by = "date") %>% 
#  select(state, date, pathogen, los, total_hosp, total_hosp_forecast)

empirical_forecast_weekly_5_14 <- inner_join(NJ_total_hosp_weekly, NJ_total_hosp_5_14_weekly_2, by = "week") %>% 
  select(state, week, pathogen, los, total_hosp, total_hosp_forecast)


# CREATE OUTCOME VARIABLE DATASETS ---------------------------------------------------------------

empirical_forecast_weekly_5_14 <- empirical_forecast_weekly_5_14 %>% 
  mutate(dif = total_hosp_forecast - total_hosp,
         ratio = total_hosp_forecast/total_hosp)

## data viz 
empirical_forecast_weekly_5_14 %>%   
  ggplot(aes(x = week, y = total_hosp_forecast)) +
  geom_line(aes(color = as.factor(los))) +
  geom_line(aes(y = total_hosp)) +
  labs(x="Week", y="Total Hospitalizations", color = "LOS") + 
  ggtitle("Obs vs Est Hosp Burden by Length of Stay")


#### DATA VIZ FOR RIP 2.27

NJ_total_hosp_weekly_los <- NJ_IH_hosp %>% 
  group_by_all() %>%
  mutate(`3` = incidH*3,
         `5` = incidH*5,
         `10` = incidH*10,
         `14` = incidH*3,
         ) %>% 
  pivot_longer(cols = c(`3`, `5`, `10`, `14`),
               names_to = "los", values_to = "total_hosp_est")
         
         
