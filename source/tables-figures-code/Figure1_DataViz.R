library(arrow)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)
library(forecast)
library(pracma)
library(lubridate)
library(performance)

opt <- list()
opt$gt_data_path_hosp_burden_estimates <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_04142024.parquet"
opt$gt_data_path_CDC <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_LIT_CDC_3.9.parquet" 
opt$gt_data_path_Rees <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_LIT_Rees_5.0.parquet"


covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_hosp_burden_estimates)
cdc_covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_CDC)
rees_covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_Rees)

# opt <- list()
# opt$gt_data_path_DailyLOS <- "data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_Daily_TimeVaryingLOS_90day.csv"
# opt$gt_data_path_basic <- "data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_Basic_LOS.csv" 
# opt$gt_data_path_EpiWeekLOS <- "data/tables-figures-data/length-of-stay-estimates/historical-data/Figure1_HistoricalHospBurdenEstimates_EpiWeek_TimeVaryingLOS_90day.csv"
# 
# covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_DailyLOS)
# cdc_covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_basic)
# rees_covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_EpiWeekLOS)
opt <- list()
opt$gt_data_path_DailyLOS <- "data/tables-figures-data/historical-hospburden-estimates-reported-data/Fig1_USA_HospBurdenEst_LOS_UniqueDates2324.parquet"
opt$gt_data_path_basicLOS <- "data/tables-figures-data/historical-hospburden-estimates-reported-data/Fig1_USA_HospBurdenEst_LOS_Basic2324.parquet"
opt$gt_data_path_EpiWeekLOS <- "data/tables-figures-data/historical-hospburden-estimates-reported-data/Fig1_USA_HospBurdenEst_LOS_EpiWeek2324.parquet"

DailyLOS_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_DailyLOS)
basicLOS_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_basicLOS)
EpiWeekLOS_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_EpiWeekLOS)
# cleaning data ---------

basicLOS_totalHosp_data_join <- basicLOS_totalHosp_data %>% 
  rename(basicLOS_total_hosp_estimate = total_hosp_estimate,
         basicLOS_difference = difference)

DailyLOS_totalHosp_data_join <- DailyLOS_totalHosp_data %>% 
  rename(DailyLOS_total_hosp_estimate = total_hosp_estimate,
         DailyLOS_difference = difference) %>% 
  dplyr::select(date, state, DailyLOS_total_hosp_estimate, DailyLOS_difference)

EpiWeekLOS_totalHosp_data_join <- EpiWeekLOS_totalHosp_data %>% 
  rename(EpiWeekLOS_total_hosp_estimate = total_hosp_estimate,
         EpiWeekLOS_difference = difference) %>% 
  dplyr::select(date, state, EpiWeekLOS_total_hosp_estimate, EpiWeekLOS_difference)

covid_totalHosp_data_lit <- basicLOS_totalHosp_data_join %>% 
  left_join(EpiWeekLOS_totalHosp_data_join, by = c("state", "date")) %>% 
  left_join(DailyLOS_totalHosp_data_join, by = c("state", "date")) 


# Paper figures -------------
figure1_LitOptBurden_State <- covid_totalHosp_data_lit %>%  
  mutate(state_names = case_when(
    state == "US" ~ "United States")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = total_hosp, linetype = "Reported"), color = "black", alpha = 0.8) +
  geom_line(aes(y = basicLOS_total_hosp_estimate, color = "Basic LOS (1 LOS per time period)"), linetype = "dashed") +
  geom_line(aes(y = EpiWeekLOS_total_hosp_estimate, color = "EpiWeek LOS (LOS each week)"), linetype = "dashed") +
  geom_line(aes(y = DailyLOS_total_hosp_estimate, color = "Daily LOS (LOS each day)"), linetype = "dashed") +  
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  ggtitle("Total Observed vs. Estimated Hospitalizations in the United States for the 23/24 Respiratory Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Comparison of Reported and Estimated Inpatient Hospitalizations in the United States for the 23/24 Respiratory Season using Multiple Methods for Estimating LOS") +
  scale_color_manual(name = "Source of Length of Stay Estimate", 
                     values = c("Basic LOS (1 LOS per time period)" = "#33a02c", "EpiWeek LOS (LOS each week)" = "#1f78b4", "Daily LOS (LOS each day)" =  "#e31a1c")) +
  scale_linetype_manual(name = "Line Type", 
                        values = c("Reported" = "solid", "Basic LOS (1 LOS per time period)" = "dotted", "EpiWeek LOS (LOS each week)" = "dashed", "CDC" = "dashed"), 
                        labels = c("Reported Hospitalizations", "Basic LOS (1 LOS per time period)", "EpiWeek LOS (LOS each week)", "Daily LOS (LOS each day)")) +
  facet_wrap(~state_names, nrow = 2, scales = "free_y")

ggsave("~/Downloads/Figure1_hosp_burden.png", plot = figure1_LitOptBurden_State, width = 10, height = 8, dpi = 300)









