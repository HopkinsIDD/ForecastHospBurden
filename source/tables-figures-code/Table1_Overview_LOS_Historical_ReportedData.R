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
library(foreach)
library(doParallel)
library(doFuture)
library(purrr)


### IMPORT INITIAL DATA -----------------------------------
# for parallelization
n_cores <- detectCores()
n_cores

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

# year_szn combo --
create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "year_szn") 

create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "year_szn")

# this takes a couple hours to run, output is 100 LOS estimates for 2023 winter in Table 1 
Sys.time()
start_time <- Sys.time()
create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp %>% filter(year_szn == "2023_Winter"), factor_col = "year_szn", sims = 100) 
end_time <- Sys.time()
end_time - start_time
Sys.time()

write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Winter_100sims.csv")

# Calculate CI from each of the LOS estimtates from written files with 100 sims for Table 1 


# Define quantile probabilities
quantile_probs <- c(
  0.010, 0.025, 0.050, 0.100, 0.150,
  0.200, 0.250, 0.300, 0.350, 0.400,
  0.450, 0.500, 0.550, 0.600, 0.650,
  0.700, 0.750, 0.800, 0.850, 0.900,
  0.950, 0.975, 0.990
)

# Function to calculate quantiles
calculate_quantiles <- function(data, quantile_probs) {
  data %>%
    group_by(state, selected_strata) %>%
    summarise(
      total_hosp_quantiles = list(quantile(optimized_los, probs = quantile_probs, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    unnest_wider(total_hosp_quantiles, names_sep = "_") %>%
    rename_with(~ paste0("q", quantile_probs), starts_with("total_hosp_quantiles_"))
}

# Get list of CSV files
csv_files <- list.files(
  path = "data/tables-figures-data/length-of-stay-estimates/historical-data/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Filter files created after June 5, 2025
cutoff_date <- as.POSIXct("2025-06-05", tz = "UTC")
file_info <- file.info(csv_files)
csv_files_recent <- rownames(file_info[file_info$ctime > cutoff_date, ])

# Loop through recent files, apply function, and combine
quantiles_results <- csv_files_recent %>%
  map_df(~ {
    los_opt_by_state <- read_csv(.x, show_col_types = FALSE)
    calculate_quantiles(los_opt_by_state, quantile_probs) %>%
      mutate(source_file = basename(.x))
  })

write_csv(quantiles_results, "data/tables-figures-data/length-of-stay-estimates/historical-data/quantile_results/Table1_Quantiles_SeasonYear.csv")


# archive generate estimates for table 1 rows
# create seasons estimates across all 4 years 
# create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "season") 
# 
# create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "season")
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp, factor_col = "season", sims = 1) 
# end_time <- Sys.time()
# end_time - start_time
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_Szn_100sims.csv")

# year season estimates 
# 
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp %>% filter(year_szn == "2023_Spring"), factor_col = "year_szn", sims = 100) 
# end_time <- Sys.time()
# end_time - start_time
# Sys.time()
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Spring_100sims.csv")
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp %>% filter(year_szn == "2023_Summer"), factor_col = "year_szn", sims = 100) 
# end_time <- Sys.time()
# end_time - start_time
# Sys.time()
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Summer_100sims.csv")
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp %>% filter(year_szn == "2023_Fall"), factor_col = "year_szn", sims = 100) 
# end_time <- Sys.time()
# end_time - start_time
# Sys.time()
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Fall_100sims.csv")
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp %>% filter(year_szn == "2024_Winter"), factor_col = "year_szn", sims = 100) 
# end_time <- Sys.time()
# end_time - start_time
# Sys.time()
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2024_Winter_100sims.csv")
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp %>% filter(year_szn == "2024_Spring"), factor_col = "year_szn", sims = 100) 
# end_time <- Sys.time()
# end_time - start_time
# Sys.time()
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2024_Spring_100sims.csv")

# 
# quantile_probs <- c(
#   0.010, 0.025, 0.050, 0.100, 0.150,
#   0.200, 0.250, 0.300, 0.350, 0.400,
#   0.450, 0.500, 0.550, 0.600, 0.650,
#   0.700, 0.750, 0.800, 0.850, 0.900,
#   0.950, 0.975, 0.990
# )
# 
# calculate_quantiles <- function(data, quantile_probs) {
#   # Group by date and calculate quantiles for total_hosp_estimate
#   quantiles_by_date <- data %>%
#     group_by(state, selected_strata) %>%
#     summarise(
#       total_hosp_quantiles = list(quantile(optimized_los, probs = quantile_probs, na.rm = TRUE))
#     ) %>%
#     unnest_wider(total_hosp_quantiles, names_sep = "_") %>%
#     rename_with(
#       ~ paste0(quantile_probs), 
#       starts_with("total_hosp_quantiles_")
#     )
#   
#   return(quantiles_by_date)
# }
# 
# 
# # Apply the function to calculate quantiles
# quantiles_results <- calculate_quantiles(los_opt_by_state, quantile_probs)
# 
# quantile_cols <- as.character(quantile_probs)
# 
# optimized_data_all_quantiles_temp <- quantiles_results %>% left_join(optimized_data_all_quantiles %>% select(state, date, total_hosp) %>%  distinct(), by = c("state", "date")) %>% 
#   mutate(n_simu = 100) 
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_Szn.csv")
# 
# # pt 3
# create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "respiratory_season") 
# 
# # Create dataframes for each state with incident hospitalization data
# create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "respiratory_season")
# 
# start_time <- Sys.time()
# create_optimization_timevarying_by_factor_parallel(parent_data = covid_HHS_data_states_lagtemp, factor_col = "respiratory_season", sims = 100) 
# end_time <- Sys.time()
# end_time - start_time
# 
# 
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_Waves_100sims.csv")
# 
# create_optimization_timevarying_by_factor(parent_data = covid_HHS_data_states_lagtemp, optimize_los, factor_col = "respiratory_season") 
# # Create dataframes for each state with incident hospitalization data
# 
# create_totalH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-incidH, -incidH_prior_day), factor = "year_szn") 
# 
# create_incidH_df_by_factor(data = covid_HHS_data_states_lagtemp %>% dplyr::select(-total_hosp, -incidH_prior_day), factor_col = "year_szn")
# 
# create_optimization_timevarying_by_factor(parent_data = covid_HHS_data_states_lagtemp, optimize_los, factor_col = "year_szn") 
# 
# write_csv(los_opt_by_state, "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn.csv")
# 

