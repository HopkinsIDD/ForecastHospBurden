# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
library(gghighlight)
library(arrow)

### IMPORT INITIAL DATA -----------------------------------

# source data functions
source("source/data_setup_source.R")

opt <- list()

# up to date data, from last reported week for 24/25 season
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

# uses the past 90 days of data for each forecast date to est LOS 
los_opt_by_state_season <- read_csv("data/US_wide_data/LOS_Optimized_by_AllStates_USA_real_time_LOS_nbinom.csv")

# Example of drawing from distribution of incidH forecast quantiles a distribution of forecast values for each state, date, and horizon -------------------
# look at one date and dist of forecast values for a state
# forecast_hosp_MD_7 <- forecast_hosp %>%
#   filter(location_name == "Maryland",
#          horizon == 7,
#          forecast_date == "2023-08-28",
#          type == "quantile") %>% 
#   select(location_name, horizon, forecast_date, target_end_date, value, quantile, type)
# 
# forecast_hosp_MD_7_all_dates_horizon <- 
#   forecast_hosp %>%
#   filter(location_name == "Maryland",
#          type == "quantile") %>% 
#   select(location_name, horizon, forecast_date, target_end_date, value, quantile, type)
# # unique(forecast_hosp_MD_7_all_dates_horizon$forecast_date)
# # unique(forecast_hosp_MD_7_all_dates_horizon$horizon)
# 
# forecast_hosp_MD_7_cleaned <- forecast_hosp_MD_7 %>% 
#   select(-quantile, -value) %>% 
#   distinct()
# 
# interpolated_cdf <- approxfun(forecast_hosp_MD_7$quantile, forecast_hosp_MD_7$value, rule = 2) 
# 
# set.seed(123) # For reproducibility
# n_samples <- 100
# samples <- interpolated_cdf(runif(n_samples))
# 
# # Replicate the single row for each sample
# forecast_hosp_MD_7_sampled_incidH <- forecast_hosp_MD_7_cleaned[rep(1, n_samples), ]
# 
# # Add the samples as a new column
# forecast_hosp_MD_7_sampled_incidH$sample <- samples
# 
# # Add to data frame
# sampled_data <- data.frame(sample = samples)
# 
# # visualize quantile distribution
# ggplot(sampled_data, aes(x = sample)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "white", alpha = 0.8) +
#   geom_vline(data = forecast_hosp_MD_7, aes(xintercept = value), color = "red", linetype = "dashed", size = 0.5) +
#   labs(title = "Sampled Distribution of incidH Quantiles MD 2023-08-28 horizon 7", x = "Values", y = "Frequency") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotate("text", x = forecast_hosp_MD_7$value, y = 0, label = forecast_hosp_MD_7$quantile, angle = 90, hjust = -0.1, size = 3)
# 
# # Create estimates of incidH pulling from incidH forecast quantile distributions for each state, date, horizon --------------------
# 
# forecast_hosp_all_dates_horizon <- forecast_hosp %>%
#   filter(location_name != "United States",
#          type == "quantile") %>%
#   mutate(horizon = as.numeric(horizon)) %>%
#   select(location_name, abbreviation, horizon, forecast_date, target_end_date, value, quantile, type) #%>% 
#   #filter(horizon <= 7)
# 
# unique_dates <- unique(forecast_hosp_all_dates_horizon$target_end_date)
# unique_horizons <- unique(forecast_hosp_all_dates_horizon$horizon)
# unique_states <- unique(forecast_hosp_all_dates_horizon$location_name)
# 
# # Initialize a data frame to store results for all states
# all_states_samples <- data.frame()
# 
# # Loop through each state
# for (state in unique_states) {
#   print(state)
#   # Subset data for the current state
#   state_data <- forecast_hosp_all_dates_horizon %>%
#     filter(location_name == state)
# 
#   # Initialize a list to store results for the current state
#   state_samples <- list()
# 
#   # Loop through unique dates and horizons
#   for (date in unique_dates) {
#     for (horizon_i in unique_horizons) {
#       # Subset the data for the current date and horizon
#       subset_data <- state_data %>%
#         filter(target_end_date == date, horizon == horizon_i)
# 
#       if (nrow(subset_data) > 0) {
#         # Interpolate CDF based on quantiles and values
#         interpolated_cdf <- approxfun(subset_data$quantile, subset_data$value, rule = 2)
# 
#         # Generate samples
#         set.seed(123) # For reproducibility
#         n_samples <- 100
#         samples <- interpolated_cdf(runif(n_samples))
# 
#         # Replicate the rows for each sample
#         replicated_data <- subset_data[rep(1, n_samples), ] %>%
#           select(-quantile, -value) %>%
#           mutate(
#             sample = samples,
#             simulation = seq_len(n_samples), # Assign simulation number
#             state = state # Add state information
#           )
# 
#         # Store the result in the list
#         state_samples[[paste(as.Date(date), horizon_i, sep = "_")]] <- replicated_data
#       }
#     }
#   }
# 
#   # Combine all results for the current state into a single data frame
#   state_combined <- do.call(rbind, state_samples)
# 
#   # Append to the main data frame for all states
#   all_states_samples <- rbind(all_states_samples, state_combined)
# }
# write_parquet(all_states_samples, "data/US_wide_data/forecast_hosp_all_dates_horizon_sampled_incidH_from_quantile_negbinom_03192025.parquet")

all_states_samples <- read_parquet("data/US_wide_data/forecast_hosp_all_dates_horizon_sampled_incidH_from_quantile_negbinom_03192025.parquet")

#forecast_hosp_MD_samples <- all_states_samples



# Create hospitalization burden estimates using LOS values from optimization ---------

# runs faster than above function (fewer outputs) 

all_states_samples_los <- all_states_samples %>%
  # mutate(year = year(target_end_date),
  #        season = sapply(target_end_date, get_season),
  #        adjusted_year = ifelse(season == "Winter" & month(target_end_date) == 12, year + 1, year),
  #        year_szn = paste0(adjusted_year, "_", season)
  # ) %>%
  # select(-adjusted_year) %>%
  left_join(los_opt_by_state_season, by = c("abbreviation" = "state", "forecast_date" = "forecast_date")) %>% 
  #filter(horizon <= 7) %>%
  mutate(incidH = sample,
         date = target_end_date
  )

write_parquet(all_states_samples_los, "data/US_wide_data/all_states_samples_realtime_los_negbinom_03202025.csv")

all_states_samples_los <- read_parquet("data/US_wide_data/all_states_samples_realtime_los_negbinom_03202025.csv")

# not parallel version of function
# create_optimize_totalHosp_data_timevarying_forecast_simulations <- function(parent_data) {
#   #parent_data <- all_states_samples_los
#   
#   states_list <- unique(parent_data$abbreviation)
#   combined_list <- list()
#   
#   for (state_abbv in states_list) {
#     #state_abbv <- "MD"
#     print(state_abbv) # Progress tracking
#     # Filter data for the current state
#     state_data <- parent_data %>% filter(abbreviation == state_abbv)
#     
#     # Retrieve unique simulation numbers
#     simulation_list <- unique(state_data$simulation)
#     #simulation_list <- c(1, 2)
#     for (simulation_num in simulation_list) {
#       print(paste("Processing simulation:", simulation_num, "for state:", state_abbv))
#       
#       #simulation_num <- 1 
#       # Filter data for the current simulation
#       sim_data <- state_data %>% filter(simulation == simulation_num,
#                                         forecast_date != "2024-04-29")
#       
#       # Retrieve LOS values for the state
#       
#       # pull in reported data upto forecast date 
#       forecast_date_list <- unique(sim_data$forecast_date)
#       for(origin_date in forecast_date_list){
#         #print(paste("Processing date:", as.Date(origin_date)))
#         #origin_date <- as.Date("2023-10-02")
#         #origin_date <- as.Date("2024-04-22")
#         #origin_date <- as.Date("2023-09-04")
#         
#         # need to make an adjustment if origin_date is earliest date to backfill 
#         
#         sim_data_origin <- sim_data %>% 
#           filter(forecast_date <= origin_date & date >= origin_date - 30) %>% 
#           mutate(
#             #origin_date = as.Date(forecast_date),  # Ensure date is properly formatted
#             incidH = if_else(forecast_date != origin_date, NA_real_, incidH)
#           ) %>%
#           group_by(target_end_date) %>%
#           arrange(desc(forecast_date)) %>%  # most recent date is first
#           slice(1) %>%  # keep the most recent row forecast per target_end_date
#           ungroup()
#         
#         reported_hosp_prior_month <- covid_HHS_data_states_lag %>% 
#           filter(date <= origin_date & date >= origin_date - 30,
#                  state == state_abbv) %>% 
#           mutate(abbreviation = state,
#                  state = unique(sim_data_origin$state)) 
# 
#         sim_and_reported_data_join <- sim_data_origin %>%
#           left_join(reported_hosp_prior_month %>% select(everything(), incidH_reported = incidH), by = c("date", "abbreviation", "state")) %>%
#           mutate(incidH = if_else(is.na(incidH), incidH_reported, incidH),
#                  date = as.Date(date))
#         
#         los_state_list <- sim_and_reported_data_join$optimized_los
#         
#       if (length(los_state_list) == nrow(sim_and_reported_data_join)) { # Ensure LOS values match data rows
# 
#         # Generate expected hospitalization data
#         expected_list <- create_hosp_dates_timevarying(sim_and_reported_data_join, los_vector = los_state_list)
#         
#         expected <- create_curr_hosp_forecast(data_burden = expected_list)
#         expected <- clean_expected(expected)
#         
#         # Fetch observed data dynamically
#         dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
#         
#         if (exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
#           observed <- get(dynamic_totalHosp_name)
#           
#           # Combine observed and expected data
#           combined <- inner_join(observed, expected, by = "date") %>%
#             left_join(sim_and_reported_data_join %>% select(date, forecast_date), by = c("date")) %>%
#             dplyr::select(state, date,forecast_date, total_hosp, total_hosp_estimate) %>%
#             mutate(
#               absolute_difference = abs(total_hosp - total_hosp_estimate),
#               difference = total_hosp - total_hosp_estimate,
#               relative_difference = total_hosp_estimate / total_hosp,
#                origin_date = as.Date(origin_date),
#               simulation = simulation_num
#             ) %>% 
#             filter(forecast_date == origin_date)
#           
#           combined_list[[paste(state_abbv, simulation_num, origin_date, sep = "_")]] <- combined
#         } else {
#           print(paste("Data not found for:", dynamic_totalHosp_name))
#         }
#       } else {
#         print(paste("Mismatch in LOS values for state:", state_abbv, "simulation:", simulation_num))
#       }
#       }
#     }
#   }
#   
#   # Combine the results into a single dataframe
#   combined_df <- do.call(rbind, combined_list)
#   return(combined_df)
# }
# optimized_data_all_quantiles <- create_optimize_totalHosp_data_timevarying_forecast_simulations(
#   parent_data = all_states_samples_los)


# chatgpt updates to code START HERE 3/10/2025 ------ 
library(foreach)
library(doParallel)
library(doFuture)
# How many cores does your CPU have
n_cores <- detectCores()
n_cores

# Register cluster
#cluster <- makeCluster(n_cores - 1, timeout = 600)
# registerDoParallel(cluster)
# options(timeout = 600)  # 10 minutes
# doFuture::registerDoFuture()
# future::plan(multisession, workers = 3) #ncores-1 = 3

# clean version 
create_optimize_totalHosp_data_timevarying_forecast_simulations_parallel <- function(parent_data, covid_HHS_data_states_lag) {
  #parent_data <- all_states_samples_los # comment out 
  
  states_list <- unique(parent_data$abbreviation)
  combined_list <- list()
  
  for (state_abbv in states_list) {
    #state_abbv <- "MD" # comment out 
    print(state_abbv) # Progress tracking
    # Filter data for the current state
    state_data <- parent_data %>% filter(abbreviation == state_abbv)
    # Fetch observed data dynamically
    dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
    observed <- get(dynamic_totalHosp_name)
    
    # Retrieve unique simulation numbers
    simulation_list <- unique(state_data$simulation)
    #simulation_list <- 1:10 # comment out 
    # Parallel processing of simulation_list
    cluster <- makeCluster(n_cores - 1, timeout = 600)
    registerDoParallel(cluster)
    options(timeout = 600)  # 10 minutes
    
    results <- foreach(simulation_num = simulation_list, 
                       .packages = c("dplyr", "tidyr"),
                       .export = c("create_hosp_dates_timevarying",
                                   "create_curr_hosp_forecast",
                                   "clean_expected",
                                   "expand_grid",
                                   "burden_est_funct",
                                   "covidhosp_stay_funct"#,
                                   # "state_data",
                                   # "covid_HHS_data_states_lag",
                                   # "dynamic_totalHosp_name",
                                   # "state_abbv"
                                   )
    ) %dopar% {
      #print(paste("Processing simulation:", simulation_num, "for state:", state_abbv))
      
      #simulation_num <- 1 # comment out 
      # Filter data for the current simulation
      sim_data <- state_data %>% filter(simulation == simulation_num, forecast_date != "2024-04-29")
      
      # Retrieve LOS values for the state
      
      # pull in reported data up to forecast date 
      forecast_date_list <- unique(sim_data$forecast_date)
      
      state_combined_list <- list()
      
      for (origin_date in forecast_date_list) {
        #origin_date <- as.Date("2023-10-02") # comment out 
        #print(paste("Processing date:", as.Date(origin_date)))
        
        # need to make an adjustment if origin_date is earliest date to backfill 
        
        sim_data_origin <- sim_data %>% 
          filter(forecast_date <= origin_date & date >= origin_date - 30) %>% 
          mutate(
            incidH = if_else(forecast_date != origin_date, NA_real_, incidH)
          ) %>%
          group_by(target_end_date) %>%
          arrange(desc(forecast_date)) %>%  # most recent date is first
          slice(1) %>%  # keep the most recent row forecast per target_end_date
          ungroup()
        
        reported_hosp_prior_month <- covid_HHS_data_states_lag %>% 
          filter(date <= origin_date & date >= origin_date - 30, state == state_abbv) %>% 
          mutate(abbreviation = state, state = unique(sim_data_origin$state)) 
        
        sim_and_reported_data_join <- sim_data_origin %>%
          left_join(reported_hosp_prior_month %>% select(everything(), incidH_reported = incidH), by = c("date", "abbreviation", "state")) %>%
          mutate(incidH = if_else(is.na(incidH), incidH_reported, incidH), date = as.Date(date))
        
        los_state_list <- sim_and_reported_data_join$optimized_los
        
        if (length(los_state_list) == nrow(sim_and_reported_data_join)) { # Ensure LOS values match data rows
          
          # Generate expected hospitalization data
          expected_list <- create_hosp_dates_timevarying(sim_and_reported_data_join, los_vector = los_state_list)
          
          expected <- create_curr_hosp_forecast(data_burden = expected_list)
          expected <- clean_expected(expected)
          
          # Combine observed and expected data
          combined <- inner_join(observed, expected, by = "date") %>%
            left_join(sim_and_reported_data_join %>% select(date, forecast_date), by = c("date")) %>%
            dplyr::select(state, date, forecast_date, total_hosp, total_hosp_estimate) %>%
            mutate(
              absolute_difference = abs(total_hosp - total_hosp_estimate),
              difference = total_hosp - total_hosp_estimate,
              relative_difference = total_hosp_estimate / total_hosp,
              origin_date = as.Date(origin_date),
              simulation = simulation_num
            ) %>% 
            filter(forecast_date == origin_date)
          
          state_combined_list[[paste(state_abbv, simulation_num, origin_date, sep = "_")]] <- combined
        
        }
      }
      return(state_combined_list)
    }
    # Merge parallel results into combined_list
    combined_list <- c(combined_list, do.call(c, results))
    # Don't forget to stop the cluster
    stopCluster(cl = cluster)
    }
  
  # Combine the results into a single dataframe
  combined_df <- do.call(rbind, combined_list)
  return(combined_df)
}

all_states_samples_los_MD <- all_states_samples_los %>%
  filter(location_name == "Maryland") #, simulation %in% c(1, 2))

#, simulation %in% c(1, 2)
start_time <- Sys.time()
optimized_data_all_quantiles <- create_optimize_totalHosp_data_timevarying_forecast_simulations_parallel(parent_data = all_states_samples_los, covid_HHS_data_states_lag)
end_time <- Sys.time()
end_time - start_time
# working ugly version 3/19 

# create_optimize_totalHosp_data_timevarying_forecast_simulations_parallel2 <- function(parent_data, covid_HHS_data_states_lag) {
#   parent_data <- all_states_samples_los # comment out 
#   
#   states_list <- unique(parent_data$abbreviation)
#   combined_list <- list()
#   
#   for (state_abbv in states_list) {
#     state_abbv <- "MD" # comment out 
#     print(state_abbv) # Progress tracking
#     # Filter data for the current state
#     state_data <- parent_data %>% filter(abbreviation == state_abbv)
#     # Fetch observed data dynamically
#     dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
#     observed <- get(dynamic_totalHosp_name)
#     
#     # Retrieve unique simulation numbers
#     simulation_list <- unique(state_data$simulation)
#     #simulation_list <- 1:10 # comment out 
#     # Parallel processing of simulation_list
#     cluster <- makeCluster(n_cores - 1, timeout = 600)
#     registerDoParallel(cluster)
#     options(timeout = 600)  # 10 minutes
#     
#     results <- foreach(simulation_num = simulation_list, 
#                        .packages = c("dplyr", "tidyr")
#                        #,
#                        # .options.future = list(seed = TRUE, packages = c("dplyr"), globals = "create_hosp_dates_timevarying",
#                        #                        "create_curr_hosp_forecast",
#                        #                        "clean_expected",
#                        #                        "expand_grid",
#                        #                        "burden_est_funct",
#                        #                        "covidhosp_stay_funct",
#                        #                        "state_data")
#                        # .export = c("create_hosp_dates_timevarying",
#                        #             "create_curr_hosp_forecast",
#                        #             "clean_expected",
#                        #             "expand_grid",
#                        #             "burden_est_funct",
#                        #             "covidhosp_stay_funct",
#                        #             "state_data",
#                        #             "covid_HHS_data_states_lag",
#                        #             "dynamic_totalHosp_name"
#                        #           )
#                        # .export = c("state_data", "dynamic_totalHosp_name", "state_abbv")
#                        ) %dopar% {
#       #print(paste("Processing simulation:", simulation_num, "for state:", state_abbv))
#       
#       #simulation_num <- 1 # comment out 
#       # Filter data for the current simulation
#       sim_data <- state_data %>% filter(simulation == simulation_num, forecast_date != "2024-04-29")
#       
#       # Retrieve LOS values for the state
#       
#       # pull in reported data up to forecast date 
#       forecast_date_list <- unique(sim_data$forecast_date)
#       
#       state_combined_list <- list()
#       
#       for (origin_date in forecast_date_list) {
#         #origin_date <- as.Date("2023-10-02") # comment out 
#         #print(paste("Processing date:", as.Date(origin_date)))
#         
#         # need to make an adjustment if origin_date is earliest date to backfill 
#         
#         sim_data_origin <- sim_data %>% 
#           filter(forecast_date <= origin_date & date >= origin_date - 30) %>% 
#           mutate(
#             incidH = if_else(forecast_date != origin_date, NA_real_, incidH)
#           ) %>%
#           group_by(target_end_date) %>%
#           arrange(desc(forecast_date)) %>%  # most recent date is first
#           slice(1) %>%  # keep the most recent row forecast per target_end_date
#           ungroup()
#         
#         reported_hosp_prior_month <- covid_HHS_data_states_lag %>% 
#           filter(date <= origin_date & date >= origin_date - 30, state == state_abbv) %>% 
#           mutate(abbreviation = state, state = unique(sim_data_origin$state)) 
#         
#         sim_and_reported_data_join <- sim_data_origin %>%
#           left_join(reported_hosp_prior_month %>% select(everything(), incidH_reported = incidH), by = c("date", "abbreviation", "state")) %>%
#           mutate(incidH = if_else(is.na(incidH), incidH_reported, incidH), date = as.Date(date))
#         
#         los_state_list <- sim_and_reported_data_join$optimized_los
#         
#         if (length(los_state_list) == nrow(sim_and_reported_data_join)) { # Ensure LOS values match data rows
#           
#           # Generate expected hospitalization data
#           expected_list <- create_hosp_dates_timevarying(sim_and_reported_data_join, los_vector = los_state_list)
#           
#           expected <- create_curr_hosp_forecast(data_burden = expected_list)
#           expected <- clean_expected(expected)
#           
#           # # Fetch observed data dynamically
#           # dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
#           
#           #if (exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
#             #observed <- get(dynamic_totalHosp_name)
#             
#             # Combine observed and expected data
#             combined <- inner_join(observed, expected, by = "date") %>%
#               left_join(sim_and_reported_data_join %>% select(date, forecast_date), by = c("date")) %>%
#               dplyr::select(state, date, forecast_date, total_hosp, total_hosp_estimate) %>%
#               mutate(
#                 absolute_difference = abs(total_hosp - total_hosp_estimate),
#                 difference = total_hosp - total_hosp_estimate,
#                 relative_difference = total_hosp_estimate / total_hosp,
#                 origin_date = as.Date(origin_date),
#                 simulation = simulation_num
#               ) %>% 
#               filter(forecast_date == origin_date)
#             
#             state_combined_list[[paste(state_abbv, simulation_num, origin_date, sep = "_")]] <- combined
#         #   } else {
#         #     print(paste("Data not found for:", dynamic_totalHosp_name))
#         #   }
#         # } else {
#         #   print(paste("Mismatch in LOS values for state:", state_abbv, "simulation:", simulation_num))
#         # }
#       }
#       return(state_combined_list)
#     }
#                        }
#     # Merge parallel results into combined_list
#     combined_list <- c(combined_list, do.call(c, results))
#   }
#   
#   # Don't forget to stop the cluster
#   stopCluster(cl = cluster)
#   
#   # Combine the results into a single dataframe
#   combined_df <- do.call(rbind, combined_list)
#   return(combined_df)
# }


# end of updates 

quantile_probs <- c(
  0.010, 0.025, 0.050, 0.100, 0.150,
  0.200, 0.250, 0.300, 0.350, 0.400,
  0.450, 0.500, 0.550, 0.600, 0.650,
  0.700, 0.750, 0.800, 0.850, 0.900,
  0.950, 0.975, 0.990
)

calculate_quantiles <- function(data, quantile_probs) {
  # Group by date and calculate quantiles for total_hosp_estimate
  quantiles_by_date <- data %>%
    group_by(state, date, forecast_date) %>%
    summarise(
      total_hosp_quantiles = list(quantile(total_hosp_estimate, probs = quantile_probs, na.rm = TRUE))
    ) %>%
    unnest_wider(total_hosp_quantiles, names_sep = "_") %>%
    rename_with(
      ~ paste0(quantile_probs), 
      starts_with("total_hosp_quantiles_")
    )
  
  return(quantiles_by_date)
}


# Apply the function to calculate quantiles
quantiles_results <- calculate_quantiles(optimized_data_all_quantiles, quantile_probs)

quantile_cols <- as.character(quantile_probs)

optimized_data_all_quantiles_temp <- quantiles_results %>% left_join(optimized_data_all_quantiles %>% select(state, date, total_hosp) %>%  distinct(), by = c("state", "date")) %>% 
  mutate(n_simu = 100) 

# %>%
#   group_by(state, date, forecast_date) %>%
#   distinct(across(all_of(quantile_cols)), .keep_all = TRUE) %>%
#   ungroup() 

optimized_data_all_quantiles_temp_MD <- quantiles_results %>% 
  filter(state == "MD",
         forecast_date 
         <= "2023-10-10")

#covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(parent_data = covid_HHS_data_states_lag, los_opt_by_state = los_opt_by_state)

# Write Final files for analysis ----------------
covid_HHS_data <- arrow::read_parquet(opt$gt_data_path) %>% 
  select(state, date, inpatient_beds, `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
         `previous_day_admission_adult_covid_confirmed_30-39`,  `previous_day_admission_adult_covid_confirmed_40-49`, `previous_day_admission_adult_covid_confirmed_50-59`,               
         `previous_day_admission_adult_covid_confirmed_60-69`, `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, `previous_day_admission_adult_covid_confirmed_unknown`,
         `previous_day_admission_pediatric_covid_confirmed_0_4`, `previous_day_admission_pediatric_covid_confirmed_12_17`, `previous_day_admission_pediatric_covid_confirmed_5_11`,
         `previous_day_admission_pediatric_covid_confirmed_unknown`, `previous_day_admission_pediatric_covid_confirmed`, `previous_day_admission_adult_covid_confirmed`)

covid_joined_totalHosp_state_data_los <- optimized_data_all_quantiles_temp %>% 
  left_join(los_opt_by_state_season, by = c("state", "forecast_date"))

covid_joined_totalHosp_state_data_los_demographic <- left_join(covid_joined_totalHosp_state_data_los, covid_HHS_data, c("state", "date"))

write_parquet(covid_joined_totalHosp_state_data_los_demographic, "data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_TIMEVARYING_FORECAST_SIMU_quantiles_14days_parallel_negbinom_realtime_LOS_03202025.parquet")

