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
library(rstan)
library(V8)
# rstanarm
# brms
# prophet 
# other stna packages

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

### IMPORT INITIAL DATA -----------------------------------

# source data functions
source("source/data_setup_source.R")

opt <- list()
opt$gt_data_path <- "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"

# estimate hospital stays with stan ----------------------------
# covidhosp_stay_funct <- function(n, los = 5) {
#   rnbinom(n = n, size = los, prob = 0.5) 
# }
# Compile the Stan model
# create result from sampling, and then sample length of stay 

# comment out stan code 
# stan_code <- "source/stan_models/hospital_stays.stan"
# stan_model <- stan_model(file = stan_code)
# 
# # Specify number of patients
# N <- 100
# 
# # Specify length of stay parameter (mean length of stay)
# los <- 5
# 
# # Generate simulated hospital stays
# sim_data <- sampling(stan_model, data = list(N = N), chains = 1, iter = 1, warmup = 0)


# read in data, define total_hosp, incidH_prior_day
covid_HHS_data_states <- arrow::read_parquet(opt$gt_data_path) %>% 
  mutate(total_hosp = total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid,
         incidH_prior_day = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as_date(date)) %>% 
  arrange(state, date) %>% 
  dplyr::select(state, date, total_hosp, incidH_prior_day)

# SETUP FILES FOR EACH STATE TO ESTIMATE HOSPITAL BURDEN -----------------------------------

# Lag incident hospitalizations 
## incidH_prior_day values are moved back one day and add to new column incidH
covid_HHS_data_states_lag <- create_incidH_lag(covid_HHS_data_states) 

# Create dataframes for each state with Hospital burden data 
create_totalH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-incidH, -incidH_prior_day), state) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, -incidH_prior_day), state)

# Estimate LOS value for each state using optimization --------------

#create_optimization(parent_data = covid_HHS_data_states_lag, optimize_los) # note: parent data just for getting list of all states

# set up optimization for Stan ----

# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------

# create functions for sampling hospitalization duration 

burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct, los = 5){
  lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, los = los), function(x = X) (0:(x-1)) + date))))
}


# ~ Functions for Empirical data --------------------------------------
create_hosp_dates <- function(data, los = 5){
  data_burden <- list()
  
  for (i in 1:nrow(data)){
    
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates = 
                    burden_est_funct(incidH = data$incidH[i], 
                                     date = data$date[i], 
                                     hospstayfunct = covidhosp_stay_funct,
                                     los = los
                    )
      )
  }
  return(data_burden)
}

# instead of function, start with estimating LOS for one state 


data = covid_incidH_data_MD
observed = covid_totalHosp_data_MD

optimize_los <- function(los, data, observed){
  
  expected_list <- create_hosp_dates(data, los = los)
  expected <- create_curr_hosp(data_burden = expected_list)
  
  expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "date") %>% 
    dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
    mutate(sq_difference = ((total_hosp - total_hosp_estimate)^2)) %>% 
    filter(!is.na(absolute_difference)) %>% 
    summarise(optimization_stat = sum(sq_difference)) 
  
  return(combined$optimization_stat)
  
}

optimize_los(los = 5, data = covid_incidH_data_MD, observed = covid_totalHosp_data_MD)
los_range <- c(3,15) # parameter range, won't need to define bounds?
# tol (accuracy)  is the default value (approx. 0.0001)
los_min <- optimize(optimize_los, los_range, data = covid_incidH_data_MD, 
                    observed = covid_totalHosp_data_MD, 
                    maximum = FALSE)

create_expected_total_hosp_vector <- function(data) {
  expected_list <- create_hosp_dates(data = data, los = 5)
  expected <- create_curr_hosp(data_burden = expected_list)
  expected <- clean_expected(expected)
  expected_total_hosp_vector <- expected$total_hosp_estimate
  assign("expected_total_hosp_vector", expected_total_hosp_vector, envir = .GlobalEnv)
}
create_expected_total_hosp_vector(data = covid_incidH_data_MD)
expected_total_hosp_vector <- expected_total_hosp_vector[1:nrow(covid_totalHosp_data_MD)]

# set up for Stan optimization ----------------------------
covid_incidH_data_MD <- covid_incidH_data_MD %>% 
  arrange(date) %>%
  mutate(incid_h_t = row_number()) # convert date to numeric
T <- length(covid_incidH_data_MD$date)
N <- sum(covid_incidH_data_MD$incidH)
covid_incidH_data_MD_long <- covid_incidH_data_MD %>% 
  uncount(incidH) 
incid_h_t <- covid_incidH_data_MD_long$incid_h_t
census_h <- covid_totalHosp_data_MD$total_hosp
los_prior <- 5
# data {
#   int<lower=0> T2;                // Number of dates  -- come back to this to make sure it matched the generated dates
#   int<lower=0> N;                // Number of obs hospitalizations
#   array[N] int<lower=0> incid_h_t; // individual's day of incident hospitalization 
#     array[T] int<lower=0> census_h; // census hosp 
#     int<lower=0> los_prior; 
# }

stan_data <- list(
  N = length(incid_h_t),      # Number of time points or observations
  incid_h_t = incid_h_t,      # Array of incident hospitalizations
  census_h = census_h         # Array of total hospitalizations
)

# stan model for estimating LOS ----------------------------

# Compile the Stan model
stan_model <- stan_model("stan_modelscreate_hospburden_estimates.stan")

df = covid_incidH_data_MD
incidH = covid_incidH_data_MD$incidH
total_hosp = covid_totalHosp_data_MD$total_hosp

# Prepare data for Stan
data_stan <- list(
  N = nrow(df),  # Number of observations
  observed_incidH = incidH,  # Observed incid hospitalizations
  observed_total_hosp = total_hosp,  # Observed total hospitalizations
  #expected_total_hosp = expected_total_hosp_vector  # Vector of expected total hospitalizations (from your model)
)
require(rstan)
fit = stan(file = 'stan_models/create_hospburden_estimates.stan', data = data_stan)

# Fit the model
#fit <- sampling(stan_model, data = data_stan)


# Extract optimized los parameter
los_optimized <- extract(fit)$los

# Print optimized los
print(los_optimized)


