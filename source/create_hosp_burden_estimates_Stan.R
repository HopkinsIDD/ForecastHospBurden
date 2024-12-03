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

# make sure there are not missing dates
covid_HHS_data_states_lag <- covid_HHS_data_states_lag %>% 
    full_join(expand_grid(state = unique(covid_HHS_data_states$state), 
                          date = lubridate::as_date(min(covid_HHS_data_states$date):max(covid_HHS_data_states$date))), 
              by = c("state", "date"))

# Create dataframes for each state with Hospital burden data 
create_totalH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-incidH, -incidH_prior_day), state) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, -incidH_prior_day), state)

# set up data for Stan ----------------------------------------



stan_model_file <- "source/stan_models/hospital_stays_v4.stan"

ret <- rstan::stanc(stan_model_file) # Check Stan file
fit1test <- stan(file = stan_model_file, data = stan_data, iter = 100, chains = 1)
traceplot(fit1test)

fit1test2 <- stan(
  file = stan_model_file,
  data = stan_data,
  iter = 500,        
  warmup = 100,     
  chains = 2)
fit1 <- stan(file = stan_model_file, data = stan_data)


df = covid_incidH_data_MD
incidH = covid_incidH_data_MD$incidH
total_hosp = covid_totalHosp_data_MD$total_hosp

# generated quantities doesn't do inference, just creates outputs 

# Prepare data for Stan
data_stan <- list(
  N = nrow(df),  # Number of observations
  observed_incidH = incidH,  # Observed incid hospitalizations
  observed_total_hosp = total_hosp,  # Observed total hospitalizations
  #expected_total_hosp = expected_total_hosp_vector  # Vector of expected total hospitalizations (from your model)
)
require(rstan)
fit = stan(file = 'stan_models/hospital_stays_v2.stan', data = stan_data)

# Fit the model
#fit <- sampling(stan_model, data = data_stan)


# Extract optimized los parameter
los_optimized <- extract(fit)$los

# Print optimized los
print(los_optimized)


