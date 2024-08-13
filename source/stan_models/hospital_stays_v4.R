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

covid_incidH_data_MD <- covid_incidH_data_MD %>% 
  filter(date != max(date)) %>% # remove most recent date to account for lag
  arrange(date) %>%
  mutate(incid_h_t = row_number()) %>% # convert date to numeric
  mutate(incidH = if_else(is.na(incidH), 0, incidH)) # replace NAs with 0
covid_incidH_data_MD_long <- covid_incidH_data_MD %>% 
  uncount(incidH) 

N <- sum(covid_incidH_data_MD$incidH)
incid_h_t <- covid_incidH_data_MD_long$incid_h_t
census_h <- c((covid_totalHosp_data_MD %>% filter(date != max(date)))$total_hosp, rep(0, 100))
T <- length(census_h)

los_prior <- 5

# parameters ------------------------------
los_mean <- 5
census_h_calc <- rep(0, T)
# parameters {
#   real<lower=0> los_mean;  // Length of stay parameter to optimize
#   // vector<lower=0>[T] census_h_calc; // list of census days calculated
#   
# }

# functions ------------------------------
num_matches <- function(x, y, a){
  n <- 0
  x_ <- 0
  y_ <- 0
  for(i in 1:length(x)){
    x_ <- x[i]
    y_ <- y[i]
    
    if(a >= x[i] && a <= y[i]){
      n <- n + 1
    }
  }
  return(n)
  
}


# transformed parameters ------------------------------

neg_binom_alpha <- los_mean * 0.5
# create LOS est for each indv 
los_indiv <- rnbinom(n = N, size = neg_binom_alpha, prob = 0.5)  #// individual length of stay
los_indiv
end_hosp_t = incid_h_t + los_indiv - 1
census_h_calc = rep(0, T)
for (i in 1:T) {
  census_h_calc[i] = num_matches(incid_h_t, end_hosp_t, i)
}

#think how to validate the estimate.. how many days until the census hosp reaches an appropriate number? 

# transformed data ------------------------------
#empty

# model ------------------------------
# los_mean ~ normal(los_prior, 2); // prior on length of stay
# los_indiv ~ gamma(neg_binom_alpha, 0.5); // gamma for now to allow continious value

# with census_h_cal estimate what value of LOS best fits the data given census_h
#target += normal_lpdf(census_h | census_h_calc, 0.5); // prior on length of stay
 

