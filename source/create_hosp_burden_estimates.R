#note


# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
library(flepicommon)


# source data functions
source("source/data_setup_source.R")

opt <- list()
opt$gt_data_source <- "hhs_hosp"
opt$delphi_api_key <- "04e7369e1541a"
opt$gt_data_path <- "data/nj_covid_hosp.parquet"
opt$gt_ensemble_data_path <- "data/ensemble_lop/2023-04-16-Ensemble_LOP-Inc_Hosp.parquet"

source("source/pull_empirical_data.R")



# LOAD DATA ---------------------------------------------------------------

# only need to run this if want to update data
nj_data <- arrow::read_parquet(opt$gt_data_path)

ensemble_data <- arrow::read_parquet(opt$gt_ensemble_data_path)




# PLOT DATA ---------------------------------------------------------------

# ensemble plot not very useful, not filtered to a state 
# cowplot::plot_grid(
#   nj_data %>%
#     ggplot(aes(x = date, y = incidH, color = pathogen)) +
#     geom_line() +
#     facet_wrap(~source, ncol = 1),
#   nj_ensemble_data %>%
#     group_by(horizon) %>%
#     summarize(mean_val = mean(value)) %>%
#     ggplot(aes(x = horizon, y = mean_val)) +
#     geom_line(),
#   #facet_wrap(~location, ncol = 1)
#   nrow = 2
# )


  nj_data %>%
    ggplot(aes(x = date, y = incidH, color = pathogen)) + 
    geom_line() +
    facet_wrap(~source, ncol = 1)



# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------

# create functions for sampling hospitalization duration 
#  -- currently these are not based on any lit or data -- need to update
covidhosp_stay_funct <- function(n) {
    rpois(n = n, lambda = 6)
}
fluhosp_stay_funct <- function(n) {
    rpois(n = n, lambda = 10)
}

burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct){
    lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH), function(x = X) (0:(x-1)) + date))))
}


# ~ COVID-19 ensemble data --------------------------------------------------------------

# create function for selecting state & scenario
# function requires code for state and scenario id from covid19-scenario-modeling-hub Ensemble_LOP file 2023-04-16
# returns data frame for specific state (ie: NJ) and scenario (ie: A-2023-04-16) with projection date and median incidence hospitalizations
select_parameters <- function(state, scenario){
  parameters_ensemble_data <- ensemble_data %>%
    filter(location == state,
           scenario_id == scenario) %>%
    mutate(date = as_date(origin_date + horizon*7)) %>% 
    group_by(date, scenario_id, target, origin_date, location, type) %>%
    # update var name to incidH for hosp functions
    summarize(incidH = median(value)) %>% 
    # convert horizon to date for burden_est_funct %>% 
    # keeping horizon made issue in create_curr_hosp

    
  return(parameters_ensemble_data)
}


#filter state to NJ, scenario to A 
NJ_A_ensemble_data <- select_parameters(state = "34", scenario = "A-2023-04-16")



# creating current hosp of ensemble data 
# NJ_A_ensemble_data_burden <- list()
# 
# for (i in 1:nrow(NJ_A_ensemble_data)){
#   
#   NJ_A_ensemble_data_burden[[i]] <- NJ_A_ensemble_data[i, ] %>%
#     # need to think about date 
#     rename(admit_date = date) %>%
#     expand_grid(hosp_dates = 
#                   burden_est_funct(incidH = NJ_A_ensemble_data$mdn_incidH[i], 
#                                    date = NJ_A_ensemble_data$date[i], 
#                                    hospstayfunct = covidhosp_stay_funct)
#     )
# }

# NJ_A_ensemble_data_burden_covid <- NJ_A_ensemble_data_burden %>%
#   bind_rows() %>%
#   select(-admit_date, -mdn_incidH) %>%
#   group_by(location, scenario_id, origin_date, type, hosp_dates) %>%
#   summarise(curr_hosp = length(hosp_dates)) %>%
#   ungroup()
# 
# NJ_A_ensemble_data_burden_covid %>%
#   ggplot(aes(x = hosp_dates, y = curr_hosp)) + 
#   geom_line() 
# 

# ~ COVID-19 --------------------------------------------------------------

covid_data <- nj_data %>%
    filter(pathogen == "COVID-19") %>%
    filter(!is.na(incidH) & incidH>0)


# create hosp_dates function for empirical and ensemble data 
# function requires dataframe with incidence Hosp of COVID-19 (empirical or ensemble) 
# returns list with hosp_dates to be used in create_curr_hosp function 
create_hosp_dates <- function(data){
  data_burden <- list()
  
  for (i in 1:nrow(data)){
    
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates = 
                    burden_est_funct(incidH = data$incidH[i], 
                                     date = data$date[i], 
                                     hospstayfunct = covidhosp_stay_funct)
      )
  }
  return(data_burden)
}

nj_data_burden <- create_hosp_dates(data = covid_data) 
NJ_A_ensemble_data_burden <- create_hosp_dates(data = NJ_A_ensemble_data)

# current hospitalization function for empirical and ensemble data 
# function requires data burden list from create_hosp_dates function 
# returns dataframe with current hospitalizations with (empirical or ensemble data)
create_curr_hosp <- function(data_burden){
  new_data_burden <- data_burden %>%
    bind_rows() %>%
    select(-admit_date, -incidH) %>%
    group_by_all() %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()
  return(new_data_burden)
}

nj_data_burden_covid <- create_curr_hosp(data_burden = nj_data_burden)
NJ_A_ensemble_data_burden_covid <- create_curr_hosp(data_burden = NJ_A_ensemble_data_burden)

# visualization

nj_data_burden_covid %>%
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line()

NJ_A_ensemble_data_burden_covid %>%
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line()

# nj_data_burden <- list()
# for (i in 1:nrow(covid_data)){
# 
#   nj_data_burden[[i]] <- covid_data[i, ] %>%
#     rename(admit_date = date) %>%
#     expand_grid(hosp_dates =
#                   burden_est_funct(incidH = covid_data$incidH[i],
#                                    date = covid_data$date[i],
#                                    hospstayfunct = covidhosp_stay_funct)
#     )
# }
# nj_data_burden_covid <- nj_data_burden %>%
#     bind_rows() %>%
#     select(-admit_date, -incidH) %>%
#     group_by(source, FIPS, pathogen, hosp_dates) %>%
#     summarise(curr_hosp = length(hosp_dates)) %>%
#     ungroup()
# 
# nj_data_burden_covid %>%
#   ggplot(aes(x = hosp_dates, y = curr_hosp)) +
#   geom_line()

# ~ Influenza --------------------------------------------------------------

flu_data <- nj_data %>%
    filter(pathogen == "Influenza") %>%
    filter(!is.na(incidH) & incidH>0)

nj_data_burden <- list()
for (i in 1:nrow(flu_data)){
    
    nj_data_burden[[i]] <- flu_data[i, ] %>%
        rename(admit_date = date) %>%
        expand_grid(hosp_dates = 
            burden_est_funct(incidH = flu_data$incidH[i], 
                             date = flu_data$date[i], 
                             hospstayfunct = fluhosp_stay_funct)
        )
}
nj_data_burden_flu <- nj_data_burden %>%
    bind_rows() %>%
    select(-admit_date, -incidH) %>%
    group_by(source, FIPS, pathogen, hosp_dates) %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()



# MERGE INCIDENT AND BURDEN -----------------------------------------------

# combine flu and covid

nj_data_burden <- nj_data_burden_flu %>% 
    bind_rows(nj_data_burden_covid)

# add incident data
nj_data_burden <- nj_data_burden %>%
    rename(date = hosp_dates) %>%
    full_join(nj_data %>% filter(!is.na(incidH) & incidH>0)) %>%
    mutate(incidH = ifelse(is.na(incidH), 0, incidH))

# create combined 
nj_data_burden <- nj_data_burden %>% 
    bind_rows(
        nj_data_burden %>%
            group_by(source, FIPS, date) %>%
            summarise(curr_hosp = sum(curr_hosp, na.rm = TRUE),
                      incidH = sum(incidH, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(pathogen = "Combined"))



# PLOT DATA ---------------------------------------------------------------

cowplot::plot_grid(
    nj_data_burden %>%
        ggplot(aes(x = date, y = incidH, color = pathogen)) + 
        geom_line() +
        theme_bw() +
        theme(legend.position = c(.8,.8)) +
        ggtitle("Daily Hospital Admissions"),
    nj_data_burden %>%
        ggplot(aes(x = date, y = curr_hosp, color = pathogen)) + 
        geom_line() +
        theme_bw() +
        theme(legend.position = c(.8,.8)) +
        ggtitle("Daily Hospital Burden"),
    nrow = 2)





