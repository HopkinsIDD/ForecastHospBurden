#note


# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
#library(flepicommon)
library(gghighlight)



# source data functions
source("source/data_setup_source.R")

opt <- list()
opt$gt_data_source <- "hhs_hosp"
opt$delphi_api_key <- "04e7369e1541a"
opt$gt_data_path <- "data/nj_covid_hosp.parquet"
opt$gt_covid_ensemble_data_path <- "data/ensemble_lop/2023-04-16-Ensemble_LOP-Inc_Hosp.parquet"
opt$gt_flu_ensemble_data_path <- "data/2024-01-06-FluSight-ensemble.parquet"
opt$gt_NJ_total_hosp_data_path <- "data/NJ_total_hosp.parquet"


#source("source/pull_empirical_data.R")



# convert weekly to daily data -- weekly causes weird oscillations

make_daily_data <- function(data = gt_formatted_counts,
                            current_timescale = "week"){
  
  if (current_timescale != "week") stop("Only weeks implemented currently")
  
  data <- data %>%
    # mutate(Update = lubridate::floor_date(Update , "weeks")) %>%
    pivot_longer(cols = starts_with("incid"), names_to = "outcome", values_to = "value") %>%
    # filter(!is.na(value)) %>%
    group_by(across(-c(date, value))) %>%
    # group_by(subpop, age_group, outcome, season) %>%
    arrange(date) %>%
    mutate(value_cum = cumsum(value)) %>%
    ungroup() %>%
    mutate(date_num = as.integer(date))
  
  data %>%
    group_by(across(-c(date, value, value_cum, date_num))) %>%
    group_split() %>%
    map_dfr(~get_spline_daily(grp_dat = .)) %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    dplyr::select(date, subpop, everything(), starts_with("incid"), starts_with("cum"), -date_num) %>%
    ungroup()
}


get_spline_daily <- function(grp_dat) {
  
  # won't run with multiple CI's and scenarios
  smth <- stats::splinefun(x = grp_dat$date_num, y = grp_dat$value_cum, method="monoH.FC")
  preds <- grp_dat %>%
    dplyr::select(-c(date, value, value_cum, date_num)) %>%
    distinct() %>%
    expand_grid(date = seq.Date(min(grp_dat$date), (max(grp_dat$date)+6), by="1 day")) %>%
    mutate(date_num = as.integer(date))
  preds <- preds %>% mutate(value = smth(x = date_num))
  
  preds <- preds %>%
    mutate(outcome = gsub("incid", "cum", outcome)) %>%
    bind_rows(preds %>%
                dplyr::arrange(date, subpop, outcome) %>%
                mutate(value = diff(c(0, value))))
  return(preds)
}







# LOAD DATA ---------------------------------------------------------------

# only need to run this if want to update data
nj_data <- arrow::read_parquet(opt$gt_data_path)

covid_ensemble_data <- arrow::read_parquet(opt$gt_covid_ensemble_data_path)

flu_ensemble_data <- arrow::read_parquet(opt$gt_flu_ensemble_data_path)

# updating flu ensemble colnames

flu_ensemble_data <- flu_ensemble_data %>% 
  rename(origin_date = reference_date,
         type = output_type,
         type_id = output_type_id)

# PLOT DATA ---------------------------------------------------------------

# ensemble plot not very useful, not filtered to a state / secenario 
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
covidhosp_stay_funct <- function(n, x_values) {
  
  stay_durations_list <- list()
  # Loop through each value of x
  for (x in x_values) {
    # Generate hospitalization stay durations for the current value of x
    stay_durations <- rpois(n = n, lambda = x)
    
    # Append the result to the list
    stay_durations_list[[as.character(x)]] <- stay_durations
  }
  
  # Return the list of results
  return(stay_durations_list)
}


covidhosp_stay_funct(100, x_values = c(6,10, 20, 30))

fluhosp_stay_funct <- function(n) {
    rpois(n = n, lambda = 10)
}


burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct){
    lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), function(x = X) (0:(x-1)) + date))))
}


# ~ COVID-19 ensemble data --------------------------------------------------------------

# create function for selecting state & scenario from ensemble data

# function requires code for state and scenario id from covid19-scenario-modeling-hub Ensemble_LOP file 2023-04-16
# returns data frame for specific state (ie: NJ) and scenario (ie: A-2023-04-16) with projection date and median incidence hospitalizations
# shorten horizon so projection goes one month out 
select_parameters <- function(state, scenario){
  parameters_covid_ensemble_data <- covid_ensemble_data %>%
    filter(location == state,
           scenario_id == scenario,
           horizon <= 4) %>%
    # convert horizon to date for burden_est function
    mutate(date = as_date(origin_date + horizon*7)) %>% 
    group_by(date, scenario_id, target, origin_date, location, type) %>%
    # update var name to incidH for hosp functions
    summarize(incidH = median(value)) 

    
  return(parameters_covid_ensemble_data)
}

#filter state to NJ, scenario to A 
NJ_A_covid_ensemble_data <- select_parameters(state = "34", scenario = "A-2023-04-16")


select_parameters_2 <- function(state, data){
  parameters_ensemble_data <- data %>%
    filter(location == state,
           #type_id %in% c(0.500, 0.025, 0.975),
           horizon <= 4) %>%
    # convert horizon to date for burden_est function
    mutate(date = as_date(origin_date + horizon*7)) %>% 
    group_by_all() %>%
    # update var name to incidH for hosp functions
    rename(incidH = value) 
  
  
  return(parameters_ensemble_data)
}


NJ_covid_ensemble_data <- select_parameters_2(state = "34", data = covid_ensemble_data)
NJ_flu_ensemble_data <- select_parameters_2(state = "34", data = flu_ensemble_data)


# creating current hosp of ensemble data 
# NJ_A_covid_ensemble_data_burden <- list()
# 
# for (i in 1:nrow(NJ_A_covid_ensemble_data)){
#   
#   NJ_A_covid_ensemble_data_burden[[i]] <- NJ_A_covid_ensemble_data[i, ] %>%
#     # need to think about date 
#     rename(admit_date = date) %>%
#     expand_grid(hosp_dates = 
#                   burden_est_funct(incidH = NJ_A_covid_ensemble_data$mdn_incidH[i], 
#                                    date = NJ_A_covid_ensemble_data$date[i], 
#                                    hospstayfunct = covidhosp_stay_funct)
#     )
# }

# NJ_A_covid_ensemble_data_burden_covid <- NJ_A_covid_ensemble_data_burden %>%
#   bind_rows() %>%
#   select(-admit_date, -mdn_incidH) %>%
#   group_by(location, scenario_id, origin_date, type, hosp_dates) %>%
#   summarise(curr_hosp = length(hosp_dates)) %>%
#   ungroup()
# 
# NJ_A_covid_ensemble_data_burden_covid %>%
#   ggplot(aes(x = hosp_dates, y = curr_hosp)) + 
#   geom_line() 
# 

# ~ COVID-19 --------------------------------------------------------------

covid_data <- nj_data %>%
    filter(pathogen == "COVID-19") %>%
    filter(!is.na(incidH) & incidH>0)

# ~ Influenza --------------------------------------------------------------

flu_data <- nj_data %>%
  filter(pathogen == "Influenza") %>%
  filter(!is.na(incidH) & incidH>0)

# nj_data_burden <- list()
# for (i in 1:nrow(flu_data)){
  
#   nj_data_burden[[i]] <- flu_data[i, ] %>%
#     rename(admit_date = date) %>%
#     expand_grid(hosp_dates = 
#                   burden_est_funct(incidH = flu_data$incidH[i], 
#                                    date = flu_data$date[i], 
#                                    hospstayfunct = fluhosp_stay_funct)
#     )
# }
# nj_data_burden_flu <- nj_data_burden %>%
#   bind_rows() %>%
#   select(-admit_date, -incidH) %>%
#   group_by(source, FIPS, pathogen, hosp_dates) %>%
#   summarise(curr_hosp = length(hosp_dates)) %>%
#   ungroup()
# 

# ~ Influenza Ensemble Data --------------------------------------------------

select_flu_parameters <- function(state){
  parameters_flu_ensemble_data <- flu_ensemble_data %>%
    filter(location == state) %>%
    # convert horizon to date for burden_est function
    mutate(date = as_date(reference_date + horizon*7)) %>% 
    group_by(date, target, reference_date, location, output_type) %>%
    # update var name to incidH for hosp functions
    summarize(incidH = median(value)) 
  
  
  return(parameters_flu_ensemble_data)
}

#filter state to NJ
# new select parameters function 
# NJ_flu_ensemble_data <- select_flu_parameters(state = "34")

# ~ Functions for Empirical and Ensemble data --------------------------------------

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
                                     # think about how to write this better 
                                     #fluhosp_stay_funct
                                     hospstayfunct = covidhosp_stay_funct
                                     )
      
      )
  }
  return(data_burden)
}


create_hosp_dates_flu <- function(data){
  data_burden <- list()
  
  for (i in 1:nrow(data)){
    
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates = 
                    burden_est_funct(incidH = data$incidH[i], 
                                     date = data$date[i], 
                                     # think about how to write this better 
                                     #fluhosp_stay_funct
                                     hospstayfunct = fluhosp_stay_funct
                    )
                  
      )
  }
  return(data_burden)
}
# think about how to write this better 
#fluhosp_stay_funct

# if ( data == "NJ_flu_ensemble_data" | data == "flu_data") {
#   return(hospstayfunct = fluhosp_stay_funct)
# } else{
#   return(hospstayfunct = covidhosp_stay_funct)
# }


# current hospitalization function for empirical and ensemble data 
# function requires data burden list from create_hosp_dates function 
# returns dataframe with current hospitalizations with (empirical or ensemble data)
create_curr_hosp <- function(data_burden){
  new_data_burden <- data_burden %>%
    bind_rows() %>%
    as_tibble() %>%
    dplyr::select(-admit_date, -incidH) %>%
    group_by_all() %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()
  return(new_data_burden)
}



# Make data daily 

## covid

## add week 0 function for covid & flu 
create_week_0 <- function(data){
    
  data_daily_1 <- data %>% 
      group_by(across(-c(contains("date"), contains("horizon"), starts_with("incid")))) %>%
      mutate(first_date = min(date)) %>% 
      ungroup() %>%
      filter(date == first_date & incidH != 0) %>%
      mutate(date = date - 7, incidH = 0) %>% 
      select(-first_date) 
  
  return(data_daily_1)
}

add_week_0_data <- function(data_daily_1, data){
  data_daily <- data_daily_1 %>% rbind(data) %>% 
    rename(subpop = location)
  return(data_daily)
}

NJ_flu_ensemble_data_daily_1 <- create_week_0(data = NJ_flu_ensemble_data) %>% mutate(target_end_date = date)
NJ_covid_ensemble_data_daily_1 <- create_week_0(data = NJ_covid_ensemble_data) %>% mutate(target_end_date = date)

NJ_flu_ensemble_data_daily <- add_week_0_data(data_daily_1 = NJ_flu_ensemble_data_daily_1, data = NJ_flu_ensemble_data %>% mutate(target_end_date = date))
NJ_covid_ensemble_data_daily <- add_week_0_data(data_daily_1 = NJ_covid_ensemble_data_daily_1, data = NJ_covid_ensemble_data %>% mutate(target_end_date = date))



## fill in values for each day

# NJ_A_ensemble_data_daily <- make_daily_data(data = NJ_A_ensemble_data_daily, current_timescale = "week") 
# NJ_A_ensemble_data_daily <- NJ_A_ensemble_data_daily %>% filter(!(date %in% NJ_A_ensemble_data_daily_1$date)) %>%
#     rename(location = subpop)

NJ_covid_ensemble_data_daily <- make_daily_data(data = NJ_covid_ensemble_data_daily %>% dplyr::select(-starts_with("target_end_date"), -starts_with("horizon")), current_timescale = "week") 
NJ_covid_ensemble_data_daily <- NJ_covid_ensemble_data_daily %>% filter(!(date %in% NJ_covid_ensemble_data_daily_1$date)) %>%
  rename(location = subpop)

NJ_flu_ensemble_data_daily <- make_daily_data(data = NJ_flu_ensemble_data_daily %>% dplyr::select(-starts_with("target_end_date"), -starts_with("horizon")), current_timescale = "week") 
NJ_flu_ensemble_data_daily <- NJ_flu_ensemble_data_daily %>% filter(!(date %in% NJ_flu_ensemble_data_daily_1$date)) %>%
  rename(location = subpop)

# gt_formatted_counts <- gt_formatted_counts %>% 
#   dplyr::group_by(subpop, age_group) %>%
#   tidyr::complete(date = seq.Date(min(date), max(date), by="day")) %>%  # fill in missing days
#   # mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # set outcome variables values on those days to zero
#   mutate_if(is.numeric, ~ rollmean(., 7, fill = "extend", align ='left')) # calculate rolling seven day average

#write.csv(gt_formatted, "data/rsvnet_sample_data.csv", row.names = FALSE)

# Check hospitalization counts timeseries

ggplot() + 
    geom_line(data = 
                  NJ_covid_ensemble_data %>% 
                  mutate(type_id = as.character(type_id)),
              aes(date, incidH, colour = type_id, linetype = "weekly")) +
        geom_line(data = 
                  NJ_covid_ensemble_data_daily %>% 
                  mutate(type_id = as.character(type_id)),
                  aes(date, incidH, colour = type_id, linetype = "daily"),) +
    facet_wrap(~scenario_id, scales = 'free_y') +
    ylab('daily hosp counts')+
    theme_bw() 

NJ_covid_ensemble_data_daily %>% 
  mutate(type_id = as.character(type_id)) %>%
  ggplot() + 
  geom_line(aes(date, incidH, colour = type_id)) +
  facet_wrap(~scenario_id, scales = 'free_y') +
  ylab('daily hosp counts')+
  theme_bw() 

NJ_covid_ensemble_data %>% 
  mutate(type_id = as.character(type_id)) %>%
  ggplot() + 
  geom_line(aes(date, incidH, colour = type_id)) +
  facet_wrap(~scenario_id, scales = 'free_y') +
  ylab('daily hosp counts')+
  theme_bw() 

NJ_flu_ensemble_data_daily %>% 
  mutate(type_id = as.character(type_id)) %>%
  ggplot() + 
  geom_line(aes(date, incidH, colour = type_id)) +
  # facet_wrap(~subpop, scales = 'free_y') +
  ylab('daily hosp counts')+
  theme_bw() 

NJ_flu_ensemble_data %>% 
  mutate(type_id = as.character(type_id)) %>%
  ggplot() + 
  geom_line(aes(date, incidH, colour = type_id)) +
  # facet_wrap(~subpop, scales = 'free_y') +
  ylab('daily hosp counts')+
  theme_bw() 


# Calculate burden --------------------------------------------------------

## covid
nj_covid_data_burden_1 <- create_hosp_dates(data = covid_data) 
#NJ_A_ensemble_data_burden <- create_hosp_dates(data = NJ_A_ensemble_data_daily %>% dplyr::select(-cumH))
NJ_covid_ensemble_data_burden_1 <- create_hosp_dates(data = NJ_covid_ensemble_data_daily %>% dplyr::select(-cumH))

nj_covid_data_burden <- create_curr_hosp(data_burden = nj_covid_data_burden_1)
#NJ_A_ensemble_data_burden_covid <- create_curr_hosp(data_burden = NJ_A_ensemble_data_burden)
NJ_covid_ensemble_data_burden <- create_curr_hosp(data_burden = NJ_covid_ensemble_data_burden_1)

## flu 

nj_flu_data_burden_1 <- create_hosp_dates_flu(data = flu_data) 
NJ_flu_ensemble_data_burden_1 <- create_hosp_dates_flu(data = NJ_flu_ensemble_data_daily %>% dplyr::select(-cumH))

nj_flu_data_burden <- create_curr_hosp(data_burden = nj_flu_data_burden_1)
NJ_flu_ensemble_data_burden <- create_curr_hosp(data_burden = NJ_flu_ensemble_data_burden_1)

# Calculate weekly burden --------------------------------------------------------

NJ_covid_ensemble_weekly_data_burden_1 <- create_hosp_dates(data = NJ_covid_ensemble_data)
NJ_covid_ensemble_weekly_data_burden <- create_curr_hosp(data_burden = NJ_covid_ensemble_weekly_data_burden_1)

NJ_covid_empirical_weekly_data_burden_1 <- create_hosp_dates(data = covid_data)
NJ_covid_empirical_weekly_data_burden <- create_curr_hosp(data_burden = NJ_covid_empirical_weekly_data_burden_1)

write_parquet(NJ_covid_ensemble_weekly_data_burden,  "data/hosp_burden/NJ_covid_ensemble_weekly_data_burden.parquet")
write_parquet(NJ_covid_empirical_weekly_data_burden,  "data/hosp_burden/NJ_covid_empirical_weekly_data_burden.parquet")


# visualization
## covid
nj_covid_data_burden %>%
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line()

NJ_covid_ensemble_data_burden %>%
  mutate(type_id = as.character(type_id)) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp, colour = type_id)) +
  geom_line() + 
  facet_wrap(~scenario_id, scales = 'free_y')

## flu 
nj_flu_data_burden %>%
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line()

NJ_flu_ensemble_data_burden %>%
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


# MERGE INCIDENT AND BURDEN -----------------------------------------------

# combine flu and covid

nj_data_burden <- nj_flu_data_burden %>% 
    bind_rows(nj_covid_data_burden)

# combine covid 
# add dates truncated to one month before covid projection
nj_covid_data_burden <- nj_covid_data_burden %>% 
  #filter(between(hosp_dates, as.Date('2023-03-01'), as.Date('2023-04-16'))) %>% 
  mutate(type_id = "empirical",
         scenario_id = "empirical") 

nj_covid_merge <- NJ_covid_ensemble_data_burden %>% 
  mutate(type_id = as.character(type_id),
         scenario_id = as.character(scenario_id)) %>% 
  bind_rows(nj_covid_data_burden)

# combine flu
nj_flu_data_burden <- nj_flu_data_burden %>% 
  filter(between(hosp_dates, as.Date('2023-12-06'), as.Date('2024-01-06'))) %>% 
  mutate(type_id = "empirical") 

nj_flu_merge <- NJ_flu_ensemble_data_burden %>% 
  mutate(type_id = as.character(type_id)) %>% 
  bind_rows(nj_flu_data_burden)

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

#note: will need to add pathogen column for covid vs flu 

# PLOT DATA ---------------------------------------------------------------

cowplot::plot_grid(
    nj_data_burden %>%
        ggplot(aes(x = date, y = incidH, color = pathogen)) + 
        geom_line() +
        theme_bw() +
        theme(legend.position = c(.9,.5)) +
        ggtitle("Empirical Daily Hospital Admissions"),
    nj_data_burden %>%
        ggplot(aes(x = date, y = curr_hosp, color = pathogen)) + 
        geom_line() +
        theme_bw() +
        theme(legend.position = c(.9,.5)) +
        ggtitle("Empirical Daily Hospital Burden"),
    NJ_covid_ensemble_data_burden %>%
      mutate(type_id = as.character(type_id)) %>% 
      ggplot(aes(x = hosp_dates, y = curr_hosp, color = type_id)) +
      geom_line() +
      theme(legend.position = c(.8,.8)) +
      ggtitle("Ensemble Daily COVID Hospital Burden"),
    NJ_flu_ensemble_data_burden %>%
      ggplot(aes(x = hosp_dates, y = curr_hosp, color = type_id)) +
      geom_line() +
      theme(legend.position = c(.8,.8)) +
      ggtitle("Ensemble Daily Flu Hospital Burden"),
    nrow = 4)

nj_covid_merge %>% 
  mutate(hosp_dates = as.Date(hosp_dates)) %>% 
  filter(between(hosp_dates, as.Date('2023-04-16'), as.Date(Sys.Date()))) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line(aes(color = type_id, linetype = scenario_id)) +
  ggtitle("Empirical and Ensemble Covid Hospital Burden")

nj_covid_merge %>% 
  mutate(type_id = as.character(type_id),
         hosp_dates = as.Date(hosp_dates)) %>% 
  filter(between(hosp_dates, as.Date('2023-04-16'), as.Date(Sys.Date()))) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line(aes(color = type_id, linetype = scenario_id)) + 
  gghighlight(type_id == "empirical" & scenario_id == "empirical",
              unhighlighted_params = list(linewidth = 0.3,
                                          colour = alpha("darkblue", 0.7),
                                          linetype = "dotted"),
              use_direct_label = FALSE) +
  ggtitle("Empirical and Ensemble Covid Hospital Burden")

nj_flu_merge %>% 
  mutate(hosp_dates = as.Date(hosp_dates)) %>% 
  filter(between(hosp_dates, as.Date('2023-12-24'), as.Date(Sys.Date()))) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line(aes(color = type_id)) +
  ggtitle("Empirical and Ensemble Flu Hospital Burden")

nj_flu_merge %>% 
  mutate(hosp_dates = as.Date(hosp_dates)) %>% 
  filter(between(hosp_dates, as.Date('2023-12-24'), as.Date(Sys.Date()))) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line(aes(color = type_id)) + 
  gghighlight(type_id == "empirical",
              unhighlighted_params = list(linewidth = 0.3,
                                          colour = alpha("darkblue", 0.7),
                                          linetype = "dotted"),
              use_direct_label = FALSE) +
  ggtitle("Empirical and Ensemble Flu Hospital Burden")

max_covid_values <- nj_covid_merge %>% 
  group_by(scenario_id, type_id) %>% 
  summarize(max(curr_hosp))

max_flu_values <- nj_flu_merge %>% 
  group_by(type_id) %>% 
  summarize(max(curr_hosp))

## creating new viz with truncated dates

nj_covid_merge %>% 
  mutate(hosp_dates = as.Date(hosp_dates)) %>% 
  filter(between(hosp_dates, as.Date('2023-04-01'), as.Date('2023-05-16'))) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line(aes(color = type_id, linetype = scenario_id)) +
  ggtitle("Empirical and Ensemble Covid Hospital Burden")

nj_flu_merge %>% 
  mutate(hosp_dates = as.Date(hosp_dates)) %>% 
  filter(between(hosp_dates, as.Date('2023-11-01'), as.Date('2024-01-27'))) %>% 
  ggplot(aes(x = hosp_dates, y = curr_hosp)) +
  geom_line(aes(color = type_id)) +
  ggtitle("Empirical and Ensemble Flu Hospital Burden")

ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue") +
  geom_line(aes(y = y2), color = "red") +
  geom_ribbon(aes(ymin = y1, ymax = y2), fill = "gray", alpha = 0.3) +
  labs(x = "X", y = "Y") +
  theme_minimal()

# filter to 97.5% CI 

upper_covid_burden <- NJ_covid_ensemble_data_burden %>% 
  filter(type_id ==0.975 & scenario_id == "B-2023-04-16")
# filter to 2.5% CI 
lower_covid_burden <- NJ_covid_ensemble_data_burden %>% 
  filter(type_id ==0.025 & scenario_id == "B-2023-04-16")


