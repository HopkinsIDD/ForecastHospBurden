
## ESTIMATE VACCINATION RATE AND OUTCOMES BY STATE ##

# METHODS
# 1. Get vacc data, clean to match population data and severity estimator
# 2. Transform vacc data to monthly age proportions vaccinated and unvacc size
# 3. Project forward monthly age props and vacc
# 4. Apply severity estimator monthly to real data


library(tidyverse)
library(uscovid19vacc)
source("R/scripts/config_writers/vaccination_est_funcs.R")

#_________________________________________________________
## MOVE THESE THINGS TO CONFIG WRITER  -------------------------------------------------------------------

# Booster scenarios
booster_limits <- c(.95) # proportion of ADULTS previously vaccinated with 2 doses getting booster

round_num = 18
date_sub = lubridate::as_date("2024-04-28")
projection_start_date <- lubridate::as_date("2024-04-28")
projection_end_date <- lubridate::as_date(projection_start_date + (52*7*5))-1
proj_vacc_date <- lubridate::as_date(projection_start_date + (52*7*5))-1

sim_states <- c(state.abb, "DC")

save_dir <- file.path("model_input/vaccination", paste0("Round", round_num))

# Age stratumn 
# - This should not be changed anymore. To model different age groups, 
#    rates and numbers from these groups will be aggregated.
# - More defined age groups are possible, but not likely needed for SMH and FChub
age_groups <- c("0_4", "5_11", "12_17", "18_64", "65_100")
age_groups_model <- c("0_17", "18_64", "65_100")


# Vaccination start dates
vacc_start_dates <- bind_rows(
  tibble(
    dose = "dose1",
    age_group = age_groups,
    date_start = lubridate::as_date(c("2022-07-01", "2021-11-03", "2021-05-10", "2020-12-10", "2020-12-10"))
  ),
  tibble(
    dose = "dose3",
    age_group = age_groups,
    date_start = lubridate::as_date(c("2023-02-01", "2022-05-17", "2022-01-03", "2021-09-22", "2021-09-22"))
  ),
  tibble(
    dose = "dose4",
    age_group = age_groups,
    date_start = lubridate::as_date(c("2023-06-01", "2022-11-17", "2022-09-01", "2022-03-29", "2022-03-29"))
  ),
  tibble(
    dose = "dose5", #bivalent booster, Fall 2022
    age_group = age_groups,
    date_start = lubridate::as_date(c("2023-03-14", "2022-10-12", "2022-09-11", "2022-09-11", "2022-09-11"))
  ),
  tibble(
    dose = "dose6", #new bivalent booster, Sep 2023
    age_group = age_groups,
    date_start = lubridate::as_date(c("2023-09-01", "2023-09-01", "2023-09-01", "2023-09-01", "2023-09-01"))
  )
)
vacc_start_dates <- bind_rows(
  vacc_start_dates, 
  vacc_start_dates %>% filter(dose == "dose1") %>% mutate(dose = "dose2", date_start = date_start+21))


vacc_start_dates_model <- bind_rows(
  tibble(
    dose = "dose1",
    age_group = age_groups_model,
    date_start = lubridate::as_date(c("2021-05-10", "2020-12-10", "2020-12-10"))
  ),
  tibble(
    dose = "dose3",
    age_group = age_groups_model,
    date_start = lubridate::as_date(c("2022-01-03", "2021-09-22", "2021-09-22"))
  ),
  tibble(
    dose = "dose4",
    age_group = age_groups_model,
    date_start = lubridate::as_date(c("2022-09-01", "2022-03-29", "2022-03-29"))
  ),
  tibble(
    dose = "dose5", # bivalent booster, Fall 2022
    age_group = age_groups_model,
    date_start = lubridate::as_date(c("2022-09-11", "2022-09-11", "2022-09-11"))
  ),
  tibble(
    dose = "dose6", #new bivalent booster, Sep 2023
    age_group = age_groups_model,
    date_start = lubridate::as_date(c("2023-09-01", "2023-09-01", "2023-09-01"))
  )
)
vacc_start_dates_model <- bind_rows(
  vacc_start_dates_model, 
  vacc_start_dates_model %>% filter(dose == "dose1") %>% mutate(dose = "dose2", date_start = date_start+21))


# Sources
# FIRST DOSES
# - https://www.fda.gov/news-events/press-announcements/fda-takes-key-action-fight-against-covid-19-issuing-emergency-use-authorization-first-covid-19
# - 12-15: https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-authorizes-pfizer-biontech-covid-19-vaccine-emergency-use
# BOOSTER 1
# - https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-authorizes-additional-vaccine-dose-certain-immunocompromised
# - [ALL adults] https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-expands-eligibility-covid-19-vaccine-boosters
# - FDA authorization: https://www.fda.gov/news-events/press-announcements/fda-authorizes-booster-dose-pfizer-biontech-covid-19-vaccine-certain-populations
# - 5-11: https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-expands-eligibility-pfizer-biontech-covid-19-vaccine-booster-dose
# - 12-15: https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-takes-multiple-actions-expand-use-pfizer-biontech-covid-19-vaccine
# - 16-17: https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-expands-eligibility-pfizer-biontech-covid-19-booster-dose-16-and-17 (Dec 9, 2021)
# BOOSTER 2
# - https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-authorizes-second-booster-dose-two-covid-19-vaccines-older-and
# - under 5: https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-fda-authorizes-bivalent-pfizer-biontech-covid-19-vaccine-booster-dose

# SETUP -------------------------------------------------------------------

# Saving Directory and Filenames
dir.create(file.path(save_dir), recursive=TRUE, showWarnings = FALSE)

# devtools::install_github("HopkinsIDD/globaltoolboxlite")
# devtools::install_github("HopkinsIDD/uscovid19vacc", force=TRUE)
# devtools::install_github("HopkinsIDD/covidSeverity", force=TRUE)
library(tidyverse)
library(lubridate)
library(uscovid19vacc)
# library(globaltoolboxlite)
source("R/scripts/config_writers/vaccination_est_funcs.R")
source("R/scripts/vacc_funcs_temp.R")

census_api_key <- "c235e1b5620232fab506af060c5f8580604d89c1"

# DATA --------------------------------------------------------------------

state_pop <- read_csv("data/geodata_2019.csv")
loc_dictionary <- read_csv("https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/data-locations/locations.csv")

# 1. Pop Age Proportions - By state and US ------------------------------------------

state_pop_agestrat <- get_state_pop_agestrat(
  round_num = round_num,
  age_groups = c("0_4", "5_11", "12_17", "18_64", "65_100"),
  state_pop_file = "data/geodata_2019.csv",
  state_pop_1yr_file = "data/state_age_pops.csv") %>% suppressMessages()
# note: population = total state population, pop2019 = age specific population 

state_pop_agestrat_model <- get_state_pop_agestrat(
  round_num = round_num,
  age_groups = c("0_17", "18_64", "65_100"),
  state_pop_file = "data/geodata_2019.csv",
  state_pop_1yr_file = "data/state_age_pops.csv") %>% suppressMessages()

state_pop_agestrat_vacc <- get_state_pop_agestrat(
  round_num = round_num,
  age_groups = c("0_17", "18_49", "50_64", "65_100"),
  state_pop_file = "data/geodata_2019.csv",
  state_pop_1yr_file = "data/state_age_pops.csv") %>% suppressMessages()

# CDC age groups
# State Populations - 1 year age
state_pop_1yr <- readr::read_csv("data/state_age_pops.csv") %>%
  rename(subpop = location, NAME = location_name, pop = population) %>%
  mutate(subpop = stringr::str_pad(subpop, width = 5, side ="right", pad="0")) %>%
  select(-state_population) 
state_pop_1yr <- state_pop_1yr %>% 
  bind_rows(state_pop_1yr %>%
              group_by(age) %>%
              summarise(pop = sum(pop)) %>%
              mutate(subpop="00000", NAME="United States") %>%  as_tibble()) %>% 
  right_join(state_pop)%>%
  mutate(age_l = age, age_r = age) %>%
  mutate(age_r = ifelse(age_l==85, 100, age_r))
# pop = age population, popoulation = total state populatipon

state_pop_agestrat_cdc <- get_state_pop_agestrat_cdc(state_pop_1yr) %>% suppressMessages()

# US Age groups
us_pop_agestrat <- state_pop_agestrat %>%
  group_by(age_group, age_l, age_r) %>%
  summarise(pop = sum(population, na.rm=TRUE),
            pop2019 = sum(pop2019)) %>%
  mutate(USPS="US") %>% arrange(as.integer(age_l))

us_pop_agestrat_model <- state_pop_agestrat_model %>%
  group_by(age_group, age_l, age_r) %>%
  summarise(pop = sum(population, na.rm=TRUE),
            pop2019 = sum(pop2019)) %>%
  mutate(USPS="US") %>% arrange(as.integer(age_l))
# pop = total US population, pop2019 = age specific population

## ~ Add risk group for 18-64 -----------------

prop_highrisk <- read_csv("data/prop_highrisk.csv") %>% rename(NAME = location_name) %>%
  left_join(state_pop_agestrat_vacc %>% filter(age_group %in% c("18_49", "50_64"))) %>% 
  na.omit() %>% 
  mutate(high_risk = round(prop_highrisk/100 * pop2019),
         low_risk = pop2019 - high_risk)

state_pop_agestrat_withrisk <- state_pop_agestrat_vacc %>% 
  mutate(risk_group = "overall") %>%
  bind_rows(prop_highrisk %>% pivot_longer(cols  = c("high_risk", "low_risk"), names_to = "risk_group") %>%
              mutate(pop = value, pop2019 = value) %>% 
              mutate(prop_risk = ifelse(risk_group == "high_risk", prop_highrisk/100, 1 - prop_highrisk/100)) %>%
              mutate(prop = prop * prop_risk) %>%
              select(USPS, subpop, NAME, population, age_group, age_l, age_r, pop, prop, pop2019, risk_group)) 
  

## PULL AND PROCESS ALL VACC DATA

if (repull_all_vacc){
  
  # 2. Vacc Age Proportions - By State and US -----------------------------------
  
  # things to fix:
  # - set minimum age by date
  # - better IFR and Hosp adjustment
  # - stan to fit
  
  
  # ~ CDC Data --------------------------------------------------------------
  # Old data: for 2020-12-13 to 2023-05-10
  # vacc_cdc <- read_csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD")
  
  # Changes to data for 2023 - 2024 season:
  # Cumulative Percentage of Adults 18 Years and Older Vaccinated with the Updated 2023-24 COVID-19 Vaccine
  # https://data.cdc.gov/Vaccinations/Cumulative-Percentage-of-Adults-18-Years-and-Older/hm35-qkiu/about_data
  # Weekly Cumulative Percentage of Adults 18 Years and Older Vaccinated with the Updated 2023-24 COVID-19 Vaccine by Jurisdiction
  # https://data.cdc.gov/Vaccinations/Weekly-Cumulative-Percentage-of-Adults-18-Years-an/sjpm-fk4b/about_data
  # Monthly Cumulative Number and Percent of Persons Who Received 1+ updated 2023-24 COVID-19 Vaccination Doses by Age Group and Jurisdiction, United States
  # https://data.cdc.gov/Vaccinations/Monthly-Cumulative-Number-and-Percent-of-Persons-W/vugp-mqip/about_data
  
  # vacc_cdc_cumul_perc <- read_csv("https://data.cdc.gov/api/views/vugp-mqip/rows.csv?accessType=DOWNLOAD") %>%
  #   rename(location_name = Jurisdiction) %>%
  #   setDT() %>% 
  #   .[loc_dictionary, on = .(location_name = location_name)] %>%
  #   na.omit()
  
  # ~ from SMH --------------------------------------------------------------
  # Vaccine coverage from 2023-24 season is PROVIDED in scenarios
  
  # Read in coverage curves from SMH repo
  # download.file("https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/round_resources/round18/COVID_RD18_Vaccination_curves.csv", 
  #               destfile = "model_input/vaccination/Round18/smh_r18_vacc_curves.csv", method = "curl")
  vacc_curves <- read_csv("model_input/vaccination/Round18/smh_r18_vacc_curves.csv")
  
  # use this vaccine coverage for 2023 - 2024 as well (as this is what projections are indexed on)
  
  age_key <- data.frame(Age = unique(vacc_curves$Age),
                        age_group = c("0_17","18_49","50_64","65_100","0_2",
                                      "0_4","5_11","5_17","12_17"))
  
  # pop_riskgroup <- prop_highrisk %>% pivot_longer(cols = c("pop", "high_risk", "low_risk")) %>% 
  #   select(subpop, name, age_group, value) %>% 
  #   mutate(name = ifelse(name == "pop", "overall", name)) %>%
  #   mutate(Risk_group = case_when(
  #     name == "overall" ~ "Overall",
  #     name == "high_risk" ~ "High risk",
  #     name == "low_risk" ~ "Low risk"
  #   )) 
  state_pop_agestrat_withrisk_ <- state_pop_agestrat_withrisk %>% 
    mutate(Risk_group = case_when(
          risk_group == "overall" ~ "Overall",
          risk_group == "high_risk" ~ "High risk",
          risk_group == "low_risk" ~ "Low risk"
        ))
    
  vacc_curves_ <- vacc_curves %>% 
    filter(Geography != "National") %>%
    mutate(epiweek = MMWRweek::MMWRweek(Date)$MMWRweek,
           year = MMWRweek::MMWRweek(Date)$MMWRyear,
           day = MMWRweek::MMWRweek(Date)$MMWRday) %>%
    mutate(year = year - 1) %>%
    mutate(date = MMWRweek::MMWRweek2Date(year, epiweek, day)) %>%
    left_join(age_key) %>% 
    filter(age_group %in% c("0_17","18_49","50_64","65_100")) %>%
    # group_by(Geography, Date) %>%
    # mutate(flag = ifelse(Risk_group == "Overall", 1, 0),
    #        total_pop_sum = sum(Pop*flag)) %>%
    separate(age_group, into=c("age_l", "age_r"), sep="_", remove = FALSE) %>%
    rename(NAME = Geography) %>%
    left_join(state_pop_agestrat_withrisk_ %>% select(USPS, subpop, NAME, age_group, pop, risk_group, Risk_group), 
              by = c("NAME", "Risk_group", "age_group")) %>%
    mutate(dose = "dose6",
           age_group = paste0(age_l,"_",age_r),
           vacc_age = Cum.Coverage.Percent * pop,
           prop_vacc_age = Cum.Coverage.Percent)  %>% 
    select(date, risk_group, USPS, dose, age_group, age_l, age_r, pop, population, dose, vacc_age, prop_vacc_age)
  
  # Quick check
  cowplot::plot_grid(
    vacc_curves_ %>% filter(USPS=="NH") %>% filter(dose =="dose6") %>%
      ggplot(aes(date, prop_vacc_age, color=age_group, linetype = risk_group)) + #geom_point() + 
      geom_line() + scale_y_continuous(breaks=seq(0,1.2,.1)),
    vacc_curves_ %>% filter(USPS=="NH") %>% filter(dose =="dose6") %>%
      ggplot(aes(date, vacc_age, color=age_group, linetype = risk_group)) + #geom_point() + 
      geom_line(),
    nrow=1)
  
  vacc_curves_ %>%
    ggplot(aes(date, prop_vacc_age, color=age_group, linetype = risk_group)) + #geom_point() + 
    geom_line() + 
    facet_wrap(~USPS, scales = 'free_y') + scale_y_continuous(breaks=seq(0,1.2,.1))
  
  # fit spline and turn weekly into daily ------------
  
  # for each location-age combination, fit a spline to the points to get the daily cumulatives
  fits <-  vacc_curves_ %>%
    mutate(loc_age = paste(USPS, age_group, risk_group, sep="-")) %>%
    group_by(loc_age) %>% distinct() %>% filter(date == max(date))
  nrow(fits)
  sum(duplicated(fits$loc_age)) # make sure this is 0
  locs <- unique(fits$loc_age)
  n_locs <- length(locs)
  
  # proj_days_ <- tibble(date = as_date(min(vacc_curves_$date):end_date_groundtruth)) %>%
  proj_days_ <- tibble(date = as_date(sim_start:end_date_groundtruth)) %>%
    mutate(day_num = as.integer(date - min(date)+1))
  
  # vacc_curves_spline <- vacc_curves_ %>%
  #   mutate(day_num = as.integer(date - min(date)+1)) %>%
  #   mutate(loc_age = paste(USPS, age_group, dose, sep="-"))
  vacc_curves_spline <- c()
  
  for(i in 1:n_locs){
    d_proj <- proj_days_$day_num
    tmp_ <- vacc_curves_ %>% #filter(date >= proj_start-10) %>%   
      mutate(loc_age = paste(USPS, age_group, risk_group, sep="-")) %>%
      filter(loc_age == locs[i])
    dat_ <- tmp_ %>%
      # bind_rows(tibble(date = as_date("2023-09-01"), 
      #                  prop_vacc_age = 0, loc_age = locs[i],
      #                  vacc_age = 0, USPS = str_split(locs[i],"-")[[1]][1], age_group = str_split(locs[i],"-")[[1]][2],
      #                  dose = str_split(locs[i],"-")[[1]][3],
      #                  pop = tmp_$pop[1],age_l = tmp_$age_l[1], age_r = tmp_$age_r[1])) %>%
      arrange(date) %>%
      # mutate(day_num = as.integer(date - min(date)+1)) %>%
      mutate(day_num = as.integer(date - sim_start+1)) 
    dates_ <- dat_$date
    d2 <- dat_$day_num
    y2 <- dat_$prop_vacc_age
    #fit_spline <- smooth.spline(d2, y2, spar=.35)
    fit_spline <- splinefun(d2, y2, #method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"),
                            method = "fmm", ties = mean)
    d3 <- min(d2):max(d2)
    y3 <- fit_spline(d3)
    
    if(any(y3<0)){
      fit_spline <- splinefun(d2, y2, method = "monoH.FC", ties = mean)
      d3 <- min(d2):max(d2)
      y3 <- fit_spline(d3)
    }
    
    plot(d2, y2)
    lines(d3, y3)
    
    fit_spline <- tibble(date = as_date(min(dates_):max(dates_)), prop_vacc_age = y3, day_num = d3, loc_age = locs[i]) %>%
      mutate(vacc_age_daily_rate = prop_vacc_age - lag(prop_vacc_age, default = NA)) %>%
      full_join(dat_ %>% dplyr::select(USPS, loc_age, age_group, age_l, age_r, risk_group, dose, pop) %>% distinct())
    
    vacc_curves_spline <- vacc_curves_spline %>% 
      bind_rows(fit_spline) 
    
  }
  
  # for overall 
  vacc_curves_spline_ <- vacc_curves_spline %>%
    filter(risk_group == "overall") %>%
    mutate(vacc_age_daily_rate = ifelse(is.na(vacc_age_daily_rate), prop_vacc_age, vacc_age_daily_rate)) %>%
    mutate(vacc_age_daily_rate = ifelse(vacc_age_daily_rate < 0, 0, vacc_age_daily_rate)) %>%
    arrange(USPS, date, age_group) %>%
    mutate(vacc_age = prop_vacc_age * pop,
           vacc_age_daily = vacc_age_daily_rate * pop) %>%
    distinct() %>%
    group_by(USPS,date) %>%
    mutate(total_state_pop = sum(pop)) %>%
    group_by(USPS,date, age_group, age_r, age_l)
  
  vacc_curves_spline_18_65 <- vacc_curves_spline %>%
    filter(age_group %in% c("18_49","50_64")) %>%
    mutate(vacc_age_daily_rate = ifelse(is.na(vacc_age_daily_rate), prop_vacc_age, vacc_age_daily_rate)) %>%
    mutate(vacc_age_daily_rate = ifelse(vacc_age_daily_rate < 0, 0, vacc_age_daily_rate)) %>%
    arrange(USPS, date, age_group, risk_group) %>%
    mutate(vacc_age = prop_vacc_age * pop,
           vacc_age_daily = vacc_age_daily_rate * pop) %>%
    distinct() %>%
    left_join(vacc_curves_spline_ %>% ungroup() %>%
                dplyr::select(USPS, total_state_pop) %>% 
                distinct(), by = "USPS")
  
  vacc_curves_spline_ <- vacc_curves_spline_ %>%
    bind_rows(vacc_curves_spline_18_65)
  
  # check 
  ggplot(vacc_curves_spline_) + geom_line(aes(date, vacc_age_daily, colour = age_group, linetype = risk_group)) + facet_wrap(~USPS, scales = 'free_y')
  ggplot(vacc_curves_spline_) + geom_line(aes(date, prop_vacc_age, colour = age_group, linetype = risk_group)) + facet_wrap(~USPS, scales = 'free_y')
  
  
  # ~ Revise Prop_vacc_age --------------------------------------------------
  
  vacc_curves_r1 <- vacc_curves_spline_ %>% 
    select(date, USPS, age_group, age_l, age_r, risk_group,pop, total_state_pop,
           vacc_age, vacc_age_daily, prop_vacc_age) %>%
    rename(dose = risk_group) %>%
    distinct() %>%
    mutate(age_l = as.integer(age_l), age_r=as.integer(age_r)) %>%
    # left_join(state_pop_agestrat_vacc %>% rename(pop_prop = prop) %>% mutate(age_l = as.integer(age_l), age_r=as.integer(age_r))) %>%
    group_by(date, USPS, age_group) %>%
    mutate(prop_vacc_age = round(vacc_age / pop, 4)) 
  
  
  # ~ Revise proportion of vacc ---------------------------------------------
  
  # add extra date at start date
  extra_dates <- expand.grid(date = lubridate::as_date(as_date(sim_start):as_date("2023-08-31")),
                             prop_vacc_age = 0, 
                             vacc_age = 0, USPS = unique(vacc_curves_r1$USPS), age_group = unique(vacc_curves_r1$age_group),
                             risk_group = c("overall", "high_risk", "low_risk")) %>% as_tibble() %>% 
    left_join(state_pop_agestrat_withrisk %>% rename(pop_prop = prop) %>% mutate(age_l = as.integer(age_l), age_r=as.integer(age_r))) %>%
    rename(dose = risk_group)
  
  # Set Day 0 to 0
  vacc_curves_r2 <- vacc_curves_r1 %>% 
    left_join(geodata %>% dplyr::select(USPS, subpop)) %>%
    bind_rows(extra_dates) %>%
    as_tibble() %>% distinct() %>%
    mutate(vacc_age = ifelse(date<=as_date("2023-08-31"), 0, vacc_age),
           prop_vacc_age  = ifelse(date<=as_date("2023-08-31"), 0, prop_vacc_age )) %>%
    select(-vacc_age_daily)
  ggplot(vacc_curves_r2) + geom_line(aes(date, prop_vacc_age, colour = age_group, linetype = dose)) + facet_wrap(~USPS, scales = 'free_y')  
  ggplot(vacc_curves_r2 %>% filter(USPS == "CA")) + 
    geom_line(aes(date, prop_vacc_age, colour = age_group, linetype = dose)) + facet_wrap(age_group~dose, scales = 'free_y')  
  
  vacc_curves_r2 <- fix_vacc_phase_errors(vacc_data = vacc_curves_r2) %>% distinct()
  ggplot(vacc_curves_r2 %>% filter(USPS == "CA")) + geom_line(aes(date, prop_vacc_age, colour = age_group, linetype = dose)) + 
    facet_wrap(age_group~dose, scales = 'free_y')  
  
  vacc_curves_r2 <- vacc_curves_r2 %>%
    as_tibble() %>%
    mutate(date = as_date(date)) %>%
    mutate(vacc_age = round(vacc_age)) %>%
    arrange(USPS, dose, age_group, date) %>%
    group_by(USPS, dose, age_group) %>% 
    mutate(vacc_age_daily = vacc_age - lag(vacc_age, default=0)) %>% 
    ungroup() %>% as_tibble()
  
  # vacc_curves_r2 <- vacc_curves_r2 %>%
  #   group_by(date, USPS, dose) %>%
  #   mutate(n_age = length(vacc_age)) %>%
  #   ungroup() 
  # 
  # vacc_data_age_props <- vacc_curves_r2 %>%
  #   filter(n_age >= length(age_groups)) %>%
  #   group_by(date, USPS, dose) %>%
  #   mutate(cum_prop_of_vacc = round(vacc_age / sum(vacc_age, na.rm=TRUE),5),
  #          prop_of_vacc = round(vacc_age_daily / sum(vacc_age_daily, na.rm=TRUE),5)) %>%
  #   ungroup() 
  # 
  # vacc_data_combo_cl <- bind_rows(
  #   vacc_curves_r2 %>% filter(n_age < length(age_groups)) %>% mutate(prop_of_vacc=NA, cum_prop_of_vacc=NA),
  #   vacc_data_age_props) %>%
  #   arrange(USPS, age_group, dose, date)
  vacc_data_combo_cl <- vacc_curves_r2 
  
  
  ggplot(vacc_curves_r2) + geom_line(aes(date, prop_vacc_age, colour = age_group, linetype = dose)) + facet_wrap(~USPS, scales = 'free_y')  
  
  ggplot(vacc_data_combo_cl %>% filter(USPS=="MA"), aes(x=date, y=vacc_age, color=age_group, linetype = dose)) + geom_line()
  ggplot(vacc_data_combo_cl %>% filter(USPS=="MA"), aes(x=date, y=prop_vacc_age, color=age_group, linetype = dose)) + geom_line()
  ggplot(vacc_data_combo_cl %>% filter(USPS=="MA"), aes(x=date, y=vacc_age_daily, color=age_group, linetype = dose)) + geom_line()
  
  # ~ Save ------------------------------------------------------------------
  
  write_csv(vacc_data_combo_cl, #%>% filter(age_group %in% age_groups), 
            paste0(save_dir, "/vaccination_agestrat_risk_r", round_num, ".csv"))
  
}


vacc_data_combo_cl <- read_csv(paste0(save_dir, "/vaccination_agestrat_risk_r", round_num, ".csv"))

# 4. Hosp & Death Age Probabilities --------------------------------------------------
# -- get model-specific ages
## IDK WHY THIS IS HERE??

select <- dplyr::select

# Get correct IFR for age groups and location
ifr_age_agestrat <- get_agestd_ifr(pop_agestrat_data = us_pop_agestrat_model)

# Get correct Pr(hosp|inf) for age groups and location
hosp_age_agestrat <- get_agestd_hosp(pop_agestrat_data = us_pop_agestrat_model, p_hosp_inf=0.0175)


# 6. CONVERT TO MODEL AGE GROUPS ------------------------------------------
# something not right here
# vacc_fits <- read_csv(paste0(save_dir, "/vaccination_agestrat_r", round_num, ".csv"))
vacc_fits <- read_csv(paste0(save_dir, "/vaccination_agestrat_risk_r", round_num, ".csv"))
vacc_model_age <- config_vacc_age_groups(
  vacc_age_data = vacc_fits,
  age_groups_config = c("0_17","18_64","65_100"),
  age_l_config = c(0,18,65),
  age_r_config = c(17,64,100),
  date_vars = c("date"),
  vars_sum = c("pop","pop_unvacc","vacc_age","vacc_age_daily")) %>%
  mutate(date_admin = as_date(date - 14))

# extract vacc rates we want and convert to 'age group'
vacc_model_age <- vacc_model_age %>% 
  filter((age_group == "0_17" & dose == "overall") | 
           (age_group =="18_64" & dose %in% c("high_risk", "low_risk")) | 
              (age_group == "65_100" & dose == "overall")) %>%
  mutate(age_group = case_when(
    age_group == "0_17" ~ "0_17",
    age_group == "18_64" ~ ifelse(dose == "high_risk","18_64HR","18_64LR"),
    age_group == "65_100" ~ "65_100"
  )) %>%
  group_by(USPS, subpop, date) %>%
  mutate(population = sum(pop)) %>%
  ungroup() 


vacc_model_age %>% 
  ggplot(aes(date, prop_vacc_age, color=age_group)) + geom_point() + geom_line() + facet_wrap(~USPS)
vacc_model_age %>% 
  ggplot(aes(date, prop_vacc_age, color=age_group)) + geom_point() + geom_line() + facet_wrap(~subpop)

vacc_model_age %>% filter(USPS=="FL") %>% 
  ggplot(aes(date, vacc_age, color=age_group)) + geom_point() + geom_line() + facet_wrap(~dose,nrow = 2)
vacc_model_age %>% filter(USPS=="FL") %>% 
  ggplot(aes(date, vacc_age_daily, color=age_group)) + geom_point() + geom_line() + facet_wrap(~dose,nrow = 2)
vacc_model_age %>% filter(date >= lubridate::as_date("2023-09-04")) %>%
  ggplot(aes(date, prop_vacc_age, color=age_group)) + geom_point() + geom_line() + facet_wrap(~USPS)
vacc_model_age %>% filter(date >= lubridate::as_date("2023-09-04")) %>%
  ggplot(aes(date, vacc_rate, color=age_group)) + geom_point() + geom_line() + facet_wrap(~USPS)


# 7. Extend dates  ------------------------------------------------

pop_info <- vacc_model_age %>% 
  select(USPS, subpop, population, age_group, pop, dose) %>%
  distinct()

# Function to create all combinations of date with population info in each row
create_combinations <- function(row, dates) {
  group_combinations <- expand.grid(pop_info[row,])
  group_combinations <- data.frame(date = dates, group_combinations)
  return(group_combinations)
}

# Create all combinations of date and each row in the "group" dataframe
new_dates <- do.call(rbind, lapply(1:dim(pop_info)[1], function(i)
  create_combinations(i, lubridate::as_date((max(vacc_model_age$date)+1):as_date("2024-08-31")))))

# extend dates to the next release of booster (sept 1) 
## AGAIN SOMETHING NOT RIGHT HERE WITH 18_64 after the fact (prop goes to 0, should stay at saturation point)
vacc_model_age_pre_scenarios <- vacc_model_age %>% 
  bind_rows(new_dates %>% mutate(vacc_age_daily = 0, vacc_rate = 0)) %>%
  # bind_rows(new_dates %>% mutate(dose = c("overall"),vacc_age_daily = 0, #prop_vacc_age = 0, 
  #                                vacc_rate = 0),
  #           new_dates %>% filter(age_group == "18_64") %>% mutate(dose = c("high_risk"),vacc_age_daily = 0, #prop_vacc_age = 0, 
  #                                vacc_rate = 0),
  #           new_dates %>% filter(age_group == "18_64") %>% mutate(dose = c("low_risk"),vacc_age_daily = 0, #prop_vacc_age = 0, 
  #                                vacc_rate = 0)) %>%
  separate(age_group, into=c("age_l", "age_r"), sep="_", remove = FALSE) %>%
  group_by(USPS, subpop, age_group, dose) %>%
  arrange(USPS, subpop, age_group, dose, date) %>%
  mutate(diff_days = c(1,diff(date)),
         pop_unvacc = pop - vacc_age, date_admin = as_date(date - 14),
         vacc_age = cumsum(vacc_age_daily * diff_days), ## SOMETHING HERE NOT RIGHT
         prop_vacc_age = vacc_age/pop,
         pop_unvacc = pop - vacc_age) %>%
  select(USPS, subpop, dose, age_l, age_r, date, vacc_age_daily, pop, population, vacc_age, pop_unvacc, vacc_rate, prop_vacc_age, age_group) %>%
  mutate(epiweek = MMWRweek::MMWRweek(date)$MMWRweek,
         year = MMWRweek::MMWRweek(date)$MMWRyear,
         day = MMWRweek::MMWRweek(date)$MMWRday,
         scenario = "all")

vacc_model_age_pre_scenarios %>% #filter(date >= lubridate::as_date("2023-09-04")) %>%
  ggplot(aes(date, prop_vacc_age, color=age_group, linetype = dose)) + #geom_point() +
  geom_line() + facet_wrap(~USPS)

# 7. Scenarios ------------------------------------------------
# noBoo -> all rates 0
# 65Boo -> 65+ and those with high risk
# allBoo -> all age groups get the booster (so exactly as in 2023)

# allBoo
vacc_model_age_allBoo_2024 <- vacc_model_age_pre_scenarios %>% 
  mutate(year = year + 1,
         date = MMWRweek::MMWRweek2Date(year, epiweek, day)) %>% 
  mutate(scenario = "allBoo")

# noBoo
vacc_model_age_noBoo_2024 <- vacc_model_age_pre_scenarios %>% 
  mutate(year = year + 1,
         date = MMWRweek::MMWRweek2Date(year, epiweek, day)) %>% 
  mutate(vacc_age_daily = 0,
         vacc_age = 0,
         pop_unvacc = pop, 
         vacc_rate = 0, 
         prop_vacc_age = 0) %>%
  mutate(scenario = "noBoo")

# 65Boo
vacc_model_age_65Boo_2024 <- vacc_model_age_pre_scenarios %>%
  mutate(year = year + 1,
         date = MMWRweek::MMWRweek2Date(year, epiweek, day)) %>%
  mutate(scenario="65Boo",
         pop_unvacc=case_when(age_group=="65_100"~pop_unvacc, 
                              age_group=="18_64HR"~pop_unvacc,
                              TRUE~pop), 
         vacc_age=case_when(age_group=="65_100"~vacc_age, 
                            age_group=="18_64HR"~vacc_age,
                            TRUE~0), 
         vacc_age_daily=case_when(age_group=="65_100"~vacc_age_daily,
                                  age_group=="18_64HR"~vacc_age_daily,
                                  TRUE~0), 
         prop_vacc_age=case_when(age_group=="65_100"~prop_vacc_age, 
                                 age_group=="18_64HR"~prop_vacc_age,
                                 TRUE~0), 
         vacc_rate=case_when(age_group=="65_100"~vacc_rate, 
                             age_group=="18_64HR"~vacc_rate,
                             TRUE~0)) 


# ~ Combine scenarios -----------------------------------------------------

vacc_model_age_scn <- bind_rows(vacc_model_age_pre_scenarios %>% filter(date < min(vacc_model_age_noBoo_2024$date)),
                                vacc_model_age_noBoo_2024,
                                vacc_model_age_65Boo_2024,
                                vacc_model_age_allBoo_2024)

# Save it
write_csv(vacc_model_age_scn, paste0("model_input/vaccination/Round", round_num, "/vacc_alldoses_age_R", round_num, ".csv"))
arrow::write_parquet(vacc_model_age_scn, paste0("model_input/vaccination/Round", round_num, "/vacc_alldoses_age_R", round_num, "_withscenarios.parquet"))

vacc_model_age_scn %>% filter(USPS=="CA") %>% 
  ggplot(aes(date, prop_vacc_age, color=age_group)) + geom_point() + geom_line() + 
  facet_grid(scenario~age_group) +
  geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")


# 8. VACC RATES BY DAY - TIMESERIES ---------------------------------------

vacc_model_age <- arrow::read_parquet(paste0("model_input/vaccination/Round", round_num, "/vacc_alldoses_age_R", round_num, "_withscenarios.parquet"))

options(scipen = 999)
vacc_model_age <- vacc_model_age  %>%
  mutate(vacc_rate = ifelse(vacc_rate<0, 0, vacc_rate)) %>% 
  mutate(agestrat = paste0("age", gsub("_", "to", age_group))) %>%
  filter(!is.na(vacc_rate)) %>%
  mutate(vacc_rate = round(vacc_rate, 7)) %>%
  filter(USPS != "US") 

vacc_model_age %>% filter(USPS=="CA") %>%
  mutate(dose_scn = paste0(dose, "-", scenario)) %>%
  ggplot(aes(date, vacc_age_daily, color=age_group, linetype=scenario)) +  geom_line(size=1.2) + 
  facet_wrap(~dose_scn, nrow = 2, scales = "free_y")

# duplicate the "all"
scns_ <- unique(vacc_model_age$scenario)
scns_ <- scns_[scns_ != "all"]
vacc_rate_daily <- list()
for (s in 1:length(scns_)){
  vacc_rate_daily[[s]] <- vacc_model_age %>%
    filter(scenario == "all") %>%
    bind_rows(vacc_model_age %>% filter(scenario == scns_[s])) %>%
    mutate(scenario = scns_[s])
}
vacc_rate_daily <- data.table::rbindlist(vacc_rate_daily)        


vacc_rate_daily %>% filter(USPS=="CA") %>%
  ggplot(aes(date, vacc_rate, color=age_group)) +  geom_line(size=1.2) + 
  facet_wrap(~scenario, nrow = 2) +
  geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")


# ~ Use weekly mean for smoother rates of vaccination: DON'T GET WHY --------------------- 

# vacc_rate_daily_orig <- vacc_rate_daily
# 
# weekly_vacc_rate <- calc_weekly_age_vacc(
#   vacc_fits=vacc_rate_daily_orig, 
#   vacc_start_dates=vacc_start_dates_model)
# 
# weekly_vacc_rate <- weekly_vacc_rate %>%
#   mutate(agestrat = paste0("age", gsub("_", "to", age_group))) #%>%
# # filter(!is.na(vacc_rate)) %>%  
# # filter(vacc_rate >= 0)
# 
# weekly_vacc_rate %>% filter(USPS=="CA") %>%
#   ggplot(aes(date, vacc_rate, color=age_group)) +  geom_line(size=1.2) + 
#   facet_wrap(~scenario, nrow = 2) +
#   geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")
# 
# vacc_rate_daily_orig %>%
#   filter(USPS != "US") %>%
#   mutate(vacc_dose_age = vacc_rate * pop_unvacc) %>%
#   group_by(dose, scenario) %>%
#   summarise(tot_vacc = sum(vacc_dose_age, na.rm=TRUE),
#             tot_vacc_pre = sum(vacc_age[date==max(date)]))
# # rates seem to have worked just fine
# 
# vacc_rate_daily %>% filter(USPS=="CA") %>%
#   ggplot(aes(date, vacc_rate, color=age_group)) +  geom_line(size=1.2) + 
#   facet_grid(scenario~dose) +
#   geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")
# 
# vacc_rate_daily %>% group_by(age_group, dose) %>% summarise(n = length(unique(USPS)))
# st_ <- vacc_rate_daily %>% filter(dose=="dose1", age_group=="18_64") %>% pull(USPS) %>% unique()
# 

# change age group
vacc_rate_daily <- vacc_rate_daily %>%
  mutate(agestrat = paste0("age", gsub("_", "to", age_group))) %>%
  select(-age_group)

vacc_rate_daily %>% filter(USPS=="CA") %>%
  ggplot(aes(date, vacc_rate, color=agestrat)) +  geom_line(size=1.2) + 
  facet_grid(scenario~dose) +
  geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")

vacc_rate_daily %>% filter(USPS=="CA") %>% filter(agestrat =="age18to64LR") %>%
  ggplot(aes(date, vacc_rate, color=agestrat)) +  geom_line(size=1.2) + 
  facet_grid(scenario~dose) +
  geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")

vacc_rate_daily %>% filter(USPS=="CA") %>% filter(agestrat =="age0to17") %>%
  ggplot(aes(date, vacc_rate, color=agestrat)) +  geom_line(size=1.2) + 
  facet_grid(scenario~dose) +
  geom_vline(aes(xintercept=as_date("2022-10-01")), color="grey")

arrow::write_parquet(vacc_rate_daily, paste0("model_input/vaccination/Round", round_num, "/vacc_rate_daily_all_R", round_num, ".parquet"))






