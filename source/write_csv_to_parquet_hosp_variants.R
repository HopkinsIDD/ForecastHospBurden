# Setup -------------
library(tidyverse)
library(arrow)
library(gtable)
library(grid)
library(gridExtra)
library(naniar)

# Import Initial dataset -----------
raw_covid_totalHosp <- read_csv("data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries__RAW__20240607.csv")

clean_covid_totalHosp <- raw_covid_totalHosp %>% 
  dplyr::select(state, date, inpatient_beds, inpatient_beds_coverage, inpatient_beds_used, inpatient_beds_used_coverage,
                inpatient_beds_used_covid, inpatient_beds_used_covid_coverage,
                #total hosp data 
                total_adult_patients_hospitalized_confirmed_and_suspected_covid, total_adult_patients_hospitalized_confirmed_and_suspected_covid_coverage,
                total_adult_patients_hospitalized_confirmed_covid, total_adult_patients_hospitalized_confirmed_covid_coverage,
                total_pediatric_patients_hospitalized_confirmed_and_suspected_covid, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_coverage,
                total_pediatric_patients_hospitalized_confirmed_covid, total_pediatric_patients_hospitalized_confirmed_covid_coverage,
                # incidH data
                previous_day_admission_adult_covid_confirmed, previous_day_admission_adult_covid_suspected,
                previous_day_admission_pediatric_covid_confirmed, previous_day_admission_pediatric_covid_suspected,
                # incidH data by age 
                `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
                `previous_day_admission_adult_covid_confirmed_30-39`, `previous_day_admission_adult_covid_confirmed_40-49`,
                `previous_day_admission_adult_covid_confirmed_50-59`, `previous_day_admission_adult_covid_confirmed_60-69`,
                `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, 
                previous_day_admission_adult_covid_confirmed_unknown,
                previous_day_admission_pediatric_covid_confirmed_0_4, previous_day_admission_pediatric_covid_confirmed_12_17,
                previous_day_admission_pediatric_covid_confirmed_5_11, previous_day_admission_pediatric_covid_confirmed_unknown,
                `previous_day_admission_adult_covid_suspected_18-19`, `previous_day_admission_adult_covid_suspected_20-29`,
                `previous_day_admission_adult_covid_suspected_30-39`, `previous_day_admission_adult_covid_suspected_40-49`,
                `previous_day_admission_adult_covid_suspected_50-59`, `previous_day_admission_adult_covid_suspected_60-69`,
                `previous_day_admission_adult_covid_suspected_70-79`, `previous_day_admission_adult_covid_suspected_80+`, 
                previous_day_admission_adult_covid_suspected_unknown, previous_day_admission_pediatric_covid_suspected,
                # incidH data by age coverage 
                previous_day_admission_adult_covid_confirmed_coverage, previous_day_admission_adult_covid_suspected_coverage,
                previous_day_admission_pediatric_covid_confirmed_coverage, previous_day_admission_pediatric_covid_suspected_coverage,
                `previous_day_admission_adult_covid_confirmed_18-19_coverage`, `previous_day_admission_adult_covid_confirmed_20-29_coverage`,
                `previous_day_admission_adult_covid_confirmed_30-39_coverage`, `previous_day_admission_adult_covid_confirmed_40-49_coverage`,
                `previous_day_admission_adult_covid_confirmed_50-59_coverage`, `previous_day_admission_adult_covid_confirmed_60-69_coverage`,
                `previous_day_admission_adult_covid_confirmed_70-79_coverage`, `previous_day_admission_adult_covid_confirmed_80+_coverage`, 
                previous_day_admission_adult_covid_confirmed_unknown_coverage,
                previous_day_admission_pediatric_covid_confirmed_0_4_coverage, previous_day_admission_pediatric_covid_confirmed_12_17_coverage,
                previous_day_admission_pediatric_covid_confirmed_5_11_coverage, previous_day_admission_pediatric_covid_confirmed_unknown_coverage,
                `previous_day_admission_adult_covid_suspected_18-19_coverage`, `previous_day_admission_adult_covid_suspected_20-29_coverage`,
                `previous_day_admission_adult_covid_suspected_30-39_coverage`, `previous_day_admission_adult_covid_suspected_40-49_coverage`,
                `previous_day_admission_adult_covid_suspected_50-59_coverage`, `previous_day_admission_adult_covid_suspected_60-69_coverage`,
                `previous_day_admission_adult_covid_suspected_70-79_coverage`, `previous_day_admission_adult_covid_suspected_80+_coverage`, 
                previous_day_admission_adult_covid_suspected_unknown_coverage, previous_day_admission_pediatric_covid_suspected_coverage)

# 06-07-24 filter to all states not sample
state_sample_covid_totalHosp <- clean_covid_totalHosp #%>% 
#filter(state %in% c("NJ", "MD", "PA", "NY"))

# Visualize missing data ------------

gg_miss_var(state_sample_covid_totalHosp, show_pct = TRUE)
# missing by year 
state_sample_covid_totalHosp %>% 
  dplyr::select(
    previous_day_admission_adult_covid_confirmed,
    previous_day_admission_pediatric_covid_confirmed, 
    total_adult_patients_hospitalized_confirmed_covid, 
    total_pediatric_patients_hospitalized_confirmed_covid,
    date) %>% 
  mutate(year = year(as.Date(date))) %>% 
  gg_miss_var(show_pct = TRUE, facet = year)

# remove missing date ---- 

state_sample_covid_totalHosp_origin_Aug2020 <- state_sample_covid_totalHosp %>% 
  filter(between(date, as.Date('2020-08-01'), Sys.Date())) 

state_sample_covid_totalHosp_origin_Aug2020 %>% 
  dplyr::select(
    previous_day_admission_adult_covid_confirmed,
    previous_day_admission_pediatric_covid_confirmed, 
    total_adult_patients_hospitalized_confirmed_covid, 
    total_pediatric_patients_hospitalized_confirmed_covid,
    date) %>% 
  mutate(year = year(as.Date(date))) %>% 
  gg_miss_var(show_pct = TRUE, facet = year)

# see which states have missing data
# missing_inpatient_beds_data <- state_sample_covid_totalHosp_origin_Aug2020 %>%
#   filter(is.na(inpatient_beds_used_covid))
# 
# unique_states_missing <- unique(missing_inpatient_beds_data$state)  
# 
# unique_states_missing

######### FINAL DATASET ORIGIN DATE

# could filter to just states + DC not territories
us_state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")


# Check incident data source ----
# 6-12-24 don't need this anymore, outdated source 
# # confirmed and suspected incident cases vs inpatient_beds_used_covid
# state_counts <- state_sample_covid_totalHosp %>%
#   mutate(check_patient_count = if_else(total_adult_patients_hospitalized_confirmed_and_suspected_covid +
#                                          total_pediatric_patients_hospitalized_confirmed_and_suspected_covid ==
#                                          inpatient_beds_used_covid,
#                                        TRUE, FALSE)) %>% 
#   group_by(state) %>%
#   summarize(TRUE_count = sum(check_patient_count == TRUE, na.rm = TRUE),
#             FALSE_count = sum(check_patient_count == FALSE, na.rm = TRUE))
# 
# 
# table <- cbind(state_counts$state, state_counts$TRUE_count, state_counts$FALSE_count)
# colnames(table) <- c("State", "Confirmed + Suspected = Inpateint Beds", "Confirmed + Suspected != Inpateint Beds")
# 
# # Create a gtable object
# table_g <- tableGrob(table)
# 
# # Draw the table
# grid.newpage()
# grid.draw(table_g)
# 
# # confirmed incident cases vs inpatient_beds_used_covid
# state_counts <- state_sample_covid_totalHosp %>%
#   mutate(check_patient_count = if_else(total_adult_patients_hospitalized_confirmed_covid +
#                                          total_pediatric_patients_hospitalized_confirmed_covid ==
#                                          inpatient_beds_used_covid,
#                                        TRUE, FALSE)) %>% 
#   group_by(state) %>%
#   summarize(TRUE_count = sum(check_patient_count == TRUE, na.rm = TRUE),
#             FALSE_count = sum(check_patient_count == FALSE, na.rm = TRUE))
# 
# 
# table <- cbind(state_counts$state, state_counts$TRUE_count, state_counts$FALSE_count)
# colnames(table) <- c("State", "Confirmed = Inpatient Beds", "Confirmed != Inpateint Beds")
# 
# # Create a gtable object
# table_g <- tableGrob(table)
# 
# # Draw the table
# grid.newpage()
# grid.draw(table_g)


# write final file ------------------------------------

state_sample_covid_totalHosp <- state_sample_covid_totalHosp %>% 
  filter(between(date, as.Date('2020-08-01'), Sys.Date())) # eliminates missing total hosp confirmed covid and previous day Incid H

# last updated 6-7-24
write_parquet(state_sample_covid_totalHosp, "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet")


##### VARIANTS DATASET --------------------------------------
variants <- read_csv("data/variants/variant_props_R17.csv")
write_parquet(variants, "data/variants/variant_props_R17.parquet")


