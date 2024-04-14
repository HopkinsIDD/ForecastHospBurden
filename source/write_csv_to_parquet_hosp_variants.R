library(tidyverse)
library(arrow)
library(gtable)
library(grid)
library(gridExtra)
library(naniar)


# # create state_files list for loop 
# # Updated state_files list with specified path structure
# state_files <- list(
#   "NJ" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_new_jersey.csv",
#   "NY" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_new_york.csv",
#   "PA" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_pennsylvania.csv",
#   "MD" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_maryland.csv"
# )
# 
# 
# # create for loop to read in / clean / export for each state 
# for (state in names(state_files)) {
#   # read in csv file 
#   data <- read_csv(state_files[[state]], skip = 2)
#   
#   # clean data up 
#   data <- data %>% 
#     mutate(date = as.Date(Date, format = "%b %d %Y"),
#            total_hosp = as.numeric(`Currently Hospitalized COVID-19 Patients`),
#            state = Geography) %>% 
#     select(state, date, total_hosp)
#   
#   # Write to parquet file
#   write_parquet(data, paste0("data/currently_hosp_covid19_by_state_parquet/", state, "_currently_hospitalized_covid19_patients.parquet"))
# }

raw_covid_totalHosp <- read_csv("data/currently_hosp_covid_data_daily/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries__RAW__20240306.csv")

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


state_sample_covid_totalHosp <- clean_covid_totalHosp %>% 
  filter(state %in% c("NJ", "MD", "PA", "NY"))

# missing data

vis_miss(state_sample_covid_totalHosp)


gg_miss_var(state_sample_covid_totalHosp, show_pct = TRUE)
# missing incid / totalH data
state_sample_covid_totalHosp %>% 
  dplyr::select(
    previous_day_admission_adult_covid_confirmed, previous_day_admission_adult_covid_suspected,
    previous_day_admission_pediatric_covid_confirmed, previous_day_admission_pediatric_covid_suspected, inpatient_beds_used_covid, date) %>% 
  mutate(year = year(as.Date(date))) %>% 
  gg_miss_var(show_pct = TRUE, facet = year)

state_sample_covid_totalHosp %>% 
  dplyr::select(
    previous_day_admission_adult_covid_confirmed, previous_day_admission_adult_covid_suspected,
    previous_day_admission_pediatric_covid_confirmed, previous_day_admission_pediatric_covid_suspected, inpatient_beds_used_covid, date) %>% 
  mutate(year = year(as.Date(date))) %>% 
  filter(year %in% c(2020)) %>% 
  mutate(month = month(date)) %>% 
  gg_miss_var(show_pct = TRUE, facet = month)

# create shadow cols of missing data 

shadowed_state_sample_covid_totalHosp <- state_sample_covid_totalHosp %>% 
  bind_shadow()
names(shadowed_state_sample_covid_totalHosp)

# plot of missingness of previous day admissions age group 
shadowed_state_sample_covid_totalHosp %>% 
  mutate(year = year(as.Date(date))) %>% 
  filter(year == 2020) %>% 
  ggplot(mapping = aes(x = date, # numeric or date column
                     colour = `previous_day_admission_adult_covid_confirmed_NA`)) + # shadow column of interest
  geom_density()                          # plots the density curves



state_sample_covid_totalHosp_origin_Aug2020 <- state_sample_covid_totalHosp %>% 
  filter(between(date, as.Date('2020-08-01'), Sys.Date()))

state_sample_covid_totalHosp_origin_Aug2020 %>% 
  dplyr::select(
    previous_day_admission_adult_covid_confirmed, previous_day_admission_adult_covid_suspected,
    previous_day_admission_pediatric_covid_confirmed, previous_day_admission_pediatric_covid_suspected, inpatient_beds_used_covid, date) %>% 
  mutate(year = year(as.Date(date))) %>% 
  gg_miss_var(show_pct = TRUE, facet = year)


######### FINAL DATASET ORIGIN DATE

state_sample_covid_totalHosp <- state_sample_covid_totalHosp %>% 
  filter(between(date, as.Date('2020-08-01'), Sys.Date()))

write_parquet(state_sample_covid_totalHosp, "data/currently_hosp_covid_data_daily/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_Subset.parquet")
# Check total COVID hospitalizations 

state_counts <- state_sample_covid_totalHosp %>%
  mutate(check_patient_count = if_else(total_adult_patients_hospitalized_confirmed_and_suspected_covid +
                                         total_pediatric_patients_hospitalized_confirmed_and_suspected_covid ==
                                         inpatient_beds_used_covid,
                                       TRUE, FALSE)) %>% 
  group_by(state) %>%
  summarize(TRUE_count = sum(check_patient_count == TRUE, na.rm = TRUE),
            FALSE_count = sum(check_patient_count == FALSE, na.rm = TRUE))


table <- cbind(state_counts$state, state_counts$TRUE_count, state_counts$FALSE_count)
colnames(table) <- c("State", "TRUE Count", "FALSE Count")

# Create a gtable object
table_g <- tableGrob(table)

# Draw the table
grid.newpage()
grid.draw(table_g)


##### VARIANTS DATASET --------------------------------------
variants <- read_csv("data/variants/variant_props_R17.csv")
write_parquet(variants, "data/variants/variant_props_R17.parquet")


