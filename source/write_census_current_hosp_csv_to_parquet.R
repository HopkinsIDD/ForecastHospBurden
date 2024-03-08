library(tidyverse)
library(arrow)
library(gtable)
library(grid)
library(gridExtra)

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
                total_adult_patients_hospitalized_confirmed_and_suspected_covid, total_adult_patients_hospitalized_confirmed_and_suspected_covid_coverage,
                total_adult_patients_hospitalized_confirmed_covid, total_adult_patients_hospitalized_confirmed_covid_coverage,
                total_pediatric_patients_hospitalized_confirmed_and_suspected_covid, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_coverage,
                total_pediatric_patients_hospitalized_confirmed_covid, total_pediatric_patients_hospitalized_confirmed_covid_coverage)


state_sample_covid_totalHosp <- clean_covid_totalHosp %>% 
  filter(state %in% c("NJ", "MD", "PA", "NY")) 

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




