
library(tidyverse)
library(arrow)

# create state_files list for loop 
# Updated state_files list with specified path structure
state_files <- list(
  "NJ" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_new_jersey.csv",
  "NY" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_new_york.csv",
  "PA" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_pennsylvania.csv",
  "MD" = "data/currently_hosp_covid19_by_state_csv/data_table_for_currently_hospitalized_covid19_patients_-_maryland.csv"
)


# create for loop to read in / clean / export for each state 
for (state in names(state_files)) {
  # read in csv file 
  data <- read_csv(state_files[[state]], skip = 2)
  
  # clean data up 
  data <- data %>% 
    mutate(date = as.Date(Date, format = "%b %d %Y"),
           total_hosp = as.numeric(`Currently Hospitalized COVID-19 Patients`),
           state = Geography) %>% 
    select(state, date, total_hosp)
  
  # Write to parquet file
  write_parquet(data, paste0("data/currently_hosp_covid19_by_state_parquet/", state, "_currently_hospitalized_covid19_patients.parquet"))
}
