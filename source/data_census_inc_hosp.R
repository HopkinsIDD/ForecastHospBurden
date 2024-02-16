
library(tidyverse)
library(arrow)

NJ_total_hosp <- read_csv("data/data_table_for_currently_hospitalized_covid19_patients_-_new_jersey.csv", skip =2)
head(NJ_total_hosp)

NJ_total_hosp <- NJ_total_hosp %>% 
  mutate(date = as.Date(Date, format = "%b %d %Y"),
                        total_hosp = as.numeric(`Currently Hospitalized COVID-19 Patients`),
                        state = Geography
                        ) %>% 
  select(state, date, total_hosp)

write_parquet(NJ_total_hosp, "data/NJ_total_hosp.parquet")
