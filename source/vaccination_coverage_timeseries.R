# Setup -----


# Data Sources ---

# Old data: for 2020-12-13 to 2023-05-10
vacc_cdc <- read_csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD") %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Vaccine coverage from 2023-24 season is PROVIDED in scenarios
#download.file("https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/round_resources/round18/COVID_RD18_Vaccination_curves.csv", 
#               destfile = "model_input/vaccination/Round18/smh_r18_vacc_curves.csv", method = "curl")
vacc_curves <- read_csv("model_input/vaccination/Round18/smh_r18_vacc_curves.csv") %>% 
  filter(Risk_group == "Overall") #Date <= as.Date(Sys.Date())

vacc_cdc_cumul_perc <- read_csv("https://data.cdc.gov/api/views/vugp-mqip/rows.csv?accessType=DOWNLOAD") %>%
  rename(location_name = Jurisdiction) #%>%
  # setDT() %>%
  # .[loc_dictionary, on = .(location_name = location_name)] %>%
  # na.omit()

# Data exploration ----
sort(unique(vacc_cdc$Location))
us_vacc_cdc <- vacc_cdc %>% filter(Location == "US")

summary(vacc_cdc$Date)

vacc_curves_overall <- vacc_curves %>% 
  mutate(Cum.Coverage = Pop*Cum.Coverage.Percent) %>% 
  group_by(Geography, Date) %>% 
  summarise(Cum.Coverage.Percent = (sum(Cum.Coverage)/sum(Pop))) 


