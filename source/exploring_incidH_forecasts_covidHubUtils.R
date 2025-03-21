# setup ---------------------------
#http://reichlab.io/covidHubUtils/articles/covidHubUtils-overview.html
library(remotes)
# remotes::install_github("reichlab/zoltr")
# remotes::install_github("reichlab/covidHubUtils")
library(tidyverse)
library(zoltr)
library(covidHubUtils)
library(doParallel)
library(ggplot2)
theme_set(theme_bw())

# Load and plot single forecast ------ 
# create a list of dates from 2023-09-01 to 2024-04-30
fluszn_23_24 <- seq(as.Date("2023-09-01"), as.Date("2024-04-30"), by = "day")
# create a list with the full name of every state in the US
state_names <- state.name


inc_hosp_targets <- paste(0:130, "day ahead inc hosp")
forecasts_hosp_US <- load_forecasts(
  models = "COVIDhub-ensemble",
  dates = fluszn_23_24,
  date_window_size = 6,
  locations = "US",
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

forecasts_hosp_states <- load_forecasts(
  models = "COVIDhub-ensemble",
  dates = fluszn_23_24,
  date_window_size = 6,
  locations = state_names,
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

forecast_hosp <- bind_rows(forecasts_hosp_US, forecasts_hosp_states)

forecast_hosp_MD_7 <- forecast_hosp %>%
  filter(location_name == "Maryland",
         horizon == 7)

write_parquet(forecast_hosp, "data/covidHubUtils_forecastData/forecast_hosp.parquet")

p_hosp <- plot_forecasts(
  forecast_data = forecast_hosp_MD_7,
  truth_source = "HealthData",
  target_variable = "inc hosp",
  intervals = c(.5, .8, .95),
  top_layer = "forecast",
)
p_hosp
ggsave("~/Downloads/forecast_hosp_MD_7.png", p_hosp, width = 8, height = 6)
plot_forecasts(
  forecast_data = forecasts_hosp_US,
  truth_source = "HealthData",
  target_variable = "inc hosp",
  intervals = c(.5, .8, .95),
)


plot_forecasts(
  forecast_data = forecasts_hosp,
  #truth_data,
  #hub = c("US", "ECDC", "FluSight"),
  #models = NULL,
  target_variable = "inc hosp",
  #locations = c("Colorado", "Maryland"),
  facet = location_name,
  facet_scales = "free_y",
  #facet_nrow = NULL,
  #facet_ncol = NULL,
  #forecast_dates,
  intervals = c(.5, .8, .95),
  #horizon,
  truth_source = "HealthData",
  #use_median_as_point = FALSE,
  #plot_truth = TRUE,
  #plot = TRUE,
  #fill_by_model = FALSE,
  #fill_transparency = 1,
  #truth_as_of = NULL,
  #top_layer = c("truth", "forecast"),
  #title = "default",
  #subtitle = "default",
  #show_caption = TRUE
)
