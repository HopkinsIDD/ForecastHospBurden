# Data loaders.
#   load_hhs:  observed admissions + census per state-day.
#   load_hub:  hub admission quantile forecasts per state-forecast_date.

# HHS reports admissions as yesterday's count, so we shift one day
# forward: admissions[d] = people admitted on day d, same timebase as
# census. Rows with any missing field are dropped.
load_hhs <- function(
  path = here::here(
    "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
  )
) {
  arrow::read_parquet(path) |>
    arrange(state, date) |>
    mutate(
      date = as.Date(date),
      census = total_adult_patients_hospitalized_confirmed_covid +
               total_pediatric_patients_hospitalized_confirmed_covid,
      admissions = lead(
        previous_day_admission_adult_covid_confirmed +
        previous_day_admission_pediatric_covid_confirmed
      ),
      .by = state
    ) |>
    select(state, date, census, admissions) |>
    drop_na()
}

# Hubverse admission forecasts (COVIDhub-ensemble, quantile format).
# One row per (state, forecast_date, target_end_date, quantile) with
# `value` holding the forecasted admissions at that quantile level.
load_hub <- function(
  path = here::here("data/covidHubUtils_forecastData/forecast_hosp.parquet")
) {
  arrow::read_parquet(path) |>
    rename(state = abbreviation) |>
    filter(state != "US", model == "COVIDhub-ensemble", type == "quantile") |>
    select(state, forecast_date, target_end_date, quantile, value)
}
