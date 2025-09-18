#=============================
# Setup
#=============================
library(tidyverse)
library(furrr)
library(arrow)
library(geofacet)
library(hydroTSM) # for time2season()
library(pipetime) # devtools::install_github("CyGei/pipetime")
#=============================
# Data
#=============================
df <- #read_csv("https://healthdata.gov/resource/g62h-syeh.csv") |>
  arrow::read_parquet(
    here::here(
      "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
    )
  ) |>
  arrange(state, date) |>
  mutate(
    state,
    date = as.Date(date),
    total_hosp = total_adult_patients_hospitalized_confirmed_covid +
      total_pediatric_patients_hospitalized_confirmed_covid,
    admissions = lead(
      previous_day_admission_adult_covid_confirmed +
        previous_day_admission_pediatric_covid_confirmed
    ),
    .by = "state",
    .keep = "none"
  ) |>
  drop_na()
df


#=============================
# Optimization
#=============================
# Predict the number of active hospitalisations:
predictH <- function(mu, k, max_stay = 50, admissions) {
  # Calculate P(LOS >= j+1) for j = 0 to max_stay
  # This is 1 - P(LOS <= j)
  survival_probs <- 1 - pnbinom(0:max_stay, size = k, mu = mu)

  # Convolve admissions with the survival probability kernel
  predicted <- convolve(admissions, rev(survival_probs), type = "open")

  return(predicted[1:length(admissions)])
}

# Optimization wrapper
negbin_sse <- function(params, admissions, total_hosp) {
  mu <- exp(params[1])
  k <- exp(params[2])
  predicted <- predictH(mu = mu, k = k, admissions = admissions)
  sum((total_hosp - predicted)^2)
}


#=============================
# Specific example
#=============================
test <- df |>
  filter(state == "CA") |>
  filter(date >= as.Date("2020-12-01") & date <= as.Date("2021-02-28"))

# Optimize
fit <- optim(
  par = c(log(2), log(1)),
  fn = negbin_sse,
  admissions = test$admissions,
  total_hosp = test$total_hosp,
  lower = c(log(1e-6), log(1e-6)), # prevent zero/negative values
  method = "L-BFGS-B"
)

test |>
  mutate(
    predicted_hosp = predictH(
      mu = exp(fit$par[1]),
      k = exp(fit$par[2]),
      admissions = admissions
    )
  ) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_hosp, color = "Actual")) +
  geom_line(
    aes(y = predicted_hosp, color = "Predicted")
  ) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "orange")) +
  labs(
    title = "Actual vs. Predicted Active Hospitalisations",
    y = "Active Hospitalisations",
    x = "Date",
    color = "Series"
  ) +
  theme_classic()

#=============================
# Fit to all states / seasons
#=============================
plan(sequential)
set.seed(123)
fits <- df |>
  mutate(
    season_year = paste0(
      hydroTSM::time2season(date, out.fmt = "seasons"),
      "_",
      lubridate::year(date)
    )
  ) |>
  nest(.by = c(state, season_year)) |>
  mutate(
    params = future_map(
      .x = data,
      .f = ~ optim(
        par = c(log(2), log(1)),
        fn = negbin_sse,
        admissions = .x$admissions,
        total_hosp = .x$total_hosp,
        lower = c(log(1e-6), log(1e-6)), # prevent zero/negative values
        method = "L-BFGS-B"
      ),
      .options = furrr_options(seed = TRUE)
    ),
    mu = exp(map_dbl(params, ~ .x$par[1])),
    k = exp(map_dbl(params, ~ .x$par[2])),
    .by = c("state", "season_year"),
  ) |>
  select(-params) |>
  time_pipe("fitting")


predictions <- fits |>
  group_by(state, season_year) |>
  unnest(data) |>
  mutate(
    predicted_hosp = predictH(
      mu = mu,
      k = k,
      admissions = admissions
    )
  )

predictions |>
  filter(season_year == "summer_2021") |>
  ggplot(aes(x = date)) +
  geofacet::facet_geo(~state, grid = "us_state_grid1", scales = "free_y") +
  geom_line(aes(y = total_hosp, color = "Actual")) +
  geom_line(aes(y = predicted_hosp, color = "Predicted")) +
  scale_color_manual(
    values = c("Actual" = "black", "Predicted" = "orange")
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Actual vs Predicted Active Hospitalisations",
    y = "Active Hospitalisations",
    x = "Date",
    colour = ""
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )
