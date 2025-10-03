# --- SetUp ---
library(tidyverse)
library(furrr)
library(arrow)
library(geofacet)
library(hydroTSM) # for time2season()
library(pipetime) # devtools::install_github("CyGei/pipetime")
options(pipetime.log = "log", pipetime.unit = "secs")
source(here::here("source/cy/helpers.R"))
# --- Data ---
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
    active_hosp = total_adult_patients_hospitalized_confirmed_covid +
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


# --- Fitting using SSE ---
plan(multisession, workers = availableCores() - 2)
set.seed(123)

# Estimate parameters for each state and season
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
        active_hosp = .x$active_hosp,
        lower = c(log(1e-6), log(1e-6)),
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

# Predictions from fitted parameters
predictions <- fits |>
  group_by(state, season_year) |>
  unnest(data) |>
  mutate(
    predicted_hosp = predictH(
      mu = mu,
      k = k,
      admissions = admissions
    )
  ) |>
  ungroup()

predictions |>
  filter(season_year == "summer_2023") |>
  ggplot(aes(x = date)) +
  geofacet::facet_geo(~state, grid = "us_state_grid1", scales = "free_y") +
  geom_line(aes(y = active_hosp, color = "Actual")) +
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

# --- Bootstrap for CIs ---
fits_with_ci <- fits |>
  mutate(
    boot_params = future_pmap(
      .l = list(data, mu, k),
      .f = bootstrap_nonparametric,
      n_boot = 100,
      .options = furrr_options(seed = TRUE)
    ),
    mu_lower = map_dbl(boot_params, ~ quantile(.x[, 1], 0.025)),
    mu_upper = map_dbl(boot_params, ~ quantile(.x[, 1], 0.975)),
    k_lower = map_dbl(boot_params, ~ quantile(.x[, 2], 0.025)),
    k_upper = map_dbl(boot_params, ~ quantile(.x[, 2], 0.975)),
    # Add predictions for plotting CI
    boot_preds = future_pmap(
      .l = list(data, boot_params),
      .f = function(data, boot_params) {
        admissions <- data$admissions
        preds <- apply(boot_params, 1, function(params) {
          predictH(mu = params[1], k = params[2], admissions = admissions)
        })
        return(preds)
      },
      .options = furrr_options(seed = TRUE)
    )
  ) |>
  time_pipe("bootstrapping")

get_log()

# --- Plotting Ribbon ---
plot_data <- fits_with_ci |>
  select(state, season_year, data, boot_preds) |>
  group_by(state, season_year) |>
  mutate(
    boot_preds = map(boot_preds, ~ as_tibble(.x))
  ) |>
  unnest(c(data, boot_preds)) |>
  pivot_longer(
    cols = starts_with("V"), # will be V1 ... Vn
    names_to = "bootstrap_id",
    values_to = "boot_pred"
  )

plot_data |>
  filter(season_year == "summer_2023") |>
  ggplot(aes(x = date)) +
  geofacet::facet_geo(~state, grid = "us_state_grid1", scales = "free_y") +
  geom_line(aes(y = active_hosp, color = "Actual")) +
  geom_point(
    aes(y = boot_pred, color = "Predicted"),
    size = 0.5,
    alpha = 0.01
  ) +
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


# Plot parameter estimates with CIs
fits_with_ci |>
  filter(season_year == "summer_2023") |>
  ggplot(aes(x = reorder(state, mu), y = mu)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = mu_lower, ymax = mu_upper),
    width = 0.2,
  ) +
  coord_flip()

# --- Sanity Check Residuals with ACF ---
ca_summer_23 <- fits |> filter(state == "CA", season_year == "summer_2023")
predicted_vals <- predictH(
  mu = ca_summer_23$mu,
  k = ca_summer_23$k,
  admissions = ca_summer_23$data[[1]]$admissions
)
residuals <- ca_summer_23$data[[1]]$active_hosp - predicted_vals
acf(residuals, main = "ACF of Residuals for CA Summer 2023")

# --- scoringutils ---
library(scoringutils)
# forecast type: "sample"
# sample: a probabilistic forecast for a continuous or discrete outcome variable, with the forecast represented by a finite set of samples drawn from the predictive distribution.

fcast <- plot_data |>
  transmute(
    state,
    season_year,
    date,
    active_hosp,
    boot_pred,
    sample_id = parse_number(bootstrap_id),
    observed = active_hosp,
    predicted = boot_pred
  ) |>
  filter(season_year == "summer_2023", state %in% c("CA", "TX")) |>
  group_by(state, season_year, date) |>
  as_forecast_sample(
    sample_id = "sample_id"
  )

# sanity check
fcast |>
  group_by(state, season_year, date) |>
  count() |>
  view()

scores <- fcast |>
  score() |>
  time_pipe("scoring")

get_log()
summarise_scores(scores, by = c("state"))
