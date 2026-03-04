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
    active_hosp = total_adult_patients_hospitalized_confirmed_covid +
      total_pediatric_patients_hospitalized_confirmed_covid,
    admissions = lead(
      previous_day_admission_adult_covid_confirmed +
        previous_day_admission_pediatric_covid_confirmed
    ),
    .by = "state",
    .keep = "none"
  ) |>
  drop_na() %>% 
  group_by(date) %>%
  summarise(active_hosp = sum(active_hosp), admissions = sum(admissions)) %>% 
  mutate(state = "USA")
  
df


#=============================
# Optimization
#=============================
# Predict the number of active hospitalisations:
mu = 5
k = 2
max_stay = 50
admissions = 10
predictH <- function(mu, k, max_stay = 50, admissions) {
  # Compute survival probabilities for lengths of stay from 0 to max_stay 
  # ie: the probability a patient is still in hospital at day t after admission based on mu&k
  # so mu = 5, k = 2 by day 20 1% chance still in hospital 
  survival_probs <- round(1 - pnbinom(0:max_stay, size = k, mu = mu), 6)
  
  survival_probs[!is.finite(survival_probs)] <- 0
  # Convolve admissions with survival probabilities to get predicted active hospitalisations
  # so if we have 10 admissions per day, on day 1 almost everyone is still hospitalized 
  predicted <- convolve(admissions, rev(survival_probs), type = "open")
  # Trim to length of admissions
  predicted <- predicted[1:length(admissions)] # is this working right.. capturing multiple days correctly
  predicted[!is.finite(predicted)] <- 0
  return(predicted)
}


# Optimization wrapper
negbin_sse <- function(params, admissions, active_hosp) {
  mu <- exp(params[1]) # why exp here 
  k <- exp(params[2])
  predicted <- predictH(mu = mu, k = k, admissions = admissions)
  sse <- sum((active_hosp - predicted)^2)

  if (!is.finite(sse)) { # i think this might introduce some error in really high mu values? 
    return(1e10)
  } # large penalty instead of NA/Inf
  return(sse)
}
#=============================
# Fit to all states / seasons
#=============================
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
        par = c(log(2), log(1)), #exp and then log to keep positive
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
  )


los_estimates <- predictions |>
  distinct(state, season_year, mu, k) |>
  mutate(
    expected_los = round(mu),
    sd_los = round(sqrt(k))
  ) 

# predictions |>
#   filter(season_year == "summer_2023") |>
#   ggplot(aes(x = date)) +
#   geofacet::facet_geo(~state, grid = "us_state_grid1", scales = "free_y") +
#   geom_line(aes(y = active_hosp, color = "Actual")) +
#   geom_line(aes(y = predicted_hosp, color = "Predicted")) +
#   scale_color_manual(
#     values = c("Actual" = "black", "Predicted" = "orange")
#   ) +
#   scale_y_continuous(labels = scales::comma_format()) +
#   labs(
#     title = "Actual vs Predicted Active Hospitalisations",
#     y = "Active Hospitalisations",
#     x = "Date",
#     colour = ""
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "bottom",
#     axis.ticks.x = element_blank(),
#     axis.text.x = element_blank()
#   )

#=============================
# Non-parametric Bootstrap (i.e. resampling residuals)
#=============================
bootstrap_nonparametric <- function(data, mu_hat, k_hat, n_boot = 1000) {
  admissions <- data$admissions
  active_hosp <- data$active_hosp
  n_obs <- length(admissions)

  # Residuals = Observed - Predicted
  predicted <- predictH(mu = mu_hat, k = k_hat, admissions = admissions)
  residuals <- active_hosp - predicted

  boot_params <- matrix(NA, nrow = n_boot, ncol = 2)
  for (i in 1:n_boot) {
    # Resample residuals with replacement
    boot_residuals <- sample(residuals, size = n_obs, replace = TRUE)
    boot_active_hosp <- predicted + boot_residuals
    boot_active_hosp <- pmax(boot_active_hosp, 0) # Ensure no negative hospitalisations

    # Refit model to bootstrap sample
    boot_fit <- optim(
      par = c(log(mu_hat), log(k_hat)),
      fn = negbin_sse,
      admissions = admissions,
      active_hosp = boot_active_hosp,
      lower = c(log(1e-3), log(1e-3)),
      method = "L-BFGS-B"
    )
    boot_params[i, ] <- exp(boot_fit$par)
  }
  return(boot_params)
}

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
  time_pipe("bootstrapping") %>% 
  select(-data, -boot_params, -boot_preds)
write_csv(
  fits_with_ci,
  "~/Downloads/table1usa_mu_k.csv"
)
# 
# #=============================
# # removing season_year combo 
# #=============================
#=============================
# Optimization Functions
#=============================
predictH <- function(mu, k, max_stay = 50, admissions) {
  survival_probs <- round(1 - pnbinom(0:max_stay, size = k, mu = mu), 6)
  survival_probs[!is.finite(survival_probs)] <- 0
  predicted <- convolve(admissions, rev(survival_probs), type = "open")
  predicted <- predicted[1:length(admissions)]
  predicted[!is.finite(predicted)] <- 0
  predicted
}

negbin_sse <- function(params, admissions, active_hosp) {
  mu <- exp(params[1])
  k  <- exp(params[2])
  predicted <- predictH(mu = mu, k = k, admissions = admissions)
  sse <- sum((active_hosp - predicted)^2)
  if (!is.finite(sse)) return(1e10)
  sse
}

#=============================
# Fit per state and season (no year)
#=============================
plan(multisession, workers = max(1, availableCores() - 2))
set.seed(123)

fits <- df |>
  mutate(
    season = hydroTSM::time2season(date, out.fmt = "seasons")
  ) |>
  nest(.by = c(state, season)) |>
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
    k  = exp(map_dbl(params, ~ .x$par[2]))
  ) |>
  select(-params) |>
  time_pipe("fitting")

#=============================
# Predictions
#=============================
predictions <- fits |>
  group_by(state, season) |>
  unnest(data) |>
  mutate(
    predicted_hosp = predictH(mu = mu, k = k, admissions = admissions)
  ) |>
  ungroup()

los_estimates <- predictions |>
  distinct(state, season, mu, k) |>
  mutate(
    expected_los = round(mu),
    sd_los = round(sqrt(k))
  )

#=============================
# Bootstrap (CIs)
#=============================
bootstrap_nonparametric <- function(data, mu_hat, k_hat, n_boot = 1000) {
  admissions  <- data$admissions
  active_hosp <- data$active_hosp
  n_obs <- length(admissions)
  
  predicted  <- predictH(mu = mu_hat, k = k_hat, admissions = admissions)
  residuals  <- active_hosp - predicted
  
  boot_params <- matrix(NA, nrow = n_boot, ncol = 2)
  for (i in 1:n_boot) {
    boot_residuals    <- sample(residuals, size = n_obs, replace = TRUE)
    boot_active_hosp  <- pmax(predicted + boot_residuals, 0)
    boot_fit <- optim(
      par = c(log(mu_hat), log(k_hat)),
      fn  = negbin_sse,
      admissions   = admissions,
      active_hosp  = boot_active_hosp,
      lower = c(log(1e-3), log(1e-3)),
      method = "L-BFGS-B"
    )
    boot_params[i, ] <- exp(boot_fit$par)
  }
  boot_params
}

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
    k_lower  = map_dbl(boot_params, ~ quantile(.x[, 2], 0.025)),
    k_upper  = map_dbl(boot_params, ~ quantile(.x[, 2], 0.975))
  ) |>
  time_pipe("bootstrapping") |>
  select(-data, -boot_params)

#=============================
# Optional: Formatted Columns
#=============================
fits_with_ci <- fits_with_ci |>
  mutate(
    mu_formatted = sprintf("%.1f (%.1f, %.1f)", mu, mu_lower, mu_upper),
    k_formatted  = sprintf("%.1f (%.1f, %.1f)", k,  k_lower,  k_upper)
  )


write_csv(
  fits_with_ci,
  "~/Downloads/table1usa_mu_k_no_season.csv"
)

