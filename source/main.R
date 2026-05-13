# ------------------------------------------------------------
#   Hospital census forecast pipeline
# ------------------------------------------------------------
# Turn hub admission forecasts into census forecasts by fitting a
# length-of-stay (LOS) distribution per state and season, then using
# the previous season's LOS on new forecasts.
#
# census(t) = Σ admissions(s) · P(LOS > t − s)
# today's census is yesterday's and earlier days' admissions, each
# weighted by the chance that patient is still in the hospital.
#
# 1. Learn LOS. Using HHS daily admissions + census per state,
# fit a negative-binomial LOS survival curve P(LOS > d) per state × respiratory season
# by minimizing SSE between observed census and the convolution of admissions with that curve. A residual bootstrap (100 resamples) gives parameter uncertainty.

# 2. Apply it to the future. Take the COVIDhub admission forecast
# (quantile paths, 14-day horizon),
# and for each quantile level convolve that admission path with
# the previous same-kind season's LOS curve.
# Output: a census quantile forecast on the same grid.

# 3.
# --- Setup ---------------------------------------------------------------
list.files(here::here("source/helpers"), full.names = TRUE) |>
  purrr::walk(source)

set.seed(123)
plan(multisession, workers = availableCores() - 2)

# --- 1. Data -------------------------------------------------------------
hhs <- load_hhs()
hub <- load_hub()

# --- 2. Fit LOS per (state, season) --------------------------------------
los_fits <-
  hhs |>
  mutate(season_year = season_of(date)) |>
  nest(.by = c(state, season_year)) |>
  filter(map_int(data, nrow) >= MAX_STAY + 30L) |>
  mutate(
    res = future_map(
      data,
      fit_los,
      dist_negbin,
      .options = furrr_options(seed = TRUE)
    )
  ) |>
  unnest_wider(res) |>
  unnest_wider(params)

# --- 3. Forecast both models into one tibble ----------------------------
# ensemble_LOS = WIS given the predicted admission forecast.
# truth_LOS    = WIS if admissions were perfectly known.
forecasts <- bind_rows(
  forecast_from_hub(hub, hhs, los_fits) |>
    mutate(model = "ensemble_LOS", target = "census"),
  forecast_from_truth(hub, hhs, los_fits) |>
    mutate(model = "truth_LOS", target = "census")
)

# --- 4. Score ------------------------------------------------------------
scores <- score_forecast(forecasts, hhs)

# --- 5. Visualise --------------------------------------------------------
plot_trajectories("NY", forecasts, hhs, horizon = 14L)


# WIS by horizon
# truth_LOS = the best possible census forecast if LOS were perfectly known. T
# The gap between ensemble_LOS and truth_LOS is the admission-attributable error,
# which can only be reduced by improving the upstream admission forecast.
wis_by_horizon <- scores |>
  summarise(
    wis = mean(wis, na.rm = TRUE),
    .by = c(model, location, horizon)
  ) |>
  pivot_wider(names_from = model, values_from = wis)


wis_by_horizon |>
  mutate(
    truth_LOS = truth_LOS,
    ensemble_LOS = pmax(ensemble_LOS - truth_LOS, 0)
  ) |>
  pivot_longer(
    c(truth_LOS, ensemble_LOS),
    names_to = "source",
    values_to = "wis"
  ) |>
  ggplot(aes(horizon, wis, fill = source)) +
  geom_col(position = position_stack(reverse = FALSE)) +
  facet_wrap(~location, scales = "free_y") +
  scale_x_continuous(breaks = seq(1, 14, 3)) +

  labs(
    x = "Horizon (days)",
    y = "Mean WIS (census)",
    fill = NULL,
    title = "Census error decomposition",
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
