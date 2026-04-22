# ------------------------------------------------------------
#   Hospital census forecast pipeline
# ------------------------------------------------------------
# Turn hub admission forecasts into census forecasts by fitting a
# length-of-stay (LOS) distribution per state and season, then using
# the previous season's LOS on new forecasts.
#
#   census(t) = sum over s <= t of admissions(s) * P(LOS > t - s)
#
# Flow:
#   1. data       observed admissions + census, and hub admission forecasts.
#   2. fit        LOS per (state, season).
#   3. forecast   five quantile forecasts, stacked:
#                   ensemble+LOS   hub admissions -> previous-season LOS.
#                   truth+LOS      observed adms  -> previous-season LOS.
#                                  (LOS-model ceiling.)
#                   ensemble       raw hub admission forecast.
#                   baseline       naive flat forecast per target,
#                                  used as the reference for rWIS.
#   4. score      WIS and relative WIS (WIS / baseline WIS). rWIS is
#                 dimensionless so census and admissions models can be
#                 compared directly.
#   5. visualise  trajectories, raw WIS, rWIS.

# --- Setup ---------------------------------------------------------------
list.files(here::here("source/helpers"), full.names = TRUE) |>
  purrr::walk(source)

set.seed(123)
plan(multisession, workers = availableCores() - 2)

# --- 1. Data -------------------------------------------------------------
hhs <- load_hhs()
hub <- load_hub()

# --- 2. Fit LOS per (state, season) --------------------------------------
los_fits <- fit_los_all(hhs, dist_negbin, min_fit_days = 30)

# --- 3. Forecast all models into one tibble ------------------------------
forecasts <- bind_rows(
  forecast_from_hub(hub, hhs, los_fits) |>
    mutate(model = "ensemble+LOS", target = "census"),
  forecast_from_truth(hub, hhs, los_fits, dist_negbin) |>
    mutate(model = "truth+LOS", target = "census"),
  hub |>
    mutate(model = "ensemble", target = "admissions"),
  baseline_from_observed(hhs, hub, "census") |>
    mutate(model = "baseline", target = "census"),
  baseline_from_observed(hhs, hub, "admissions") |>
    mutate(model = "baseline", target = "admissions")
)

# --- 4. Score ------------------------------------------------------------
scores <- score_forecast(forecasts, hhs)
rel <- relative_wis(scores, baseline = "baseline")

# --- 5. Visualise --------------------------------------------------------
plot_trajectories("CA", "admissions", forecasts, hhs)
plot_trajectories("CA", "census", forecasts, hhs)

# within census target
scores |>
  summarise(
    wis = mean(wis, na.rm = TRUE),
    .by = c(model, target, location, horizon)
  ) |>
  filter(target == "census", model != "baseline") |>
  ggplot(aes(horizon, wis, colour = model)) +
  geom_line() +
  facet_wrap(~location, scales = "free_y") +
  scale_x_continuous(breaks = seq(1, 14, 3)) +
  labs(x = "Horizon (days)", y = "Mean WIS", colour = NULL, linetype = NULL) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# across targets
rel |>
  as_tibble() |>
  filter(
    (target == "census" & model == "ensemble+LOS") |
      (target == "admissions" & model == "ensemble")
  ) |>
  ggplot(aes(horizon, rwis, colour = model)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line() +
  facet_wrap(~location, scales = "free_y") +
  scale_x_continuous(breaks = seq(1, 14, 3)) +
  labs(
    x = "Horizon (days)",
    y = "Relative WIS (model / baseline)",
    colour = NULL,
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
