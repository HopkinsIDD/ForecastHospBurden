# ------------------------------------------------------------
#   Hospital census forecast pipeline (powered by censcast)
# ------------------------------------------------------------
# Turn hub admission forecasts into census forecasts by fitting a
# length of stay distribution per state and respiratory season, then
# applying the previous same kind season's LOS to new forecasts.
#
#   census(t) = sum_d admissions(t - d) * P(LOS > d)
#
# Two models, identical schema, scored side by side:
#   ensemble_LOS  WIS using the hub admission quantile forecast.
#   truth_LOS     WIS if admissions were perfectly known (LOS only
#                 floor, with spread from a residual bootstrap on the
#                 fitted LOS).
#
# The gap (ensemble_LOS - truth_LOS) is the admission forecast attributable
# error: the share of census error that can only be reduced by
# improving the upstream admission forecast.

# --- Setup ---------------------------------------------------------------
list.files(here::here("source/helpers"), full.names = TRUE) |>
  purrr::walk(source)

set.seed(123)
plan(multisession, workers = availableCores() - 2)

MAX_STAY <- 50L
MAX_HORIZON <- 14L

# --- 1. Data -------------------------------------------------------------
# Long format (state, date, admissions, census) for fitting.
hhs <- load_hhs()
# Hub admission quantile forecasts, raw shape.
hub <- load_hub()

# Reshape both to censcast's hubverse format. censcast functions take
# this shape in and out.
admission_fcast <- hub |>
  transmute(
    model_id = "COVIDhub-ensemble",
    location = state,
    reference_date = forecast_date,
    horizon = as.integer(target_end_date - forecast_date),
    target_end_date,
    target = "day inc covid hosp",
    output_type = "quantile",
    output_type_id = as.character(quantile),
    value
  ) |>
  mutate(prev_season = previous_season(season_of(reference_date)))

admission_history <- hhs |>
  transmute(
    target_end_date = date,
    target = "day inc covid hosp",
    location = state,
    observation = admissions
  )

census_history <- hhs |>
  transmute(
    target_end_date = date,
    target = "day inc covid hosp census",
    location = state,
    observation = census
  )

q_levels <- hub |> pull(quantile) |> unique() |> sort()

# --- 2. Fit LOS per (state, season) --------------------------------------
# Point fit from censcast; residual bootstrap from helpers/bootstrap.R.
los_fits <-
  hhs |>
  mutate(season_year = season_of(date)) |>
  nest(.by = c(state, season_year)) |>
  filter(map_int(data, nrow) >= MAX_STAY + 30L) |>
  mutate(
    fit = future_map(
      data,
      \(d) fit_los(d, family = "negbin", max_stay = MAX_STAY),
      .options = furrr_options(seed = TRUE)
    ),
    boot_surv = future_map2(
      data,
      fit,
      \(d, f) bootstrap_los(d, f, n_boot = 100, max_stay = MAX_STAY),
      .options = furrr_options(seed = TRUE)
    )
  ) |>
  pipetime::time_pipe("LOS fitting")

# --- 3. Forecasts --------------------------------------------------------
# Each forecast date uses the previous same kind season's LOS.

# ensemble_LOS: censcast::fcast_census per (state, previous season).
ensemble_los_fcasts <-
  los_fits |>
  rename(prev_season = season_year) |>
  mutate(
    census_fcast = future_pmap(
      list(state, prev_season, fit),
      \(loc, ps, los) {
        fc_subset <- admission_fcast |>
          filter(location == loc, prev_season == ps) |>
          select(-prev_season)
        if (nrow(fc_subset) == 0) {
          return(NULL)
        }
        fcast_census(
          fc_subset,
          los,
          admission_history |> filter(location == loc)
        )
      },
      .options = furrr_options(seed = TRUE)
    )
  ) |>
  pull(census_fcast) |>
  bind_rows() |>
  mutate(model_id = "ensemble_LOS") |>
  time_pipe("Ensemble LOS forecasts")

# truth_LOS: convolve observed admissions with bootstrap LOS draws.
truth_los_fcasts <-
  los_fits |>
  rename(prev_season = season_year) |>
  mutate(
    census_fcast = future_pmap(
      list(state, prev_season, boot_surv),
      \(loc, ps, boot) {
        ref_dates <- admission_fcast |>
          filter(location == loc, prev_season == ps) |>
          distinct(reference_date) |>
          pull(reference_date)
        if (length(ref_dates) == 0) {
          return(NULL)
        }
        map(
          ref_dates,
          \(rd) {
            truth_los_one(
              loc,
              rd,
              boot,
              hhs,
              max_stay = MAX_STAY,
              max_horizon = MAX_HORIZON,
              q_levels = q_levels
            )
          }
        ) |>
          bind_rows()
      },
      .options = furrr_options(seed = TRUE)
    )
  ) |>
  pull(census_fcast) |>
  bind_rows()

forecasts <- bind_rows(ensemble_los_fcasts, truth_los_fcasts)

# --- 4. Score ------------------------------------------------------------
scores <- score_census(forecasts, census_truth = census_history) |>
  time_pipe("Scoring forecasts")

# --- 5. Visualise --------------------------------------------------------
# Trajectories for one location, both models on stacked facets.
plot_fan(
  forecasts |> filter(horizon <= MAX_HORIZON),
  location = "CA",
  truth = census_history
) +
  facet_wrap(~model_id, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Census")

# WIS by horizon. The truth_LOS curve is the recoverable floor;
# (ensemble_LOS - truth_LOS) is the admission attributable error.
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
  scale_x_continuous(breaks = seq(1, MAX_HORIZON, 3)) +
  labs(
    x = "Horizon (days)",
    y = "Mean WIS (census)",
    fill = NULL,
    title = "Census error decomposition"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

#last season's LOS is a bit too long for this season,
# so each admission's "still in hospital" probability is overstated,
# which inflates the convolution and pushes the predicted census
# slightly above the actual census.
