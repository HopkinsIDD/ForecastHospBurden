# =============================================
#   Census forecasts from admissions
# =============================================
# Two paths, same output shape. Both use the previous same-kind season's
# LOS (Winter -> Winter, Summer -> Summer), so nothing at forecast time
# uses current-season data.
#
#   forecast_from_hub    hub admission quantiles -> census quantiles.
#                        Spread comes from the hub; LOS is a point
#                        estimate. This is the production pipeline.
#
#   forecast_from_truth  observed admissions -> census quantiles.
#                        Spread comes from the LOS residual bootstrap.
#                        Used to score the LOS model on its own, free
#                        of admission-forecast error.
#
# Both return one row per (state, forecast_date, target_end_date,
# quantile) with a `value` column (predicted census).

# Point-estimate survival for (state, previous-season). NULL if not fit.
lookup_survival <- function(los_fits, state_i, season_i) {
  row <- los_fits |>
    filter(state == state_i, season_year == previous_season(season_i))
  if (nrow(row) == 0) NULL else row$survival[[1]]
}

# Bootstrap survivals for (state, previous-season) as a
# (MAX_STAY+1) x n_boot matrix. NULL if not fit.
lookup_boot_survivals <- function(los_fits, state_i, season_i, los_dist) {
  row <- los_fits |>
    filter(state == state_i, season_year == previous_season(season_i))
  if (nrow(row) == 0) return(NULL)
  surv <- apply(log(row$boot_params[[1]]), 1, los_dist$surv_fn)
  surv[!is.finite(surv)] <- 0
  surv
}

# One (state, forecast_date): hub admission trajectory -> census.
# Convolution is linear, so the census quantile is the convolution of
# the admission quantile path. Returns NA census when the previous
# season's LOS isn't fit, or when fewer than MAX_STAY days of observed
# admissions end on forecast_date.
census_from_trajectory <- function(state_i, forecast_date_i, trajectory,
                                   los_fits, hhs) {
  surv <- lookup_survival(los_fits, state_i, season_of(forecast_date_i))
  if (is.null(surv)) return(mutate(trajectory, value = NA_real_))

  past <- hhs |>
    filter(state == state_i,
           between(date, forecast_date_i - MAX_STAY + 1L, forecast_date_i)) |>
    arrange(date)
  if (nrow(past) < MAX_STAY) return(mutate(trajectory, value = NA_real_))

  # `value` in the input is the hub admission quantile; we replace it
  # in place with the convolved census prediction.
  trajectory |>
    arrange(target_end_date) |>
    mutate(
      value = predict_census(surv, c(past$admissions, value))[-seq_along(past$admissions)],
      .by = quantile
    )
}

# One (state, forecast_date): observed admissions -> census.
# Convolves observed admissions with every bootstrap LOS draw, then
# takes empirical quantiles across draws. Returns NULL if the previous
# season's LOS isn't fit, or if the admission history / horizon window
# isn't fully observed.
census_from_truth <- function(state_i, forecast_date_i, hhs, los_fits,
                              los_dist, horizons, q_levels) {
  boot_surv <- lookup_boot_survivals(
    los_fits, state_i, season_of(forecast_date_i), los_dist
  )
  if (is.null(boot_surv)) return(NULL)

  max_h <- max(horizons)
  series <- hhs |>
    filter(state == state_i,
           between(date,
                   forecast_date_i - MAX_STAY + 1L,
                   forecast_date_i + max_h)) |>
    arrange(date)
  if (nrow(series) < MAX_STAY + max_h) return(NULL)

  preds <- apply(boot_surv, 2, \(s) {
    predict_census(s, series$admissions)[-seq_len(MAX_STAY)]
  })
  target_dates <- series$date[-seq_len(MAX_STAY)]
  q_mat <- apply(preds, 1, quantile, probs = q_levels, na.rm = TRUE)

  tibble(
    target_end_date = rep(target_dates, each = length(q_levels)),
    quantile        = rep(q_levels,   times = length(target_dates)),
    value           = as.vector(q_mat)
  ) |>
    filter(as.integer(target_end_date - forecast_date_i) %in% horizons)
}

# Census quantile forecast from hub admission quantiles.
forecast_from_hub <- function(hub, hhs, los_fits) {
  hub |>
    nest(.by = c(state, forecast_date), .key = "trajectory") |>
    mutate(
      trajectory = future_pmap(
        list(state, forecast_date, trajectory),
        census_from_trajectory,
        los_fits = los_fits, hhs = hhs,
        .options = furrr_options(seed = TRUE)
      )
    ) |>
    unnest(trajectory)
}

# Census quantile forecast from observed admissions (LOS ceiling).
# Uses the hub grid (same state-date pairs, same quantile levels) so
# the two paths can be scored side by side.
forecast_from_truth <- function(hub, hhs, los_fits, los_dist,
                                horizons = 1:14) {
  q_levels <- hub |> pull(quantile) |> unique() |> sort()
  hub |>
    distinct(state, forecast_date) |>
    mutate(
      trajectory = future_pmap(
        list(state, forecast_date),
        census_from_truth,
        hhs = hhs, los_fits = los_fits, los_dist = los_dist,
        horizons = horizons, q_levels = q_levels,
        .options = furrr_options(seed = TRUE)
      )
    ) |>
    unnest(trajectory)
}
