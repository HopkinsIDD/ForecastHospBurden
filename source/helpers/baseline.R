# =============================================
#   Naive baseline quantile forecasts
# =============================================
# The baseline forecast is a naive benchmark. For each location and forecast date:

# Median: stays flat at today's observed value across all horizons.
# Prediction intervals: simulate many random walks forward, drawing day-to-day changes from the last lookback days of history, then take quantiles across the simulated paths.
# This gives a unit-free reference — dividing any model's WIS by this baseline lets you compare admission and census models on the same scale.

baseline_trajectory <- function(
  state_i,
  forecast_date_i,
  obs,
  horizons,
  q_levels,
  lookback = 28L,
  n_sim = 1000L
) {
  last_val <- obs |>
    filter(state == state_i, date == forecast_date_i) |>
    pull(value)
  if (length(last_val) != 1 || !is.finite(last_val)) {
    return(NULL)
  }

  recent <- obs |>
    filter(
      state == state_i,
      date <= forecast_date_i,
      date >= forecast_date_i - lookback
    ) |>
    arrange(date) |>
    pull(value)
  deltas <- diff(recent)
  if (length(deltas) < 2) {
    return(NULL)
  }

  max_h <- max(horizons)
  walks <- replicate(n_sim, cumsum(sample(deltas, max_h, replace = TRUE)))
  preds <- pmax(last_val + walks, 0) # counts can't go negative
  q_mat <- apply(preds, 1, quantile, probs = q_levels, na.rm = TRUE)

  tibble(
    target_end_date = rep(
      forecast_date_i + seq_len(max_h),
      each = length(q_levels)
    ),
    quantile = rep(q_levels, times = max_h),
    value = as.vector(q_mat)
  ) |>
    filter(as.integer(target_end_date - forecast_date_i) %in% horizons)
}

# Baseline forecast for one observed series ("admissions" or "census")
# over the hub's (state, forecast_date) grid and quantile levels.
baseline_from_observed <- function(hhs, hub, target_col, horizons = 1:14) {
  q_levels <- hub |> pull(quantile) |> unique() |> sort()
  obs <- hhs |> select(state, date, value = all_of(target_col))

  hub |>
    distinct(state, forecast_date) |>
    mutate(
      trajectory = future_pmap(
        list(state, forecast_date),
        baseline_trajectory,
        obs = obs,
        horizons = horizons,
        q_levels = q_levels,
        .options = furrr_options(seed = TRUE)
      )
    ) |>
    unnest(trajectory)
}
