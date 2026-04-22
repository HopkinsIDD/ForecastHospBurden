# =============================================
#   Scoring + WIS plots
# =============================================
# `forecasts` is a long tibble with:
#   model         "ensemble+LOS", "truth+LOS", "ensemble", "baseline", ...
#   target        "census" or "admissions"
#   state, forecast_date, target_end_date, quantile, value

# Join each forecast row with its matching observed value and score WIS.
score_forecast <- function(forecasts, hhs) {
  obs <- hhs |>
    pivot_longer(
      c(census, admissions),
      names_to = "target",
      values_to = "observed"
    ) |>
    rename(target_end_date = date)

  forecasts |>
    inner_join(obs, by = c("state", "target_end_date", "target")) |>
    transmute(
      model,
      target,
      location = state,
      forecast_date,
      target_end_date,
      horizon = as.integer(target_end_date - forecast_date),
      quantile_level = quantile,
      predicted = value,
      observed
    ) |>
    as_forecast_quantile() |>
    score()
}

# Relative WIS = WIS(model) / WIS(baseline), per (target, location,
# horizon). Baseline rows are removed from the output. rWIS < 1 means
# the model beats the baseline on that cell.
relative_wis <- function(scores, baseline = "baseline") {
  means <- scores |>
    summarise(
      wis = mean(wis, na.rm = TRUE),
      .by = c(model, target, location, horizon)
    )
  base <- means |>
    filter(model == baseline) |>
    select(target, location, horizon, wis_baseline = wis)
  means |>
    filter(model != baseline) |>
    inner_join(base, by = c("target", "location", "horizon")) |>
    mutate(rwis = wis / wis_baseline)
}
