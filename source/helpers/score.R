# =============================================
#   Scoring
# =============================================
# `forecasts` is a long tibble with:
#   model         "ensemble+LOS", "truth+LOS"
#   target        "census"
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
