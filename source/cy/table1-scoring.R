library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(scoringutils)

qs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

make_quant_tbl <- function(obs, boot_preds, qs) {
  n_dates <- nrow(obs)
  bp <- as.matrix(boot_preds)
  
  # ensure dates are in columns
  if (ncol(bp) == n_dates) {
    bp_dates_in_cols <- bp
  } else if (nrow(bp) == n_dates) {
    bp_dates_in_cols <- t(bp)
  } else {
    stop(
      "boot_preds dimensions don't match obs dates. ",
      "nrow(obs)=", n_dates,
      ", dim(boot_preds)=", paste(dim(bp), collapse = "x")
    )
  }
  
  # quantiles per date (per column)
  qmat <- apply(bp_dates_in_cols, 2, stats::quantile, probs = qs, na.rm = TRUE)
  qmat <- t(qmat)  # n_dates x length(qs)
  
  out <- tibble(
    target_end_date = obs$date,
    truth = obs$active_hosp
  )
  
  qnames <- paste0("q", qs)
  out[qnames] <- as.data.frame(qmat)
  out
}

# -------------------------------
# 1) Build a quantile forecast data frame + truth
# -------------------------------
scoring_df <- fits_with_ci %>%
  filter(state == "USA") %>%
  transmute(
    season_year,
    obs = data,
    boot_preds
  ) %>%
  mutate(
    quant_tbl = pmap(
      list(obs, boot_preds),
      ~ make_quant_tbl(..1, ..2, qs = qs)
    )
  ) %>%
  select(season_year, quant_tbl) %>%
  unnest(quant_tbl) %>%
  pivot_longer(
    cols = starts_with("q"),
    names_to = "quantile",
    values_to = "prediction"
  ) %>%
  mutate(
    quantile = as.numeric(sub("^q", "", quantile)),
    model = "calibrated_bootstrap",
    location = "USA",
    target = "active_hosp",
    forecast_date = target_end_date - 1
  ) %>%
  select(
    model, forecast_date, target_end_date, location,
    target, quantile, prediction, truth, season_year
  )

library(dplyr)
library(scoringutils)

forecast_quantiles3 <- scoring_df %>%
  transmute(
    model,
    forecast_date,
    target_end_date,
    location,
    target,
    quantile_level = as.numeric(quantile),   # <-- KEY FIX
    predicted = prediction,
    observed = truth,
    season_year
  )

# Convert to forecast object
fc <- scoringutils::as_forecast_quantile(forecast_quantiles3)

# Score
scores_by_date <- scoringutils::score(fc)

# keep only the metrics you care about (if they exist in your version)
scores_by_date_selected <- scores_by_date |>
  dplyr::select(dplyr::any_of(c(
    "model","forecast_date","target_end_date","location","target",
    "wis","dispersion","overprediction","underprediction","bias"
  )))

scores_by_season <- scores_by_date_selected |>
  dplyr::left_join(
    forecast_quantiles3 |> dplyr::distinct(target_end_date, season_year),
    by = "target_end_date"
  ) |>
  dplyr::group_by(season_year) |>
  dplyr::summarise(
    n_days = dplyr::n(),
    wis = mean(wis, na.rm = TRUE),
    dispersion = mean(dispersion, na.rm = TRUE),
    overprediction = mean(overprediction, na.rm = TRUE),
    underprediction = mean(underprediction, na.rm = TRUE),
    bias = mean(bias, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(season_year)

scores_by_season
