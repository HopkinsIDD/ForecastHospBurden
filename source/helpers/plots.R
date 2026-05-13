# Census forecast trajectories for one state: 50%/95% bands and median,
# observed series overlaid. Faceted by `model` so one or many models can
# be passed. `fcasts` must have a `model` column.
plot_trajectories <- function(state_i, fcasts, hhs, horizon = 7L) {
  fcast_pi <- fcasts |>
    filter(
      state == state_i,
      between(as.integer(target_end_date - forecast_date), 0L, horizon),
      quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)
    ) |>
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "q") |>
    arrange(model, forecast_date, target_end_date)

  obs <- hhs |>
    filter(state == state_i, date >= min(fcast_pi$forecast_date))

  ggplot() +
    geom_ribbon(
      data = fcast_pi,
      aes(target_end_date, ymin = q0.025, ymax = q0.975, group = forecast_date),
      fill = "#0D0887FF", alpha = 0.18
    ) +
    geom_ribbon(
      data = fcast_pi,
      aes(target_end_date, ymin = q0.25, ymax = q0.75, group = forecast_date),
      fill = "#0D0887FF", alpha = 0.35
    ) +
    geom_line(
      data = fcast_pi,
      aes(target_end_date, q0.5, group = forecast_date),
      col = "#0D0887FF", linewidth = 0.4
    ) +
    geom_line(data = obs, aes(date, census), col = "black", linewidth = 0.3) +
    facet_wrap(~ model, ncol = 1) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = "Census") +
    theme_bw()
}
