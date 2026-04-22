# Weekly forecast trajectories for one state, with 50% and 95% bands,
# observed series overlaid. One trajectory per forecast_date, out to
# `horizon` days ahead.
#
#   target_name   "admissions" or "census"; selects both the forecast
#                 filter and the observed series to overlay.
#   model_label   which model in `forecasts` to plot. Defaults to
#                 "ensemble" for admissions, "ensemble+LOS" for census.
#
# Typical use:
#   plot_trajectories("CA", "admissions", forecasts, hhs)
#   plot_trajectories("CA", "census",     forecasts, hhs)
plot_trajectories <- function(
  state_i,
  target_name = c("admissions", "census"),
  forecasts,
  hhs,
  model_label = NULL,
  horizon = 7L
) {
  target_name <- match.arg(target_name)
  if (is.null(model_label)) {
    model_label <- if (target_name == "admissions") "ensemble" else "ensemble+LOS"
  }
  qs <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  fcast_pi <- forecasts |>
    filter(
      state == state_i,
      target == target_name,
      model == model_label,
      as.integer(target_end_date - forecast_date) >= 0,
      as.integer(target_end_date - forecast_date) <= horizon,
      quantile %in% qs
    ) |>
    pivot_wider(
      id_cols = c(forecast_date, target_end_date),
      names_from = quantile, values_from = value, names_prefix = "q"
    )

  obs <- hhs |>
    filter(state == state_i, date >= min(fcast_pi$forecast_date)) |>
    transmute(target_end_date = date, observed = .data[[target_name]])

  origin_pts <- obs |>
    filter(target_end_date %in% unique(fcast_pi$forecast_date))

  ggplot() +
    # Observed series
    geom_line(
      data = obs,
      aes(target_end_date, observed),
      col = "black", linewidth = 0.3
    ) +
    # Dot at each forecast origin
    geom_point(
      data = origin_pts,
      aes(target_end_date, observed),
      col = "black", size = 1.2
    ) +
    # 95% band per forecast_date
    geom_ribbon(
      data = fcast_pi,
      aes(target_end_date, ymin = q0.025, ymax = q0.975,
          group = forecast_date),
      fill = "#0D0887FF", alpha = 0.18
    ) +
    # 50% band per forecast_date
    geom_ribbon(
      data = fcast_pi,
      aes(target_end_date, ymin = q0.25, ymax = q0.75,
          group = forecast_date),
      fill = "#0D0887FF", alpha = 0.35
    ) +
    # Median line per forecast_date
    geom_line(
      data = fcast_pi,
      aes(target_end_date, q0.5, group = forecast_date),
      col = "#0D0887FF", linewidth = 0.4
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_date(date_labels = "%b %Y") +
    labs(
      title = sprintf(
        "%s: weekly %s forecast trajectories (up to %d days ahead, %s)",
        state_i, target_name, horizon, model_label
      ),
      x = NULL, y = stringr::str_to_title(target_name)
    ) +
    theme_bw()
}
