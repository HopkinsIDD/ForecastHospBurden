library(future)

# -------------------------------------
#           Setup
# -------------------------------------
set.seed(123)
options(pipetime.log = "log", pipetime.unit = "secs")
plan(multisession, workers = availableCores() - 2)

list.files(here::here("source/cy/helpers"), full.names = TRUE) |>
  purrr::walk(source)

# -------------------------------------
#           Data (US-wide)
# -------------------------------------
df <- arrow::read_parquet(
  here::here(
    "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
  )
) |>
  arrange(state, date) |>
  mutate(
    date = as.Date(date),
    active_hosp = total_adult_patients_hospitalized_confirmed_covid +
      total_pediatric_patients_hospitalized_confirmed_covid,
    admissions = lead(
      previous_day_admission_adult_covid_confirmed +
        previous_day_admission_pediatric_covid_confirmed
    ),
    .by = "state",
    .keep = "none"
  ) |>
  drop_na() |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    active_hosp = sum(active_hosp, na.rm = TRUE),
    admissions  = sum(admissions,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(state = "USA") |>
  dplyr::arrange(date)

df

# -------------------------------------
#           Initial fit (US-wide)
# -------------------------------------
fits <- df |>
  # update season year so winter respiratory seasons are grouped together 
  # ie: dec 2022-feb 2023 = winter_2023
  mutate(
    season = hydroTSM::time2season(date, out.fmt = "seasons"),
    season_year = paste0(
      season,
      "_",
      dplyr::if_else(
        season == "winter" & lubridate::month(date) == 12,
        lubridate::year(date) + 1L,
        lubridate::year(date)
      )
    )
  ) |>
  tidyr::nest(.by = c(state, season_year)) |>
  mutate(
    params = furrr::future_map(
      .x = data,
      .f = function(d) {
        optim(
          par = c(log(2), log(1)),
          fn = negbin_sse,
          admissions = d$admissions,
          active_hosp = d$active_hosp,
          lower = c(log(1e-6), log(1e-6)),
          method = "L-BFGS-B"
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    ),
    mu = exp(purrr::map_dbl(params, ~ .x$par[1])),
    k  = exp(purrr::map_dbl(params, ~ .x$par[2])),
    .by = c("state", "season_year")
  ) |>
  dplyr::select(-params) |>
  time_pipe("fitting")

# -------------------------------------
#           Bootstrap CIs (US-wide)
# -------------------------------------
# here lower bound mu is higher than mu for some seasons
fits_with_ci <- fits |>
  mutate(
    boot_params = furrr::future_pmap(
      .l = list(data, mu, k),
      .f = bootstrap_nonparametric,
      n_boot = 1000,
      .options = furrr::furrr_options(seed = TRUE)
    ),
    mu_median = purrr::map_dbl(boot_params, ~ stats::quantile(.x[, 1], 0.5, na.rm = TRUE)),
    mu_lower = purrr::map_dbl(boot_params, ~ stats::quantile(.x[, 1], 0.025, na.rm = TRUE)),
    mu_upper = purrr::map_dbl(boot_params, ~ stats::quantile(.x[, 1], 0.975, na.rm = TRUE)),
    k_median = purrr::map_dbl(boot_params, ~ stats::quantile(.x[, 2], 0.5, na.rm = TRUE)),
    k_lower  = purrr::map_dbl(boot_params, ~ stats::quantile(.x[, 2], 0.025, na.rm = TRUE)),
    k_upper  = purrr::map_dbl(boot_params, ~ stats::quantile(.x[, 2], 0.975, na.rm = TRUE)),
    
    # Predictions for plotting CI bands
    boot_preds = furrr::future_pmap(
      .l = list(data, boot_params),
      .f = function(d, boot_params) {
        admissions <- d$admissions
        preds <- apply(boot_params, 1, function(params) {
          predictH(mu = params[1], k = params[2], admissions = admissions)
        })
        preds
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  ) |>
  time_pipe("bootstrapping")

# ============================================================
#   Rounded version of fits_with_ci (nearest 0.1)
# ============================================================

library(dplyr)
library(purrr)

round1 <- function(x) round(x, 1)

fits_with_ci_rounded <- fits_with_ci %>%
  mutate(
    # round observed season-only data
    data = map(data, ~ .x %>%
                 mutate(
                   active_hosp = round1(active_hosp),
                   admissions  = round1(admissions)
                 )
    ),
    # round bootstrap predictions (matrix or data.frame)
    boot_preds = map(boot_preds, ~ {
      bp <- as.matrix(.x)
      round1(bp)
    }),
    # round parameter estimates + CI bounds
    across(
      .cols = c(mu, k, mu_lower, mu_upper, k_lower, k_upper),
      .fns  = round1
    )
  )

fits_with_ci_rounded_summary <- fits_with_ci |>
  dplyr::select(
    state, season_year,
    mu, mu_lower, mu_upper,
    k,  k_lower,  k_upper
  )

write.csv(
  fits_with_ci_rounded_summary,
  "/Users/sarahcotton/Library/Mobile Documents/com~apple~CloudDocs/hopkins mph /COVID-19 Hosp burden project/2026 paper/output/USA_active_hosp_fits_with_bootstrap_CI_rounded.csv",
  row.names = FALSE
)


# -------------------------------------
#           Visualization 
# -------------------------------------
# ============================================================
# Save plots HERE
# ============================================================


# ============================================================
# Save each plot to disk
# - Plot 1 saved as ONE faceted figure
# - Plots 2 & 3 saved as separate figures
# - Also saves Plot 1 as ONE FILE PER season_year (optional but usually what people mean)
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(scales)
library(forcats)
library(ggridges)
library(ggh4x)

out_dir <- "/Users/sarahcotton/Library/Mobile Documents/com~apple~CloudDocs/hopkins mph /COVID-19 Hosp burden project/2026 paper/plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Plot 1 data prep
# ------------------------------------------------------------
plot_df <- fits_with_ci |>
  dplyr::select(state, season_year, data, boot_preds) |>
  dplyr::filter(state == "USA") |>
  dplyr::mutate(
    boot_preds = purrr::map(boot_preds, ~ tibble::as_tibble(.x))
  ) |>
  tidyr::unnest(c(data, boot_preds)) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("V"),
    names_to = "bootstrap_id",
    values_to = "boot_pred"
  )

# ------------------------------------------------------------
# Plot 1 (FACETED ALL SEASONS IN ONE FILE)
# ------------------------------------------------------------
p1_all <- ggplot2::ggplot(plot_df, ggplot2::aes(x = date)) +
  ggplot2::facet_wrap(~ season_year, scales = "free") +
  ggplot2::geom_line(ggplot2::aes(y = active_hosp, color = "Actual")) +
  ggplot2::geom_point(
    ggplot2::aes(y = boot_pred, color = "Predicted"),
    size = 0.5,
    alpha = 0.01
  ) +
  ggplot2::scale_color_manual(
    values = c("Actual" = "black", "Predicted" = "orange"),
    guide = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 3))
  ) +
  ggplot2::scale_y_continuous(labels = scales::comma_format()) +
  ggplot2::scale_x_date(
    date_labels = "%b %Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::labs(
    title = "USA: Actual vs Predicted Active Hospitalisations by Season-Year",
    y = "Active Hospitalisations",
    x = "Date",
    colour = ""
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

# trimmed version 

plot_df_trimmed <- plot_df %>%
  group_by(season_year) %>%
  filter(date >= min(date) + 10) %>%
  ungroup()
p1_all <- ggplot2::ggplot(plot_df_trimmed, ggplot2::aes(x = date)) +
  ggplot2::facet_wrap(~ season_year, scales = "free") +
  ggplot2::geom_line(ggplot2::aes(y = active_hosp, color = "Actual")) +
  ggplot2::geom_point(
    ggplot2::aes(y = boot_pred, color = "Predicted"),
    size = 0.5,
    alpha = 0.01
  ) +
  ggplot2::scale_color_manual(
    values = c("Actual" = "black", "Predicted" = "orange"),
    guide = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 3))
  ) +
  ggplot2::scale_y_continuous(labels = scales::comma_format()) +
  ggplot2::scale_x_date(
    date_labels = "%b %Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::labs(
    title = "USA: Actual vs Predicted Active Hospitalisations by Season-Year",
    y = "Active Hospitalisations",
    x = "Date",
    colour = ""
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )
ggplot2::ggsave(
  filename = file.path(out_dir, "USA_actual_vs_predicted_active_hosp_all_seasons.png"),
  plot = p1_all,
  width = 14, height = 10, dpi = 300
)

# ------------------------------------------------------------
# Plot 1 (ONE FILE PER season_year)  <-- usually what "save each plot" means
# ------------------------------------------------------------
season_levels <- sort(unique(plot_df$season_year))

for (sy in season_levels) {
  p1_sy <- ggplot2::ggplot(
    plot_df |> dplyr::filter(season_year == sy),
    ggplot2::aes(x = date)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = active_hosp, color = "Actual")) +
    ggplot2::geom_point(
      ggplot2::aes(y = boot_pred, color = "Predicted"),
      size = 0.5,
      alpha = 0.01
    ) +
    ggplot2::scale_color_manual(
      values = c("Actual" = "black", "Predicted" = "orange"),
      guide = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 3))
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_x_date(
      date_labels = "%b %Y",
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::labs(
      title = paste0("USA: Actual vs Predicted Active Hospitalisations — ", sy),
      y = "Active Hospitalisations",
      x = "Date",
      colour = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  safe_sy <- gsub("[^A-Za-z0-9_\\-]+", "_", sy)
  
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0("USA_actual_vs_predicted_active_hosp_", safe_sy, ".png")),
    plot = p1_sy,
    width = 9, height = 5, dpi = 300
  )
}

# ------------------------------------------------------------
# mk_data for parameter plots
# ------------------------------------------------------------
mk_data <- fits_with_ci |>
  dplyr::select(season_year, boot_params) |>
  dplyr::mutate(
    boot_params = purrr::map(boot_params, ~ as.data.frame(.x))
  ) |>
  tidyr::unnest(boot_params) |>
  dplyr::rename(mu = V1, k = V2) |>
  dplyr::mutate(
    season_year = forcats::fct_reorder(season_year, mu, .fun = mean, .desc = TRUE)
  ) |>
  tidyr::pivot_longer(
    cols = c(mu, k),
    names_to = "parameter",
    values_to = "value"
  ) |>
  dplyr::mutate(parameter = factor(parameter, levels = c("mu", "k")))

# ------------------------------------------------------------
# Plot 2: μ ridges + k errorbars
# ------------------------------------------------------------
p2 <- ggplot2::ggplot() +
  ggplot2::facet_grid(
    cols = ggplot2::vars(parameter),
    scales = "free_x",
    labeller = ggplot2::labeller(parameter = c("mu" = "μ", "k" = "k"))
  ) +
  ggridges::geom_density_ridges_gradient(
    data = mk_data |> dplyr::filter(parameter == "mu"),
    ggplot2::aes(y = season_year, x = value, height = ..density..),
    stat = "density",
    trim = TRUE,
    col = NA,
    fill = "#0D0887FF",
    alpha = 0.8
  ) +
  ggplot2::geom_errorbarh(
    data = mk_data |>
      dplyr::filter(parameter == "k") |>
      dplyr::group_by(season_year, parameter) |>
      dplyr::summarise(
        median_k = median(value),
        lower_k  = quantile(value, 0.025),
        upper_k  = quantile(value, 0.975),
        .groups = "drop"
      ),
    ggplot2::aes(y = season_year, x = median_k, xmin = lower_k, xmax = upper_k),
    height = 0.3,
    color = "black"
  ) +
  ggh4x::facetted_pos_scales(
    x = list(
      parameter == "k"  ~ ggplot2::scale_x_continuous(limits = c(0, 5)),
      parameter == "mu" ~ ggplot2::scale_x_continuous(limits = c(0, 15))
    )
  ) +
  ggplot2::labs(
    title = "USA: Bootstrapped Parameter Estimates by Season-Year",
    x = "parameter value",
    y = "season-year"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white", color = "black"),
    strip.text = ggplot2::element_text(face = "bold", size = 12)
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "USA_bootstrap_params_ridge_mu_errorbar_k.png"),
  plot = p2,
  width = 12, height = 8, dpi = 300
)

# ------------------------------------------------------------
# Plot 3: ridges for both μ and k
# ------------------------------------------------------------
season_levels <- c(
  "summer_2020",
  "autumn_2020",
  "winter_2021",
  "spring_2021",
  "summer_2021",
  "autumn_2021",
  "winter_2022",
  "spring_2022",
  "summer_2022",
  "autumn_2022",
  "winter_2023",
  "spring_2023",
  "summer_2023",
  "autumn_2023",
  "winter_2024",
  "spring_2024"
)
mk_data$season_year <- factor(
  mk_data$season_year,
  levels = rev(season_levels)
)

p3 <- ggplot2::ggplot(mk_data, ggplot2::aes(x = value, y = season_year)) +
  ggplot2::facet_wrap(
    ~ parameter,
    scales = "free_x",
    labeller = ggplot2::labeller(parameter = c("mu" = "μ", "k" = "k"))
  ) +
  ggridges::geom_density_ridges(
    fill = "#0D0887FF",
    alpha = 0.8,
    col = NA,
    trim = TRUE
  ) +
  ggplot2::labs(
    title = "USA: Bootstrapped Parameter Distributions by Season-Year",
    x = "parameter value",
    y = "season-year"
  ) +
  ggplot2::theme_bw()

ggplot2::ggsave(
  filename = file.path(out_dir, "USA_bootstrap_param_distributions_ridges.png"),
  plot = p3,
  width = 12, height = 8, dpi = 300
)

message("Saved plots to: ", out_dir)
# -------------------------------------
fits_with_ci |>
  dplyr::select(state, season_year, data, boot_preds) |>
  dplyr::filter(state == "USA") |>
  dplyr::mutate(
    boot_preds = purrr::map(boot_preds, ~ tibble::as_tibble(.x))
  ) |>
  tidyr::unnest(c(data, boot_preds)) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("V"),
    names_to = "bootstrap_id",
    values_to = "boot_pred"
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::facet_wrap(
    ~ season_year,
    scales = "free"   
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = active_hosp, color = "Actual")
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = boot_pred, color = "Predicted"),
    size = 0.5,
    alpha = 0.01
  ) +
  ggplot2::scale_color_manual(
    values = c("Actual" = "black", "Predicted" = "orange"),
    guide = ggplot2::guide_legend(
      override.aes = list(alpha = 1, size = 3)
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::comma_format()
  ) +
  ggplot2::scale_x_date(
    date_labels = "%b %Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::labs(
    title = "USA: Actual vs Predicted Active Hospitalisations by Season-Year",
    y = "Active Hospitalisations",
    x = "Date",
    colour = ""
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

mk_data <- fits_with_ci |>
  dplyr::select(season_year, boot_params) |>
  dplyr::mutate(
    boot_params = purrr::map(boot_params, ~ as.data.frame(.x))
  ) |>
  tidyr::unnest(boot_params) |>
  dplyr::rename(mu = V1, k = V2) |>
  dplyr::mutate(
    season_year = forcats::fct_reorder(
      season_year, mu, .fun = mean, .desc = TRUE
    )
  ) |>
  tidyr::pivot_longer(
    cols = c(mu, k),
    names_to = "parameter",
    values_to = "value"
  ) |>
  dplyr::mutate(parameter = factor(parameter, levels = c("mu", "k")))

ggplot2::ggplot() +
  ggplot2::facet_grid(
    cols = ggplot2::vars(parameter),
    scales = "free_x",
    labeller = ggplot2::labeller(
      parameter = c("mu" = "μ", "k" = "k")
    )
  ) +
  ggridges::geom_density_ridges_gradient(
    data = mk_data |> dplyr::filter(parameter == "mu"),
    ggplot2::aes(
      y = season_year,
      x = value,
      height = ..density..
    ),
    stat = "density",
    trim = TRUE,
    col = NA,
    fill = "#0D0887FF",
    alpha = 0.8
  ) +
  ggplot2::geom_errorbarh(
    data = mk_data |>
      dplyr::filter(parameter == "k") |>
      dplyr::group_by(season_year, parameter) |>
      dplyr::summarise(
        median_k = median(value),
        lower_k  = quantile(value, 0.025),
        upper_k  = quantile(value, 0.975),
        .groups = "drop"
      ),
    ggplot2::aes(
      y = season_year,
      x = median_k,
      xmin = lower_k,
      xmax = upper_k
    ),
    height = 0.3,
    color = "black"
  ) +
  ggh4x::facetted_pos_scales(
    x = list(
      parameter == "k"  ~ ggplot2::scale_x_continuous(limits = c(0, 5)),
      parameter == "mu" ~ ggplot2::scale_x_continuous(limits = c(0, 15))
    )
  ) +
  ggplot2::labs(
    title = "USA: Bootstrapped Parameter Estimates by Season-Year",
    x = "parameter value",
    y = "season-year"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "white", color = "black"),
    strip.text = ggplot2::element_text(face = "bold", size = 12)
  )

ggplot2::ggplot(
  mk_data,
  ggplot2::aes(x = value, y = season_year)
) +
  ggplot2::facet_wrap(
    ~ parameter,
    scales = "free_x",
    labeller = ggplot2::labeller(
      parameter = c("mu" = "μ", "k" = "k")
    )
  ) +
  ggridges::geom_density_ridges(
    fill = "#0D0887FF",
    alpha = 0.8,
    col = NA,
    trim = TRUE
  ) +
  ggplot2::labs(
    title = "USA: Bootstrapped Parameter Distributions by Season-Year",
    x = "parameter value",
    y = "season-year"
  ) +
  ggplot2::theme_bw()

