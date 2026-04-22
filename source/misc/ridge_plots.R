# ------------------------------------------------------------
#   Accessory: seasonal LOS parameter ridge plots
# ------------------------------------------------------------
# Density ridges of the bootstrap distributions of mu (mean LOS) and k
# (dispersion) for each (state, season) in los_fits.
#
# Facet grid: season x parameter. k on log10 x-axis; mu clipped to
# [0, 15]. States ordered top-down by descending median mu.
#
# Requires los_fits from main.R in the calling environment, plus
# ggridges and ggh4x. Run main.R first.
stopifnot(exists("los_fits"))
library(ggridges)
library(ggh4x)

boot_seasonal <- los_fits |>
  select(state, season_year, boot_params) |>
  mutate(boot_params = map(boot_params, as_tibble)) |>
  unnest(boot_params) |>
  pivot_longer(c(mu, k), names_to = "parameter", values_to = "value") |>
  mutate(
    parameter = factor(parameter, levels = c("mu", "k")),
    season_year = fct_reorder(
      season_year,
      case_when(
        str_starts(season_year, "Winter") ~
          as.numeric(str_extract(season_year, "\\d{4}")),
        str_starts(season_year, "Summer") ~
          as.numeric(str_extract(season_year, "\\d{4}")) - 0.5
      )
    )
  )

# Order states by descending median mu (plot reads top-down = longest LOS).
state_ord <- boot_seasonal |>
  filter(parameter == "mu") |>
  summarise(med = median(value), .by = state) |>
  arrange(desc(med)) |>
  pull(state)

boot_seasonal <- mutate(
  boot_seasonal,
  state = factor(state, levels = rev(state_ord))
)

boot_seasonal |>
  ggplot(aes(x = value, y = state)) +
  facet_grid(
    season_year ~ parameter, scales = "free_x",
    labeller = labeller(parameter = c(
      mu = "\u03bc (mean LOS)",
      k  = "k (dispersion)"
    ))
  ) +
  geom_density_ridges(
    fill = "#0D0887FF", col = NA, alpha = 0.7,
    scale = 0.9, trim = TRUE, rel_min_height = 0.01
  ) +
  facetted_pos_scales(x = list(
    parameter == "mu" ~ scale_x_continuous(limits = c(0, 15)),
    parameter == "k"  ~ scale_x_log10()
  )) +
  labs(
    title = "Seasonal Negbin LOS Parameters by State",
    subtitle = "Winter (Oct\u2013Mar) vs Summer (Apr\u2013Sep) \u00b7 k on log\u2081\u2080 scale",
    x = "Parameter value", y = NULL
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.y = element_text(size = 5),
    panel.grid.minor = element_blank()
  )
