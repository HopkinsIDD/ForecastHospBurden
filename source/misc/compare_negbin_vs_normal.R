# ------------------------------------------------------------
#   Accessory: negbin vs normal LOS comparison
# ------------------------------------------------------------
# Whole-series (non-seasonal), per-state fits under two LOS families,
# compared by delta AIC (negbin - normal) and by 95% prediction
# interval ribbons. Not used in the production pipeline.

# --- Setup ---------------------------------------------------------------
list.files(here::here("source/helpers"), full.names = TRUE) |>
  purrr::walk(source)

set.seed(123)
plan(multisession, workers = availableCores() - 2)

# --- Data ----------------------------------------------------------------
hhs <- load_hhs()

# --- Fit one LOS family across all states --------------------------------
# Thin wrapper around fit_los. Appends a Gaussian-SSE AIC so families
# can be ranked:  AIC = n_fit * log(SSE / n_fit) + 2 * n_params
fit_all <- function(hhs, los_dist, n_boot = 100) {
  message(sprintf("Fitting %s ...", los_dist$name))
  hhs |>
    nest(.by = state) |>
    mutate(
      res = future_map(
        data, fit_los, los_dist, n_boot = n_boot,
        .options = furrr_options(seed = TRUE)
      )
    ) |>
    unnest_wider(res) |>
    unnest_wider(params) |>
    mutate(
      n_params = length(los_dist$param_names),
      aic      = n_fit * log(sse / n_fit) + 2 * n_params,
      model    = los_dist$name
    )
}

nb   <- fit_all(hhs, dist_negbin)
norm <- fit_all(hhs, dist_normal)

# --- Delta AIC plot (per state) ------------------------------------------
delta_aic <- bind_rows(nb, norm) |>
  select(model, state, aic) |>
  pivot_wider(names_from = model, values_from = aic) |>
  mutate(delta_aic = negbin - normal)

delta_aic |>
  ggplot(aes(x = reorder(state, delta_aic), y = delta_aic)) +
  geom_col(aes(fill = delta_aic < 0), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "#2a9d8f", "FALSE" = "#e76f51")) +
  scale_y_continuous(limits = c(-150, 150)) +
  coord_flip() +
  labs(
    title = expression(paste("", Delta, "AIC (negbin \u2212 normal) by State")),
    subtitle = "< 0 = negbin preferred, > 0 = normal preferred",
    x = NULL, y = expression(Delta * "AIC")
  ) +
  theme_bw()

# --- 95% prediction interval ribbons, faceted by size x model ------------
ci_data <- bind_rows(
  nb   |> select(state, model, ci) |> unnest(ci),
  norm |> select(state, model, ci) |> unnest(ci)
)

state_groups <- ci_data |>
  filter(model == "negbin") |>
  summarise(mean_hosp = mean(observed, na.rm = TRUE), .by = state) |>
  mutate(
    size_group = factor(
      ntile(mean_hosp, 3),
      labels = c("Low", "Medium", "High")
    )
  )

ci_data |>
  left_join(state_groups, by = "state") |>
  ggplot(aes(x = date, y = observed)) +
  facet_grid(rows = vars(size_group), cols = vars(model), scales = "free_y") +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, group = state),
    col = "orange", alpha = 0.15
  ) +
  geom_line(aes(group = state), linewidth = 0.15, alpha = 0.6) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "95% Prediction Intervals by LOS Distribution",
    subtitle = "States grouped by mean census magnitude (Low / Medium / High)",
    y = "Hospital Census", x = "Date"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
