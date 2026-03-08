# ------------------------------------
#           Aim
# ------------------------------------
#' Estimate hospital length of stay (LOS) for COVID-19 in the United States
#' using a convolution model: census = admissions * P(still in hospital).
#'
#' Pipeline:
#'   1. Data: extract admissions + census from HHS dataset
#'   2. Fit: per state, minimise SSE between observed and predicted census
#'   3. Bootstrap: residual bootstrap for parameter uncertainty
#'   4. Compare: AIC across distribution families
#'   5. Visualise: 95% CI for each model vs observed census

# -------------------------------------
#           Setup
# -------------------------------------
list.files(here::here("source/helpers"), full.names = TRUE) |>
  purrr::walk(source)

set.seed(123)
plan(multisession, workers = availableCores() - 2)

# -------------------------------------
#           Data
# -------------------------------------
df <-
  arrow::read_parquet(
    here::here(
      "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
    )
  ) |>
  arrange(state, date) |>
  mutate(
    state,
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
  drop_na()

# =============================================
#    A. Fit Models
# =============================================
nb <- fit_los(df, dist_negbin, n_boot = 100)
norm <- fit_los(df, dist_normal, n_boot = 100)

# =============================================
#    B. Model Selection (AIC)
# =============================================
aic_comparison <- bind_rows(nb, norm) |>
  select(model, state, aic, sse, n_fit, n_params)

best_per_state <- aic_comparison |>
  slice_min(aic, by = state)

best_per_state |>
  count(model) |>
  mutate(pct = round(n / sum(n) * 100, 1))

# --------- Delta AIC by state ---------
delta_aic <- aic_comparison |>
  select(model, state, aic) |>
  pivot_wider(names_from = model, values_from = aic) |>
  mutate(delta_aic = negbin - normal)

delta_aic |>
  ggplot(aes(x = reorder(state, delta_aic), y = delta_aic)) +
  geom_col(aes(fill = delta_aic < 0), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "#2a9d8f", "FALSE" = "#e76f51")) +
  coord_flip() +
  labs(
    title = expression(paste("", Delta, "AIC (negbin \u2212 normal) by State")),
    subtitle = "< 0 = negbin preferred, > 0 = normal preferred",
    x = NULL,
    y = expression(Delta * "AIC")
  ) +
  theme_bw()

# =============================================
#    C. Visualisation (both models' 95% CI)
# =============================================
ci_data <- bind_rows(
  nb |> select(state, model, ci) |> unnest(ci),
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
    col = "orange",
    alpha = 0.15
  ) +
  geom_line(aes(group = state), linewidth = 0.15, alpha = 0.6) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "95% Prediction Intervals by LOS Distribution",
    subtitle = "States grouped by mean census magnitude (Low / Medium / High)",
    y = "Hospital Census",
    x = "Date"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
