# scoring of new method vs. og method 

source("source/data_setup_source.R")
source("source/cy/table1-main.R")
covid_HHS_data_states_lagtemp <- arrow::read_parquet(
  "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
) %>%
  mutate(
    date = as_date(date),
    total_hosp = total_adult_patients_hospitalized_confirmed_covid +
      total_pediatric_patients_hospitalized_confirmed_covid,
    incidH_prior_day = previous_day_admission_adult_covid_confirmed +
      previous_day_admission_pediatric_covid_confirmed
  ) %>%
  select(state, date, total_hosp, incidH_prior_day) %>%
  arrange(state, date) %>%
  create_incidH_lag() %>%
  mutate(
    year = year(date),
    season = sapply(date, get_season),
    adjusted_year = ifelse(season == "Winter" & month(date) == 12, year + 1, year),
    year_szn = paste0(adjusted_year, "_", season)
  ) %>%
  select(-adjusted_year)

# check for consistency in dates between 2 methods 
#old code
season_definitions <- covid_HHS_data_states_lagtemp %>%
  mutate(
    year = year(date),
    season = sapply(date, get_season)
  ) %>%
  group_by(year, season) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    .groups = "drop"
  )
winter_2021 <- covid_HHS_data_states_lagtemp %>% 
  filter(year_szn == "2021_Winter") 
# new code
season_year_dates <- df |>
  mutate(
    season_year = paste0(
      hydroTSM::time2season(date, out.fmt = "seasons"),
      "_",
      lubridate::year(date)
    )
  ) |>
  group_by(season_year) |>
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    .groups = "drop"
  )

# Create USA level data-frame for analysis 
covid_HHS_data_USA_lag <- arrow::read_parquet(
  "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"
) %>%
  mutate(
    date = as_date(date),
    total_hosp = total_adult_patients_hospitalized_confirmed_covid +
      total_pediatric_patients_hospitalized_confirmed_covid,
    incidH_prior_day = previous_day_admission_adult_covid_confirmed +
      previous_day_admission_pediatric_covid_confirmed
  ) %>%
  select(state, date, total_hosp, incidH_prior_day) %>%
  arrange(state, date) %>%
  create_incidH_lag() %>%
  group_by(date) %>%
  summarise(
    total_hosp = sum(total_hosp, na.rm = TRUE),
    incidH_prior_day = sum(incidH_prior_day, na.rm = TRUE),
    incidH = sum(incidH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(state = "US") %>% 
  mutate(
    year = year(date),
    season = sapply(date, get_season),
    adjusted_year = ifelse(season == "Winter" & month(date) == 12, year + 1, year),
    year_szn = paste0(adjusted_year, "_", season)
  ) %>%
  select(-adjusted_year)

# summarize in old output -----
files <- c(
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2024_Spring_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2024_Winter_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Fall_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Summer_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Spring_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2023_Winter_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2022_Fall_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2022_Summer_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2022_Spring_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2022_Winter_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2021_Summer_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2021_Fall_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2021_Spring_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2021_Winter_100sims.csv",
  "data/tables-figures-data/length-of-stay-estimates/historical-data/Table1_LOS_YearSzn_2020_Fall_100sims.csv"
)

los_summaries <- map_dfr(files, \(f) {
  dat <- read_csv(f, show_col_types = FALSE)
  
  # Pull year + season from filename like "...YearSzn_2024_Spring_100sims.csv"
  m <- str_match(basename(f), "YearSzn_(\\d{4})_([A-Za-z]+)_100sims\\.csv$")
  yr <- m[, 2]
  sz <- m[, 3]
  
  dat %>%
    mutate(
      year = as.integer(yr),
      season = sz,
      YearSzn = paste0(year, "_", season)
    )
}) %>%
  # If you ONLY want US:
  filter(state == "US") %>%
  group_by(YearSzn) %>%
  summarise(
    mean_los = mean(optimized_los, na.rm = TRUE),
    lo_95 = quantile(optimized_los, 0.025, na.rm = TRUE),
    hi_95 = quantile(optimized_los, 0.975, na.rm = TRUE),
    n_sims = sum(!is.na(optimized_los)),
    .groups = "drop"
  ) %>%
  separate(YearSzn, into = c("year", "season"), sep = "_", remove = FALSE, convert = TRUE) %>%
  arrange(year, factor(season, levels = c("Winter","Spring","Summer","Fall"))) %>% 
  mutate(
    optimized_los = mean_los
  )

# source 
# ~/Documents/ForecastHospBurden/source/create_hosp_burden_estimates_timevarying_simulations.R

los_opt_by_state_season <- read_csv("data/US_wide_data/LOS_Optimized_by_AllStates_USA_time_varying.csv")

covid_HHS_data_states_lagtemp2 <- covid_HHS_data_USA_lag %>% 
  left_join(los_summaries, by = c("year_szn" = "YearSzn")) %>% 
  filter(year_szn == "2022_Fall") %>% mutate(state =="US") 

create_totalH_df_year_szn(data = covid_HHS_data_states_lagtemp2 %>% dplyr::select(-incidH, -incidH_prior_day)) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df_year_szn(data = covid_HHS_data_states_lagtemp2 %>% dplyr::select(-total_hosp, -incidH_prior_day))

covid_incidH_data_US <- covid_incidH_data_USA_2022_Fall
simu_create_optimize_totalHosp_data_timevarying <- function(parent_data, n_simulations = 1) {
  states_list <- unique(parent_data$state)
  combined_list <- list()
  
  for (state_abbv in states_list) {
    print(state_abbv) # Progress tracking
    print(Sys.time())
    state_data <- parent_data %>% filter(state == state_abbv)
    los_state_list <- state_data$optimized_los
    
    if (length(los_state_list) == nrow(state_data)) { # Ensure LOS values match data rows
      for (sim_round in 1:n_simulations) {
        # Generate expected hospitalization data
        expected_list <- create_hosp_dates_timevarying(state_data, los_vector = los_state_list)
        expected <- create_curr_hosp_forecast(data_burden = expected_list)
        expected <- clean_expected(expected)
        
        # Fetch observed data dynamically
        dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
        
        if (exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
          observed <- get(dynamic_totalHosp_name)
          
          # Combine observed and expected data
          combined <- inner_join(observed, expected, by = "date") %>% 
            dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
            mutate(
              absolute_difference = abs(total_hosp - total_hosp_estimate),
              difference = total_hosp - total_hosp_estimate,
              relative_difference = total_hosp_estimate / total_hosp,
              simulation_round = sim_round # Add simulation round
            )
          
          # Store simulation results
          if (is.null(combined_list[[state_abbv]])) {
            combined_list[[state_abbv]] <- combined
          } else {
            combined_list[[state_abbv]] <- bind_rows(combined_list[[state_abbv]], combined)
          }
        } else {
          print(paste("Data not found for:", dynamic_totalHosp_name))
        }
      }
    } else {
      print(paste("Mismatch in LOS values for state:", state_abbv))
    }
  }
  
  # Combine the results into a single dataframe
  combined_df <- do.call(rbind, combined_list)
  return(combined_df)
}


optimized_data <- simu_create_optimize_totalHosp_data_timevarying(parent_data = covid_HHS_data_states_lagtemp2)

# summarize new output -----------
mse_by_season_year <- predictions %>%
  mutate(year = year(date),
         season = sapply(date, get_season),
         season_year = paste0(season, "_", year)) %>%
  group_by(state, season_year) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    mse = mean((predicted_hosp - active_hosp)^2, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

plot_pred <- predictions %>%
  mutate(date = as.Date(date)) %>%
  select(state, date, active_hosp, predicted_hosp) %>%
  pivot_longer(
    cols = c(active_hosp, predicted_hosp),
    names_to = "series",
    values_to = "hosp"
  ) %>%
  mutate(
    series = recode(
      series,
      active_hosp = "Observed",
      predicted_hosp = "Predicted"
    )
  )

p_pred_vs_obs <- ggplot(plot_pred, aes(x = date, y = hosp)) +
  geom_line(
    aes(color = series, linetype = series),
    linewidth = 1.05,
    na.rm = TRUE
  ) +
  facet_wrap(~ state, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Observed"  = "black",
      "Predicted" = "#1F4E79"   # deep blue
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Observed"  = "solid",
      "Predicted" = "dashed"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = comma,
    name = "Hospitalized patients"
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Observed vs. Predicted COVID-19 Hospitalizations",
    subtitle = "State-level time series",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(3, "pt"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = margin(10, 10, 10, 10)
  )

p_pred_vs_obs

