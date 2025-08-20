# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
library(gghighlight)
library(arrow)
library(Hmisc)
library(ggplot2)
library(covidHubUtils)
library(doParallel)
library(ggplot2)
theme_set(theme_bw())

### IMPORT INITIAL DATA -----------------------------------


# updates 2/5/25
forecast_quantile_covid_total_hosp_simulations <- arrow::read_parquet("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_TIMEVARYING_FORECAST_SIMU_quantiles_14days_02112025.parquet") %>% mutate(optimization = "Simulations Forecasted incidH from forecast_date 14 days, LOS: prior yr time varying (3 months)", total_hosp_estimate = `0.5`)

forecast_quantile_covid_total_hosp_simulations <- arrow::read_parquet("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_TIMEVARYING_FORECAST_SIMU_quantiles_14days_parallel_negbinom_realtime_LOS_03202025.parquet"
) %>% mutate(optimization = "Simulations Forecasted incidH from forecast_date 14 days, LOS: prior yr time varying (3 months)", total_hosp_estimate = `0.5`)                                                                               
forecast_quantile_covid_total_hosp_simulations <- forecast_quantile_covid_total_hosp_simulations %>% 
  mutate(Lower_95_CI = `0.025`, Upper_95_CI = `0.975`, Lower_80_CI = `0.1`, Upper_80_CI = `0.9`, Lower_50_CI = `0.25`, Upper_50_CI = `0.75`)

# length of stay summary --------------------------------------


# incidH forecasts and graphs ----------------

fluszn_23_24 <- seq(as.Date("2023-09-01"), as.Date("2024-04-30"), by = "day")
# create a list with the full name of every state in the US
state_names <- state.name

# why isnt inc hosp targets working !!!! 
# forecasts_hosp_states <- load_forecasts(
#   models = "COVIDhub-ensemble",
#   dates = fluszn_23_24,
#   date_window_size = 6,
#   locations = state_names,
#   types = c("point", "quantile"),
#   targets = inc_hosp_targets,
#   source = "zoltar",
#   verbose = FALSE,
#   as_of = NULL,
#   hub = c("US")
# )

forecasts_hosp_states <- arrow::read_parquet("data/covidHubUtils_forecastData/forecast_hosp.parquet") %>% mutate(horizon = as.numeric(horizon))

truth_data <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = state_names
) %>% 
  mutate(incidH = value)

# Get all column names except "incidH" and "value"
join_by_columns <- setdiff(names(truth_data), c("incidH", "value", "model"))

# Perform the left join using the selected columns
forecasts_hosp_states <- forecasts_hosp_states %>%
  left_join(truth_data %>% select(-value, -model), by = join_by_columns)


forecasts_hosp_states_quantiles <- forecasts_hosp_states %>%
  mutate(horizon = as.numeric(horizon),
         quantile = as.numeric(quantile),
         date = target_end_date,
         state = abbreviation,
    quantile_name = case_when(
    quantile == 0.025 ~ "Lower_95_CI",
    quantile == 0.975 ~ "Upper_95_CI",
    quantile == 0.100 ~ "Lower_80_CI",
    quantile == 0.900 ~ "Upper_80_CI",
    quantile == 0.250 ~ "Lower_50_CI",
    quantile == 0.750 ~ "Upper_50_CI",
    quantile == 0.500 ~ "incidH_estimate"
  )) %>% 
  filter(quantile_name %in% c("Lower_95_CI", "Upper_95_CI", "Lower_80_CI", "Upper_80_CI", "Lower_50_CI", "Upper_50_CI", "incidH_estimate")) 
  

forecasts_hosp_US_quantiles_wide <- forecasts_hosp_states_quantiles %>%
  select(-c(quantile)) %>% 
  group_by(date) %>% 
  pivot_wider(
    names_from = quantile_name,   # Use the new descriptive names
    values_from = value # Specify the value column
  )

# simulated data graphs forecasted incidH -----------------
# USE THESE GRAPHS !!! 

combined_data <- forecast_quantile_covid_total_hosp_simulations %>%
  select(state, date, forecast_date, total_hosp, total_hosp_estimate, Lower_95_CI, Upper_95_CI, Lower_80_CI, Upper_80_CI, Lower_50_CI, Upper_50_CI) %>%
  left_join(
    forecasts_hosp_US_quantiles_wide %>% 
      select(state, date, forecast_date, incidH_estimate, incidH, Lower_95_CI, Upper_95_CI, Lower_80_CI, Upper_80_CI, Lower_50_CI, Upper_50_CI),
    by = c("state", "date", "forecast_date"),
    suffix = c("_total", "_incidH")
  )

combined_data_MD <- combined_data %>% 
  filter(state == "MD")

state_list <- unique(combined_data$state)
pdf("~/Downloads/hospitalizations_combined_visualizations_forecast_simulations_14days_parallel_negbinom_REALTIMELOS.pdf", width = 10, height = 8)
# Loop through each state and save combined plots
#state_list <- c("MD")

for (state_abbv in state_list) {
  combined_viz <- combined_data %>%  
    filter(state == state_abbv) %>% 
    ggplot(aes(x = date)) + 
    # Observed total hospitalizations
    geom_line(aes(y = total_hosp, linetype = "Observed TotalH"), color = "black") +
    geom_line(aes(y = total_hosp_estimate, group = as.factor(forecast_date), color = as.factor(forecast_date)), show.legend = FALSE) + #color = "darkblue"
    geom_point(aes(y = total_hosp_estimate, group = forecast_date, color = as.factor(forecast_date)), size = 0.5, show.legend = FALSE) + #color = "darkblue"
    scale_color_viridis_d(option = "turbo", name = "Forecast Date") +
    # Forecast incidH
    geom_line(aes(y = incidH, linetype = "Observed IncidH"), color = "black") +
    geom_line(aes(y = incidH_estimate, group = forecast_date, linetype = "Forecasted IncidH"), color = "darkred") +
    geom_point(aes(y = incidH_estimate, group = forecast_date), size = 0.3, color = "darkred") +
    # Confidence intervals
    geom_ribbon(aes(
      ymin = Lower_95_CI_total, ymax = Upper_95_CI_total, fill = "95% CI (Total)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_95_CI_incidH, ymax = Upper_95_CI_incidH, fill = "95% CI (IncidH)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_50_CI_total, ymax = Upper_50_CI_total, 
      fill = as.factor(forecast_date), 
      group = as.factor(forecast_date)), alpha = 0.2) +
    scale_fill_viridis_d(option = "turbo", name = "Forecast Date") +
    geom_ribbon(aes(
      ymin = Lower_50_CI_incidH, ymax = Upper_50_CI_incidH, fill = "50% CI (IncidH)"), alpha = 0.2) +
    # Labels and themes
    labs(x = "Date", 
         y = "Hospitalizations (Observed & Estimated)", 
         title = paste0(state_abbv, " TotalH and IncidH - LOS: past 90 days")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "left") +
    # Legends for linetype and fill
    scale_linetype_manual(name = "Line Type", 
                          values = c("Observed TotalH" = "solid", 
                                     "Forecasted TotalH" = "solid", 
                                     "Observed IncidH" = "solid", 
                                     "Forecasted IncidH" = "solid")) +
    scale_fill_manual(name = "Confidence Interval", 
                      values = c("95% CI (Total)" = "lightblue", "95% CI (IncidH)" = "pink", 
                                 #"50% CI (Total)" = "darkblue", 
                                 "50% CI (IncidH)" = "darkred"))
  
  # Print each plot to a new page in the PDF
  print(combined_viz)
  
}

 # Close PDF 
dev.off()



# compare old vs new extrapolated forecasts
library(patchwork)

forecast_quantile_covid_total_hosp_simulations_archive <- forecast_quantile_covid_total_hosp_simulations_archive %>% 
  mutate(Lower_95_CI = `0.025`, Upper_95_CI = `0.975`, Lower_80_CI = `0.1`, Upper_80_CI = `0.9`, Lower_50_CI = `0.25`, Upper_50_CI = `0.75`)

combined_data_archive <- forecast_quantile_covid_total_hosp_simulations_archive %>%
  select(state, date, total_hosp, total_hosp_estimate, Lower_95_CI, Upper_95_CI, Lower_80_CI, Upper_80_CI, Lower_50_CI, Upper_50_CI) %>%
  left_join(
    forecasts_hosp_US_quantiles_wide %>% 
      select(state, date, incidH_estimate, incidH, Lower_95_CI, Upper_95_CI, Lower_80_CI, Upper_80_CI, Lower_50_CI, Upper_50_CI),
    by = c("state", "date"),
    suffix = c("_total", "_incidH")
  )

state_list <- unique(combined_data$state)
pdf("~/Downloads/forecast_plots_comparison_01-30-25.pdf", width = 10, height = 8)
# Loop through each state and save combined plots

for (state_abbv in state_list) {
  combined_viz <- combined_data %>%  
    filter(state == state_abbv) %>% 
    ggplot(aes(x = date)) + 
    # Observed total hospitalizations
    geom_line(aes(y = total_hosp, linetype = "Observed TotalH"), color = "black") +
    geom_line(aes(y = total_hosp_estimate, linetype = "Forecasted TotalH", group = as.factor(forecast_date), color = as.factor(forecast_date)), show.legend = FALSE) + #color = "darkblue"
    geom_point(aes(y = total_hosp_estimate, linetype = "Forecasted TotalH", group = as.factor(forecast_date), color = as.factor(forecast_date)), size = 0.5, show.legend = FALSE) + #color = "darkblue"
    scale_color_viridis_d(option = "plasma", name = "Forecast Date") +
    # Forecast incidH
    geom_line(aes(y = incidH, linetype = "Observed IncidH"), color = "black") +
    geom_line(aes(y = incidH_estimate, linetype = "Forecasted IncidH"), color = "darkred") +
    # Confidence intervals
    geom_ribbon(aes(
      ymin = Lower_95_CI_total, ymax = Upper_95_CI_total, fill = "95% CI (Total)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_95_CI_incidH, ymax = Upper_95_CI_incidH, fill = "95% CI (IncidH)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_50_CI_total, ymax = Upper_50_CI_total, fill = "50% CI (Total)"), alpha = 0.2) +
    geom_ribbon(aes(
      ymin = Lower_50_CI_incidH, ymax = Upper_50_CI_incidH, fill = "50% CI (IncidH)"), alpha = 0.2) +
    # Labels and themes
    labs(x = "Date", 
         y = "Hospitalizations (Observed & Estimated)", 
         title = paste0(state_abbv, "New Version Combined Hospitalizations: Total and IncidH")) +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1),
    #      legend.position = "left") +
    # Legends for linetype and fill
    scale_linetype_manual(name = "Line Type", 
                          values = c("Observed TotalH" = "solid", 
                                     "Forecasted TotalH" = "solid", 
                                     "Observed IncidH" = "solid", 
                                     "Forecasted IncidH" = "dotted")) +
    scale_fill_manual(name = "Confidence Interval", 
                      values = c("95% CI (Total)" = "lightblue", "95% CI (IncidH)" = "pink", 
                                 "50% CI (Total)" = "darkblue", "50% CI (IncidH)" = "darkred"))
  
  
  combined_viz_archive <- combined_data_archive %>%  
    filter(state == state_abbv) %>% 
    ggplot(aes(x = date)) + 
    # Observed total hospitalizations
    geom_line(aes(y = total_hosp, linetype = "Observed TotalH"), color = "black") +
    geom_line(aes(y = total_hosp_estimate, linetype = "Forecasted TotalH"), color = "darkblue") +
    # Forecast incidH
    geom_line(aes(y = incidH, linetype = "Observed IncidH"), color = "black") +
    geom_line(aes(y = incidH_estimate, linetype = "Forecasted IncidH"), color = "darkred") +
    # Confidence intervals
    geom_ribbon(aes(
      ymin = Lower_95_CI_total, ymax = Upper_95_CI_total, fill = "95% CI (Total)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_95_CI_incidH, ymax = Upper_95_CI_incidH, fill = "95% CI (IncidH)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_50_CI_total, ymax = Upper_50_CI_total, fill = "50% CI (Total)"), alpha = 0.2) +
    geom_ribbon(aes(
      ymin = Lower_50_CI_incidH, ymax = Upper_50_CI_incidH, fill = "50% CI (IncidH)"), alpha = 0.2) +
    # Labels and themes
    labs(x = "Date", 
         y = "Hospitalizations (Observed & Estimated)", 
         title = paste0(state_abbv, "Old Version Combined Hospitalizations: Total and IncidH")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "left") +
    # Legends for linetype and fill
    scale_linetype_manual(name = "Line Type", 
                          values = c("Observed TotalH" = "solid", 
                                     "Forecasted TotalH" = "dashed", 
                                     "Observed IncidH" = "solid", 
                                     "Forecasted IncidH" = "dotted")) +
    scale_fill_manual(name = "Confidence Interval", 
                      values = c("95% CI (Total)" = "lightblue", "95% CI (IncidH)" = "pink", 
                                 "50% CI (Total)" = "darkblue", "50% CI (IncidH)" = "darkred"))
  
  
  # Combine the two plots side by side
  combined_plot <- combined_viz_archive | combined_viz
  
  # Print the combined plot to the PDF
  print(combined_plot)
  
  
}

# Close PDF 
dev.off()






# see if sampling is working of incidH forecast data -----------------

all_states_samples <- arrow::read_parquet("data/US_wide_data/forecast_hosp_all_dates_horizon_sampled_incidH_from_quantile.parquet") %>% 
  mutate(date = target_end_date,
         state = abbreviation)

all_states_samples_simu_wide <- all_states_samples %>% 
  select(-horizon) %>% 
  group_by(date, state) %>% 
  pivot_wider(
    names_from = simulation,   # Use the new descriptive names
    values_from = sample # Specify the value column
  ) %>% 
  rename_with(~ paste0("simulation_", .), -c(date, state, location_name, abbreviation, forecast_date, target_end_date, type, state, date))


# forecast_quantile_covid_total_hosp_simulations <- forecast_quantile_covid_total_hosp_simulations %>% 
#   mutate(Lower_95_CI = `0.025`, Upper_95_CI = `0.975`, Lower_80_CI = `0.1`, Upper_80_CI = `0.9`, Lower_50_CI = `0.25`, Upper_50_CI = `0.75`)
# 

# Combine datasets by state and date
combined_data <- all_states_samples_simu_wide %>%
  select(-c(location_name, abbreviation, type)) %>%
  left_join(
    forecasts_hosp_US_quantiles_wide, #%>% 
      #select(state, date, incidH_estimate, incidH, Lower_95_CI, Upper_95_CI, Lower_80_CI, Upper_80_CI, Lower_50_CI, Upper_50_CI),
    by = c("state", "date"),
    suffix = c("_sample", "_incidH")
  )

state_list <- unique(combined_data$state)

pdf("~/Downloads/visualizations_sampledincidH_vs_incidH_forecast.pdf", width = 10, height = 8)
# Loop through each state and save combined plots
for (state_abbv in state_list) {
  combined_viz <- combined_data %>%  
    filter(state == state_abbv) %>% 
    ggplot(aes(x = date)) + 
    # Observed total hospitalizations
    # Forecast incidH
    geom_line(aes(y = incidH, linetype = "Observed IncidH"), color = "black") +
    geom_line(aes(y = incidH_estimate, linetype = "Forecasted IncidH"), color = "darkred") +
    # Confidence intervals
    geom_ribbon(aes(
      ymin = Lower_95_CI, ymax = Upper_95_CI, fill = "95% CI (IncidH)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_50_CI, ymax = Upper_50_CI, fill = "50% CI (IncidH)"), alpha = 0.2) +
    lapply(1:100, function(i) {
      geom_line(aes_string(y = paste0("simulation_", i), linetype = '"Sampled IncidH"'), color = "grey")
    }) +
    # Labels and themes
    labs(x = "Date", 
         y = "Hospitalizations (Observed & Estimated)", 
         title = paste0(state_abbv, " Combined Hospitalizations: Total and IncidH")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "left") +
    # Legends for linetype and fill
    scale_linetype_manual(name = "Line Type", 
                          values = c("Sampled IncidH" = "dashed", 
                                     "Observed IncidH" = "solid", 
                                     "Forecasted IncidH" = "dotted")) +
    scale_fill_manual(name = "Confidence Interval", 
                      values = c("95% CI (IncidH)" = "pink", "50% CI (IncidH)" = "darkred"))
  
  # Print each plot to a new page in the PDF
  print(combined_viz)
  
}

# Close PDF 
dev.off()

# incidH graphs for Data scientist interview

state_list <- unique(combined_data$state)
pdf("~/Downloads/UTincidHforecasts.pdf", width = 10, height = 8)
# Loop through each state and save combined plots
state_list <- c("UT")

for (state_abbv in state_list) {
  combined_viz <- combined_data %>%  
    filter(state == state_abbv) %>% 
    ggplot(aes(x = date)) + 
    # Observed total hospitalizations
    #geom_line(aes(y = total_hosp, linetype = "Observed TotalH"), color = "black") +
    geom_line(aes(y = incidH, linetype = "Observed IncidH"), color = "black") +
    geom_line(aes(y = incidH_estimate, group = forecast_date, linetype = "Forecasted IncidH"), color = "darkred") +
    geom_point(aes(y = incidH_estimate, group = forecast_date), size = 0.3, color = "darkred") +
    # Confidence intervals
    geom_ribbon(aes(
      ymin = Lower_95_CI_incidH, ymax = Upper_95_CI_incidH, fill = "95% CI (IncidH)"), alpha = 0.3) +
    geom_ribbon(aes(
      ymin = Lower_50_CI_incidH, ymax = Upper_50_CI_incidH, fill = "50% CI (IncidH)"), alpha = 0.2) +
    # Labels and themes
    labs(x = "Date", 
         y = "Hospitalizations (Observed & Forecasted)", 
         title = paste0("Incident COVID-19 Forecasts in Utah for the 23/24 Respiratory Virus Season")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "left") +
    # Legends for linetype and fill
    scale_linetype_manual(name = "Line Type", 
                          values = c(
                            #"Observed TotalH" = "solid", 
                                     "Observed IncidH" = "solid", 
                                     "Forecasted IncidH" = "solid")) +
    scale_fill_manual(name = "Confidence Interval", 
                      values = c("95% CI (IncidH)" = "pink", 
                                 #"50% CI (Total)" = "darkblue", 
                                 "50% CI (IncidH)" = "darkred"))
  
  # Print each plot to a new page in the PDF
  print(combined_viz)
  
}

# Close PDF 
dev.off()

state_list <- unique(combined_data$state)
pdf("~/Downloads/ReportedUTData.pdf", width = 10, height = 8)
# Loop through each state and save combined plots
state_list <- c("UT")

for (state_abbv in state_list) {
  combined_viz <- combined_data %>%  
    filter(state == state_abbv) %>% 
    ggplot(aes(x = date)) + 
    # Observed total hospitalizations
    geom_line(aes(y = total_hosp, linetype = "Observed TotalH"), color = "darkblue") +
    geom_line(aes(y = incidH, linetype = "Observed IncidH"), color = "darkred") +
    #geom_line(aes(y = incidH_estimate, group = forecast_date, linetype = "Forecasted IncidH"), color = "darkred") +
    #geom_point(aes(y = incidH_estimate, group = forecast_date), size = 0.3, color = "darkred") +
    # Confidence intervals
    #geom_ribbon(aes(
    #  ymin = Lower_95_CI_incidH, ymax = Upper_95_CI_incidH, fill = "95% CI (IncidH)"), alpha = 0.3) +
    #geom_ribbon(aes(
    #  ymin = Lower_50_CI_incidH, ymax = Upper_50_CI_incidH, fill = "50% CI (IncidH)"), alpha = 0.2) +
    # Labels and themes
    labs(x = "Date", 
         y = "Incident and Total Hospitalizations", 
         title = paste0("Incident and Total Hospitalizations in Utah for the 23/24 Respiratory Virus Season")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "left") +
    # Legends for linetype and fill
    scale_linetype_manual(name = "Line Type",
                          values = c(
                            "Observed TotalH" = "solid",
                            "Observed IncidH" = "solid"
                            #"Forecasted IncidH" = "solid")
                            )) #+
    # scale_fill_manual(name = "Confidence Interval", 
    #                   values = c("95% CI (IncidH)" = "pink", 
    #                              #"50% CI (Total)" = "darkblue", 
    #                              "50% CI (IncidH)" = "darkred"))
  
  # Print each plot to a new page in the PDF
  print(combined_viz)
  
}

# Close PDF 
dev.off()
