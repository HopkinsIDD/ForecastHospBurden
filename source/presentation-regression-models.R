## clean regression script 
library(arrow)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)
#library(forecast)
#library(pracma)
library(lubridate) # package for working with dates
library(jtools)  # includes summ() functions for better aesthetic summaries from regression models
#library(multcomp) # includes glht command for computing linear combinations (and CIs) from regression
#library(expss) # includes tool for labeling variable names with descriptive text
library(table1) # includes tools for easily making table of summary statistics for specific subgroup comparisons
library(car) # for avPlots() function which constructs added-variables plots for linear models 
#library(forecast)
library(nlme) # use gls functoin
library(lme4)
library(cAIC4)
library(performance)
library(broom)
library(dplyr)
library(writexl)
library(sjPlot)

opt <- list()
opt$gt_data_path_analysis <- "data/analysis_data/hospburden_analysis.parquet"
opt$gt_data_path_total_incidH_observed <- "data/total_incidH_by_State_Lag_table1/covid_HHS_data_states_lag.parquet" # need incid H for table 1
opt$gt_data_path_variants <- "data/variants/variant_props_R17.parquet"

### Creating datasets 
covid_hosp_analysis <- arrow::read_parquet(opt$gt_data_path_analysis)
HHS_totalHosp_incidH_data <- arrow::read_parquet(opt$gt_data_path_total_incidH_observed)
variants <- arrow::read_parquet(opt$gt_data_path_variants) %>% 
  rename(state = source, 
         date = Update)


covid_totalHosp_incidH_data_table1 <- left_join(covid_hosp_analysis, HHS_totalHosp_incidH_data, by = c("state", "date", "total_hosp")) %>% 
  select(state, date, total_hosp, incidH, year, academic_year, four_week_peak, dominant_variant, season)

covid_hosp_analysis <- covid_hosp_analysis %>%
  mutate(dominant_variant = if_else(
    date > as.Date("2023-04-14"),
    "OMICRON",
    dominant_variant
  )) %>% 
  mutate(
    year = factor(year),
    academic_year = factor(academic_year),
    season = factor(season),
    dominant_variant = factor(dominant_variant),
    four_week_peak = factor(four_week_peak),
    four_week_valley = factor(four_week_valley),
    peak_period = factor(peak_period),
    middle_period = factor(middle_period),
    trough_period = factor(trough_period),
    state_numeric = as.factor(case_when(
      state == "NJ" ~ 1,
      state == "NY" ~ 2,
      state == "MD" ~ 3,
      state == "PA" ~ 4))
  ) %>% 
  left_join(variants, by = c("state", "date")) %>% 
  spread(key = variant, value = prop, fill = 0) %>% 
  rowwise() %>% 
  mutate(OMICRON = if_else(
    date > as.Date("2023-04-14"),
    1,
    OMICRON
  )) %>% 
  mutate(omicron_percent = 100 * OMICRON,
         wild_percent = 100 * WILD,
         alpha_percent = 100 * ALPHA,
         delta_percent = 100 * DELTA,    
         omicron_percent_rescaled = omicron_percent / 10,
         wild_percent_rescaled = wild_percent / 10,
         alpha_percent_rescaled = alpha_percent / 10,
         delta_percent_rescaled = delta_percent / 10)

############## change reference levels 
levels(covid_hosp_analysis$academic_year)
covid_hosp_analysis$academic_year <- relevel(covid_hosp_analysis$academic_year, ref = "2021-2022")

levels(covid_hosp_analysis$dominant_variant)
covid_hosp_analysis$year <- relevel(covid_hosp_analysis$year, ref = "2021")

covid_hosp_analysis$season <- factor(covid_hosp_analysis$season, ordered = FALSE)
covid_hosp_analysis$season <- relevel(covid_hosp_analysis$season, ref = "Spring")

covid_hosp_analysis$capacity_prop_per100thousand <- -covid_hosp_analysis$capacity_prop_per100thousand

# multivariate regression ------------

mod3 <- lmer(capacity_prop_per100thousand ~ academic_year*season + four_week_peak + (1|state),
             data =covid_hosp_analysis)
summary(mod3)

# univariate regression ------------
mod_yr <- lmer(capacity_prop_per100thousand ~ academic_year + (1|state),
               data =covid_hosp_analysis)
summary(mod_yr)

mod_szn <- lmer(capacity_prop_per100thousand ~ season + (1|state),
                data =covid_hosp_analysis)
summary(mod_szn)

mod_var <- lmer(capacity_prop_per100thousand ~ dominant_variant + (1|state),
                data =covid_hosp_analysis)
summary(mod_var)

mod_peak <- lmer(capacity_prop_per100thousand ~ four_week_peak + (1|state),
                 data =covid_hosp_analysis)
summary(mod_peak)

mod_yrszn <- lmer(capacity_prop_per100thousand ~ academic_year*season + (1|state),
                data =covid_hosp_analysis)
summary(mod_yrszn)

mod_state <- lmer(capacity_prop_per100thousand ~ (1|state),
                  data =covid_hosp_analysis)
summary(mod_state)

mod_date <- lmer(capacity_prop_per100thousand ~ date + (1|state),
               data =covid_hosp_analysis)
summary(mod_date)

# table of output 
mod_yr_coef <- coef(summary(mod_yr))
mod_szn_coef <- coef(summary(mod_szn))
mod_var_coef <- coef(summary(mod_var))
mod_peak_coef <- coef(summary(mod_peak))

summ(mod_yr,confint=getOption("summ-confint",TRUE),digits=4)
summ(mod_szn,confint=getOption("summ-confint",TRUE),digits=4)
summ(mod_var,confint=getOption("summ-confint",TRUE),digits=4)
summ(mod_peak,confint=getOption("summ-confint",TRUE),digits=4)

get_p_values <- function(model) {
  coefficients <- fixef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  t_values <- coefficients / standard_errors
  df <- df.residual(model)
  p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)
  return(p_values)
}

calc_ci <- function(estimates, std_errors, alpha = 0.05) {
  z_score <- qnorm(1 - alpha / 2)
  lower <- estimates - z_score * std_errors
  upper <- estimates + z_score * std_errors
  return(list(lower = lower, upper = upper))
}

# Calculate p-values for each model
p_values_yr <- get_p_values(mod_yr)
p_values_szn <- get_p_values(mod_szn)
p_values_var <- get_p_values(mod_var)
p_values_peak <- get_p_values(mod_peak)
p_values_yrszn <- get_p_values(mod_yrszn)

# Combine results into data frames
results_yr <- data.frame(
  Model = "mod_yr",
  Predictor = names(fixef(mod_yr)),
  Estimate = fixef(mod_yr),
  Std_Error = sqrt(diag(vcov(mod_yr))),
  CI = calc_ci(fixef(mod_yr), sqrt(diag(vcov(mod_yr)))),
  t_value = fixef(mod_yr) / sqrt(diag(vcov(mod_yr))),
  p_value = p_values_yr
)

results_szn <- data.frame(
  Model = "mod_szn",
  Predictor = names(fixef(mod_szn)),
  Estimate = fixef(mod_szn),
  Std_Error = sqrt(diag(vcov(mod_szn))),
  CI = calc_ci(fixef(mod_szn), sqrt(diag(vcov(mod_szn)))),
  t_value = fixef(mod_szn) / sqrt(diag(vcov(mod_szn))),
  p_value = p_values_szn
)

results_var <- data.frame(
  Model = "mod_var",
  Predictor = names(fixef(mod_var)),
  Estimate = fixef(mod_var),
  Std_Error = sqrt(diag(vcov(mod_var))),
  CI = calc_ci(fixef(mod_var), sqrt(diag(vcov(mod_var)))),
  t_value = fixef(mod_var) / sqrt(diag(vcov(mod_var))),
  p_value = p_values_var
)

results_peak <- data.frame(
  Model = "mod_peak",
  Predictor = names(fixef(mod_peak)),
  Estimate = fixef(mod_peak),
  Std_Error = sqrt(diag(vcov(mod_peak))),
  CI = calc_ci(fixef(mod_peak), sqrt(diag(vcov(mod_peak)))),
  t_value = fixef(mod_peak) / sqrt(diag(vcov(mod_peak))),
  p_value = p_values_peak
)

results_yrszn <- data.frame(
  Model = "mod_yrszn",
  Predictor = names(fixef(mod_yrszn)),
  Estimate = fixef(mod_yrszn),
  Std_Error = sqrt(diag(vcov(mod_yrszn))),
  CI = calc_ci(fixef(mod_yrszn), sqrt(diag(vcov(mod_yrszn)))),
  t_value = fixef(mod_yrszn) / sqrt(diag(vcov(mod_yrszn))),
  p_value = p_values_yrszn
)

# Combine all results into one table
combined_results <- bind_rows(results_yr, results_szn, results_var, results_peak, results_yrszn)

# Export to CSV
write.csv(combined_results, file = "univariate_results.csv", row.names = FALSE)


# table 1 -----------------

# Paper Table 1 -------------

# Summarizing by academic_year and season
table1_summary_season <- covid_totalHosp_incidH_data_table1 %>%
  group_by(academic_year, season) %>%
  summarise(
    total_hosp = sum(total_hosp),
    incidH = sum(incidH)
  )

# Summarizing by academic_year and other variables separately
table1_summary_dominant_variant <- covid_totalHosp_incidH_data_table1 %>%
  group_by(academic_year, dominant_variant) %>%
  summarise(
    total_hosp = sum(total_hosp),
    incidH = sum(incidH)
  )

table1_summary_four_week_peak <- covid_totalHosp_incidH_data_table1 %>%
  group_by(academic_year, four_week_peak) %>%
  summarise(
    total_hosp = sum(total_hosp),
    incidH = sum(incidH)
  )

table1_summary_state <- covid_totalHosp_incidH_data_table1 %>%
  group_by(academic_year, state) %>%
  summarise(
    total_hosp = sum(total_hosp),
    incidH = sum(incidH)
  )

# Write the data frames to an Excel file
write_xlsx(
  list(
    "Summary by Season" = table1_summary_season,
    "Summary by Dominant Variant" = table1_summary_dominant_variant,
    "Summary by Four Week Peak" = table1_summary_four_week_peak,
    "Summary by State" = table1_summary_state
  ),
  path = "capstone figures/table1_data_totalHosp_incidH.xlsx"
)

# adjusted paper model --------------------------------------------------
mod1 <- lmer(capacity_prop_per100thousand ~ season + four_week_peak + wild_percent_rescaled + delta_percent_rescaled + omicron_percent_rescaled + (1|state),
             data =covid_hosp_analysis)
summary(mod1)

mod2 <- lmer(capacity_prop_per100thousand ~ season + four_week_peak + (1|state),
             data =covid_hosp_analysis)
summary(mod2)

mod3 <- lmer(capacity_prop_per100thousand ~ four_week_peak + wild_percent_rescaled + delta_percent_rescaled + omicron_percent_rescaled + (1 |state),
             data =covid_hosp_analysis)
summary(mod3)

mod4 <- lmer(capacity_prop_per100thousand ~ four_week_peak + season*wild_percent_rescaled + season*delta_percent_rescaled + omicron_percent_rescaled + (1|state),
             data =covid_hosp_analysis)
summary(mod4)

mod5 <- lmer(capacity_prop_per100thousand ~ season*four_week_peak + wild_percent_rescaled + delta_percent_rescaled + omicron_percent_rescaled + (1|state),
             data =covid_hosp_analysis)
mod5 <- lmer(capacity_prop_per100thousand ~ season + four_week_peak*wild_percent_rescaled + delta_percent_rescaled + omicron_percent_rescaled + (1|state),
             data =covid_hosp_analysis)
summary(mod5)
anova(mod2, mod1)
anova(mod3, mod1)
anova(mod3, mod4)

# final model --------------
#label(covid_hosp_analysis$season) <- "Season"
#label(covid_hosp_analysis$four_week_peak) <- "Peak Period"
finalmod <- lmer(capacity_prop_per100thousand ~ season + four_week_peak + wild_percent_rescaled + delta_percent_rescaled + omicron_percent_rescaled + (1|state),
             data =covid_hosp_analysis)
#summ(finalmod,confint=getOption("summ-confint",TRUE),digits=4)
summary(finalmod)
cAIC(finalmod)
finalmod

regression_plot <- sjPlot::plot_model(finalmod,
                   show.values=TRUE, show.p=TRUE,
                   show.legend = TRUE,
                   title="Association of Season, Peak, and Variants on Under- and Over- Estimating Inpatient Hospitalizations (per 100,000)") +
  theme_bw()
ggsave("capstone figures/plot_model_output.png", regression_plot, width = 10, height = 6, units = "in")

sjPlot:: tab_model(finalmod)
effect_plot(finalmod, pred = imdb_rating, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)

dist_summ_plot <- plot_summs(finalmod, plot.distributions = TRUE, inner_ci_level = .95)

ggsave("capstone figures/plot_model_dist_output.png", dist_summ_plot, width = 10, height = 6, units = "in")
