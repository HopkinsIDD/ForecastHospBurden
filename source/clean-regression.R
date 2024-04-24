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
opt <- list()
opt$gt_data_path_analysis <- "data/analysis_data/hospburden_analysis.parquet"

### Creating datasets 
covid_hosp_analysis <- arrow::read_parquet(opt$gt_data_path_analysis)


covid_hosp_analysis <- covid_hosp_analysis %>%
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
    dominant_variant = if_else(is.na(dominant_variant), "mixed", dominant_variant),
    state_numeric = as.factor(case_when(
      state == "NJ" ~ 1,
      state == "NY" ~ 2,
      state == "MD" ~ 3,
      state == "PA" ~ 4))
  )

# change reference levels 
levels(covid_hosp_analysis$dominant_variant)
covid_hosp_analysis$year <- relevel(covid_hosp_analysis$year, ref = "2023")

###############################################################
# create regression mixed effects models 
mod1<-lmer(capacity_prop_per100thousand~year + season + (1|state), data =covid_hosp_analysis)
cAIC(mod1)
mod2 <- lmer(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
## FINAL MODEL
mod3 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant + four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
mod4 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant*season +  four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
mod5 <- lmer(capacity_prop_per100thousand ~ year*season + year*dominant_variant + four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
mod6 <- lmer(capacity_prop_per100thousand ~ year*season + season*dominant_variant + season*dominant_variant + season*four_week_peak + season*four_week_valley + (1|state),
             data =covid_hosp_analysis)


anova(mod1, mod2)
anova(mod2, mod3) 
anova(mod3, mod4) 
anova(mod4, mod5) 
cAIC(mod5)

summary(mod3)
#summ(mod3,confint=getOption("summ-confint",TRUE),digits=4)
tbl_regression(mod3, exponentiate = FALSE,
               show_single_row= c("four_week_peak", "four_week_valley"),
               pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

#summ(mod3,confint=getOption("summ-confint",TRUE),digits=4)
summary(mod3)

library(ggplot2)

###################################
# residuals 

# Extract predicted values for each state
predicted_values <- predict(mod3, type = "response")  # Replace "response" with appropriate type if not logistic regression

# Create a data frame with actual and predicted values
data <- data.frame(
  Actual = covid_hosp_analysis$capacity_prop_per100thousand,  # Replace with your actual response variable
  Predicted = predicted_values,
  State = covid_hosp_analysis$state  # Replace with your state variable
)

# Plot actual vs. predicted values, grouped by state
ggplot(data, aes(x = Actual, y = Predicted, color = State)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +  # Add diagonal line for perfect fit
  labs(x = "Actual", y = "Predicted", title = "Actual vs. Predicted Values by State") +
  theme_minimal()
library(sjPlot)
sjPlot::plot_model(mod3,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Covars on Estimated Hospitalizatins (per 100,000)")
sjPlot:: tab_model(mod3)


##############################################
# example GLS

gls1 <- gls(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley,
            data = covid_hosp_analysis,
            correlation = corAR1(form=~state))  
round(summary(gls1)$tTable, 3)
summary(gls1)
anova(mlrfull_MD, gls1)
mean(gls1$residuals)


glsall <- nlme::gls(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley + (1|state),
                    data = covid_hosp_analysis,
                    correlation = corAR1(form=~date|state))  
round(summary(glsall)$tTable, 3)
summary(glsall)
anova(mlrfull_MD, gls1)

##############################################
##########################################################
# table 1

footnote = "N = number of days"
table1(~year + season + dominant_variant + four_week_peak + four_week_valley  | state, data= covid_hosp_analysis,
       footnote=footnote, 
)

#########################################################
# unadjusted variables TABLE

tbl_unadjusted_md <- covid_hosp_analysis %>%
  filter(state == "MD") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value
tbl_unadjusted_md

tbl_unadjusted_pa <- covid_hosp_analysis %>%
  filter(state == "PA") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value
tbl_unadjusted_pa

tbl_unadjusted_ny <- covid_hosp_analysis %>%
  filter(state == "NY") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value

tbl_unadjusted_nj <- covid_hosp_analysis %>%
  filter(state == "NJ") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value
tbl_unadjusted_nj

tbl_unadjusted_overall <- covid_hosp_analysis %>%
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value


merged_tbl <- tbl_merge(
  tbls = list(tbl_unadjusted_md, tbl_unadjusted_nj, tbl_unadjusted_ny, tbl_unadjusted_pa),
  tab_spanner(c("Unadjusted Maryland", "Unadjusted NJ", "Unadjusted NY", "Unadjusted PA"))
)

merged_tbl <- tbl_merge(
  tbls = list(tbl_unadjusted_overall, tbl_unadjusted_md, tbl_unadjusted_nj, tbl_unadjusted_ny, tbl_unadjusted_pa),
  tab_spanner = c("Overall", "Maryland", "New Jersey", "New York", "Pennsylvania")
)

merged_tbl

#####################################
# Visualizations of hospitalizations

hosp_viz <- covid_hosp_analysis %>%  
  ggplot(aes(x = date, y = total_hosp_estimate)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed")) +
  geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  facet_wrap(~state, ncol = 1) +
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  labs(color = "Difference (Observed - Expected)", 
       title = "Time Series of Hospitalization Estimates") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("solid", "dashed"), 
                        labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
ggsave("hosp_viz.png", plot = hosp_viz, width = 10, height = 8, dpi = 300)
