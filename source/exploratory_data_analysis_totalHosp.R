library(arrow)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)

covid_joined_totalHosp_state_data <- arrow::read_parquet("data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily.parquet")

covid_joined_totalHosp_state_data_analysis <- covid_joined_totalHosp_state_data %>% 
  mutate(peak_average = ifelse(total_hosp > mean(total_hosp), "Yes", "No"),
         peak_median = ifelse(total_hosp > median(total_hosp), "Yes", "No"),
         year = year(as.Date(date)),
         log_absolute_difference = ifelse(absolute_difference <= 0, NA, log(absolute_difference + 0.001)),
         state_numeric = case_when(
           state == "NJ" ~ 1,
           state == "NY" ~ 2,
           state == "MD" ~ 3,
           state == "PA" ~ 4),
         season = case_when(
           month(date) %in% c(9, 10, 11) ~ "Fall",
           month(date) %in% c(12, 1, 2) ~ "Winter",
           month(date) %in% c(3, 4, 5) ~ "Spring",
           month(date) %in% c(6, 7, 8) ~ "Summer"))
         #,
         #variant = case_when())

########################################################################################

# Tables --------------------------

# Overall distribution of variables 
covid_joined_totalHosp_state_data_analysis %>% 
  select(state, year, season, peak_average, totalHosp_split) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    type = list(state      ~ "categorical",
                year ~ "categorical",
                season ~ "categorical",
                peak_average   ~ "categorical"),
    label  = list(state      ~ "State",
                 year ~ "Year",
                 season ~ "Season",
                 peak_average   ~ "Above Average",
                 totalHosp_split   ~ "Dist of Hosp Estimates"),
  ) %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("Total Hospitalization Time Series Characteristics") %>%
  bold_labels()

ft <- as_flextable(tbl)

# Save the flextable as an image
flextable::save_as_image(ft, path = "summary_table_image.png")

# Distribution of Variables by under / over estimating total hospitalizations 

covid_joined_totalHosp_state_data_analysis <- covid_joined_totalHosp_state_data_analysis %>% 
  mutate(totalHosp_split = case_when(
    difference >  25 ~ "Overestimate",
    difference >= -25 & difference <= 25 ~ "Estimate +/- 25 Hosp",
    difference <  -25 ~ "Underestimate"))

# Create table
covid_joined_totalHosp_state_data_analysis %>% 
  select(state, year, season, peak_average, totalHosp_split) %>%
  tbl_summary(
    by = totalHosp_split,
    statistic = list(all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    type = list(state      ~ "categorical",
                year ~ "categorical",
                season ~ "categorical",
                peak_average   ~ "categorical"),
    label  = list(state      ~ "State",
                  year ~ "Year",
                  season ~ "Season",
                  peak_average   ~ "Above Average"),
  ) %>%
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) %>%
  modify_caption("Total Hospitalization Time Series Characteristics, by Hospitalization Estimate") %>%
  bold_labels() %>% 
  add_overall(last = FALSE,
              col_label = "**Overall**<br>N = {N}")

########################################################################################
# Visualizations of hospitalizations

covid_joined_totalHosp_state_data_analysis %>%   
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_hosp_estimate, color = "Estimated")) +
  geom_line(aes(y = total_hosp, color = "Observed")) +
  scale_color_manual(values = c(Estimated = "blue", Observed = "lightblue"),
                     labels = c("Estimated", "Observed")) +
  labs(x = "Date", y = "Total Hospitalizations", color = "Data Type") + 
  facet_wrap(~state, ncol = 1) +
  ggtitle("Total Observed vs. Estimated Hospitalizations\nby State")

# visuals of median and peak 
# Median peak 

covid_joined_totalHosp_state_data_analysis %>%   
  group_by(state) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_hosp_estimate, color = "Estimated")) +
  geom_line(aes(y = total_hosp, color = "Observed")) +
  geom_line(aes(y = median(total_hosp), color = "Median value"), color = "red") +
  scale_color_manual(values = c(Estimated = "blue", Observed = "lightblue", "Median value" = "red"),
                     labels = c("Estimated", "Observed", "Median value")) +
  labs(x = "Date", y = "Total Hospitalizations", color = "Data Type") + 
  geom_text(data = summarise(group_by(covid_joined_totalHosp_state_data_analysis, state),
                             median_value = median(total_hosp),
                             date = min(date)),
            aes(x = date, y = median_value, label = paste("Median:", median_value)), 
            color = "black", hjust = 0, vjust = 0, size = 2) +
  facet_wrap(~state, ncol = 1) +
  ggtitle("Total Observed vs. Estimated Hospitalizations\nby State")

#Mean Peak

covid_joined_totalHosp_state_data_analysis %>%   
  group_by(state) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_hosp_estimate, color = "Estimated")) +
  geom_line(aes(y = total_hosp, color = "Observed")) +
  geom_line(aes(y = mean(total_hosp), color = "Mean value"), color = "red") +
  scale_color_manual(values = c(Estimated = "blue", Observed = "lightblue", "Mean value" = "red"),
                     labels = c("Estimated", "Observed", "Mean value")) +
  labs(x = "Date", y = "Total Hospitalizations", color = "Data Type") + 
  geom_text(data = summarise(group_by(covid_joined_totalHosp_state_data_analysis, state),
                             mean_value = mean(total_hosp),
                             date = min(date)),
            aes(x = date, y = mean_value, label = paste("Mean:", round(mean_value, 2))), 
            color = "black", hjust = 0, vjust = 0, size = 2) +
  facet_wrap(~state, ncol = 1) +
  ggtitle("Total Observed vs. Estimated Hospitalizations\nby State")

########################################################################################

# Exploratory Data Visualizations ----------------------------------------

# STATE
covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = state, y = difference)) +  
  geom_boxplot(aes(color = state)) +  
  labs(x = "State", y = "Difference")

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = state, y = absolute_difference)) +  
  geom_boxplot(aes(color = state)) +  
  labs(x = "State", y = "absolute_difference")

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = state, y = log_absolute_difference)) +  
  geom_boxplot(aes(color = state)) +  
  labs(x = "State", y = "log_absolute_difference")

# Seasonal Peak 
# look at log dif w NY here 
covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = as.factor(peak_average), y = difference)) +  
  geom_boxplot(aes(color = peak_average)) +  
  labs(x = "Peak", y = "Difference")

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = as.factor(peak_average), y = log_absolute_difference)) +  
  geom_boxplot(aes(color = peak_average)) +  
  labs(x = "Peak", y = "log_absolute_difference")

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = as.factor(peak_average), y = log_absolute_difference)) +  
  geom_bar(aes(color = peak_average)) +  
  labs(x = "Peak", y = "log_absolute_difference")

covid_joined_totalHosp_state_data_analysis %>%
  ggplot(aes(x = as.factor(peak_average), y = log_absolute_difference, fill = as.factor(peak_average))) +  
  geom_boxplot() +  
  labs(x = "Peak", y = "Log Absolute Difference") +
  scale_fill_discrete(name = "Seasonal Peak") +
  theme_minimal()

# Year

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = as.factor(year), y = log_absolute_difference)) +  
  geom_boxplot(aes(color = year)) +  
  labs(x = "Year", y = "log_absolute_difference")


# Matrix Plot 

pairs(~ year + peak_average + state_numeric + difference, data=covid_joined_totalHosp_state_data_analysis, upper.panel=NULL, pch=".")

# Distribution of difference 

summary(covid_joined_totalHosp_state_data_analysis$difference)

summary(covid_joined_totalHosp_state_data_analysis$absolute_difference)
summary(covid_joined_totalHosp_state_data_analysis$log_absolute_difference)


########################################################################################
########################################################################################

# Difference SLR --------------------------

# State
model <- lm(difference ~ as.factor(state), data = covid_joined_totalHosp_state_data_analysis)
summary(model)

# Year

model <- lm(difference ~ as.factor(year), data = covid_joined_totalHosp_state_data_analysis)
summary(model)

#Seasonal Peak 
model <- lm(difference ~ peak_average, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

model <- lm(difference ~ peak_median, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

# Season
model <- lm(difference ~ as.factor(season), data = covid_joined_totalHosp_state_data_analysis)
summary(model)


# MLR 
model <- lm(difference ~ peak_average + as.factor(state) + as.factor(year) + season, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

model <- lm(difference ~ peak_average + as.factor(state) + as.factor(year)*season, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

model <- lm(difference ~ peak_average + as.factor(state)*as.factor(year)*season, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

# Log Absolute Difference SLR --------------------------

# State
model <- lm(log_absolute_difference ~ as.factor(state), data = covid_joined_totalHosp_state_data_analysis)
summary(model)

# Year

model <- lm(log_absolute_difference ~ as.factor(year), data = covid_joined_totalHosp_state_data_analysis)
summary(model)

#Seasonal Peak 
model <- lm(log_absolute_difference ~ peak_average, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

model <- lm(log_absolute_difference ~ peak_median, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

# Season
model <- lm(log_absolute_difference ~ as.factor(season), data = covid_joined_totalHosp_state_data_analysis)
summary(model)


# Log Absolute Difference MLR 

model <- lm(log_absolute_difference ~ peak_average + as.factor(state) + as.factor(year) + season, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

model <- lm(log_absolute_difference ~ peak_average + as.factor(state) + as.factor(year)*season, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

model <- lm(log_absolute_difference ~ peak_average + as.factor(state)*as.factor(year)*season, data = covid_joined_totalHosp_state_data_analysis)
summary(model)

########################################################################################

# EXCLUDE NY -----------------------------------------
covid_joined_totalHosp_MD_NJ_PA <- covid_joined_totalHosp_state_data_analysis %>% 
  filter(state != "NY")
# Difference SLR --------------------------

# State
model <- lm(difference ~ as.factor(state), data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

# Year

model <- lm(difference ~ as.factor(year), data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

#Seasonal Peak 
model <- lm(difference ~ peak_average, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

model <- lm(difference ~ peak_median, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

# Season
model <- lm(difference ~ as.factor(season), data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)


# MLR 
model <- lm(difference ~ peak_average + as.factor(state) + as.factor(year) + season, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

model <- lm(difference ~ peak_average + as.factor(state) + as.factor(year)*season, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

model <- lm(difference ~ peak_average + as.factor(state)*as.factor(year)*season, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

# Log Absolute Difference SLR --------------------------

# State
model <- lm(log_absolute_difference ~ as.factor(state), data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

# Year

model <- lm(log_absolute_difference ~ as.factor(year), data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

#Seasonal Peak 
model <- lm(log_absolute_difference ~ peak_average, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

model <- lm(log_absolute_difference ~ peak_median, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

# Season
model <- lm(log_absolute_difference ~ as.factor(season), data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)


# Log Absolute Difference MLR 
model <- lm(log_absolute_difference ~ peak_average + as.factor(state) + as.factor(year) + season, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

model <- lm(log_absolute_difference ~ peak_average + as.factor(state) + as.factor(year)*season, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

model <- lm(log_absolute_difference ~ peak_average + as.factor(state)*as.factor(year)*season, data = covid_joined_totalHosp_MD_NJ_PA)
summary(model)

