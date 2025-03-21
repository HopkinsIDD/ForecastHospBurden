library(arrow)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)
library(forecast)
library(pracma)
library(performance)
### Data Setup ----------------
opt <- list()
opt$gt_data_path_hosp_burden_estimates <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_04142024.parquet"
opt$gt_data_path_variants <- "data/variants/variant_props_R17.parquet"


### Creating datasets 
covid_joined_totalHosp_state_data <- arrow::read_parquet(opt$gt_data_path_hosp_burden_estimates)
variants <- arrow::read_parquet(opt$gt_data_path_variants)

##### Variants ---------------

variants_temp <- variants %>% 
  rename(state = source, 
         date = Update)

#### Adding covariate to data for analysis ----------
covid_joined_totalHosp_state_data_analysis <- covid_joined_totalHosp_state_data %>%
  mutate(year = year(as.Date(date)),
         academic_year = case_when(
           month(date) >= 8 ~ paste(year(date), "-", year(date) + 1, sep = ""),
           TRUE ~ paste(year(date) - 1, "-", year(date), sep = "")
         ),
         capacity_dif_prop = difference / inpatient_beds,
         capacity_prop_per100thousand = (difference / inpatient_beds)*100000,
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
           month(date) %in% c(6, 7, 8) ~ "Summer")) %>% 
  left_join(variants_temp, by = c("state", "date")) %>% 
  spread(key = variant, value = prop, fill = 0) %>% 
  rowwise() %>%
  mutate(dominant_variant = {
    max_proportion <- max(ALPHA, DELTA, OMICRON, WILD)
    if (max_proportion == ALPHA) {
      "ALPHA"
    } else if (max_proportion == DELTA) {
      "DELTA"
    } else if (max_proportion == OMICRON) {
      "OMICRON"
    } else {
      "WILD"
    }
  }) %>%
  ungroup()

unique(covid_joined_totalHosp_state_data_analysis$dominant_variant)

# 
# covid_joined_totalHosp_state_data_analysis <- covid_joined_totalHosp_state_data_analysis %>% 
#   mutate(total_admissions = previous_day_admission_pediatric_covid_confirmed + previous_day_admission_adult_covid_confirmed,
#          age_group_pediatric_prop = previous_day_admission_pediatric_covid_confirmed / total_admissions,
#          age_group_18_29_prop = (`previous_day_admission_adult_covid_confirmed_18-19` +
#                                  `previous_day_admission_adult_covid_confirmed_20-29`) / total_admissions,
#        age_group_30_39_prop = `previous_day_admission_adult_covid_confirmed_30-39` / total_admissions,
#        age_group_40_49_prop = `previous_day_admission_adult_covid_confirmed_40-49`/ total_admissions,
#        age_group_50_59_prop = `previous_day_admission_adult_covid_confirmed_50-59` / total_admissions,
#        age_group_60_69_prop = `previous_day_admission_adult_covid_confirmed_60-69` / total_admissions,
#        age_group_70_79_prop = `previous_day_admission_adult_covid_confirmed_70-79` / total_admissions,
#        age_group_80_plus_prop = `previous_day_admission_adult_covid_confirmed_80+` / total_admissions,
#        age_group_unknown_prop = previous_day_admission_adult_covid_confirmed_unknown / total_admissions) %>%
#   mutate(age_group_pediatric_prop = lead(age_group_pediatric_prop, 1),
#          age_group_18_29_prop = lead(age_group_18_29_prop, 1),
#          age_group_30_39_prop = lead(age_group_30_39_prop, 1),
#          age_group_40_49_prop = lead(age_group_40_49_prop, 1),
#          age_group_50_59_prop = lead(age_group_50_59_prop, 1),
#          age_group_60_69_prop = lead(age_group_60_69_prop, 1),
#          age_group_70_79_prop = lead(age_group_70_79_prop, 1),
#          age_group_80_plus_prop = lead(age_group_80_plus_prop, 1),
#          age_group_unknown_prop = lead(age_group_unknown_prop, 1))
# 
# # Replace proportions with NA if the sum of proportions in a row is greater than 1
# covid_joined_totalHosp_state_data_analysis <- covid_joined_totalHosp_state_data_analysis %>%
#   mutate(total_prop_sum = rowSums(select(., starts_with("age_group")), na.rm = TRUE),
#          across(starts_with("age_group"), ~ ifelse(total_prop_sum > 1, NA, .)),
#          across(starts_with("age_group"), ~ ifelse(is.na(total_prop_sum), NA, .)))
# 
# # check proportions 
# # Calculate row sums for proportion columns
# prop_columns <- c("age_group_pediatric_prop", "age_group_18_29_prop", "age_group_30_39_prop", 
#                   "age_group_40_49_prop", "age_group_50_59_prop", "age_group_60_69_prop",
#                   "age_group_70_79_prop", "age_group_80_plus_prop", "age_group_unknown_prop")
# 
# row_sums <- rowSums(covid_joined_totalHosp_state_data_analysis[, prop_columns], na.rm = TRUE)
# 
# # Check if row sums equal 1.0
# all(row_sums == 1.0)
# 
# num_inconsistent_rows <- sum(row_sums != 1.0)
# 
# # Print the number of inconsistent rows
# print(num_inconsistent_rows)

################
#optimized values table 
summary_los <- covid_joined_totalHosp_state_data_analysis %>% 
  mutate(days_overestimate = summarise_if(.predicate = ~sum(. > 0), vars(difference))) %>% 
  group_by(state) %>% 
  summarise(optimized_los = optimized_los,
            days_overestimate = days_overestimate
  )
summary_los <- covid_joined_totalHosp_state_data_analysis %>%
  mutate(days_overestimate = summarise_if(.predicate = ~sum(. > 0), vars(difference)))

summary_los <- covid_joined_totalHosp_state_data_analysis %>%
  group_by(state) %>%
  summarise(sum_difference_greater_than_0 = sum(difference[difference > 0]),
            sum_difference_less_than_0 = sum(difference[difference < 0])) %>%
  mutate(prop_difference_greater_than_0 = sum_difference_greater_than_0 / sum(difference),
         prop_difference_less_than_0 = sum_difference_less_than_0 / sum(difference))

summary_los <- covid_joined_totalHosp_state_data_analysis %>%
  group_by(state) %>%
  summarise(
    optimized_los = first(optimized_los),
    sum_difference_greater_than_0 = sum(difference[difference > 0]),
    sum_difference_less_than_0 = sum(difference[difference < 0]),
    absolute_sum_difference = sum(abs(difference))) 
summary_los <- covid_joined_totalHosp_state_data_analysis %>%
  group_by(state) %>%
  summarise(
    optimized_los = first(optimized_los),
    count_difference_between_minus10_and_10 = sum(difference >= -10 & difference <= 10),
    count_difference_less_than_minus10 = sum(difference < -10),
    count_difference_greater_than_10 = sum(difference > 10),
    count_difference_between_minus25_and_25 = sum(difference >= -25 & difference <= 25),
    count_difference_less_than_minus25 = sum(difference < -25),
    count_difference_greater_than_25 = sum(difference > 25),
    total_days = n(),
    proportion_difference_between_minus10_and_10 = sum(difference >= -10 & difference <= 10) / total_days,
    proportion_difference_between_minus25_and_25 = sum(difference >= -25 & difference <= 25) / total_days,
    proportion_difference_less_than_minus10 = sum(difference < -10) / total_days,
    proportion_difference_less_than_minus25 = sum(difference < -25) / total_days,
    proportion_difference_greater_than_10 = sum(difference > 10) / total_days,
    proportion_difference_greater_than_25 = sum(difference > 25) / total_days
  )

summary_los_percent <- covid_joined_totalHosp_state_data_analysis %>%
  group_by(state) %>%
  summarise(
    optimized_los = first(optimized_los),
    count_difference_between_minus10_and_10 = sum(difference >= -0.1 * total_hosp & difference <= 0.1 * total_hosp),
    count_difference_less_than_minus10 = sum(difference < -0.1 * total_hosp),
    count_difference_greater_than_10 = sum(difference > 0.1 * total_hosp),
    count_difference_between_minus25_and_25 = sum(difference >= -0.25 * total_hosp & difference <= 0.25 * total_hosp),
    count_difference_less_than_minus25 = sum(difference < -0.25 * total_hosp),
    count_difference_greater_than_25 = sum(difference > 0.25 * total_hosp),
    total_days = n(),
    proportion_difference_between_minus10_and_10 = sum(difference >= -0.1 * total_hosp & difference <= 0.1 * total_hosp) / total_days,
    proportion_difference_between_minus25_and_25 = sum(difference >= -0.25 * total_hosp & difference <= 0.25 * total_hosp) / total_days,
    proportion_difference_less_than_minus10 = sum(difference < -0.1 * total_hosp) / total_days,
    proportion_difference_less_than_minus25 = sum(difference < -0.25 * total_hosp) / total_days,
    proportion_difference_greater_than_10 = sum(difference > 0.1 * total_hosp) / total_days,
    proportion_difference_greater_than_25 = sum(difference > 0.25 * total_hosp) / total_days
  )

write.csv(summary_los, "summary_los.csv", row.names = FALSE)

summary_los_5percent <- covid_joined_totalHosp_state_data_analysis %>%
  mutate(difference = -difference) %>% 
  group_by(state) %>%
  summarise(
    optimized_los = first(optimized_los),
    count_difference_between_minus5_and_5 = sum(difference >= -0.05 * total_hosp & difference <= 0.05 * total_hosp),
    count_difference_less_than_minus5 = sum(difference < -0.05 * total_hosp),
    count_difference_greater_than_5 = sum(difference > 0.05 * total_hosp),
    total_days = n(),
    proportion_difference_between_minus5_and_5 = sum(difference >= -0.05 * total_hosp & difference <= 0.05 * total_hosp) / total_days,
    proportion_difference_less_than_minus5 = sum(difference < -0.05 * total_hosp) / total_days,
    proportion_difference_greater_than_5 = sum(difference > 0.05 * total_hosp) / total_days
  )

write.csv(summary_los_5percent, "summary_los_5percent.csv", row.names = FALSE)

# check distribution of total_cases to ID peaks and valleys -----------

# Calculate the mean and standard deviation of total_hosp for each state and year
summary_stats_state_academicyear <- covid_joined_totalHosp_state_data_analysis %>%
  mutate(academic_year = case_when(
    month(date) >= 8 ~ paste(year(date), "-", year(date) + 1, sep = ""),
    TRUE ~ paste(year(date) - 1, "-", year(date), sep = "")
  )) %>% group_by(state, academic_year) %>%
  summarize(mean_total_hosp = mean(total_hosp),
            sd_total_hosp = sd(total_hosp),
            Q3 = quantile(total_hosp, 0.75),
            Q1 = quantile(total_hosp, 0.25)) %>% 
  mutate(sd_gt_mean = ifelse(mean_total_hosp < sd_total_hosp, "Yes", "No"),
         sd2_gt_mean = ifelse(mean_total_hosp < 2*sd_total_hosp, "Yes", "No"))

summary_stats_state_year <- covid_joined_totalHosp_state_data_analysis %>%
  group_by(state, year) %>%
  summarize(mean_total_hosp = mean(total_hosp),
            sd_total_hosp = sd(total_hosp),
            Q3 = quantile(total_hosp, 0.75),
            Q1 = quantile(total_hosp, 0.25)) %>% 
  mutate(sd_gt_mean = ifelse(mean_total_hosp < sd_total_hosp, "Yes", "No"),
         sd2_gt_mean = ifelse(mean_total_hosp < 2*sd_total_hosp, "Yes", "No"))

# Create the boxplot
dist_state_year <- ggplot(covid_joined_totalHosp_state_data_analysis, aes(x = "", y = total_hosp)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  geom_hline(data = summary_stats_state_year, aes(yintercept = mean_total_hosp), color = "red", linetype = "dashed") +
  geom_hline(data = summary_stats_state_year, aes(yintercept = mean_total_hosp - sd_total_hosp), color = "blue", linetype = "dashed") +
  geom_hline(data = summary_stats_state_year, aes(yintercept = mean_total_hosp + sd_total_hosp), color = "blue", linetype = "dashed") +
  labs(title = "Boxplot of total_hosp by State and Year",
       x = "",
       y = "Total Hospitalizations") +
  theme_minimal() +
  facet_wrap(~ state + year, ncol = 5)  # Facet by both state and year with 3 columns

# Print the boxplot
print(dist_state_year)

summary_stats_state <- covid_joined_totalHosp_state_data_analysis %>%
  group_by(state) %>%
  summarize(mean_total_hosp = mean(total_hosp),
            sd_total_hosp = sd(total_hosp)) %>% 
  mutate(sd_gt_mean = ifelse(mean_total_hosp < sd_total_hosp, "Yes", "No"))


dist_state <- ggplot(covid_joined_totalHosp_state_data_analysis, aes(x = "", y = total_hosp)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  geom_hline(data = summary_stats_state, aes(yintercept = mean_total_hosp), color = "red", linetype = "dashed") +
  geom_hline(data = summary_stats_state, aes(yintercept = mean_total_hosp - sd_total_hosp), color = "blue", linetype = "dashed") +
  geom_hline(data = summary_stats_state, aes(yintercept = mean_total_hosp + sd_total_hosp), color = "blue", linetype = "dashed") +
  labs(title = "Boxplot of total_hosp by State",
       x = "",
       y = "Total Hospitalizations") +
  theme_minimal() +
  facet_wrap(~ state, ncol = 4)  # Facet by both state and year with 3 columns
print(dist_state)


# Create functions to find Peaks/Valleys -------------------

find_peaks_and_valleys <- function(df, column_name) {
  # Calculate mean and standard deviation
  median_val <- median(column_name)
  sd_val <- sd(column_name)
  
  Q3 <- quantile(column_name, 0.75)
  Q1 <- quantile(column_name, 0.25)
  
  # Find peaks above 2 standard deviations from the mean
  peaks <- findpeaks(column_name, minpeakheight = Q3)
  
  # Find valleys below 2 standard deviations from the mean
  valleys <- findpeaks(-column_name, minpeakheight = -Q1) 
  
  # Initialize new columns to indicate peaks and valleys
  df$is_peak <- FALSE
  df$is_valley <- FALSE
  
  # Initialize 4-week peak and valley vectors to mark peak and valley periods +/- 14 days
  peak_window <- 14
  valley_window <- 14
  four_week_peak <- rep(FALSE, nrow(df))
  four_week_valley <- rep(FALSE, nrow(df))
  
  # Mark peaks
  for (i in 1:nrow(peaks)) {
    peak_index <- peaks[i, 2]
    start_index <- max(1, peak_index - peak_window)
    end_index <- min(nrow(df), peak_index + peak_window)
    four_week_peak[start_index:end_index] <- TRUE
  }
  
  # Mark valleys
  for (i in 1:nrow(valleys)) {
    valley_index <- valleys[i, 2]
    start_index <- max(1, valley_index - valley_window)
    end_index <- min(nrow(df), valley_index + valley_window)
    four_week_valley[start_index:end_index] <- TRUE
  }
  
  # Mark peaks and valleys with TRUE in respective columns
  df$is_peak[peaks[,2]] <- TRUE
  df$is_valley[valleys[,2]] <- TRUE
  
  # Add peak and valley period variables to the dataframe
  df$four_week_peak <- four_week_peak
  df$four_week_valley <- four_week_valley
  
  return(df)
}

find_peaks_and_valleys_all <- function(df) {
  # Initialize an empty dataframe to store results
  master_df <- data.frame()
  
  # Extract unique states from the dataframe
  states <- unique(df$state)
  
  # Iterate over each state
  for (state in states) {
    # Subset the dataframe for the current state
    state_df <- df %>%
      filter(state == state) %>% 
      dplyr::select(state, date, total_hosp, academic_year)
    
    # Extract unique academic years for the current state
    academic_years <- unique(state_df$academic_year)
    
    # Iterate over each academic year for the current state
    for (academic_year in academic_years) {
      # Subset the dataframe for the current academic year and state
      year_df <- state_df %>%
        filter(academic_year == academic_year)
      
      # Find peaks and valleys for the current academic year and state
      peaks_and_valleys <- find_peaks_and_valleys(year_df, year_df$total_hosp)
      
      # join peaks/valleys to df 
      master_df <- left_join(df, peaks_and_valleys, by = c("state", "date", "total_hosp", "academic_year"))
    }
  }
  
  return(master_df)
}

covid_joined_totalHosp_state_data_analysis <- find_peaks_and_valleys_all(covid_joined_totalHosp_state_data_analysis)
# peaks and valleys just by quantiles not findpeak function

peaks_and_valleys_quantiles <- function(df) {
  # Initialize an empty dataframe to store results
  master_df <- data.frame()
  
  # Extract unique states from the dataframe
  states <- unique(df$state)
  
  # Iterate over each state
  for (state in states) {
    # Subset the dataframe for the current state
    state_df <- df %>%
      filter(state == state) %>% 
      dplyr::select(state, date, total_hosp, academic_year)
    
    # Extract unique academic years for the current state
    academic_years <- unique(state_df$academic_year)
    
    # Iterate over each academic year for the current state
    for (academic_year in academic_years) {
      # Subset the dataframe for the current academic year and state
      year_df <- state_df %>%
        filter(academic_year == academic_year)
      
      above_Q75 <- quantile(year_df$total_hosp, 0.75)
      below_Q25 <- quantile(year_df$total_hosp, 0.25)
      
      year_df <- year_df %>% 
        mutate(peak_period = as.factor(ifelse(total_hosp > above_Q75, 1, 0)),
               trough_period = as.factor(ifelse(total_hosp < below_Q25, 1, 0)),
               middle_period = as.factor(ifelse(peak_period == 0 & trough_period == 0, 1, 0)))
      
      # Find peaks and valleys for the current academic year and state
      
      
      # join peaks/valleys to df 
      master_df <- left_join(df, year_df, by = c("state", "date", "total_hosp", "academic_year"))
    }
  }
  
  return(master_df)
}


covid_joined_totalHosp_state_data_analysis <- peaks_and_valleys_quantiles(covid_joined_totalHosp_state_data_analysis)

# visualize valley's and peaks

# Create a new column to indicate period
covid_joined_totalHosp_state_data_analysis <- covid_joined_totalHosp_state_data_analysis %>%
  mutate(period = ifelse(four_week_peak, "Peak", ifelse(four_week_valley, "Valley", "Normal")))

# Plot with updated period column
# period using function
ggplot(covid_joined_totalHosp_state_data_analysis, aes(x = date, y = total_hosp_estimate)) + 
  geom_point(aes(color = period), size =0.25) +
  geom_line(aes(y = total_hosp), size =0.25, alpha = .4) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.75))) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.25))) +
  facet_wrap(~state, ncol = 5) +
  labs(color = "Period")

# period just using 25/75% quantiles
ggplot(covid_joined_totalHosp_state_data_analysis, aes(x = date, y = total_hosp_estimate)) + 
  geom_point(aes(color = as.factor(peak_period)), size =0.25) +
  geom_line(aes(y = total_hosp), size =0.25, alpha = .4) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.75))) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.25))) +
  facet_wrap(~state, ncol = 5) +
  labs(color = "Peak Period")

ggplot(covid_joined_totalHosp_state_data_analysis, aes(x = date, y = total_hosp_estimate)) + 
  geom_point(aes(color = as.factor(trough_period)), size =0.25) +
  geom_line(aes(y = total_hosp), size =0.25, alpha = .4) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.75))) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.25))) +
  facet_wrap(~state, ncol = 5) +
  labs(color = "Trough Period")

ggplot(covid_joined_totalHosp_state_data_analysis, aes(x = date, y = total_hosp_estimate)) + 
  geom_point(aes(color = as.factor(middle_period)), size =0.25) +
  geom_line(aes(y = total_hosp), size =0.25, alpha = .4) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.75))) +
  geom_hline(aes(yintercept = quantile(total_hosp, 0.25))) +
  facet_wrap(~state, ncol = 5) +
  labs(color = "Middle Period")

## select vars for analysis 
covid_hosp_analysis <- covid_joined_totalHosp_state_data_analysis %>% 
  select(state, date, year, academic_year, total_hosp, total_hosp_estimate, difference, relative_difference, capacity_dif_prop, capacity_prop_per100thousand,
         season, dominant_variant, four_week_peak, four_week_valley, peak_period, middle_period, trough_period)

write_parquet(covid_hosp_analysis, "data/analysis_data/hospburden_analysis.parquet")                                     
# explore as indicator vars vs. dummy vars 
#state, year, season, dominant_variant,            


########################################################################################

# Tables --------------------------

# Overall distribution of variables 
covid_joined_totalHosp_state_data_analysis %>% 
  select(state, year, season, peak_period, totalHosp_split) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    type = list(state ~ "categorical",
                year ~ "categorical",
                season ~ "categorical",
                peak_period   ~ "categorical"),
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

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = state, y = relative_difference)) +  
  geom_boxplot(aes(color = state)) +  
  labs(x = "State", y = "relative_difference")

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
  ggplot(aes(x = as.factor(year), y = difference)) +  
  geom_boxplot(aes(color = year)) +  
  labs(x = "Year", y = "difference")

covid_joined_totalHosp_state_data_analysis %>% 
  ggplot(aes(x = as.factor(year), y = relative_difference)) +  
  geom_boxplot(aes(color = year)) +  
  labs(x = "Year", y = "relative_difference")


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


#### TIME SERIES REGRESSION 

trModel <- lm(difference ~ c(1:length(difference)), data = covid_joined_totalHosp_state_data_analysis)
plot(resid(trModel), type="l") 
summary(trModel)

trModel2 <- lm(difference ~ c(1:length(difference)) + as.factor(season) + as.factor(state) + as.factor(year) + as.factor(dominant_variant) + peak_average + trough_average, data = covid_joined_totalHosp_state_data_analysis)
plot(resid(trModel2), type="l") 
summary(trModel2)

covid_analysis_MD <- covid_joined_totalHosp_state_data_analysis %>% 
  filter(state == "MD")
trModel_MD <- lm(difference ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + peak_average + trough_average, data = covid_analysis_MD)
plot(resid(trModel_MD), type="l") 
summary(trModel_MD)


covid_analysis_NY <- covid_joined_totalHosp_state_data_analysis %>% 
  filter(state == "NY")
trModel_NY <- lm(difference ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + peak_average + trough_average, data = covid_analysis_NY)
plot(resid(trModel_NY), type="l") 
summary(trModel_NY)

covid_analysis_PA <- covid_joined_totalHosp_state_data_analysis %>% 
  filter(state == "PA")
trModel_PA <- lm(difference ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + peak_average + trough_average, data = covid_analysis_PA)
plot(resid(trModel_PA), type="l") 
summary(trModel_PA)

covid_analysis_NJ <- covid_joined_totalHosp_state_data_analysis %>% 
  filter(state == "NJ")
trModel_NJ <- lm(difference ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + peak_average + trough_average, data = covid_analysis_NJ)
plot(resid(trModel_NJ), type="l") 
summary(trModel_NJ)

# each state as seperate regression 
# LOS measure 
# difference make proprtion per __ hospitalized 
# hospital beds occupied per day per state population per ___ thousand 
# proportion of capacity 

### regressions again with final data !! 
covid_analysis_MD <- covid_hosp_analysis %>% 
  filter(state == "MD")
trModel_MD <- lm(capacity_dif_prop ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + four_week_valley + four_week_peak, data = covid_analysis_MD)
plot(resid(trModel_MD), type="l") 
summary(trModel_MD)

covid_analysis_NJ <- covid_hosp_analysis %>% 
  filter(state == "NJ")
trModel_NJ <- lm(capacity_dif_prop ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + four_week_valley + four_week_peak, data = covid_analysis_NJ)
plot(resid(trModel_NJ), type="l") 
summary(trModel_NJ)

covid_analysis_NY <- covid_hosp_analysis %>% 
  filter(state == "NY")
trModel_NY <- lm(capacity_dif_prop ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + four_week_valley + four_week_peak, data = covid_analysis_NY)
plot(resid(trModel_NY), type="l") 
summary(trModel_NY)

covid_analysis_PA <- covid_hosp_analysis %>% 
  filter(state == "PA")
trModel_PA <- lm(capacity_dif_prop ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + four_week_valley + four_week_peak, data = covid_analysis_PA)
plot(resid(trModel_PA), type="l") 
summary(trModel_PA)

covid_analysis_PA <- covid_hosp_analysis %>% 
  filter(state == "PA")
trModel_PA <- lm(capacity_dif_prop ~ as.factor(season) + as.factor(year) + as.factor(dominant_variant) + peak_period + trough_period + middle_period, data = covid_analysis_PA)
plot(resid(trModel_PA), type="l") 
summary(trModel_PA)
