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
library(openxlsx)
library(car) # for levene test

### IMPORT INITIAL DATA -----------------------------------
source("source/data_setup_source_bestfit.R")
source("source/data_setup_source.R")

opt <- list()
#read in paths for distributions
for(dist_type in distribution_list){
  
  opt_name <- paste0("gt_data_path_", dist_type)
  opt[[opt_name]] <- paste0("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_", dist_type, ".parquet")
  
  assign(paste0(dist_type, "_hosp_burden_estimates"), read_parquet(opt[[opt_name]]), envir = .GlobalEnv)

}

#read in paths for sum of squares estimates 
# for(dist_type in distribution_list){
#   
#   opt_name <- paste0("gt_data_path_", dist_type, "_sumofsquares")
#   opt[[opt_name]] <- paste0("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_", dist_type, "sum of squares", ".parquet")
#   
#   assign(paste0(dist_type, "_sumofsquares_hosp_burden_estimates"), read_parquet(opt[[opt_name]]), envir = .GlobalEnv)
#   
# }

# read in path for 3m LOS estimate
# opt$gt_data_path_3m_LOS <- "data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_USA_3m.parquet"
# three_month_LOS_hosp_burden_estimates <- read_parquet(opt$gt_data_path_3m_LOS)

wb <- createWorkbook() # create excel wb to add results to
# sumofsquares_hosp_burden_estimates

#### Compare distributions ---------------------------------
# notes: use F test (variance test) to see which distribution minimizes the error (produces the most accurate estimates) across all states

# check normality of data  
for (dist in distribution_list) {
  dist1_state <- get(paste0(dist, "_hosp_burden_estimates")) %>% 
    filter(state != "USA")
  
  hist(dist1_state$difference, 
       breaks = 20, 
       xlab = "Difference", 
       main = paste("Histogram of", dist, "Data"))
}

# since data isn't normally, don't use F test 
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35a.htm
# switch from F to levene since data is not normally distributed

# note on levene: when the sample size is large, small differences in group variances can produce a statistically significant Levene’s test 
# Fligner-Killeen’s test is a non-parametric test which is robust to departures from normality and provides a good alternative or double check for the previous parametric tests.
# compare distributions with Levene & Fligner-Killeen’s test ---------------------------------

# step 1 combine data into one master df 
hosp_burden_diff_by_distributions <- bind_rows(
  poisson_hosp_burden_estimates %>% 
    mutate(distribution = "poisson"),
  negbinomial_hosp_burden_estimates %>% 
    mutate(distribution = "negbinomial"),
  gamma_hosp_burden_estimates %>% 
    mutate(distribution = "gamma"),
  normal_hosp_burden_estimates %>% 
    mutate(distribution = "normal"),
  lognormal_hosp_burden_estimates %>% 
    mutate(distribution = "lognormal")) %>% 
  filter(state != "USA") %>% 
  select(state, date, distribution, difference)

hosp_burden_diff_by_distributions_variance <- hosp_burden_diff_by_distributions %>% 
  group_by(distribution) %>% 
  summarise(variance = var(difference))
print(hosp_burden_diff_by_distributions_variance)
#neg binom lowest variance 

# step 2 perform levene / fligner test
levene_test_results <- leveneTest(difference ~ distribution, data = hosp_burden_diff_by_distributions)
levene_test_results
fligner_test_results <- fligner.test(difference ~ distribution, data = hosp_burden_diff_by_distributions)
fligner_test_results
# results show variances for at least one distribution are significant, now ID which dist is significant

compare_distributions_tests <- function(comparison_list = distribution_list, data = hosp_burden_diff_by_distributions) {
  # Initialize an empty data frame to store results
  distribution_results <- data.frame(
    null_dist = character(),
    alternate_dist = character(),
    levene_F_value = numeric(),
    levene_p_value = numeric(),
    fligner_chi_squared = numeric(),
    fligner_df = numeric(),
    fligner_p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (dist1 in comparison_list) {
    for (dist2 in comparison_list) {
      if (dist1 != dist2) {
        print(paste("Comparing", dist1, "and", dist2))ß
        # Perform var.test and extract results
        data_dist <- data %>% 
          filter(distribution %in% c(dist1, dist2))
        
        # Perform Levene's test
        levene_test_results <- leveneTest(difference ~ distribution, data = data_dist)
        levene_F_value <- levene_test_results$statistic
        levene_p_value <- levene_test_results$`Pr(>F)`
        
        # Perform Fligner-Killeen test
        fligner_test_results <- fligner.test(difference ~ distribution, data = data_dist)
        fligner_chi_squared <- fligner_test_results$statistic
        fligner_df <- fligner_test_results$parameter
        fligner_p_value <- fligner_test_results$p.value
        
        # Store results in the data frame
        distribution_results <- rbind(distribution_results, data.frame(
          null_dist = dist1,
          alternate_dist = dist2,
          levene_F_value = levene_test_results$`F value`[1],
          levene_p_value = levene_test_results$`Pr(>F)`[1],
          fligner_chi_squared = round(fligner_chi_squared, 4),
          fligner_df = fligner_df,
          fligner_p_value = format.pval(fligner_p_value, digits = 4)
        ))
      }
    }
  }
  assign("distribution_results", distribution_results, envir = .GlobalEnv)
  
}

compare_distributions_tests(comparison_list = distribution_list, data = hosp_burden_diff_by_distributions)
# compare full test output 
compare_distributions_tests_full_results <- function(comparison_list = distribution_list, data = hosp_burden_diff_by_distributions) {

  for (dist1 in comparison_list) {
      if (dist1 != "negbinomial") {
        print(paste("Comparing", "negbinomial", "and", dist1))
        # Perform var.test and extract results
        data_dist <- data %>% 
          filter(distribution %in% c(dist1, "negbinomial"))
        
        # Perform Levene's test
        levene_test_results <- leveneTest(difference ~ distribution, data = data_dist)
        print(levene_test_results)
        # Perform Fligner-Killeen test
        fligner_test_results <- fligner.test(difference ~ distribution, data = data_dist)
        print(fligner_test_results)
      }
    }
  # assign("distribution_results", distribution_results, envir = .GlobalEnv)
  
}
compare_distributions_tests_full_results(comparison_list = distribution_list, data = hosp_burden_diff_by_distributions)
# neg binom lowest variance, not stat sig than gamma and poisson, but is compared to normal and lognormal
# distribution_list defined in source code 
compare_distributions <- function(comparison_list = distribution_list) {
  # Create an empty df to store the results
  distribution_results <- data.frame(
    null_dist = character(),
    alternate_dist = character(),
    F_value = numeric(),
    p_value = numeric(),
    null_var = numeric(),
    alternate_var = numeric(),
    ratio_of_variances = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform var.test for each pair of distribution types
  for (dist1 in comparison_list) {
    for (dist2 in comparison_list) {
      if (dist1 != dist2) {
        # Perform var.test and extract results
        var_test_result <- var.test(get(paste0(dist2, "_hosp_burden_estimates"))$difference,
                                    get(paste0(dist1, "_hosp_burden_estimates"))$difference)
        
        # Extract results from var.test
        F_value <- var_test_result$statistic
        p_value <- var_test_result$p.value
        ratio_of_variances <- var_test_result$estimate[1]
        CI <- var_test_result$conf.int
        
        # Store results in the data frame and round F_value and p_value to 4 decimals
        distribution_results <- rbind(distribution_results, data.frame(
          null_dist = dist1,
          alternate_dist = dist2,
          null_var = var(get(paste0(dist1, "_hosp_burden_estimates"))$difference),
          alternate_var = var(get(paste0(dist2, "_hosp_burden_estimates"))$difference),
          F_value = round(F_value, 4),
          p_value = round(p_value, 4),
          ratio_of_variances = round(ratio_of_variances, 2),
          CI_lower = round(CI[1], 2),
          CI_upper = round(CI[2], 2),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  assign("distribution_results", distribution_results, envir = .GlobalEnv)
}

compare_distributions(comparison_list = distribution_list)

# binomial fits better than poisson, result is significant 

addWorksheet(wb, sheetName = "LOS Discrete Dist Overall")
writeData(wb, sheet = "LOS Discrete Dist Overall", distribution_results, startCol = 1, startRow = 1)

# compare distributions overall without USA ---------------------------------
compare_distributions_no_USA <- function(comparison_list = distribution_list) {
  # Create an empty df to store the results
  distribution_results <- data.frame(
    null_dist = character(),
    alternate_dist = character(),
    F_value = numeric(),
    p_value = numeric(),
    null_var = numeric(),
    alternate_var = numeric(),
    ratio_of_variances = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform var.test for each pair of distribution types
  for (dist1 in comparison_list) {
    for (dist2 in comparison_list) {
      if (dist1 != dist2) {
        dist1_state <- get(paste0(dist1, "_hosp_burden_estimates")) %>% 
          filter(state != "USA")
        dist2_state <- get(paste0(dist2, "_hosp_burden_estimates")) %>% 
          filter(state != "USA")
        
        # Perform var.test and extract results
        var_test_result <- var.test(dist2_state$difference,
                                    dist1_state$difference)
        
        # Extract results from var.test
        F_value <- var_test_result$statistic
        p_value <- var_test_result$p.value
        ratio_of_variances <- var_test_result$estimate[1]
        CI <- var_test_result$conf.int
        
        # Store results in the data frame and round F_value and p_value to 4 decimals
        distribution_results <- rbind(distribution_results, data.frame(
          null_dist = dist1,
          alternate_dist = dist2,
          null_var = var(dist1_state$difference),
          alternate_var = var(dist2_state$difference),
          F_value = round(F_value, 4),
          p_value = round(p_value, 4),
          ratio_of_variances = round(ratio_of_variances, 2),
          CI_lower = round(CI[1], 2),
          CI_upper = round(CI[2], 2),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  assign("distribution_results_no_USAOverall", distribution_results, envir = .GlobalEnv)
}

compare_distributions_no_USA(comparison_list = distribution_list)

addWorksheet(wb, sheetName = "LOS Disc Dist Overall (no USA)")
writeData(wb, sheet = "LOS Disc Dist Overall (no USA)", distribution_results_no_USAOverall, startCol = 1, startRow = 1)

# compare distributions by state ---------------------------------
compare_distributions_by_state <- function(parent_data = poisson_hosp_burden_estimates, comparison_list = distribution_list) {
  states_list <- unique(parent_data$state)
  
  # Create an empty df to store the results
  distribution_results <- data.frame(
    state = character(),
    null_dist = character(),
    alternate_dist = character(),
    F_value = numeric(),
    p_value = numeric(),
    null_var = numeric(),
    alternate_var = numeric(),
    ratio_of_variances = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (st in states_list) {
    # Perform var.test for each pair of distribution types
    for (dist1 in comparison_list) {
      for (dist2 in comparison_list) {
        if (dist1 != dist2) {
          dist1_state <- get(paste0(dist1, "_hosp_burden_estimates")) %>% 
            filter(state == st)
          dist2_state <- get(paste0(dist2, "_hosp_burden_estimates")) %>% 
            filter(state == st)
          
          # Perform var.test and extract results
          var_test_result <- var.test(dist2_state$difference,
                                      dist1_state$difference)
          
          # Extract results from var.test
          F_value <- var_test_result$statistic
          p_value <- var_test_result$p.value
          ratio_of_variances <- var_test_result$estimate[1]
          CI <- var_test_result$conf.int
          
          # Store results in the data frame and round F_value and p_value to 4 decimals
          distribution_results <- rbind(distribution_results, data.frame(
            state = st,
            null_dist = dist1,
            alternate_dist = dist2,
            null_var = var(dist1_state$difference),
            alternate_var = var(dist2_state$difference),
            F_value = round(F_value, 4),
            p_value = round(p_value, 4),
            ratio_of_variances = round(ratio_of_variances, 2),
            CI_lower = round(CI[1], 2),
            CI_upper = round(CI[2], 2),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  distribution_results_by_state <- distribution_results %>% 
    group_by(state) %>% 
    filter(null_var == min(null_var))
  
  assign("distribution_results_by_state", distribution_results_by_state, envir = .GlobalEnv)
}

compare_distributions_by_state(parent_data = poisson_hosp_burden_estimates, comparison_list = distribution_list)

addWorksheet(wb, sheetName = "LOS Discrete Dist by State")
writeData(wb, sheet = "LOS Discrete Dist by State", distribution_results_by_state, startCol = 1, startRow = 1)

# compare optimization values (sum of squares vs absolute difference) ---------------------------------

compare_optimization_values <- function(comparison_list = distribution_list) {
  # Create an empty df to store the results
  results <- data.frame(
    null_dist = character(),
    alternate_dist = character(),
    F_value = numeric(),
    p_value = numeric(),
    null_var = numeric(),
    alternate_var = numeric(),
    ratio_of_variances = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform var.test for each pair of distribution types
  for (dist in comparison_list) {
        # Perform var.test and extract results
        var_test_result <- var.test(get(paste0(dist, "_sumofsquares_hosp_burden_estimates"))$difference,
                                    get(paste0(dist, "_hosp_burden_estimates"))$difference)
        
        # Extract results from var.test
        F_value <- var_test_result$statistic
        p_value <- var_test_result$p.value
        ratio_of_variances <- var_test_result$estimate[1]
        CI <- var_test_result$conf.int
        
        # Store results in the data frame and round F_value and p_value to 4 decimals
        results <- rbind(results, data.frame(
          null_dist = dist,
          alternate_dist = paste0(dist, " sum of squares"),
          null_var = var(get(paste0(dist, "_hosp_burden_estimates"))$difference),
          alternate_var = var(get(paste0(dist, "_sumofsquares_hosp_burden_estimates"))$difference),
          F_value = round(F_value, 4),
          p_value = round(p_value, 4),
          ratio_of_variances = round(ratio_of_variances, 2),
          CI_lower = round(CI[1], 2),
          CI_upper = round(CI[2], 2),
          stringsAsFactors = FALSE
        ))
      }
  
  assign("optimization_value_results", results, envir = .GlobalEnv)
}

compare_optimization_values(comparison_list = distribution_list)

addWorksheet(wb, sheetName = "LOS Opt value by Dist Overall")
writeData(wb, sheet = "LOS Opt value by Dist Overall", optimization_value_results, startCol = 1, startRow = 1)

compare_optimization_values_noUSA <- function(comparison_list = distribution_list) {
  # Create an empty df to store the results
  results <- data.frame(
    null_dist = character(),
    alternate_dist = character(),
    F_value = numeric(),
    p_value = numeric(),
    null_var = numeric(),
    alternate_var = numeric(),
    ratio_of_variances = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform var.test for each pair of distribution types
  for (dist in comparison_list) {
    dist_sum_of_squares <- get(paste0(dist, "_sumofsquares_hosp_burden_estimates")) %>% 
      filter(state != "USA")
    dist_abs_dif <- get(paste0(dist, "_hosp_burden_estimates")) %>% 
      filter(state != "USA")
    # Perform var.test and extract results
    var_test_result <- var.test(dist_sum_of_squares$difference,
                                dist_abs_dif$difference)
    
    # Extract results from var.test
    F_value <- var_test_result$statistic
    p_value <- var_test_result$p.value
    ratio_of_variances <- var_test_result$estimate[1]
    CI <- var_test_result$conf.int
    
    # Store results in the data frame and round F_value and p_value to 4 decimals
    results <- rbind(results, data.frame(
      null_dist = dist,
      alternate_dist = paste0(dist, " sum of squares"),
      null_var = var(dist_abs_dif$difference),
      alternate_var = var(dist_sum_of_squares$difference),
      F_value = round(F_value, 4),
      p_value = round(p_value, 4),
      ratio_of_variances = round(ratio_of_variances, 2),
      CI_lower = round(CI[1], 2),
      CI_upper = round(CI[2], 2),
      stringsAsFactors = FALSE
    ))
  }
  
  assign("optimization_value_results_noUSA", results, envir = .GlobalEnv)
}

compare_optimization_values_noUSA(comparison_list = distribution_list)

addWorksheet(wb, sheetName = "LOS Opt value by Dist (No USA)")
writeData(wb, sheet = "LOS Opt value by Dist (No USA)", optimization_value_results_noUSA, startCol = 1, startRow = 1)

# compare 3m LOS hosp burden estimates to original ---------------------------------

  results_3m <- data.frame(
    null_dist = character(),
    alternate_dist = character(),
    F_value = numeric(),
    p_value = numeric(),
    null_var = numeric(),
    alternate_var = numeric(),
    ratio_of_variances = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  estimates <- poisson_hosp_burden_estimates %>% 
      filter(state != "USA")
  estimates_3m_LOS <- three_month_LOS_hosp_burden_estimates %>% 
      filter(state != "USA")
  # Perform var.test and extract results
  var_test_result <- var.test(estimates_3m_LOS$difference,
                                estimates$difference)
    
    # Extract results from var.test
    F_value <- var_test_result$statistic
    p_value <- var_test_result$p.value
    ratio_of_variances <- var_test_result$estimate[1]
    CI <- var_test_result$conf.int
    
    # Store results in the data frame and round F_value and p_value to 4 decimals
    results_3m <- rbind(results_3m, data.frame(
      null_dist = "poisson estimates",
      alternate_dist = "poisson 3m LOS estimates",
      null_var = var(estimates$difference),
      alternate_var = var(estimates_3m_LOS$difference),
      null_abs_dif = sum(estimates$absolute_difference),
      alternate_abs_dif = sum(estimates_3m_LOS$absolute_difference),
      F_value = round(F_value, 4),
      p_value = round(p_value, 4),
      ratio_of_variances = round(ratio_of_variances, 2),
      CI_lower = round(CI[1], 2),
      CI_upper = round(CI[2], 2),
      stringsAsFactors = FALSE
    ))

    hist(estimates_3m_LOS$difference, 
         breaks = 1, 
         col = "lightblue", 
         xlab = "Difference", 
         main = "Histogram of estimates_3m_LOS$difference")
    
    qqnorm(estimates_3m_LOS$difference)
    qqline(estimates_3m_LOS$difference, col = 2)
# look at LOS distributions for 3m estimates ---------------------------------
    
three_month_LOS_distributions <- arrow::read_parquet("data/US_wide_data/LOS_EstimatesbyStatebyDist/LOS_Optimized_by_AllStates_USA_3m.parquet")
three_month_LOS_distributions_summary <- three_month_LOS_distributions %>% 
  group_by(state) %>% 
  summarise(min_LOS = min(optimized_los),
    mean_LOS = mean(optimized_los), 
    max_LOS = max(optimized_los))

three_month_LOS_distributions %>% 
  ggplot(aes(x = interval, y = optimized_los, group = state, color = state)) +
  geom_line() 

three_month_LOS_distributions_interval_summary <- three_month_LOS_distributions %>% group_by(interval) %>% 
  mutate(min = min(optimized_los),
             Q1 = quantile(optimized_los, 0.25),
             mean = mean(optimized_los),
             Q3 = quantile(optimized_los, 0.75),
             max = max(optimized_los)) 
  
three_month_LOS_distributions_interval_summary %>% ggplot(aes(x = interval)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "lightblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "blue", alpha = 0.3) +
  geom_line(aes(y = Q1, color = "IQR"), alpha = 0.3, size = 1.2) +
  geom_line(aes(y = Q3, color = "IQR"), alpha = 0.3, size = 1.2) +
  geom_line(aes(y = min, color = "Min/Max Range"), size = 1.2) +
  geom_line(aes(y = mean, color = "Mean LOS"), linetype = "dashed", size = 1.2) +
  geom_line(aes(y = max, color = "Min/Max Range"), size = 1.2) +
  scale_color_manual(values = c("IQR" = "blue", "Min/Max Range" = "lightblue", "Mean LOS" = "blue")) +
  labs(x = "3 month Interval", y = "Optimized LOS", title = "Optimized LOS Distribution over 3 month Intervals") +
  theme_minimal()

# save excel wb to google drive folder ---------------------------------
saveWorkbook(wb, file = "/Users/sarahcotton/Library/CloudStorage/GoogleDrive-sarahcotton180@gmail.com/My Drive/COVID-19 Hosp Burden Project/HospBurden_Comparison_Results.xlsx", overwrite = TRUE)
