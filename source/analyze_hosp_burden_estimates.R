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
for(dist_type in distribution_list){
  
  opt_name <- paste0("gt_data_path_", dist_type, "_sumofsquares")
  opt[[opt_name]] <- paste0("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_", dist_type, "sum of squares", ".parquet")
  
  assign(paste0(dist_type, "_sumofsquares_hosp_burden_estimates"), read_parquet(opt[[opt_name]]), envir = .GlobalEnv)
  
}

wb <- createWorkbook() # create excel wb to add results to
# sumofsquares_hosp_burden_estimates

#### Compare distributions ---------------------------------
# notes: use F test (variance test) to see which distribution minimizes the error (produces the most accurate estimates) across all states

# for(dist_type in distribution_list){
#   df <- paste0(dist_type, "_hosp_burden_estimates")
# }


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
results_filter <- distribution_results %>% 
  filter(alternate_var > null_var) 

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

addWorksheet(wb, sheetName = "LOS Discete Dist by State")
writeData(wb, sheet = "LOS Discete Dist by State", distribution_results_by_state, startCol = 1, startRow = 1)

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
#saveWorkbook(wb, file = " data/US_wide_data/HospBurden_Comparison_Results.xlsx", overwrite = TRUE)
