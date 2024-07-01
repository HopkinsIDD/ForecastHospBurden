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

### IMPORT INITIAL DATA -----------------------------------
source("source/data_setup_source_bestfit.R")
source("source/data_setup_source.R")

opt <- list()
for(dist_type in distribution_list){
  
  opt_name <- paste0("gt_data_path_", dist_type)
  opt[[opt_name]] <- paste0("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_", dist_type, ".parquet")
  
  assign(paste0(dist_type, "_hosp_burden_estimates"), read_parquet(opt[[opt_name]]), envir = .GlobalEnv)

}

#### Compare distributions ---------------------------------
# notes: use F test (variance test) to see which distribution minimizes the error (produces the most accurate estimates) across all states

for(dist_type in distribution_list){
  df <- paste0(dist_type, "_hosp_burden_estimates")
}

# 
# #create empty df 
# results <- data.frame(
#   null_dist = character(),
#   alternate_dist = character(),
#   null_var = numeric(),
#   alternate_var = numeric(),
#   F_value = numeric(),
#   p_value = numeric(),
#   ratio_of_variances = numeric(),
#   CI_lower = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# # Perform var.test for each unique combination of distribution types
# for (dist1 in distribution_list) {
#   for (dist2 in distribution_list) {
#     if (dist1 != dist2) {
#         # Perform var.test and extract results
#         var_test_result <- var.test(get(paste0(dist2, "_hosp_burden_estimates"))$difference,
#                                     get(paste0(dist1, "_hosp_burden_estimates"))$difference)
#         
#         # Store results in the data frame
#         results <- rbind(results, data.frame(
#           null_dist = dist1,
#           alternate_dist = dist2,
#           F_value = round(var_test_result$statistic, 4),
#           p_value = round(var_test_result$p.value, 4),
#           null_var = var(get(paste0(dist1, "_hosp_burden_estimates"))$difference),
#           alternate_var = var(get(paste0(dist2, "_hosp_burden_estimates"))$difference),
#           null_var2 = var_test_result$estimate[1],
#           alternate_var2 = var_test_result$estimate[2],
#           ratio_of_variances = var_test_result$estimate[2] / var_test_result$estimate[1],
#           CI = var_test_result$conf.int,
#           stringsAsFactors = FALSE
#         ))
#       }
#     }
# }
# 
# var.test(normal_hosp_burden_estimates$difference, poisson_hosp_burden_estimates$difference)

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
for (dist1 in distribution_list) {
  for (dist2 in distribution_list) {
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
      results <- rbind(results, data.frame(
        null_dist = dist1,
        alternate_dist = dist2,
        F_value = round(F_value, 4),
        p_value = round(p_value, 4),
        null_var = var(get(paste0(dist1, "_hosp_burden_estimates"))$difference),
        alternate_var = var(get(paste0(dist2, "_hosp_burden_estimates"))$difference),
        ratio_of_variances = round(ratio_of_variances, 4),
        CI_lower = round(CI[1], 4),
        CI_upper = round(CI[2], 4),
        stringsAsFactors = FALSE
      ))
    }
  }
}

results_filter <- results %>% 
  filter(F_value > 1.0)
