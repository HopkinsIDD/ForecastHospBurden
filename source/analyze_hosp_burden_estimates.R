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
  
  # Calculate the variance of the 'difference' column in the data frame
  variance_col <- var(get(df)$difference)
  variance_abs <- var(get(df)$absolute_difference)
  
  # Assign the variance to a global variable
  assign(paste0(dist_type, "_variance"), variance_col, envir = .GlobalEnv)
  assign(paste0(dist_type, "_abs_variance"), variance_abs, envir = .GlobalEnv)
  
}


var(gamma_hosp_burden_estimates$difference, poisson_hosp_burden_estimates$difference)

var.test(gamma_hosp_burden_estimates$difference, poisson_hosp_burden_estimates$difference) 

#create empty df 
results <- data.frame(
  null_dist = character(),
  alternate_dist = character(),
  null_var = numeric(),
  alternate_var = numeric(),
  F_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Perform var.test for each unique combination of distribution types
for (i in 1:(length(distribution_list) - 1)) {
  for (j in (i + 1):length(distribution_list)) {
    dist1 <- distribution_list[i]
    dist2 <- distribution_list[j]
    
    # Perform var.test and extract results
    var_test_result <- var.test(get(paste0(dist1, "_hosp_burden_estimates"))$difference,
                                get(paste0(dist2, "_hosp_burden_estimates"))$difference)
    
    # Store results in the data frame
    results <- rbind(results, data.frame(
      null_dist = dist2,
      alternate_dist = dist1,
      F_value = var_test_result$statistic,
      p_value = var_test_result$p.value,
      null_var = var(get(paste0(dist2, "_hosp_burden_estimates"))$difference),
      alternate_var = var(get(paste0(dist1, "_hosp_burden_estimates"))$difference),
      stringsAsFactors = FALSE
    ))
  }
}


