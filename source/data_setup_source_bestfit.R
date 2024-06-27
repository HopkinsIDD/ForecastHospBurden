# Setup Functions ---------------------------------------------

# Create lag of one day for incidH for each state  --------
## notes: output single df with all states, lagged (lead()) by one day; incident data is reported for the day prior, so need to offset by one day

create_incidH_lag <- function(state_data){
  states_list <- unique(state_data$state)
  lagged_dfs <- list()
  
  for (state in states_list) {
    state_data_state <- state_data[state_data$state == state, ]
    state_data_state <- state_data_state %>% 
      mutate(incidH = lead(incidH_prior_day)) %>%  
      filter(date < max(date))  # remove most recent date to account for lag 
    
    
    lagged_dfs[[state]] <- state_data_state #store lags in a list 
  }
  
  merged_df <- do.call(bind_rows, lagged_dfs) #combine into one df 
  
  return(merged_df)
}

# Create dataframes for each state with Hospital burden data -----------------------------------
# output: dfs for each unique state w/ hospital burden (totalHosp) data (to be used during optimization)

create_totalH_df <- function(data, state){
  states_list <- unique(data$state)
  
  for (state in states_list) {
    
    state_data <- data[data$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_totalHosp_data_", state), state_data, envir = .GlobalEnv)
  }
}

# Create dataframes for each state with incident hospitalizations  ------------------------------------
# output: dfs for each unique state w/ incident admissions (incidH) data (to be used during optimization)
create_incidH_df <- function(data, state){
  states_list <- unique(data$state)
  
  for (state in states_list) {
    
    state_data <- data[data$state == state, ]
    
    # put new df in global env 
    assign(paste0("covid_incidH_data_", state), state_data, envir = .GlobalEnv)
  }
}

# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------

# create functions for sampling hospitalization duration 
covidhosp_stay_funct <- function(n, los = 5) {
  rnorm(n = n, mean = los) 
}

distribution_type <- function(dist){
  if(dist == "poisson"){
    covidhosp_stay_funct <<- function(n, los = 5) {
      rpois(n = n, lambda = los) 
    }
  }
  else if(dist == "normal"){
    covidhosp_stay_funct <<- function(n, los = 5) {
      rnorm(n = n, mean = los)
    }
  }
  else if(dist == "binomial"){
    covidhosp_stay_funct <<- function(n, los = 5) {
      data_nbinom <- rnbinom(n = n, size = los, prob = 0.5)
    }
  }
  return(covidhosp_stay_funct)
}
distribution_type(dist = "binomial")
  
burden_est_funct <- function(incidH, date, hospstayfunct = covidhosp_stay_funct, los = 5){
  lubridate::as_date(sort(unlist(sapply(X = hospstayfunct(n = incidH, los = los), function(x = X) (0:(x-1)) + date))))
}


# ~ Functions for Empirical data --------------------------------------
create_hosp_dates <- function(data, los = 5){
  data_burden <- list()
  
  for (i in 1:nrow(data)){
    
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates = 
                    burden_est_funct(incidH = data$incidH[i], 
                                     date = data$date[i], 
                                     hospstayfunct = covidhosp_stay_funct,
                                     los = los
                    )
      )
  }
  return(data_burden)
}

create_curr_hosp <- function(data_burden){
  new_data_burden <- data_burden %>%
    bind_rows() %>%
    as_tibble() %>%
    dplyr::select(-admit_date, -incidH) %>% 
    group_by_all() %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()
  return(new_data_burden)
}

# CLEAN DATA FOR MERGE  ----------------------------------- 
# notes: rename estimated hospitalizations columns, this could be removed by renaming above vars?? 
clean_expected <- function(expected){
  expected <- expected %>%
    rename(total_hosp_estimate = curr_hosp,
           date = hosp_dates) %>%
    select(date, total_hosp_estimate) 
  
  return(expected)
}

# Create function that returns LOS value to be optimized on ----------------------------
# notes: output returns absolute difference between observed (totalHosp) and expected (total_hosp_estimate) estimates of hosp burden
# fed into create_optimization function below 

optimize_los <- function(los, data, observed){
  
  expected_list <- create_hosp_dates(data, los = los)
  expected <- create_curr_hosp(data_burden = expected_list)
  
  expected <- clean_expected(expected)
  
  combined <- inner_join(observed, expected, by = "date") %>% 
    dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
    mutate(absolute_difference = abs(total_hosp - total_hosp_estimate)) %>% 
    filter(!is.na(absolute_difference)) %>% 
    summarise(sum_absolute_difference = sum(absolute_difference)) # mean or median instead here? 
  
  return(combined$sum_absolute_difference)
  
}

# Estimate LOS value for each state using optimization ------------------------------------
# notes: optimization function, searches for lowest absolute difference across range of 3-15 avg LOS  
# output df with state, estimated LOS, and error 

create_optimization <- function(parent_data, optimize_los){
  states_list <- unique(parent_data$state)
  los_opt_by_state <- list()
  
  for (state in states_list) {
    print(state) #for tracking progress
    
    data = get(paste0("covid_incidH_data_", state))
    observed = get(paste0("covid_totalHosp_data_", state))
    
    
    los_range <- c(3,15)
    # tol (accuracy)  is the default value (approx. 0.0001)
    los_min <- optimize(optimize_los, los_range, data = get(paste0("covid_incidH_data_", state)), 
                        observed = get(paste0("covid_totalHosp_data_", state)), 
                        maximum = FALSE)
    
    state_df <- data.frame(state = state, 
                           optimized_los = los_min$minimum, 
                           objective = los_min$objective)
    los_opt_by_state[[state]] <- state_df
    
  }
  
  los_opt_by_state <- do.call(rbind, los_opt_by_state)
  
  assign("los_opt_by_state", los_opt_by_state, envir = .GlobalEnv)
  
}

## Create master dataset containing optimized hosp burden for each state -----------------------------------
# notes
create_optimize_totalHosp_data <- function(parent_data, los_opt_by_state = los_opt_by_state){
  states_list <- unique(parent_data$state)
  combined_list <- list()
  
  for (state in states_list) {
    data = get(paste0("covid_incidH_data_", state)) # incident data used to estimate totalHosp with estimated LOS (optimization)
    observed = get(paste0("covid_totalHosp_data_", state)) # need to join observed vs. expected at end 
    
    expected_list <- create_hosp_dates(data, los = los_opt_by_state[los_opt_by_state$state == state, "optimized_los"])
    expected <- create_curr_hosp(data_burden = expected_list)
    
    expected <- clean_expected(expected)
    
    combined <- inner_join(observed, expected, by = "date") %>% 
      dplyr::select(state, date, total_hosp, total_hosp_estimate) %>% 
      mutate(absolute_difference = abs(total_hosp - total_hosp_estimate),
             difference = total_hosp - total_hosp_estimate,
             relative_difference = total_hosp_estimate/total_hosp)
    
    combined_list[[state]] <- combined
  }
  
  combined_df <- do.call(rbind, combined_list) # return df with estimates Hosp and observed hosp
  
  return(combined_df)
  
}
