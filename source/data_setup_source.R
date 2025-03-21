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
  #rpois(n = n, lambda = los) 
  rnbinom(n = n, size = los, prob = 0.5)
}

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

# update functions for timevarying forecasts -----------------------------------

get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  }
}

create_incidH_df_year_szn <- function(data) {
  states_list <- unique(data$state)
  year_szn_list <- unique(data$year_szn)
  
  for (state in states_list) {
    for (year_szn in year_szn_list) {
      
      # Filter data for the current state and year_szn
      state_year_szn_data <- data[data$state == state & 
                                    data$year_szn == year_szn, ]
      
      # Ensure there's data to process
      if (nrow(state_year_szn_data) > 0) {
        # Assign the filtered data to a dynamically created variable name
        assign(
          paste0("covid_incidH_data_", state, "_", year_szn),
          state_year_szn_data,
          envir = .GlobalEnv
        )
      }
    }
  }
}

create_totalH_df_year_szn <- function(data) {
  states_list <- unique(data$state)
  year_szn_list <- unique(data$year_szn)
  
  for (state in states_list) {
    for (year_szn in year_szn_list) {
      
      # Filter data for the current state and year_szn
      state_year_szn_data <- data[data$state == state & 
                                    data$year_szn == year_szn, ]
      
      # Ensure there's data to process
      if (nrow(state_year_szn_data) > 0) {
        # Assign the filtered data to a dynamically created variable name
        assign(
          paste0("covid_totalHosp_data_", state, "_", year_szn),
          state_year_szn_data,
          envir = .GlobalEnv
        )
      }
    }
  }
}

create_optimization_timevarying <- function(parent_data, optimize_los) {
  states_list <- unique(parent_data$state)
  los_opt_by_state_season <- list()
  
  for (state in states_list) {
    print(state) # for tracking progress
    
    # Filter data for the current state
    state_data <- parent_data %>% filter(state == state)
    year_szn_list <- unique(state_data$year_szn)
    
    for (year_szn in year_szn_list) {
      # Filter data for the current state and year_szn
      filtered_data <- state_data %>% filter(year_szn == year_szn)
      
      # Ensure there's data to process
      if (nrow(filtered_data) > 0) {
        # Construct dynamic variable names
        dynamic_incidH_name <- paste0("covid_incidH_data_", state, "_", year_szn)
        dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state, "_", year_szn)
        
        # Check if the dynamic objects exist before proceeding
        if (exists(dynamic_incidH_name, envir = .GlobalEnv) && exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
          data <- get(dynamic_incidH_name)
          observed <- get(dynamic_totalHosp_name)
          
          # Perform optimization
          los_range <- c(3, 15)
          los_min <- optimize(optimize_los, los_range, 
                              data = data, 
                              observed = observed, 
                              maximum = FALSE)
          
          # Create a results dataframe
          state_season_df <- data.frame(
            state = state, 
            year_szn = year_szn,
            optimized_los = los_min$minimum, 
            objective = los_min$objective
          )
          los_opt_by_state_season[[paste(state, year_szn, sep = "_")]] <- state_season_df
        } else {
          print(paste("Dynamic objects not found for:", dynamic_incidH_name, "or", dynamic_totalHosp_name))
        }
      }
    }
  }
  
  # Combine list of dataframes into a single dataframe
  los_opt_by_state_season <- do.call(rbind, los_opt_by_state_season)
  
  # Save the result to the global environment
  assign("los_opt_by_state_season", los_opt_by_state_season, envir = .GlobalEnv)
  
  print("Optimization completed!")
}

create_curr_hosp_forecast <- function(data_burden){
  #data_burden <- expected_list %>% bind_rows()
  new_data_burden <- data_burden %>%
    bind_rows() %>%
    as_tibble() %>%
    dplyr::select(-admit_date, -incidH) %>% 
    #group_by(across(-c(horizon, target_end_date))) %>%
    group_by(hosp_dates) %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()
  return(new_data_burden)
}

create_hosp_dates_timevarying <- function(data, los_vector) {
  if (length(los_vector) != nrow(data)) {
    stop("The length of los_vector must match the number of rows in data.")
  }
  #data <- sim_and_reported_data_join 
  #los_vector <- los_state_list
  data_burden <- list()
  
  #i <- 2
  for (i in 1:nrow(data)) {
    data_burden[[i]] <- data[i, ] %>%
      rename(admit_date = date) %>%
      expand_grid(hosp_dates =
                    burden_est_funct(
                      incidH = data$incidH[i],
                      date = data$date[i],
                      hospstayfunct = covidhosp_stay_funct,
                      los = los_vector[i] # Use the LOS value for this date
                    )
      )
  }
  
  return(data_burden)
}


create_optimize_totalHosp_data_timevarying <- function(parent_data, los_opt_by_state_season) {
  states_list <- unique(parent_data$state)
  combined_list <- list()
  
  for (state_abbv in states_list) {
    print(state_abbv) # Progress tracking
    #state_data <- covid_HHS_data_states_lagtemp %>% filter(state == "MD")
    state_data <- parent_data %>% filter(state == state_abbv)
    #year_szn_list <- unique(state_data$year_szn)
    
    los_state_list <- state_data$optimized_los
    if (length(los_state_list) == nrow(state_data)) { # Ensure LOS values match data rows
      # Generate expected hospitalization data
      expected_list <- create_hosp_dates_timevarying(state_data, los_vector = los_state_list)
      #expecteddf <- bind_rows(expected_list)
      expected <- create_curr_hosp_forecast(data_burden = expected_list)
      #expected <- create_curr_hosp_forecast_quantiles(data_burden = expected_list)
      expected <- clean_expected(expected)
      #expected <- clean_expected_forecast(expected)
      
      # Fetch observed data dynamically
      dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
      
      if (exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
        observed <- get(dynamic_totalHosp_name)
        
        # Combine observed and expected data
        combined <- inner_join(observed, expected, by = "date") %>% 
          dplyr::select(state, date, total_hosp, total_hosp_estimate)%>% 
          mutate(
            absolute_difference = abs(total_hosp - total_hosp_estimate),
            difference = total_hosp - total_hosp_estimate,
            relative_difference = total_hosp_estimate / total_hosp
            #optimized_los_value = los_state_list # Add LOS values to the dataframe
          )
        
        combined_list[[state_abbv]] <- combined
      } else {
        print(paste("Data not found for:", dynamic_totalHosp_name))
      }
    } else {
      print(paste("Mismatch in LOS values for state:", state_abbv))
    }
  }
  
  # Combine the results into a single dataframe
  combined_df <- do.call(rbind, combined_list)
  return(combined_df)
}

# source functions for FORECASTS ------

create_curr_hosp_forecast_quantiles <- function(data_burden){
  #data_burden <- expected_list %>% bind_rows()
  new_data_burden <- data_burden %>%
    bind_rows() %>%
    as_tibble() %>%
    dplyr::select(-admit_date, -incidH) %>% 
    #group_by(across(-c(horizon, target_end_date))) %>%
    group_by(hosp_dates, quantile) %>%
    summarise(curr_hosp = length(hosp_dates)) %>%
    ungroup()
  return(new_data_burden)
}
clean_expected_forecast <- function(expected){
  expected <- expected %>%
    rename(total_hosp_estimate = curr_hosp,
           date = hosp_dates) %>%
    select(date, total_hosp_estimate, quantile)
  
  return(expected)
}
create_optimize_totalHosp_data_timevarying_forecast <- function(parent_data) {
  #parent_data <- forecast_hosp_23_24
  states_list <- unique(parent_data$abbreviation)
  combined_list <- list()
  
  #state_abbv <- states_list[1]
  
  for (state_abbv in states_list) {
    print(state_abbv) # Progress tracking
    # Filter data for the current state
    state_data <- parent_data %>% filter(abbreviation == state_abbv)
    
    # Retrieve LOS values for the state
    los_state_list <- state_data$optimized_los
    
    if (length(los_state_list) == nrow(state_data)) { # Ensure LOS values match data rows
      # Generate expected hospitalization data
      expected_list <- create_hosp_dates_timevarying(state_data, los_vector = los_state_list)
      #expecteddf <- bind_rows(expected_list)
      #expected <- create_curr_hosp_forecast(data_burden = expected_list)
      expected <- create_curr_hosp_forecast_quantiles(data_burden = expected_list)
      #expected <- clean_expected(expected)
      expected <- clean_expected_forecast(expected)
      
      # Fetch observed data dynamically
      dynamic_totalHosp_name <- paste0("covid_totalHosp_data_", state_abbv)
      
      if (exists(dynamic_totalHosp_name, envir = .GlobalEnv)) {
        observed <- get(dynamic_totalHosp_name)
        
        # Combine observed and expected data
        combined <- inner_join(observed, expected, by = "date") %>%
          dplyr::select(state, date, total_hosp, total_hosp_estimate, quantile)%>%
          mutate(
            absolute_difference = abs(total_hosp - total_hosp_estimate),
            difference = total_hosp - total_hosp_estimate,
            relative_difference = total_hosp_estimate / total_hosp
            #optimized_los_value = los_state_list # Add LOS values to the dataframe
          )
        
        combined_list[[state_abbv]] <- combined
      } else {
        print(paste("Data not found for:", dynamic_totalHosp_name))
      }
    } else {
      print(paste("Mismatch in LOS values for state:", state_abbv))
    }
  }
  
  # Combine the results into a single dataframe
  combined_df <- do.call(rbind, combined_list)
  return(combined_df)
}
