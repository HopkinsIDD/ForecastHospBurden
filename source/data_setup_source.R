# Setup Functions ---------------------------------------------

# Create lag of one day for incidH for each state

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