# PART 1 --------------------------------------------------------------------------------
# note: this is code from a source file to write functions used in part 2 
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
  rpois(n = n, lambda = los) 
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

# PART 2 --------------------------------------------------------------------------------
# this is the main script that uses the functions from the source file to estimate COVID-19 hospital burden using incident data 
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

# source data functions
source("source/data_setup_source_bestfit.R")

opt <- list()
opt$gt_data_path <- "data/US_wide_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_All_States_06-07-2024.parquet"

# read in data, define total_hosp, incidH_prior_day
covid_HHS_data_states <- arrow::read_parquet(opt$gt_data_path) %>% 
  mutate(total_hosp = total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid,
         incidH_prior_day = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as_date(date)) %>% 
  arrange(state, date) %>% 
  dplyr::select(state, date, total_hosp, incidH_prior_day)

# add labels to new variables 
attr(covid_HHS_data_states[["total_hosp"]], "label") <- "total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid"
attr(covid_HHS_data_states[["incidH_prior_day"]], "label") <- "previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed"

# SETUP FILES FOR EACH STATE TO ESTIMATE HOSPITAL BURDEN -----------------------------------

# Lag incident hospitalizations 
## incidH_prior_day values are moved back one day and add to new column incidH
covid_HHS_data_states_lag <- create_incidH_lag(covid_HHS_data_states) 

# add labels 
attr(covid_HHS_data_states_lag[["incidH"]], "label") <- "lead(previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed)"
attr(covid_HHS_data_states_lag[["total_hosp"]], "label") <- "total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid"
attr(covid_HHS_data_states_lag[["incidH_prior_day"]], "label") <- "previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed"

# create dataset for US across time
## add all the incident/totalHosp on a given day across all states and territories
covid_HHS_data_USA_lag <- covid_HHS_data_states_lag %>% 
  group_by(date) %>% 
  summarise(total_hosp = sum(total_hosp),
            incidH_prior_day = sum(incidH_prior_day),
            incidH = sum(incidH)) %>% 
  mutate(state = "USA")

# Stack datasets
## add rows of covid_HHS_data_USA_lag df to covid_HHS_data_states_lag

covid_HHS_data_states_lag <- bind_rows(covid_HHS_data_states_lag, covid_HHS_data_USA_lag)

# Create dataframes for each state with Hospital burden data 
create_totalH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-incidH, -incidH_prior_day), state) 

# Create dataframes for each state with incident hospitalization data
create_incidH_df(data = covid_HHS_data_states_lag %>% dplyr::select(-total_hosp, -incidH_prior_day), state)


# Check incidH and totalHosp master file and USA dataframes --------------------------------------------------------------

covid_HHS_data_states_lag %>%
  ggplot(aes(x = date, y = incidH, color = state)) +
  geom_line()

covid_HHS_data_states_lag %>%
  ggplot(aes(x = date, y = total_hosp, color = state)) +
  geom_line()

covid_incidH_data_USA %>%
  ggplot(aes(x = date, y = incidH, color = state)) +
  geom_line()

covid_totalHosp_data_USA %>%
  ggplot(aes(x = date, y = total_hosp, color = state)) +
  geom_line()


# Estimate LOS value for each state using optimization with varying distributions--------------

for(dist_type in distribution_list){
  print(dist_type)
  
  select_distribution_type(dist = dist_type)
  
  # this take a long time to run, prints states in alphabetical in console to check progress
  create_optimization(parent_data = covid_HHS_data_states_lag, optimize_los) # note: parent data just for getting list of all states

  # update only when want to overwrite file
  write_parquet(los_opt_by_state, paste0("data/US_wide_data/LOS_EstimatesbyStatebyDist/LOS_Optimized_by_AllStates_USA_", dist_type, ".parquet"))
  write_csv(los_opt_by_state, paste0("data/US_wide_data/LOS_EstimatesbyStatebyDist/LOS_Optimized_by_AllStates_USA_", dist_type, ".csv"))
  
  # Create hospitalization burden estimates using LOS values from optimization ---------
  
  # runs faster than above function (fewer outputs)
  covid_joined_totalHosp_state_data <- create_optimize_totalHosp_data(parent_data = covid_HHS_data_states_lag, los_opt_by_state = los_opt_by_state)
  
  # Write Final files for analysis ----------------
  covid_HHS_data <- arrow::read_parquet(opt$gt_data_path) %>%
    select(state, date, inpatient_beds, `previous_day_admission_adult_covid_confirmed_18-19`, `previous_day_admission_adult_covid_confirmed_20-29`,
           `previous_day_admission_adult_covid_confirmed_30-39`, `previous_day_admission_adult_covid_confirmed_40-49`, `previous_day_admission_adult_covid_confirmed_50-59`,
           `previous_day_admission_adult_covid_confirmed_60-69`, `previous_day_admission_adult_covid_confirmed_70-79`, `previous_day_admission_adult_covid_confirmed_80+`, `previous_day_admission_adult_covid_confirmed_unknown`,
           `previous_day_admission_pediatric_covid_confirmed_0_4`, `previous_day_admission_pediatric_covid_confirmed_12_17`, `previous_day_admission_pediatric_covid_confirmed_5_11`,
           `previous_day_admission_pediatric_covid_confirmed_unknown`, `previous_day_admission_pediatric_covid_confirmed`, `previous_day_admission_adult_covid_confirmed`)
  
  covid_joined_totalHosp_state_data_los <- inner_join(covid_joined_totalHosp_state_data, los_opt_by_state, by = "state")
  
  covid_joined_totalHosp_state_data_los_demographic <- left_join(covid_joined_totalHosp_state_data_los, covid_HHS_data, c("state", "date"))
  write_parquet(covid_joined_totalHosp_state_data_los_demographic, paste0("data/US_wide_data/estimated_hospitalizations_data/Obs_Exp_totalHosp_daily_", dist_type, ".parquet"))
  
}

# PART 3 --------------------------------------------------------------------------------
# notes: this is an example of an interactive shiny app that visualizes the total observed and estimated hospitalizations over time by state
# interactive options: state selection, time range, plotting observed or estimated hospitalizations (or both), and download plot as PNG
# Example interactive shiny app code ----------------
# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)  

# UI section
ui <- fluidPage(
  titlePanel("Total Observed vs. Estimated Hospitalizations by State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_states", "Select State(s):", 
                  choices = NULL,  
                  multiple = TRUE,  
                  selectize = TRUE, 
                  selected = NULL),
      
      dateRangeInput("date_range", "Select Date Range:",
                     start = NULL, 
                     end = NULL,
                     format = "yyyy-mm-dd"),
      
      # select which lines to show on plot 
      checkboxInput("show_observed", "Show Observed Hospitalizations", TRUE),
      checkboxInput("show_estimated", "Show Estimated Hospitalizations", TRUE),
      
      # add button to download plot 
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    mainPanel(
      plotOutput("hospital_plot")
    )
  )
)

# Server section
server <- function(input, output, session) {
  
  # Load dataset
  covid_totalHosp_data <- arrow::read_parquet("data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_04142024.parquet")  
  # update input to selected states 
  observe({
    states <- unique(cdc_covid_totalHosp_data$state)
    updateSelectInput(session, "selected_states", choices = states, selected = states)
  })
  
  # default date range 
  observe({
    min_date <- min(ymd(cdc_covid_totalHosp_data$date))
    max_date <- max(ymd(cdc_covid_totalHosp_data$date))
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
  })
  
  filtered_data <- reactive({
    data <- cdc_covid_totalHosp_data
    
    if (!is.null(input$selected_states) && length(input$selected_states) > 0) {
      data <- data %>% filter(state %in% input$selected_states)
    }
    
    if (!is.null(input$date_range)) {
      data <- data %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    }
    
    return(data)
  })
  
  # create plot 
  output$hospital_plot <- renderPlot({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = as.Date(date), y = total_hosp_estimate)) + 
      facet_wrap(~state, ncol = 1, scales = "free_y")  +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
      labs(x = "Date", 
           y = "Hospitalizations (Observed & Estimated)", 
           color = "Difference (Observed - Expected)") +
      ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
      labs(subtitle = "All Time CDC Mean LoS Estimate: 3.9")
    
    # Conditionally add lines based on user input
    if (input$show_observed) {
      p <- p + geom_line(aes(y = total_hosp, linetype = "Observed"))
    }
    if (input$show_estimated) {
      p <- p + geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference))
    }
    
    # Customize linetype and labels
    p <- p + scale_linetype_manual(name = "Line Type", 
                                   values = c("solid", "dashed"), 
                                   labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
    
    return(p)
  })
  
  # Download plot as PNG
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("hospitalizations_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = output$hospital_plot(), device = "png", width = 10, height = 8)
    }
  )
}

# run shiny 
shinyApp(ui = ui, server = server)
