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
