# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI section
ui <- fluidPage(
  titlePanel("Total Observed vs. Estimated Hospitalizations by State"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This app visualizes the total observed and estimated hospitalizations over time by state.")
    ),
    
    mainPanel(
      plotOutput("hospital_plot")
    )
  )
)

# Server section
server <- function(input, output) {
  covid_totalHosp_data <- arrow::read_parquet("data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_04142024.parquet")
  # Render the plot
  output$hospital_plot <- renderPlot({
    covid_totalHosp_data %>%  
      ggplot(aes(x = date, y = total_hosp_estimate)) + 
      geom_line(aes(y = total_hosp, linetype = "Observed")) +
      geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
      labs(x = "Date", 
           y = "Hospitalizations (Observed & Estimated)", 
           color = "Difference (Observed - Expected)") + 
      facet_wrap(~state, ncol = 1, scales = "free_y")  +
      ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_color_gradient2(low = "red", 
                            mid = "forestgreen", 
                            high = "red", 
                            midpoint = 0) +
      labs(color = "Difference (Observed - Expected)", 
           title = "Hospitalization Burden Estimates Time Series",
           subtitle = "All Time CDC Mean LoS Estimate: 3.9") +
      scale_linetype_manual(name = "Line Type", 
                            values = c("solid", "dashed"), 
                            labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
