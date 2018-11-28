# KABLE FROM CALABRO
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

spreads_table <- read_rds("../spreads.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Over/Under Lines"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("ou",
                  "Over/Under",
                  min = 25,
                  max = 70,
                  value = c(40,60))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- renderDataTable({
    spreads_table %>%
      select(schedule_date, team_home, team_away, over_under_line) %>%
      filter(over_under_line %in% input$ou)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


