library(tidyverse)

x <- read_rds("spreads.rds")

ui <- fluidPage(
  tabsetPanel(
  tabPanel("Choices", fluid = TRUE,
           
  
  # Note that I am using the simplest possible format. We would normally
  # structure this layout with a sidebar, as in the default Shiny example.
  
  # I looked at the data to select this min and max. Probably better to choose
  # this on the fly, perhaps with some padding/rounding to make it look nice.
  
  sliderInput(inputId = "number", label = "Over/Under",
              min = 25, max = 70,
              value = c(50, 55)),
  
  tableOutput("table")
)

server <- function(input, output) {
  
  
  
  output$table <- renderTable({
    
    # Always remember that, if you are going to use information from the input
    # object, you need to do so within a reactive function like render*. 
    
    x %>% 
      filter(over_under_line >= input$number[1], over_under_line <= input$number[2]) %>%
      mutate(combined = score_home + score_away) %>%
      select(schedule_date, team_home, team_away, over_under_line, combined) %>% 
      mutate(schedule_date = as.character(schedule_date)) %>% 
      arrange(over_under_line) %>%
      rename(Date = schedule_date, Home = team_home, 
             Visitor = team_away, `Over/Under` = over_under_line, 'True Combined Total' = combined)
    
  })
}

shinyApp(ui, server)