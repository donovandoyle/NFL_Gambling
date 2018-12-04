# GRAPH FROM MEAGHAN
library(shiny)
library(ggplot2)
library(tidyverse)




x <- read_rds("spreads.rds") %>%
  filter(!is.na(over_under_line))

y <- read_rds("spreads_when_under.rds")
z <- read_rds("spreads_team_summary.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

  
  # Application title
  titlePanel("How to Make a Lot of Money"),
  
  # Show a plot of the generated distribution
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Over/Under Lines and Temperature", plotOutput("one")),
                tabPanel("$10 Bets: When They're the Underdog", tableOutput("two")),
                tabPanel("$10 Bets: When They're the Favorite", tableOutput("three")),
                tabPanel("Summary", htmlOutput("about")))))
    
    # Note that I am using the simplest possible format. We would normally
    # structure this layout with a sidebar, as in the default Shiny example.
    
    # I looked at the data to select this min and max. Probably better to choose
    # this on the fly, perhaps with some padding/rounding to make it look nice.

  
    


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$one <- renderPlot({
    
    x %>%
      mutate(combined = score_home + score_away) %>%
        ggplot(aes(x = weather_temperature, y = over_under_line)) + 
      geom_point() +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = "Expected vs Actual Scoring",
           subtitle = "In relation to temperature",
           x = "Temperature", 
           y = "Over/Under Line") + geom_smooth(aes(y = combined, color = "red")) + guides(color = "none")
  })
  
  output$two <- renderTable({
    y
  }
    
  )
  
  output$three <- renderTable({
      
      # Always remember that, if you are going to use information from the input
      # object, you need to do so within a reactive function like render*. 
      
      z
      
    })}
    
    
    
    




# Run the application 
shinyApp(ui = ui, server = server)
