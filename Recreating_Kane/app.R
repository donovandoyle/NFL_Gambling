# GRAPH FROM MEAGHAN
library(shiny)
library(ggplot2)
library(tidyverse)



data <- spreads %>%
  select(weather_temperature, over_under_line) %>%
  filter(!is.na(over_under_line))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Recreating Mr. Doyle's Final Project"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    ggplot(data, aes(x = weather_temperature, y = over_under_line)) + 
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Higher Temperatures are Correlated with Higher (Expected) NFL Scoring",
           subtitle = "The effect is not very strong",
           x = "Temperature", 
           y = "Over/Under Line")
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
