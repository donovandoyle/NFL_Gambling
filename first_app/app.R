#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(readxl)
library(janitor)
library(tidytext)
library(lubridate)
library(fs)
library(formattable)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NFL Gambling Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     spreads <- read.csv("spreadspoke_scores.csv")
     
      spreads %>%
       filter(!is.na(schedule_week)) %>%
       mutate(week = recode_factor(schedule_week, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10, "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15, "16" = 16, "17" = 17, "18" = 18)) %>%
       filter(!is.na(schedule_week)) %>%
       filter(!is.na(over_under_line)) %>%
       mutate(over_under_line = parse_number(over_under_line)) %>%
       group_by(week, over_under_line) %>%
       summarize(avgline = sum(over_under_line) / 825) %>%
       ggplot(aes(x = week, y = avgline)) + 
        geom_bar(stat = "identity") + 
       labs(title = "Average Over/Under Lines by Week", x = "Week", y = "O/U set")})
}

# Run the application 
shinyApp(ui = ui, server = server)

