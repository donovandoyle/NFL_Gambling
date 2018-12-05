# Here's nothing special, just loading in our libraries needed.
library(shiny)
library(ggplot2)
library(tidyverse)



#I use 3 rds files, one for each tab. I try to simplify my data as much as possible
#in order to keep my Shiny abbreviated
x <- read_rds("spreads.rds") %>%
  filter(!is.na(over_under_line))

y <- read_rds("spreads_when_under.rds")
z <- read_rds("spreads_team_summary.rds")

# Define UI for application
ui <- fluidPage(

  
  # Application title
  titlePanel("NFL Gambling Data: Beating the System"),
  
  # Tab setup
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Over/Under Lines and Temperature", plotOutput("one")),
                tabPanel("$10 Bets: When They're the Underdog", dataTableOutput("two")),
                tabPanel("$10 Bets: When They're the Favorite", dataTableOutput("three")),
                tabPanel("Summary", htmlOutput("about")))))
    
    # Note that I am using the simplest possible format. We would normally
    # structure this layout with a sidebar, as in the default Shiny example.
# ^This comment is from Preceptor, but I want to double down on keeping my data in the simplest
# possible format. The reality is, when looking at gambling data, the markets are already pretty efficient
# otherwise sportsbooks would be going out of business like crazy. Any type of vague correlation backed by data
# is already better than by how most people bet, which is usually guesswork/team affiliation.

  
# Define server logic required to create first plot
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
           y = "Over/Under Line", caption = "
The blue line represents the over/under line set by Vegas in a least-squared regression, 
           and the red line is a fluid representation of the true combined points total from those games.  
           If looking to convert this into gambling advice, the main point is that in games that are less than 50 degrees, 
           on average, the over will hit.") + 
      geom_smooth(aes(y = combined, color = "red")) + guides(color = "none")
  })
  
  output$two <- renderDataTable({
    y
      
  }
    
  )
  
  output$three <- renderDataTable({
      
      
      z
      
  })
  output$about <- renderUI({
    
    str1 <- paste("Graph Interpretation")
    str2 <- paste("The first graph shows that if you are looking for a pattern in over/under line betting,
                  keying in on the temperature of the game should be the deciding factor on if you place your bet.
                  I highly recommend betting the over, especially when the temperature is 50 degrees or less.")
    str3 <- paste("Table Interpretation")
    str4 <- paste("The tables show the expected value of your payout if you were to follow that specific rule
                  of betting from 1979 to 2017. For example, looking at the first table, if one were to bet $10 on
                  Arizona Cardinals every time they were the underdog, the expected value would be $9.82, or a 1.8% decrease.
                  If looking to this for betting advice, the logical thing to do would be to type '10' in the search function,
                  identify the betting rules that result in a positive payout, and follow those trends. For example, if a bettor
                  were to bet against the Eagles every time they were underdogs, the bettor would have made over 10% of their money
                 over time. These numbers even factor in the house cut, meaning an expected payout for a winning bet was $8.70 on top
                  of the wager, as most sportsbooks operate with a cut of this size.")
    str5 <- paste("Conclusion")
    str6 <- paste("My betting advice can be summarized as the following:
                  1) If the gametime temperature is listed as under 50 degrees, bet the over.
                  2) When the Rams are underdogs, bet on them. 
                  3) When the Panthers, Broncos, Chiefs, Chargers, Dolphins, Vikings, Patriots, Steelers,
                  and Redskins are underdogs, bet against them.
                  4) When the Packers are favorites, bet on them.
                  5) When the Lions, Dolphins, Jets, Raiders, Buccaneers, Titans, and Redskins are favorites,
                  bet against them.
                  By following these 5 rules since 1979, a bettor would have created a robust portfolio that
                  generated positive returns, something not many people in the gambling world can say. ")
    
    HTML(paste(h1(str1), p(str2), h1(str3), p(str4), h1(str5), p(str6)))
  })}
    
    
    
    




# Run the application 
shinyApp(ui = ui, server = server)
