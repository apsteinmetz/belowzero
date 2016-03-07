
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

START_DATE =  as.Date("2012-01-01","%Y-%m-%d")
END_DATE = Sys.Date()

library(shiny)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)

server<- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$date_range + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
}

ui <- fluidPage(

  # Application title
  titlePanel("The Spreading Virus of Negative Yields"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("date_range",
                  "Date",
                  min = as.Date("2016-02-01"), max = Sys.Date(), 
                  value = c(as.Date("2016-02-25"), Sys.Date())
                  )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

shinyApp(ui = ui, server=server)
