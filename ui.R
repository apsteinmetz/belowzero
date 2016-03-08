
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(dplyr)

START_DATE =  as.Date("2012-01-01","%Y-%m-%d")
END_DATE = Sys.Date()

load("allYields.RData")
allDates<- levels(as.factor(allYields$date))
dateCount <- length(allDates)



shinyUI(fluidPage(
  
  # Application title
  titlePanel("The Spreading Virus of Negative Yields"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("dateIndex",
                  paste("Date Index:",START_DATE,"to",END_DATE),
                  min = 1,
                  max = dateCount,
                  value = 1,
                  step = 1,
                  animate = animationOptions(interval=300)
                  
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)) # end ui

