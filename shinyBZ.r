
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



ui <- fluidPage(
  
  # Application title
  titlePanel("The Spreading Virus of Negative Yields"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("dateIndex",
                  "Date Index:",
                  min = 1,
                  max = dateCount,
                  value = 1,
                  animate = TRUE
                  
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
) # end ui


server<- function(input, output) {
  
  buildGraph <- function(gdata) {
    p <- ggplot(gdata, aes(tenor, country))
    p<- p + geom_tile(aes(fill = yield), colour = "white")
    p<-p+scale_fill_gradientn(colours=c("red","white","steelblue"),values=c(0,zeroPos,1),na.value = "white")
    p<- p + expand_limits(fill=c(yieldRange[1]*1.1,yieldRange[2]))
    p<- p + labs(title=gdata$date[1]) + theme_classic()
    return(p)
  }
  
  
  output$distPlot <- renderPlot({
    
    dt<- input$dateIndex
    gdata<-filter(allYields,date==allDates[dt])
    buildGraph(gdata)

  })
  
} #end server


shinyApp(ui = ui, server=server)
