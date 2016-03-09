
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
yieldRange<-range(allYields$yield,na.rm=TRUE)
zeroPos<-(0-yieldRange[1])/(yieldRange[2]-yieldRange[1])



shinyServer(function(input, output) {
  
  buildGraph <- function(gdata) {
    p <- ggplot(gdata, aes(tenor, country))
    p<- p + geom_tile(aes(fill = yield), colour = "white")
    p<-p+scale_fill_gradientn(colours=c("red","white","steelblue"),values=c(0,zeroPos,1),na.value = "white")
    p<- p + expand_limits(fill=c(yieldRange[1]*1.1,yieldRange[2]))
    p<- p + labs(title=gdata$date[1],x="Years to Maturity")+ theme_classic()
    p<-p + ylab("Americas         Asia                 Europe                                   ") 
    return(p)
  }
  
  
  output$distPlot <- renderPlot({
    
    dt<- input$dateIndex
    gdata<-filter(allYields,date==allDates[dt])
    buildGraph(gdata)

  })
  
}) #end server
