# Evolution of Global Yield Curves: An animated heat map
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

#set up data

load("allYields.RData")
#change PX_LAST to yield
names(allYields)[6]<-"yield"
allDates<- levels(as.factor(allYields$date))
dateCount <- length(allDates)
START_DATE =  allDates[1]
END_DATE = allDates[dateCount]

#prepare averages for plot
regionAvgYields<-allYields%>%group_by(date,region,tenor)%>%summarise(yield=mean(yield))


#determine where zero will be in the overall range of all the data
yieldRange<-range(allYields$yield,na.rm=TRUE)
zeroPos<-(0-yieldRange[1])/(yieldRange[2]-yieldRange[1])

#set up heat map template.  The only thing that changes going forward is the date.
p <- ggplot(filter(allYields,date==START_DATE), aes(tenor, country))
p<- p + geom_tile(aes(fill = yield), colour = "white")
#the next two lines give us very precise control over our color ranges
#so that below zero is shaded white to red and above zero is white to blue
p<- p + scale_fill_gradientn(colours=c("red","white","steelblue"),values=c(0,zeroPos,1),na.value = "white")
p<- p + expand_limits(fill=c(yieldRange[1]*1.1,yieldRange[2]))
p<- p + labs(x="Years to Maturity")+ theme_classic()
p<- p + ylab("America           Asia                          Europe")+ theme(axis.title.y=element_text(hjust=0))


# set up mess of curves template
ggc<- ggplot(filter(allYields,date==START_DATE),
       aes(x=tenor,y=yield,color=region,group=country))+
  geom_line()+
  expand_limits(y=yieldRange) +
  labs(x="Years to Maturity",y="Yield")+ theme_classic()+
  geom_hline(yintercept = 0,size=1)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Evolution of Global Yield Curves"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
         sliderInput("monthIndex",
                     "Month:",
                     min = 1,
                     max = dateCount,
                     value = 1,
                     step=1,
                     animate=TRUE)
      )),
      
      # Show a plot of the generated distribution
   fluidRow(
     column(6,
     plotOutput("distPlot.heat")
     ),
     column(6,
       plotOutput("distPlot.curve")
     )
   )
)


# Define server logic to plot both a heat map and a mess of curves with
# regional averages.
server <- function(input, output) {
  
  output$distPlot.heat <- renderPlot({
    current<-allDates[input$monthIndex]
    p %+% filter(allYields,date==current) + labs(title=current)
    
  })
  output$distPlot.curve <- renderPlot({
    current<-allDates[input$monthIndex]
    #create a mess of all curves colored by region
    ggc<-ggc %+% filter(allYields,date==current)
    ggc<- ggc + labs(title=current)
    ggc + geom_line(data=filter(regionAvgYields,date==current),
                  aes(tenor,yield,group=region,color=region),
                  size=2)

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

