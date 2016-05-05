#devtools::install_github("dgrtwo/gganimate")

library(ggplot2)
library(gganimate)
library(dplyr)
library(animation)
ani.options(convert=gsub('convert','magick',ani.options('convert')))


START_DATE =  as.Date("2012-01-01","%Y-%m-%d")
END_DATE = Sys.Date()

load("allYields.RData")
allDates<- levels(as.factor(allYields$date))
dateCount <- length(allDates)
yieldRange<-range(allYields$yield,na.rm=TRUE)
zeroPos<-(0-yieldRange[1])/(yieldRange[2]-yieldRange[1])


#asfasdf
buildGraph <- function(gdata) {
  p <- ggplot(gdata, aes(tenor, country))
  # does frame break code if we don't use gganimate?
  p<- p + geom_tile(aes(fill = yield,frame=date), colour = "white")
  p<-p+scale_fill_gradientn(colours=c("red","white","steelblue"),values=c(0,zeroPos,1),na.value = "white")
  p<- p + expand_limits(fill=c(yieldRange[1]*1.1,yieldRange[2]))
  p<- p + labs(title=gdata$date[1],x="Years to Maturity")+ theme_classic()
  p<-p + ylab("America           Asia                          Europe")+ theme(axis.title.y=element_text(hjust=0))
  return(p)
}

# #devAskNewPage(ask=TRUE)
for (dt in allDates) {
    gdata<-filter(allYields,date==dt)
    print(dt)
    png(paste("bz",dt,".png",sep=""))
    print(buildGraph(gdata))
    dev.off()
  }
devAskNewPage(ask=FALSE)

# or...using gganimate if you have ImageMagick
p <- buildGraph(allYields)
gg_animate(p,convert='gm convert')

