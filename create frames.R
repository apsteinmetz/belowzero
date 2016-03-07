library(dplyr)
library(reshape2)
library(scales)

devAskNewPage(ask=TRUE)
yields<-read.csv("below zero yields.csv")
# get high and low for whole data set
yieldRange<-select(yields,-(Date:MonthsAgo))%>%melt(value.name="Yield")%>%select(Yield)%>%range(na.rm=TRUE)
#calculate zero position in range.  Will fail if low is non-negative
zeroPos<-(0-yieldRange[1])/(yieldRange[2]-yieldRange[1])

buildGraph <- function(test) {
  p <- ggplot(test, aes(variable, Country))
  p<- p + geom_tile(aes(fill = Yield), colour = "white")
  p<-p+scale_fill_gradientn(colours=c("red","white","steelblue"),values=c(0,zeroPos,1),na.value = "white")
  p<- p + expand_limits(fill=yieldRange)
  print(p)
}

for (mo in levels(as.factor(yields$MonthsAgo))) {
  print(mo)
  test<-filter(yields,MonthsAgo==mo)%>%select(-(Date:MonthsAgo))%>%melt(value.name="Yield")
  #have to do this so ggplot doesn't alphabetize the countries which are already grouped by region and sorted by yeild
  test$Country<- factor(test$Country,levels=test$Country)
  buildGraph(test)
}


devAskNewPage(ask=FALSE)

# n<-data.frame(x=1,y=seq(0:10),z=seq(-2,8))
# p<-ggplot(n,aes(x,y))+geom_tile(aes(fill=z),colour="white")
# # I want the midpoint to be zero so show I show negative values in shades of red, white for zero and something else for postive.
# p<-p+ scale_fill_gradient2(low="red3",mid="white",high="steelblue",midpoint=0)
# print (p)
# #but I want the most negative value in the set to be fully color saturated, like here.
# p<-p+scale_fill_gradient2(low="red3",mid="white",high="steelblue",midpoint=3)
# print(p)
# p<-p+scale_fill_gradientn(colours=c("red3","white","steelblue"),values=c(0,0.2,1))
# print(p)

