# get bloomberg data on yield curve histories
#devtools::install_github("dgrtwo/gganimate")

library(dplyr)
library(ggplot2)
library(gganimate)

#specify which maturities to get from the bbg yield curve vector
curveTenors<-data.frame(bbgColumns=c(3,4,5,6,7,11),tenors=c(1,2,3,4,5,10))
START_DATE =  as.Date("2012-01-01","%Y-%m-%d")
BDH_OPTIONS = c("periodicitySelection"="MONTHLY")

# from excel

#=BDH(BDS($A2,"CURVE_MEMBERS","Dir=h","startrow="&R$1&",endrow="&R$1),"PX_LAST",$M$1,$M$1,"fill=x")

blpConnect()
allSecTickers<-data.frame()
curveTickers<-read.csv("curve tickers.csv",stringsAsFactors = FALSE)
for (i in  1:nrow(curveTickers)){
  secTickers<-bds(curveTickers$Ticker[i],"CURVE_MEMBERS")
  # narrow tickers down to the ones we want from curveTenors
  secTickers2<-secTickers[curveTenors$bbgColumns,]
  print (paste(i,curveTickers$Ticker[i],curveTickers$CountryISO[i]))
  allSecTickers<-rbind(allSecTickers,data.frame(region=curveTickers$Region[i],
                                                country=curveTickers$CountryISO[i],
                                                tenor=curveTenors$tenors,
                                                secTicker=secTickers2))
  
}

#don't need a factor vector for tickers
allSecTickers$secTicker=as.character(allSecTickers$secTicker)

#get the yields from START_DATE by BDH_OPTIONS periodicity
allYields<-data.frame()
for (j in 1:nrow(allSecTickers)) {
  print(allSecTickers[j,])
  yields<-bdh(allSecTickers$secTicker[j],"PX_LAST",start.date = START_DATE,options = BDH_OPTIONS)
  allYields<-rbind(allYields,cbind(allSecTickers[j,],yields,row.names=NULL))
}

# change "PX_LAST" to "yield"
names(allYields)<-c("region", "country","tenor" ,"secTicker", "date","yield")
#need tenor to be a factor so we don't plot intermediate values
allYields$tenor <-as.factor(allYields$tenor)
#have to do this so ggplot doesn't alphabetize the countries which are already grouped by region and sorted by yield
allYields$country<- factor(allYields$country,levels=allYields$country)

# that's a lot of data.  don't lose it!
save(allYields,file="allYields.RData")

# done with the bbg stuff
#blpDisconnect() not needed

#now build the plots
yieldRange<-range(allYields$yield,na.rm=TRUE)
zeroPos<-(0-yieldRange[1])/(yieldRange[2]-yieldRange[1])

buildGraph <- function(gdata) {
  p <- ggplot(gdata, aes(tenor, country))
  p<- p + geom_tile(aes(fill = yield), colour = "white")
  p<-p+scale_fill_gradientn(colours=c("red","white","steelblue"),values=c(0,zeroPos,1),na.value = "white")
  p<- p + expand_limits(fill=c(yieldRange[1]*1.1,yieldRange[2]))
  p<- p + labs(title=gdata$date[1]) + theme_classic()
  # print(p)
  return(p)
}

devAskNewPage(ask=TRUE)
# loop through all the dates
for (dt in levels(as.factor(allYields$date))) {
  print(dt)
  gdata<-filter(allYields,date==dt)
  buildGraph(gdata)
}
devAskNewPage(ask=FALSE)


#blpDisconnect()
