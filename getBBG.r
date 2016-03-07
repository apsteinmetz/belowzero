# get bloomberg data on yield curve histories
library(Rblpapi)
library(dplyr)

#specify which maturities to get from the bbg yield curve vector
curveTenors<-data.frame(bbgColumns=c(3,4,5,6,7,11),tenors=c(1,2,3,4,5,10))

# from excel

#=BDH(BDS($A2,"CURVE_MEMBERS","Dir=h","startrow="&R$1&",endrow="&R$1),"PX_LAST",$M$1,$M$1,"fill=x")

blpConnect()
allSecTickers<-data.frame()
curveTickers<-read.csv("curve tickers.csv",stringsAsFactors = FALSE)
for (i in  1:nrow(curveTickers)){
  secTickers<-bds(curveTickers$Ticker[i],"CURVE_MEMBERS")
  secTickers2<-secTickers[curveTenors$bbgColumns,]
  print (paste(i,curveTickers$Ticker[i],curveTickers$CountryISO[i]))
  allSecTickers<-rbind(allSecTickers,data.frame(region=curveTickers$Region[i],
                                                Country=curveTickers$CountryISO[i],
                                                tenor=curveTenors$tenors,
                                                secTicker=secTickers2))
  
}
allSecTickers$secTicker=as.character(allSecTickers$secTicker)



#blpDisconnect()
