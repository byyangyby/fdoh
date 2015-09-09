library(UScensus2010)

install.blk("osx") #this takes a long time


library(UScensus2010)
library(UScensus2010county)

data(florida.county10)

Over65 = function(dfCensus) {
  Over65 = dfCensus$H0170009 + dfCensus$H0170010 + dfCensus$H0170011
  Over65 = Over65 + dfCensus$H0170019 + dfCensus$H0170020 + dfCensus$H0170021
  Over65
}

PercentOver65 = function(dfCensus) {
  PercentOver65 = Over65(dfCensus)/dfCensus$H0170001
}
florida.county10$Over65 = Over65(florida.county10)
florida.county10$PercentOver65 = PercentOver65(florida.county10)
florida.county10$PercentOver65 = PercentOver65(florida.county10)

