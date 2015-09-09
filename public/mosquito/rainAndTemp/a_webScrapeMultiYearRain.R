#THE FOLLOWING SCRIPT TAKES RAINFALL DATA FROM WUNDERGROUND
#THE NAs SHOW UP BECAUSE OF THIS MYSTERIOUS T
library(pingr)

#Establish start and end dates
startDate <- "2008-03-01"
nDays <- 2200

#Set up URL
linkPart1 <- "http://www.wunderground.com/history/airport/KGNV/"
linkPart3 <-  "/DailyHistory.html"

ts <- as.data.frame(c(as.Date(startDate, format="%Y-%m-%d"), 
                      as.Date(startDate, format="%Y-%m-%d")+1:(nDays-1)))
colnames(ts) <- "date"
ts$dateRec <- format(ts$date, format="%Y/%m/%d")

#RAINFALL DATA
ts$pui <- NA
for (i in 1:nrow(ts)){
  linkPart2 <- ts$dateRec[i]
  link <- paste0(linkPart1, linkPart2, linkPart3)
  webPage <- readLines(link)
  webPage <- webPage[grepl("  <span class=\"nobr\"><span class=\"b\">", webPage) &
                    grepl("</span>&nbsp;in</span>", webPage)][1]
  ts$pui[i] <- as.numeric(gsub(paste0("  <span class=\"nobr\"><span class=\"b\">",
                                      "|", "</span>&nbsp;in</span>"), 
                               "",
                               webPage)) 
}

ts$rain <- ts$pui
ts$pui <- NULL
ts$rain[is.na(ts$rain)] <- 0

#write.csv(ts, "C:/Users/BrewJR/Desktop/mosquito/rainFall2013/rain2008-2013.csv")


ping(8)


write.csv(ts, "E:/workingdirectory/mosquito/rainAndTemp/a_rain2008-2013.csv")
ping(8)
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################

######NOW FOR TEMPERATURE DATA
#TEMPERATURE DATA (TAKES FOREVER)
ts$temp <- NA
for (i in 1:nrow(ts)){
  linkPart2 <- ts$dateRec[i]
  link <- paste0(linkPart1, linkPart2, linkPart3)
  webPage <- readLines(link)
  webPage <- webPage[grepl("  <span class=\"nobr\"><span class=\"b\">", webPage) &
                       grepl("</span>&nbsp", webPage)][1]
  ts$temp <- as.numeric(gsub(paste0("  <span class=\"nobr\"><span class=\"b\">",
                                    "|", "</span>&nbsp;&deg;F</span>"), "", webPage))
}

ts$heat[is.na(ts$heat)] <- 0
############################################
#BEGIN THE RAIN ANALYSIS MULTIYEAR FILE (RUNNING TOGETHER OVER WEEKEND)
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################

library(pingr)
#########################################
#READ IN THE RAIN TIME SERIES DATA [CREATED FROM WEBSCRAPING WUNDERGROUND]
#READ IN MOSQUITO TIME SERIES DATA [CREATED FROM 2013 MOSQ SEASON SURVEIL]
#########################################
tsRain <- read.csv("C:/Users/BrewJR/Desktop/mosquito/rainFall2013/rain2008-2013.csv")
tsMosq <- read.csv("C:/Users/BrewJR/Desktop/mosquito/simple.csv")
tsTemp <- read.csv("C:/Users/BrewJR/Desktop/mosquito/rainFall2013/rainAndTemp2008-2013.csv")
#########################################
#CONVERT DATES INTO R DATE OBJECTS
#########################################
tsRain$date <- as.Date(tsRain$date, format="%Y-%m-%d")
tsMosq$date <- as.Date(tsMosq$date, format="%m/%d/%Y")
tsTemp$date <- as.Date(tsTemp$date, format="%m/%d/%Y")

#########################################
#ADD MOSQUITOES (TOTAL AND VECTOR) TO RAINFALL
#########################################
tsRain$total <- NA
for (i in tsMosq$date){
  tsRain$total[which(tsRain$date == i)] <- 
    tsMosq$total[which(tsMosq$date == i)]
}

tsRain$vector <- NA
for (i in tsMosq$date){
  tsRain$vector[which(tsRain$date == i)] <- 
    tsMosq$vector[which(tsMosq$date == i)]
}


#########################################
#ADD DAILY MEDIAN TEMPERATURE TO tsRain
#########################################
tsRain$temp <- NA
for (i in tsRain$date){
  tsRain$temp[which(tsRain$date == i)] <- 
    tsTemp$temp[which(tsTemp$date == i)]
}

#########################################
#########################################
#########################################
#########################################
#########################################





#########################################
#ADD RAINFALL RANGES
#########################################

#Make columns for a range of 5-20 days old, plus 5-20 days older than that
for (j in 5:20){
  for (k in 5:20){
    tsRain[,paste0("rain", j, ".", j+k)] <- NA
  }
}

#Add rainfall for each of the columns
for (j in colnames(tsRain)[grepl("rain", colnames(tsRain))][-1]){
  for (i in 30:nrow(tsRain)){
    tsRain[i,j] <-
      sum(tsRain$rain[which(tsRain$date <= tsRain$date[i-min(as.numeric(unlist(strsplit(gsub("rain", "", j), ".", fixed=TRUE))))] &
                              tsRain$date >= tsRain$date[i-max(as.numeric(unlist(strsplit(gsub("rain", "", j), ".", fixed=TRUE))))])], na.rm=TRUE)
  }
}


#########################################
#ADD TEMPERATURE RANGES
#########################################

#Make columns for a range of 5-20 days old, plus 5-20 days older than that
for (j in 5:20){
  for (k in 5:20){
    tsRain[,paste0("temp", j, ".", j+k)] <- NA
  }
}

#Add rainfall for each of the columns
for (j in colnames(tsRain)[grepl("temp", colnames(tsRain))][-1]){
  for (i in 30:nrow(tsRain)){
    tsRain[i,j] <-
      sum(tsRain$temp[which(tsRain$date <= tsRain$date[i-min(as.numeric(unlist(strsplit(gsub("temp", "", j), ".", fixed=TRUE))))] &
                              tsRain$date >= tsRain$date[i-max(as.numeric(unlist(strsplit(gsub("temp", "", j), ".", fixed=TRUE))))])], na.rm=TRUE)
  }
}

#########################################
#Test the r-squared for each column (RAIN ONLY)
#########################################
#Create a dataframe with the R-squared and correlation coefficient for each range
pred <- as.data.frame(colnames(tsRain)[grepl("rain", colnames(tsRain))][-1])
colnames(pred) <- "range"

for (i in pred$range){
  mylm <- summary(lm(tsRain[,"total"] ~ tsRain[,i]))
  mycor <- cor(tsRain[,i], tsRain[,"total"], use="complete.obs")
  
  pred$r.squared[which(pred$range == i)] <- mylm$r.squared
  pred$cor[which(pred$range == i)] <- mycor
  
}

pred <- pred[order(pred$r.squared),]

#########################################
#Select best predicition model
#########################################
best <- as.character(pred$range[which(pred$r.squared == max(pred$r.squared))])  




#########################################
#Select best predicition model
#########################################
best <- as.character(pred$range[which(pred$r.squared == max(pred$r.squared))])  


#########################################
#Save it for use in Sweave (RDATA FILE)
#########################################
#save.image("C:/Users/BrewJR/Desktop/mosquito/rainFall2013/rainAndHeat2008-2013.RData")
#########################################
ping(8)
