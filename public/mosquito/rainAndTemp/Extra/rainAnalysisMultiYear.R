library(pingr)
#########################################
#READ IN THE RAIN TIME SERIES DATA [CREATED FROM WEBSCRAPING WUNDERGROUND]
#READ IN MOSQUITO TIME SERIES DATA [CREATED FROM 2013 MOSQ SEASON SURVEIL]
#########################################
tsRain <- read.csv("E:/workingdirectory/mosquito/rainFall2013/rain2008-2013.csv")
tsMosq <- read.csv("E:/workingdirectory/mosquito/simple.csv")
tsTemp <- read.csv("E:/workingdirectory/mosquito/rainFall2013/tsTemp2008-2013.csv")
#########################################
#CONVERT DATES INTO R DATE OBJECTS
#########################################
tsRain$date <- as.Date(tsRain$date, format="%Y-%m-%d")
tsMosq$date <- as.Date(tsMosq$date, format="%m/%d/%Y")
tsTemp$date <- as.Date(tsTemp$date, format="%Y-%m-%d")

#########################################
#CREATE A MASTER TS
#########################################
ts <- as.data.frame(tsRain$date)
colnames(ts) <- "date"

#########################################
#ADD RAIN TO TS
#########################################
ts$rain <- tsRain$rain 


#########################################
#ADD MOSQUITOES (TOTAL AND VECTOR) TO TS
# (NOTE, THESE ARE MOSQUITOES PER TRAP)
#########################################
ts$total <- NA
for (i in tsMosq$date){
  ts$total[which(ts$date == i)] <- 
    tsMosq$total[which(tsMosq$date == i)]
}

ts$vector <- NA
for (i in tsMosq$date){
  ts$vector[which(ts$date == i)] <- 
    tsMosq$vector[which(tsMosq$date == i)]
}

#########################################
#ADD MINIMUM TEMP TO TS
#########################################
ts$minTemp <- NA
for (i in ts$date){
  ts$minTemp[which(ts$date == i)] <- 
    tsTemp$MinTemp[which(tsTemp$date == i)]
}

ts$minTemp[which(ts$minTemp < -100)] <- NA

#########################################
#ADD MAXIMUM TEMP TO TS
#########################################
ts$maxTemp <- NA
for (i in ts$date){
  ts$maxTemp[which(ts$date == i)] <- 
    tsTemp$MaxTemp[which(tsTemp$date == i)]
}

ping()
#########################################
#########################################
#########################################
#########################################
#########################################



#########################################
#ADD COLUMNS FOR RAIN AND MINTEMP RANGES
#########################################

ts2 <- ts
#Make columns for a range of 5-20 days old, plus 5-20 days older than that
for (j in 5:20){
  for (k in 5:20){
    ts2[,paste0("rain", j, ".", j+k)] <- NA
    ts2[,paste0("minTemp", j, ".", j+k)] <- NA
  }
}

#########################################
#ADD CUMULATIVE RAINFALL TO CORRESPONDING COLUMNS
#########################################
for (j in colnames(ts2)[grepl("rain", colnames(ts2))][-1]){
  for (i in 30:nrow(ts2)){
    ts2[i,j] <-
      sum(ts2$rain[which(ts2$date <= ts2$date[i-min(as.numeric(unlist(strsplit(gsub("rain", "", j), ".", fixed=TRUE))))] &
                           ts2$date >= ts2$date[i-max(as.numeric(unlist(strsplit(gsub("rain", "", j), ".", fixed=TRUE))))])], na.rm=TRUE)
  }
}
write.csv(ts2, "E:/workingdirectory/mosquito/rainAndTemp/rainOnly.csv")


#########################################
#ADD MINIMUM MINTEMP FOR EACH MINTEMP COLUMN COLUMN
#########################################
for (j in colnames(ts2)[grepl("minTemp", colnames(ts2))][-1]){
  for (i in 30:nrow(ts2)){
    ts2[i,j] <-
      min(ts2$minTemp[which(ts2$date <= ts2$date[i-min(as.numeric(unlist(strsplit(gsub("minTemp", "", j), ".", fixed=TRUE))))] &
                           ts2$date >= ts2$date[i-max(as.numeric(unlist(strsplit(gsub("minTemp", "", j), ".", fixed=TRUE))))])], na.rm=TRUE)
  }
}
ping(2)
write.csv(ts2, "E:/workingdirectory/mosquito/rainAndTemp/rainAndTemp")


save.image("E:/workingdirectory/mosquito/rainAndTemp/rainAndTemp")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#########################################
#ADD RAINFALL RANGES
#########################################

#Make columns for a range of 5-20 days old, plus 5-20 days older than that
for (j in 5:20){
  for (k in 5:20){
    ts[,paste0("rain", j, ".", j+k)] <- NA
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
ping(8)


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
#PLOT TOGETHER THE PREDICTED RANGE OF IMPORTANCE AND THE NUMBER OF TOTAL MOSQUITOES
#########################################
library(splines)
par(mar=c(5,4,4,5))

plot(tsRain$date, tsRain$total, xlab="Date", ylab="Mosquitoes trapped",
     type="n",
     xlim=c(min(tsRain$date)+80, max(tsRain$date)-15))

for (i in seq(0.1,1,.05)){
  xspline(tsRain$date[which(is.na(tsRain$total)==FALSE)], 
          tsRain$total[which(is.na(tsRain$total)==FALSE)],
          shape=i, border=adjustcolor("darkred", alpha.f=0.2), lwd=1)
  xspline(tsRain$date, tsRain[,best]*150, shape=i,lwd=1,
          border=adjustcolor("blue", alpha.f=0.2))
}
xspline(tsRain$date[which(is.na(tsRain$total)==FALSE)], 
        tsRain$total[which(is.na(tsRain$total)==FALSE)],
        shape=0.8, border=adjustcolor("darkred", alpha.f=0.6), lwd=3)
xspline(tsRain$date, tsRain[,best]*150, shape=1,lwd=3,
      border=adjustcolor("blue", alpha.f=0.6))

plot(tsRain$date, tsRain$total, xlab="Date", ylab="Mosquitoes trapped",
     pch=16, col=adjustcolor("darkred", alpha.f=0.4), cex=0.7)
lines(tsRain$date[which(is.na(tsRain$total)==FALSE)], 
      tsRain$total[which(is.na(tsRain$total)==FALSE)],
      col=adjustcolor("darkred", alpha.f=0.4), lwd=3)

abline(h=seq(0,2000,200), col=adjustcolor("black", alpha.f=0.15))
lines(tsRain$date, tsRain[,best]*150, lwd=3,
      col=adjustcolor("blue", alpha.f=0.3))
points(tsRain$date, tsRain[,best]*150, pch=17,
       col=adjustcolor("blue", alpha.f=0.2), cex=0.7)

legend(x="topleft", lty=1, lwd=3, pch=c(16,17),
       legend=c("Mosquitoes", "Rainfall"),
       col=adjustcolor(c("darkred", "blue"), alpha.f=0.4), 
       bty="n")

axis(side=4, at=seq(0, 2000, 100), labels=seq(0, 2000, 100)/150)
mtext(gsub("[.]", "-",gsub("rain", "",  paste0("Rainfall (inches) ", best," days before"))),
      side=4, line=2.5, cex.lab=1,las=3 )

#########################################
#Save it for use in Sweave (RDATA FILE)
#########################################
save.image("E:/workingdirectory/mosquito/rainFall2013/rainAndHeat2008-2013.RData")
#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

#########################################
#
#########################################

