#THE FOLLOWING TAKES ABOUT 6 HOURS TO RUN.
#NO NEED TO RUN AGAIN... OUTPUT INTO :
#write.csv(ts2, "E:/workingdirectory/mosquito/rainAndTemp/rainAndTemp.csv")
#save.image("E:/workingdirectory/mosquito/rainAndTemp/rainAndTemp.RData")

library(pingr)
#########################################
#READ IN THE RAIN TIME SERIES DATA [CREATED FROM WEBSCRAPING WUNDERGROUND]
#READ IN MOSQUITO TIME SERIES DATA [CREATED FROM 2013 MOSQ SEASON SURVEIL]
#########################################
tsRain <- read.csv("E:/workingdirectory/mosquito/rainAndTemp/a_rain2008-2013.csv")
tsMosq <- read.csv("E:/workingdirectory/mosquito/simple.csv")
tsTemp <- read.csv("E:/workingdirectory/mosquito/rainAndTemp/a_tsTemp2008-2013.csv")
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
#write.csv(ts2, "E:/workingdirectory/mosquito/rainAndTemp/rainOnly.csv")


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
write.csv(ts2, "E:/workingdirectory/mosquito/rainAndTemp/b_rainAndTemp.csv")


save.image("E:/workingdirectory/mosquito/rainAndTemp/b_rainAndTemp.RData")
ping(8)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
