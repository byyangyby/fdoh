library(pingr)

#########################################
# READ IN DATA FOR TEMP AND RAINFALL FOR LAST 6 YEARS
# THIS DATA ALREADY HAS RANGES CALCULATED INTO IT (SEE COMBINERAINANDTEMP.R)
#########################################
ts <- read.csv("E:/workingdirectory/mosquito/rainAndTemp/b_rainAndTemp.csv")

#########################################
#### CREATE A VECTOR OF ALL THE POSSIBLE COMBINATIONS OF MIN TEMP RANGES AND RAINFALL RANGES
#########################################

rainPosibs <- rep(colnames(ts)[grepl("rain", colnames(ts))][-1],
                 length(colnames(ts)[grepl("minTemp", colnames(ts))][-1]))
minTempPosibs <- sort(rep(colnames(ts)[grepl("minTemp", colnames(ts))][-1],
                     length(colnames(ts)[grepl("rain", colnames(ts))][-1])))
posibs <- paste0(rainPosibs, "AND", minTempPosibs)

#########################################
#CREATE A DATA FRAME FROM MY POSIBS VECTOR
# THIS IS WHERE I'LL PUT MY MODEL QUALITY INDICATORS
#########################################
pred <- as.data.frame(posibs)

#########################################
#Test the r-squared for each column (RAIN ONLY)
#########################################
pred$r.squared <- NA
for (i in 1:length(pred$posibs)){
  mylm <- summary(lm(ts[,"total"] ~
                       ts[,unlist(strsplit(posibs, "AND")[i])[1]] +
                       ts[,unlist(strsplit(posibs, "AND")[i])[2]]
                     ))
  pred$r.squared[i] <- mylm$r.squared
}
  
#########################################
#Select best predicition model
#########################################
pred <- pred[rev(order(pred$r.squared)),]

best <- as.character(pred$posibs[which(pred$r.squared == max(pred$r.squared))])  

myModel <- lm(ts$total ~ ts[, unlist(strsplit(best, "AND"))[1]] +
                ts[,unlist(strsplit(best, "AND"))[2]])
summary(myModel)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########################################
# USING BEST MODEL, MAKE A PREDICTED COLUMN IN TS
#########################################
ts$predicted <- -234.105 + (ts$rain16.36*53.74) + (ts$minTemp16.31*3.496)

save.image("E:/workingdirectory/mosquito/rainAndTemp/c_rainAndTempDone.Rdata")
write.csv(pred, "E:/workingdirectory/mosquito/rainAndTemp/c_rainAndTempDone.csv")
ping(2)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


