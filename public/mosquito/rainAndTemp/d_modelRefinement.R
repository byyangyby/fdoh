library(pingr)
#########################################
# READ IN DATA FOR TEMP AND RAINFALL FOR LAST 6 YEARS
#########################################
ts <- read.csv("E:/workingdirectory/mosquito/rainAndTemp/b_rainAndTemp.csv")

#########################################
#LOAD THE MODEL VARIATIONS WITH THEIR R-SQUARED VALUES
#########################################
pred <- read.csv("E:/workingdirectory/mosquito/rainAndTemp/c_rainAndTempDone.csv")

#########################################
#Select best predicition model
#########################################
best <- as.character(pred$posibs[which(pred$r.squared == max(pred$r.squared))])  

#########################################
#GET MODEL DETAILS
#########################################
bestModel <- lm(ts$total ~ ts$rain16.36 + ts$minTemp16.31)
summary(bestModel)

#########################################
# USING BEST MODEL, MAKE A PREDICTED COLUMN IN TS
#########################################
ts$predicted <- -234.105 + (ts$rain16.36*53.74) + (ts$minTemp16.31*3.496)

#########################################
#PLAY AROUND WITH A FEW ALTERNATIVE MODELS
#########################################
#SQRT
sqrtModel <- lm(sqrt(ts$total) ~ ts$rain16.36 + ts$minTemp16.31)
summary(sqrtModel) #R.squared == 0.5094
#LOG
logModel <- lm(log(ts$total) ~ ts$rain16.36 + ts$minTemp16.31)
summary(logModel) #R.squared == 0.5325
#ALTMODEL
plot(seq(6.3,6.5,0.01), seq(6.3,6.5,0.01)/6.5, type="n", ylim=c(0,1))
for (i in seq(6.3,6.5,0.01)){
  altModel <- summary(lm((ts$total)^(1/i) ~ ts$rain16.36 + ts$minTemp16.31))
  #summary(altModel) #R.squared == 0.5094
  bla <- cbind(i, altModel$r.squared)
  points(i, altModel$r.squared, pch=".")
  print(unlist(bla))
} #THIS FINDS BEST R-SQUARED USING ts$total^(1/6.4)

altModel <- lm((ts$total)^(1/6.4) ~ ts$rain16.36 + ts$minTemp16.31)
summary(altModel) #R.squared == 0.5408

# Though the 6.4 model is slightly better, I'm sticking with log
# But if taking the log of total works best, maybe other predictors would have done a better job?
# Time to re-run the 65536 simulations, this time taking the log of total

########### CREATING LOG MODEL

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
predLog <- as.data.frame(posibs)

#########################################
#Test the r-squared for each column (RAIN ONLY)
#########################################
predLog$r.squared <- NA
for (i in 1:length(predLog$posibs)){
  mylm <- summary(lm(log(ts[,"total"]) ~
                       ts[,unlist(strsplit(posibs, "AND")[i])[1]] +
                       ts[,unlist(strsplit(posibs, "AND")[i])[2]]
  ))
  predLog$r.squared[i] <- mylm$r.squared
}

#########################################
#Select best predicition model
#########################################
predLog <- predLog[rev(order(predLog$r.squared)),]

bestLog <- as.character(predLog$posibs[which(predLog$r.squared == max(predLog$r.squared))])  

bestModelLog <- lm(log(ts$total) ~ ts[, unlist(strsplit(bestLog, "AND"))[1]] +
                ts[,unlist(strsplit(bestLog, "AND"))[2]])
summary(bestModelLog)
summary(lm(log(ts$total)~ 
             ts$rain17.37 +
             ts$minTemp14.32))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#########################################
# USING BEST LOG MODEL, MAKE A PREDICTEDLOG COLUMN IN TS
#########################################
ts$predictedLog <- exp(-1.121293 + (ts$rain17.37*.200055) + (ts$minTemp14.32*0.074435))


save.image("E:/workingdirectory/mosquito/rainAndTemp/d_rainAndTempDoneLog.Rdata")
write.csv(predLog, "E:/workingdirectory/mosquito/rainAndTemp/d_rainAndTempDoneLog.csv")

write.csv(ts, "E:/workingdirectory/mosquito/rainAndTemp/d_tsRainTempPredPredLog.csv")

ping(2)

#The above is a simulation in which the log(total) is regressed against all the 512
#possible combinations of minTemp and cum rain. 
#When this is done running, I'll compare it's r-squared values with those of before
#(before being the linear, not log model)
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
