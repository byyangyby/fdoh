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
# PLOT DISTRIBUTION OF ALL MODELS' R-SQUARED
#########################################
hist(pred$r.squared, col="darkgrey", 
     xlab="R-squared", main=NA, breaks=300, border="darkgrey")

#########################################
#Select best predicition model
#########################################
best <- as.character(pred$posibs[which(pred$r.squared == max(pred$r.squared))])  

#########################################
#GET BEST MODEL DETAILS
#########################################
bestModel <- lm(ts$total ~ ts$rain16.36 + ts$minTemp16.31)
summary(bestModel)

#########################################
#PLOT INDIVIDUAL PREDICTORS AGAINT OUTCOME
#########################################
#rain
plot(ts$rain16.36, ts$total,
     xlab="Cumulative rainfall 16-36 days prior",
     ylab="Mosquitoes per trap",
     pch=16, col=adjustcolor("darkred", alpha.f=0.4))
abline(lm(ts$total ~ ts$rain16.36), col=adjustcolor("black", alpha.f=0.6), lwd=2)
summary(lm(ts$total ~ ts$rain16.36)) # good = RAIN IS LINEAR


#rain, log total
plot(ts$rain16.36, log(ts$total))
abline(lm(log(ts$total) ~ ts$rain16.36), col="red")
summary(lm(log(ts$total) ~ ts$rain16.36)) # bad = RAIN IS NOT LOG
 

#minTemp
plot(ts$minTemp16.31, ts$total)
abline(lm(ts$total ~ ts$minTemp16.31), col="red")
summary(lm(ts$total ~ ts$minTemp16.31)) # bad = TEMP IS NOT LINEAR

#mintemp, log total
plot(ts$minTemp16.31, log(ts$total),
     xlab="Minimum temperature 16-31 days prior",
     ylab="Mosquitoes per trap", yaxt="n",
     pch=16, col=adjustcolor("darkred", alpha.f=0.4))
axis(side=2, at=as.numeric(quantile(log(ts$total), na.rm=TRUE)), 
     labels=as.numeric(quantile(ts$total, na.rm=TRUE)))
abline(lm(log(ts$total) ~ ts$minTemp16.31), 
       col=adjustcolor("black", alpha.f=0.6), lwd=2)
summary(lm(log(ts$total) ~ ts$minTemp16.31)) # good = TEMP IS LOG

#########################################
#SO, WHAT'S BETTER, LOG()ING THE TEMP OR LOGGING THE total?
#########################################
#linear
bestModel <- lm(ts$total ~ ts$rain16.36 + ts$minTemp16.31)
summary(bestModel) #R.squared == 0.4305
#log
logModel <- lm(log(ts$total) ~ ts$rain16.36 + ts$minTemp16.31)
summary(logModel) #R.squared == 0.5325
#logTemp
logTempModel <- lm(ts$total ~ ts$rain16.36 + log(ts$minTemp16.31))
summary(logTempModel) #R.squared == 0.4284

#expTemp, no transformations for anything else
expTempModel <- lm(ts$total ~ ts$rain16.36 + exp(ts$minTemp16.31))
summary(logTempModel) #R.squared == 


#ANSWER - LOGGING THE TOTAL
#########################################
# USING BEST MODEL, MAKE A PREDICTED COLUMN IN TS
#########################################
ts$predicted <- -234.105 + (ts$rain16.36*53.74) + (ts$minTemp16.31*3.496)

#########################################
#PLOT PREDICTED VS OBSERVED
#########################################
plot(ts$predicted, ts$total,
     xlab="Predicted", ylab="Observed", pch=16,
     col=adjustcolor("darkblue", alpha.f=0.4))

plot(ts$predicted, ts$total^(1/2),
     xlab="Predicted", ylab="Observed", pch=16,
     col=adjustcolor("darkblue", alpha.f=0.4))


#WRONG WAY - ARBITRARILY SORTS, NOT MATCHING [originally appeared in article this way]
plot(sort(ts$predicted[which(is.na(ts$total) == FALSE)]), 
     (sort(ts$total[which(is.na(ts$total) == FALSE)])),
     xlab="Predicted", ylab="Observed", pch=16,
     col=adjustcolor("darkblue", alpha.f=0.4))


#########################################
#PLOT OBSERVED MOSQUITO NUMBERS
#########################################
par(mar=c(4,4,2,1))
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
plot(ts$date, ts$total, type="n", xaxt="n", col="white", type="n")
axis(side=1, at=ts$date[seq(1,2100,100)], label=ts$date[seq(1,2100,100)],
     las=3, cex.axis=0.65, line=-0.85, tick=FALSE)
#########################################
#POLYGON OF OBSERVED
#########################################
polygon(c(ts$date[which(is.na(ts$total) == FALSE)], 
          rev(ts$date[which(is.na(ts$total) == FALSE)])), 
        c(ts$total[which(is.na(ts$total) == FALSE)], rep(0, length(ts$date[which(is.na(ts$total) == FALSE)]))), col= adjustcolor("darkred", alpha.f=0.6), border=NA)
#########################################
#LINES OF PREDICTED (ALL DATES)
#########################################
lines(ts$date,
      ts$predicted,
      col=adjustcolor("darkblue", alpha.f=0.6), lwd=2)
#########################################
#LINES OF OBSERVED
#########################################
lines(ts$date[which(is.na(ts$total) == FALSE)], 
      ts$total[which(is.na(ts$total) == FALSE)],
      col=adjustcolor("darkred", alpha.f=0.6), lwd=2)
#########################################
#POLYGON OF PREDICTED
#########################################
polygon(c(ts$date[which(is.na(ts$total) == FALSE)], 
          rev(ts$date[which(is.na(ts$total) == FALSE)])), 
        c(ts$predicted[which(is.na(ts$total) == FALSE)], rep(0, length(ts$date[which(is.na(ts$total) == FALSE)]))), col= "red", border=NA)
#########################################
#LINES OF PREDICTED (ONLY FOR COLLECTION DATES)
#########################################
lines(ts$date[which(is.na(ts$total) == FALSE)],
      ts$predicted[which(is.na(ts$total) == FALSE)],
      col=adjustcolor("darkblue", alpha.f=0.6), lwd=2)





#IDENTICAL, BUT USING LOG OF TOTAL
#########################################
#PLOT OBSERVED MOSQUITO NUMBERS
#########################################
par(mar=c(4,4,2,1))
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
plot(ts$date, log(ts$total), type="n", xaxt="n", col="white", type="n")
axis(side=1, at=ts$date[seq(1,2100,100)], label=ts$date[seq(1,2100,100)],
     las=3, cex.axis=0.65, line=-0.85, tick=FALSE)
#########################################
#POLYGON OF OBSERVED
#########################################
polygon(c(ts$date[which(is.na(log(ts$total)) == FALSE)], 
          rev(ts$date[which(is.na(log(ts$total)) == FALSE)])), 
        c(log(ts$total)[which(is.na(log(ts$total)) == FALSE)], rep(0, length(ts$date[which(is.na(log(ts$total)) == FALSE)]))), col= adjustcolor("darkred", alpha.f=0.6), border=NA)
#########################################
#LINES OF PREDICTED (ALL DATES)
#########################################
library(splines)
ts$predicted2 <- log(ts$predicted)
ts$predicted2[which(is.na(ts$predicted2)==TRUE)] <- 0
lines(ts$date,
      ts$predicted2,
      col=adjustcolor("darkblue", alpha.f=0.6),
      lwd=3)

#ONLY USING TRAP DATES
lines(ts$date[which(is.na(ts$total) == FALSE)],
      ts$predicted2[which(is.na(ts$total) == FALSE)],
      col=adjustcolor("darkblue", alpha.f=0.6), lwd=2)



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
