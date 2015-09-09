############
# THE FOLLOWING SCRIPT READS IN EACH DATE'S TRAP DATA
# (WHICH SHOULD BE INDIVIDUALLY SAVED IN THE 
# /mosquito/historical FOLDER WITH NOTHING ELSE)
# AND COMBINES THEM INTO ONE MASTER DATAFRAME
# (NAMED raw.dat)
#
# DATA IN EACH CSV SHOULD HAVE COLUMN NAMES OF
# mosquito, site1, site2,... siteN, code
############


############
# SET WD TO WHERE I HAVE ALL YEARS OF MOSQUITO DATA
############
if(Sys.info()['sysname'] == 'Windows'){
  setwd("C:/Users/BrewJR/Documents/fdoh/public/mosquito/historical")
} else{
  setwd('/home/joebrew/Documents/fdoh/public/mosquito/historical')
}

############
# READ IN ALL DATA INTO LIST
############
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv, skip=1)
myfileswithdate = lapply(temp, read.csv, skip=0)

############
# MAKE A NUMBER OF TRAPS COLUMN
############
for (i in 1:length(myfiles)){
  myfiles[[i]]$nTraps <-
    ncol(myfiles[[i]]) - 2
}


############
#ADD DATE COLUMN
############
for (i in 1:length(myfiles)){
  myfiles[[i]]$date <- 
    as.Date(gsub("X", "",colnames(myfileswithdate[[i]])[1]),
            format="%m.%d.%Y")
}

############
#CBIND ALL ELEMENTS OF THE MYFILES LIST INTO ONE DF
############
library(plyr)
raw.dat <- rbind.fill(myfiles)

############
#CODE ALL "OTHER" MOSQUITOES AS 100
############
raw.dat$code[which(grepl("other|Other|OTHER", 
                         raw.dat$mosquito))] <-  100

# ############
# # WRITE A CSV FOR CESAR ESCOBAR (24 June 2014)
# ############
# setwd("C:/Users/BrewJR/Documents/fdoh/public/mosquito/cesar")
# write.csv(raw.dat, "raw_data.csv")

############
# MAKE A TIME SERIES DF
############
ts <- as.data.frame(unique(sort(raw.dat$date)))
colnames(ts) <- "date"

############
# POPULATE TS WITH TOTAL NUMBER OF TRAPS
############
for (i in ts$date){
  ts$nTraps[which(ts$date == i)] <-
    raw.dat$nTraps[which(raw.dat$date == i)][1]
}

############
# POPULATE WITH TOTAL NUMBER OF MOSQUITOES
############
for (i in ts$date){
  ts$tot[which(ts$date == i)] <-
    sum(raw.dat[which(raw.dat$date == i),
                grepl("site", colnames(raw.dat))], na.rm=TRUE)
  
}

############
# POPULATE WITH EACH KIND OF MOSQUITO 
############
for (i in unique(sort(raw.dat$code))){
  ts[,paste0("code", i)] <- NA  
}

for (i in ts$date){
  for (j in unique(sort(raw.dat$code))){  
    
    ts[which(ts$date == i),
       paste0("code", j)] <- ifelse(
         nrow(raw.dat[which(raw.dat$date == i &
                              raw.dat$code == j),]) == 0,
         0,
         sum(raw.dat[which(raw.dat$date == i &
                             raw.dat$code == j),
                     colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])],
             na.rm=TRUE))
  }
}
#this ifelse statement was necessary due to 
#the fact that many dates did not have a row in which
#code was equal to j


############
# POPULATE WITH N MOSQUITOES PER TRAP 
############
for (i in as.numeric(gsub("site", "",colnames(raw.dat[which(grepl("site", 
                                                                  colnames(raw.dat)))])))){
  ts[,paste0("site", i)] <- NA
}

for (i in ts$date){
  for (j in as.numeric(gsub("site", "",colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])))){
    ts[which(ts$date == i),
       paste0("site",j)] <-
      sum(raw.dat[which(raw.dat$date == i),
                  paste0("site", j)], na.rm=TRUE)
    
  }
}


############
# POPULATE WITH EACH KIND OF MOSQUITO AT EACH TRAP
############

for (i in unique(sort(raw.dat$code))){
  for (j in as.numeric(gsub("site", "",colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])))){
    ts[,paste0("code", i, "site", j)] <- NA    
  }
}

for (i in ts$date){
  for (j in as.numeric(gsub("site", "",colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])))){
    for (k in unique(sort(raw.dat$code))){
      
      ts[which(ts$date == i),
         paste0("code", k, "site", j)] <-
        
        ifelse(nrow(raw.dat[which(raw.dat$date == i &
                                    raw.dat$code == k),]) == 0,
               0,
               sum(raw.dat[which(raw.dat$date == i &
                                   raw.dat$code == k),
                           paste0("site", j)],  na.rm=TRUE)
        )
    } 
  }
}

#############
# READ IN DISEASE INFORMATION
#############
if(Sys.info()['sysname'] == 'Windows'){
  vec <- read.csv("C:/Users/BrewJR/Documents/fdoh/public/mosquito/benVectors.csv", sep=";")
} else{
  vec <- read.csv("/home/joebrew/Documents/fdoh/public/mosquito/benVectors.csv", sep=";")
}

colnames(vec) <- c("code", "name", "wnv", "slev", "eeev", "chik", "malaria", "dengue", "yellow")

############
# POPULATE WITH N OF VECTORS PER DAY 
############

#CREATE NEW COLUMNS FOR EACH VECTOR IN raw.dat
for (i in colnames(vec[which(grepl("code|name", colnames(vec))==FALSE)])){
  raw.dat[,i] <- NA
}

#POPULATE BOOLEAN DISEASE COLUMNS IN raw.dat
for (i in unique(sort(raw.dat$code))){
  for (j in rev(colnames(vec))[1:7]){
    raw.dat[which(raw.dat$code == i), j] <-
      vec[which(vec$code == i),j]
  }
}

#CREATE NEW COLUMNS FOR EACH VECTOR IN ts
for (i in colnames(vec[which(grepl("code|name", colnames(vec))==FALSE)])){
  ts[,i] <- NA
}

#POPULATE ts WITH TOTAL NUMBER OF EACH DISEASE TYPE
for (i in ts$date){
  for (j in rev(colnames(ts))[1:7]){
    ts[which(ts$date == i), j] <-
      sum(raw.dat[which(raw.dat$date == i &
                          raw.dat[,j] == TRUE),
                  colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])],
          na.rm=TRUE)  
  }
}

###################
# POPULATE ts WITH DISEASE TYPE AT EACH SITE
###################

for (i in rev(colnames(vec))[1:7]){
  for (j in as.numeric(gsub("site", "",colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])))){
    ts[,paste0(i, "site", j)] <- NA    
  }
}

for (i in ts$date){
  for (j in as.numeric(gsub("site", "",colnames(raw.dat[which(grepl("site", colnames(raw.dat)))])))){
    for (k in rev(colnames(vec))[1:7]){
      
      ts[which(ts$date == i),
         paste0(k, "site", j)] <-
        
        ifelse(nrow(raw.dat[which(raw.dat$date == i &
                                    raw.dat[,k] == TRUE),]) == 0,
               0,
               
               
               sum(raw.dat[which(raw.dat$date == i & 
                                   raw.dat[,k] == TRUE), 
                           paste0("site", j) ], na.rm=TRUE)
        )
    } 
  }
}



############
# ADD A year COLUMN TO TS
############
ts$year <- as.numeric(format(ts$date, format="%Y"))


############
# ADD A day COLUMN TO TS
############
ts$day <- as.numeric(format(ts$date, format="%j"))

############
# WRITE A CSV FOR CESAR ESCOBAR (24 June 2014)
############
# setwd("C:/Users/BrewJR/Documents/fdoh/public/mosquito/cesar")
# write.csv(ts, "time_series.csv")

############
# ADD A COLOR TO EACH ROW OF TS (BY YEAR)
############
library(RColorBrewer)
yearcols <- brewer.pal(length(unique(sort(ts$year))), "Set3")

ts$col <- NA
for (i in 1: length(unique(sort(ts$year)))){
  ts$col[which(ts$year == unique(sort(ts$year))[i])] <-
    yearcols[i]
  
}


############
# CREATE A SIMPLE DF LINKING $code TO MOSQUITO NAME
# ############
# id <- as.data.frame(unique(sort(raw.dat$code)))
# colnames(id) <- "code"
# 
# id$name <- NA
# for (i in id$code){
#   id$name[which(id$code == i)] <-
#     as.character(raw.dat$mosquito[which(raw.dat$code == i)])[1]
# }
# 
# id$name <- gsub("Cx coronator|[()]", "", id$name)
# write.csv(id, "C:/Users/BrewJR/Documents/fdoh/public/mosquito/id.csv")

#############
# READ IN MOSQUITO ID INFO
#############
#id <- read.csv("C:/Users/BrewJR/Documents/fdoh/public/mosquito/id.csv")


############
# READ IN TRAP LOCATION DATA

############

if(Sys.info()['sysname'] == 'Windows'){
  loc <- read.csv("C:/Users/BrewJR/Documents/fdoh/public/mosquito/traplatlong.csv")
} else {
  loc <- read.csv("/home/joebrew/Documents/fdoh/public/mosquito/traplatlong.csv")
}


############
# READ IN RAINFALL / TEMP DATA FROM 2008-13
############

if(Sys.info()['sysname'] == 'Windows'){
   rt <- read.csv("C:/Users/BrewJR/Documents/fdoh/public/mosquito/rainAndTemp/rainAndTempUpdated.csv")
} else{
  rt <- read.csv("/home/joebrew/Documents/fdoh/public/mosquito/rainAndTemp/rainAndTempUpdated.csv")
}


rt$date <- as.Date(rt$date, format="%Y-%m-%d")

##############
# ADD MORE RECENT RAINFALL
##############
library(weatherData)
#getWeatherForDate("GNV", "2014-04-14")
newTemp <- getSummarizedWeather("GNV", start_date = max(rt$date) + 1, 
                                end_date = Sys.Date() - 1,
                                opt_custom_columns = TRUE,
                                custom_columns = c(2,4,20))

############
#CLEAN UP newTemp A BIT SO AS TO MERGE WITH RT
############
newTemp$date <- as.Date(gsub(" CDT", "", newTemp$Date), format="%Y-%m-%d")
newTemp$Date <- NULL
newTemp$rain <- newTemp$PrecipitationIn
newTemp$rain <- as.numeric(newTemp$rain)
newTemp$rain[which(is.na(newTemp$rain))] <- 0
newTemp$PrecipitationIn <- NULL
newTemp$minTemp <- newTemp$Min_TemperatureF
newTemp$Min_TemperatureF <- NULL
newTemp$maxTemp <- newTemp$Max_TemperatureF
newTemp$Max_TemperatureF <- NULL


############
# PREPARE rt FOR MERGING BY REMOVING EXTRA DATES
############
rt <- rt[which(rt$date < min(newTemp$date)),]

#########################################
# MERGE newTemp WITH  rt
#########################################
rt <- rbind.fill(rt, newTemp)

#If you run twice on one day, you get non-consequential errors

#########################################
#ADD CUMULATIVE RAINFALL TO CORRESPONDING COLUMNS
#########################################
rowsToAdd <- as.numeric(row.names(rt[which(is.na(rt$rain20.39)),]))
rowsToAdd <- rowsToAdd[rowsToAdd>30]

for (j in colnames(rt)[grepl("rain", colnames(rt))][-1]){
  for (i in rowsToAdd){
    rt[i,j] <-
      sum(rt$rain[which(rt$date <= rt$date[i-min(as.numeric(unlist(strsplit(gsub("rain", "", j), ".", fixed=TRUE))))] &
                          rt$date >= rt$date[i-max(as.numeric(unlist(strsplit(gsub("rain", "", j), ".", fixed=TRUE))))])], na.rm=TRUE)
  }
}

#########################################
#ADD MINIMUM MINTEMP FOR EACH MINTEMP COLUMN COLUMN
#########################################
for (j in colnames(rt)[grepl("minTemp", colnames(rt))][-1]){
  for (i in rowsToAdd){
    rt[i,j] <-
      min(rt$minTemp[which(rt$date <= rt$date[i-min(as.numeric(unlist(strsplit(gsub("minTemp", "", j), ".", fixed=TRUE))))] &
                             rt$date >= rt$date[i-max(as.numeric(unlist(strsplit(gsub("minTemp", "", j), ".", fixed=TRUE))))])], na.rm=TRUE)
  }
}

#########################################
# WRITE CSV WITH UPDATED RAIN AND TEMP INFO
#########################################

if(Sys.info()['sysname'] == 'Windows'){
  write.csv(rt, "C:/Users/BrewJR/Documents/fdoh/public/mosquito/rainAndTemp/rainAndTempUpdated.csv")
} else {
  write.csv(rt, "/home/joebrew/Documents/fdoh/public/mosquito/rainAndTemp/rainAndTempUpdated.csv")
}


##########################################
# DEFINE MOST RECENT TRAP DATE
##########################################
recent <- max(ts$date[which(is.na(ts$tot)==FALSE)])

#########################################
# ADD MOSQUITO DATA TO RT
#########################################
#master <- merge(ts, rt[which(rt$date != recent),], by="date")
master <- merge(ts, rt, by="date")

#########################################
# CREATE A MOSQUITOES PER TRAP COLUMN
#########################################
master$totPer <- master$tot / master$nTraps

#########################################
# CREATE A DAY OF YEAR COLUMN
#########################################
master$day <- as.numeric(format(master$date, format="%j"))

#########################################
# CREATE A MOST RECENT TRAP COLUMN
#########################################
master$mostRecent <- NA
for (i in 2:nrow(master)){
  master$mostRecent[i] <-
    ifelse(master$date[i] - master$date[i-1] > 20,
           NA,
           master$totPer[i-1])
}

#########################################
#### CREATE A VECTOR OF ALL THE POSSIBLE COMBINATIONS OF MIN TEMP RANGES AND RAINFALL RANGES
#########################################

rainPosibs <- rep(colnames(master)[grepl("rain", colnames(master))][-1],
                  length(colnames(master)[grepl("minTemp", colnames(master))][-1]))
# minTempPosibs <- sort(rep(colnames(master)[grepl("minTemp", colnames(master))][-1],
#                           length(colnames(master)[grepl("rain", colnames(master))][-1])))
# posibs <- paste0(rainPosibs, "AND", minTempPosibs)


#########################################
#CREATE A DATA FRAME FROM MY POSIBS VECTOR
# THIS IS WHERE I'LL PUT MY MODEL QUALITY INDICATORS
#########################################
#########################################
#pred <- as.data.frame(posibs)
predRain <- as.data.frame(unique(sort(rainPosibs)))
colnames(predRain) <- "posibs"
#predTemp <- as.data.frame(unique(sort(minTempPosibs)))
#colnames(predTemp) <- "posibs"

# RANDOM FOREST
library(randomForest)
library(dplyr)
predictors <- master[,which(grepl('rain|Temp|date', colnames(master)))]
for (j in 1:ncol(predictors)){
  predictor <- predictors[,j]
  predictor[which(is.infinite(predictor))] <- NA
  predictors[,j] <- predictor
}
predictors <- predictors[complete.cases(predictors),]

right <- master[,c('date', 'totPer')]

predictors <- left_join(predictors, right)
fit <- randomForest(totPer ~ .,
                    data = predictors,
                    na.action = na.omit)

predictors$predicted <- predict(fit, predictors)

plot(predictors$date, predictors$predicted, type = 'l')
lines(predictors$date, predictors$totPer, col = 'red')



# Homespun #####
folds <- 10
# randomize order of rows
predictors <- predictors[sample(1:nrow(predictors), nrow(predictors), replace = FALSE),]

# assign fold number
predictors$fold <- sample(1:folds, nrow(predictors), replace = TRUE)

# place to put rmses
rmses <- rep(NA, folds)
r2s <- rep(NA, folds)
oos_errors <- vector(mode = 'numeric', length = 0)
for (i in 1:folds){
  #subset
  sub_data <- predictors[which(predictors$fold != i),]
  hold_out <- predictors[which(predictors$fold == i),]
  #train
  fit <- randomForest(totPer ~ .,
                      data = sub_data,
                      na.action = na.omit)
  # predict
  hold_out$predicted <- predict(fit, hold_out)
  
  # rmse
  rmse <- sqrt(mean((hold_out$totPer - hold_out$predicted)^2))
  # put in place
  rmses[i] <- rmse
  
  # r-squared
  r2 <- mean(fit$rsq)
  r2s[i] <- r2
  
  # out of sample error
  oos_error <- hold_out$totPer - hold_out$predicted
  oos_errors <- c(oos_errors, oos_error)
  
  print(i)
}


#####
# ROLLING PREDICTION BY TIME
#####
predictors <- predictors[order(predictors$date),]
predictors$rolling <- NA

# Get rid of cheaters / bad
predictors$predicted <- NULL
predictors$fold <- NULL

# No NAs
#predictors <- predictors[complete.cases(predictors),]


for (i in 15:nrow(predictors)){
  # Subset
  sub_data <- predictors[1:(i-1),]
  
  # remove any column with NAs
  flags <- rep(FALSE, ncol(sub_data))
  
  for (j in 1:ncol(sub_data)){
    if(NA %in% sub_data[,j]){
      flags[j] <- TRUE
    }
  }
  
  sub_data <- sub_data[,which(!flags)]
  
  
  #train
  fit <- randomForest(totPer ~ .,
                      data = sub_data,
                      na.action = na.omit)
  
  # predict_on
  predict_on <- predictors[i,]
  # predict
  predictors$rolling[i] <- predict(fit, predict_on)
  
  print(paste0(i, ' of ', nrow(predictors), '|||', ncol(sub_data), ' columns'))
  
}


#####
# OVERALL MODEL
#####
fit <- randomForest(totPer ~ .,
                    data = predictors,
                    na.action = na.omit)

predictors$predicted <- predict(fit, predictors)

plot(predictors$date, predictors$totPer, type = 'l',
     col = adjustcolor('black', alpha.f = 0.8),
     lwd = 1,
     lty = 3,
     xlab = 'Date',
     ylab = 'Mosquitoes per trap')

# Add basic fit
lines(predictors$date, predictors$predicted, 
      col = adjustcolor('red', alpha.f = 0.5),
      lwd = 2)

# Add rolling fit
lines(predictors$date, predictors$rolling,
      col = adjustcolor('darkgreen', alpha.f = 0.5),
      lwd = 2)

legend('topleft',
       lty = c(3,1,1),
       col = adjustcolor(c('black', 'red', 'darkgreen'), alpha.f =0.6),
       lwd = c(1,2,2),
       legend = c('Observed', 'Full model', 'Naive model'))

#####
# Polygon density
#####
poly_den <- function(x,
                     plot_hist = FALSE,
                     col = 'darkred',
                     add = FALSE,
                     ...){
  
  if(plot_hist){
    hist(x, freq = FALSE,
         main = NA, xlab = NA, ylab = NA)
  } else{
    if(!add){
      hist(x, border = 'white',freq = FALSE,
           ...)
    }
    
  }
  
  xl <- density(x)
  polygon(x = c(xl$x, rev(xl$x)), 
          y = c(xl$y, rep(0, length(xl$y))), 
          col = adjustcolor(col, alpha.f = 0.5)) 
}


poly_den(rmses, col = 'yellow', 
         #xlim = c(0,1),
         xlim = c(0, 300),
         xlab = 'RMSE', main = 'Distribution of 10-fold RMSE')
grid()
abline(v = mean(rmses), col = 'red')

poly_den(r2s, col = 'yellow', xlim = c(0, 1),
         xlab = 'R-squared', main = 'Distribution of 10-fold R-squared')
grid()
abline(v = mean(r2s), col = 'red')

hist(oos_errors, xlab = 'Out of sample error', main = 'Out of sample error',
     angle = 45, density = 25, col = adjustcolor('darkblue', alpha.f = 0.6), 
     freq = FALSE, 
     #ylim = c(0, 0.01), 
     breaks = 15)
poly_den(oos_errors, add = T, col = adjustcolor('black', alpha.f = 0.6))
summary(oos_errors)
sd(oos_errors)




#########################################
#Test the r-squared for each column (RAIN ONLY)
#########################################
predRain$r.squared <- NA
predRain$adj.r.squared <- NA
predRain$sigma <- NA

# #GOOD
# for (i in predRain$posibs){
#   mylm <- summary(lm(master[,"totPer"] ~
#                        master[,i] +
#                        I(master[,i]^2)))
#   
#   predRain$r.squared[which(predRain$posibs == i)] <-
#     mylm$r.squared
#   
#   predRain$adj.r.squared[which(predRain$posibs == i)] <-
#     mylm$adj.r.squared
#   
#   predRain$sigma[which(predRain$posibs == i)] <-
#     mylm$sigma
# }


# EXPERIMENTAL
for (i in predRain$posibs){
  mylm <- summary(lm(master[,"totPer"] ~
                       I(master[,i]^2) +
                       master$mostRecent + 0))
  
  predRain$r.squared[which(predRain$posibs == i)] <-
    mylm$r.squared
  
  predRain$adj.r.squared[which(predRain$posibs == i)] <-
    mylm$adj.r.squared
  
  predRain$sigma[which(predRain$posibs == i)] <-
    mylm$sigma
}


# 
# #########################################
# #Test the r-squared for each column (TEMP ONLY)
# #########################################
# predTemp$r.squared <- NA
# predTemp$adj.r.squared <- NA
# predTemp$sigma <- NA
# for (i in 1:length(predTemp$posibs)){
#   mylm <- summary(lm(master[,"totPer"] ~
#                        master[,minTempPosibs[i]]
#   ))
#   predTemp$r.squared[i] <- mylm$r.squared
#   predTemp$adj.r.squared[i] <- mylm$adj.r.squared
#   predTemp$sigma[i] <- mylm$sigma
#   
# }
# 
# #########################################
# #Test the r-squared for each column (RAIN AND TEMP)
# #########################################
# pred$r.squared <- NA
# pred$adj.r.squared <- NA
# pred$sigma <- NA
# for (i in 1:length(pred$posibs)){
#   mylm <- summary(lm(master[,"totPer"] ~
#                        master[,unlist(strsplit(posibs, "AND")[i])[1]] +
#                        master[,unlist(strsplit(posibs, "AND")[i])[2]]
#   ))
#   pred$r.squared[i] <- mylm$r.squared
#   pred$adj.r.squared[i] <- mylm$adj.r.squared
#   pred$sigma[i] <- mylm$sigma
#   
# }

#########################################
#Select best predicition model
#########################################
#pred2 <- rbind(pred, predTemp, predRain)
# 
# pred2 <- pred2[rev(order(pred2$adj.r.squared)),] #or should I order by Sigma?
# 
# head(pred2)
# 
# pred3 <- pred2[order(pred2$sigma),]
# pred3 <- pred3[which(is.na(pred3$sigma) == FALSE),]
# best <- as.character(pred2$posibs[which(pred2$adj.r.squared == 
#                                           max(pred2$adj.r.squared))])  
# 
# bestModel <- lm(master$totPer ~ master[,best])
# names(bestModel$coefficients)[2] <-
#   as.character(best)
# summary(bestModel) # IT LOOKS LIKE MIN TEMP DOESN'T EVEN MATTER

#BASICALLY, my best bets are 5.25 (r.sq = .39)
# and 20.40 (r.sq = .38)

predRain <- predRain[rev(order(predRain$adj.r.squared)),]
head(predRain)

#good
# best <- lm(totPer ~ rain17.36 + 
#              I(rain17.36^(2)) + 0, data = master)


#############
# RANDOM FOREST MODEL (September 5, 2014)
#############
# library(randomForest)
# 
# my_rf <- randomForest(totPer ~ ., na.action = na.omit, data = master )

##############
# FIX MASTER INF STUFF
##############
master$minTemp5.10[which(!is.finite(master$minTemp5.10))] <- NA

############## 
# MODELING
##############
train <- master[which(master$date < "2014-01-01" ),
                grepl("rain|Temp|totPer", colnames(master)) &
                  # grepl("5|totPer", colnames(master)) &
                  #  grepl("1|totPer", colnames(master)) &
                  !grepl("mostRecent", colnames(master))]
test <- master[which(master$date >= "2014-01-01"),
               grepl("rain|Temp|totPer", colnames(master)) &
                 # grepl("5|totPer", colnames(master)) &
                 # grepl("1|totPer", colnames(master)) &
                 !grepl("mostRecent", colnames(master))]


###########
# GAM
###########
library(mgcv)
modformchar <- paste0("totPer ~", 
                      paste(colnames(train[which(!grepl("totPer", colnames(train)))]), collapse = "+"),
                      " + 0")
modform <- as.formula(modformchar)

modform2 <- totPer ~ s(rain5.19) + s(rain20.40) #s(minTemp5.10) +s(rain15.29)  

fit <- gam(modform2, data = train, direction = "backward")

x <- predict(fit, newdata = test)

plot(x, test$totPer)
vis.gam(fit, view=c("rain5.19", "rain20.40"), type="response",
        plot.type = "contour", color="topo",
        n.grid=500, too.far=1)

master$gam <- predict(fit, newdata = master)
plot(master$gam, master$totPer)
###########
# RANDOM FOREST
###########
library(rpart)
library(randomForest)
fit <- rpart(modform, data = train, control=rpart.control(minsplit=10, cp=0.00001))

fit <- randomForest(modform, data = train)

### PREDICT
test$x <- predict(fit, newdata = test)
plot(test$x, test$totPer)

master$x <- predict(fit, newdata = master)
plot(master$x, master$totPer)

lines(0:2000, 0:2000)

#experimental
best <- lm(totPer ~ 
             I(rain15.29^2) +
             mostRecent + 
             0, 
           data=master)

summary(best)

coefTemp <- 
  (master$rain15.29^2) +
  master$mostRecent 

# coefTemp <-master$rain17.36 + 
#   (master$rain17.36^2)

plot(coefTemp, master$totPer)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#write.csv(pred, "C:/Users/BrewJR/Documents/fdoh/public/mosquito/rainAndTemp/pred.csv")

#########################################
# USING BEST LOG MODEL, MAKE A PREDICTEDLOG COLUMN IN master
#########################################
#predict(bestModel, master[which(master$date )])

#master$predicted<- exp(-1.121293 + (master$rain17.37*.200055) + (master$minTemp14.32*0.074435))

#########################################
# MAKE A DF OF FUTURE DATES
#########################################

futureDates <- as.data.frame(seq(max(master$date)-1, max(rt$date)+17, 1))
colnames(futureDates) <- "date"

#futureDates <- rbind.fill(rt, futureDates)
#futureDates <- futureDates[which(futureDates$date >= Sys.Date()-40),]

futureDates$rain15.29 <- NA
for (i in 1:nrow(futureDates)){
  futureDates$rain15.29[i] <-
    sum(rt$rain[which(rt$date >= futureDates$date[i] - 40 &
                        rt$date <= futureDates$date[i] - 20)],
        na.rm=TRUE)
}


########## ADD MOST RECENT TRAPPING TO futureDates
futureDates$mostRecent <- NA
for (i in 1:nrow(futureDates)){
  futureDates$mostRecent[i] <-
    ifelse(futureDates$date[i] - max(master$date) > 20,
           NA,
           master$totPer[which(master$date == max(master$date))])  
}


#########################################
# ADD A COLUMN OF PREDICTED
##########################################
master$predicted <- predict.lm(object = best,
                               newdata = master,
                               interval="none",
                               level=0.7)
master$lwr <- predict.lm(object = best,
                         newdata = master,
                         interval="prediction",
                         level=0.7)[, "lwr"]
master$upr <- predict.lm(object = best,
                         newdata = master,
                         interval="prediction",
                         level=0.7)[, "upr"]

##########################################
# PREDICT FOR NEXT 20 DAYS
##########################################
myPredictions <- cbind(futureDates, predict(best, 
                                            futureDates, 
                                            interval="prediction",
                                            level=0.7))
myPredictions$predicted <- myPredictions$fit

############################################
# REMOVE PREDICTION FOR MOST RECENT TRAP DATE
############################################
myPredictions <- myPredictions[which(myPredictions$date >
                                       max(master$date)),]
# library(stats)
# myPredictions <- cbind(futureDates, predict(loess(best, span=1), 
#         futureDates, 
#         interval="confidence",
#         level=0.95))
#########################################
# COMBINE master AND myPredictions
#########################################
master <- rbind.fill(master, myPredictions)



#########################################
# PLOT PREDICTED VS OBSERVED (FUTURE)
#########################################
master2014 <- master[which(master$date > "2014-01-01"),]
plot(master2014$date, master2014$predicted, type="n", ylim=c(0,400),
     xlab="Date",
     ylab="Mosquitoes per trap")
lines(master2014$date, master2014$totPer, col=adjustcolor("black", alpha.f=0.4),
      lwd=3)
lines(master2014$date[which(master2014$date >= max(master$date[which(is.na(master$totPer)==FALSE)]))], 
      master2014$predicted[which(master2014$date >= max(master$date[which(is.na(master$totPer)==FALSE)]))],
      col=adjustcolor("darkred", alpha.f=0.4), lwd=5)
#lines(master2014$date,
#      master2014$predicted)

#########################################
# PLOT PREDICTED VS OBSERVED (PAST)
#########################################
plot(master$date, master$predicted, type="n",
     ylim=c(0,1500))
lines(master$date, master$predicted,
      col=adjustcolor("black", alpha.f=0.4),
      lwd=3)
lines(master$date, master$totPer,
      col=adjustcolor("red", alpha.f=0.4),
      lwd=3)

plot(master$date, log(master$predicted), type="n")
lines(master$date, log(master$predicted),
      col=adjustcolor("black", alpha.f=0.4))
lines(master$date, log(master$totPer),
      col=adjustcolor("red", alpha.f=0.4))


########################
# HISTORICAL RAINFALL
########################
plot(rt$date, 
     rt$rain, 
     type="p", 
     col=adjustcolor("black", alpha.f=0.2), 
     pch=16,
     xlab="Date",
     ylab="Daily rainfall (in.)",
     cex.axis=0.6)



############
# PLOT OVERLAPPED TIME SERIES 
############
plot(ts$day, ts$tot/ts$nTraps, 
     type="n",
     xlab="Day",
     ylab="Mosquitoes")

#ADD LINES
for (i in unique(sort(ts$year))){
  lines(ts$day[which(ts$year == i)],
        ts$tot[which(ts$year == i)] / 
          ts$nTraps[which(ts$year == i)],
        col=adjustcolor(ts$col[which(ts$year == i)], alpha.f=0.5),
        lwd=3)
  text(x=ts$day[which(ts$year == i &
                        ts$tot / ts$nTraps == 
                        max(ts$tot[which(ts$year == i)]/ 
                              ts$nTraps[which(ts$year == i)]))],
       y=max(ts$tot[which(ts$year == i)] / 
               ts$nTraps[which(ts$year == i)]),
       labels=i,
       col=adjustcolor(ts$col[which(ts$year == i)], alpha.f=0.7)[1])
  Sys.sleep(1)
}

################
# PLOT MOSQUITOES PER TRAP BY SPECIES
################
tsCode <- ts[,which(grepl("code|date|day|nTraps", colnames(ts)) & 
                      grepl("site", colnames(ts)) == FALSE)]

par(mfrow=c(5,4))
par(mar=c(4,4,1,1))
par(oma=c(0,0,0,0))
for (i in unique(sort(raw.dat$code))[1:20]){
  plot(tsCode$date,
       tsCode[,paste0("code", i)]/ tsCode$nTraps,
       type="l",
       col=adjustcolor("red",alpha.f=0.4),
       main=vec$name[which(vec$code == i)],
       cex.axis=1,
       cex.main=0.8,
       xlab="Year",
       ylab="Mosquitoes")
  points(tsCode$date,
         tsCode[,paste0("code",i)]/ tsCode$nTraps,
         pch=16,
         col=adjustcolor("red", alpha.f=0.4),
         cex=0.5)
}



par(mfrow=c(5,4))
par(mar=c(4,4,1,1))
par(oma=c(0,0,0,0))
for (i in unique(sort(raw.dat$code))[21:33]){
  plot(tsCode$date,
       tsCode[,paste0("code", i)]/ tsCode$nTraps,
       type="l",
       col=adjustcolor("red",alpha.f=0.4),
       main=vec$name[which(vec$code == i)],
       cex.axis=1,
       cex.main=0.8,
       xlab="Year",
       ylab="Mosquitoes")
  points(tsCode$date,
         tsCode[,paste0("code",i)]/ tsCode$nTraps,
         pch=16,
         col=adjustcolor("red", alpha.f=0.4),
         cex=0.5)
}
# 
# plot(x=tsCode$date, 
#      y=log(tsCode$code2/
#        ts$nTraps), 
#      type="n",
#      xlab="Day",
#      ylab="Mosquitoes",
#      ylim=c(0,7))
# for (i in colnames(tsCode[which(grepl("code", colnames(tsCode)))])){
#   lines(tsCode$date,
#         log(tsCode[,i]/ tsCode$nTraps),
#         col=adjustcolor("black", alpha.f=0.2))
#   text(x=tsCode$date[which(tsCode[,i]/tsCode$nTraps == max(tsCode[,i])/tsCode$nTraps)],
#        y=max(tsCode[,i]/tsCode$nTraps),
#        labels=id$name[which(id$code == as.numeric(gsub("code", "", i)))],
#        col=adjustcolor("black", alpha.f=0.2),
#        cex=0.8)
#   Sys.sleep(1)
# }


##############
# PLOT FOR EACH TRAP
##############
tsSite <- ts[,which(grepl("nTraps|day|date|site", colnames(ts)) &
                      grepl("code", colnames(ts))==FALSE)]

par(mfrow=c(5,2))
par(mar=c(4,4,2,1))
par(oma=c(0,0,0,0))
for (i in as.numeric(gsub("site", "",colnames(tsSite[which(grepl("site", colnames(tsSite)))])))){
  plot(ts$date,
       ts[,paste0("site", i)],
       xlab="Year",
       ylab="Mosquitoes",
       main=loc$name[which(loc$Trap == i)],
       col=adjustcolor("darkred", alpha.f=0.6),
       pch=16,
       cex=0.5)
  lines(ts$date,
        ts[,paste0("site", i)],
        col=adjustcolor("darkred", alpha.f=0.6))
}
############
# MAP THE TRAPS
############
library(maps)
library(rJava)
library(OpenStreetMap)
library(rgdal)
library(maps)
library(RColorBrewer)

#Assorted colors for trap legend
#display.brewer.all()

trapclr <- brewer.pal(10, "Set3")

par(mfrow=c(1,1))
#SIMPLE 

totPerSiteRecent <- as.numeric(unlist(master[which(master$date == 
                                                     recent),
                                             colnames(master[which(grepl("site", colnames(master)) & 
                                                                     nchar(colnames(master)) < 7)])]))

totPerSiteNormal <- apply(master[which(master$date < recent),
                                 colnames(master[which(grepl("site", colnames(master)) & 
                                                         nchar(colnames(master)) < 7)])],
                          2,
                          sum)



#WATERCOLOR MAP
wcMap <- openmap(c(30, -82.65), c(29.4,-82.0),
                 type="stamen-watercolor")
wcMap <- openproj(wcMap, projection = "+proj=longlat")
plot(wcMap, raster=TRUE)
#plot(wcMap)

alachuacol <- NA
countycols <- rep(adjustcolor("black", alpha.f=0.2), 67)
countycols <- rep("white", 67)

map("county", "florida", 
    fill=TRUE, 
    col=c(alachuacol,countycols),
    xlim=c(-82.75,-82), 
    ylim=c(29.4,30),
    border=NA, 
    add=T)

#ADD TRAP POINTS
points(loc$long, loc$lat,
       pch=16,
       col=adjustcolor("darkred", alpha.f=0.6),
       cex=totPerSite^(1/3))
# text(loc$long, loc$lat,
#      labels=loc$name,
#      col="darkgrey",
#      cex=totPerSite^(1/8)/3,
#      pos=3)

# legend(x="topright",
#        pch=16,
#        col=adjustcolor(trapclr, alpha.f=0.8),
#        legend=loc$name,
#        cex=0.5,
#        ncol=2,
#        bg=adjustcolor("black", alpha.f=0.1),
#        pt.cex=2)

# 
# sabho <- openmap(c(40, -100), c(20, -80),
#                type="stamen-watercolor")
# plot(sabho)


##########################
# WNV MAP
##########################
par(mfrow=c(1,1))
countycols2 <- rep(adjustcolor("white", alpha.f=0.2), 67)
alachuacol2 <- adjustcolor("black", alpha.f=0.2)


map("county", "florida", 
    fill=TRUE, 
    col=c(alachuacol2,countycols2),
    xlim=c(-82.75,-82), 
    ylim=c(29.4,30),
    border=NA, 
    add=F)

temp <- master[,which(grepl("wnv" ,colnames(master)) & grepl("site", colnames(master)))]
temp <- as.data.frame(cbind(date=master$date, temp))
temp <- temp[which(temp$date == max(temp$date[which(is.na(temp$wnvsite1) == FALSE)])),]
temp <- as.numeric(temp[-1])


map("county", "florida", 
    fill=TRUE, 
    col=c(alachuacol2,countycols2),
    xlim=c(-82.75,-82), 
    ylim=c(29.4,30),
    border=NA, 
    add=F)

#ADD TRAP POINTS
points(loc$long, loc$lat,
       pch=16,
       col=adjustcolor("darkred", alpha.f=0.6),
       cex=temp^(1/3))


#

DiseaseMapFun <- function(disease){
  temp <- master[,which(grepl(disease ,colnames(master)) & 
                          grepl("site", colnames(master)))]
  temp <- as.data.frame(cbind(date=master$date, temp))
  temp <- temp[which(temp$date == 
                       max(temp$date[which(is.na(temp[,paste0(disease, "site1")]) == FALSE)])),]
  temp <- as.numeric(temp[-1])
  
  
  map("county", "florida", 
      fill=TRUE, 
      col=c(alachuacol2,countycols2),
      xlim=c(-82.75,-82), 
      ylim=c(29.4,30),
      border=NA, 
      add=F)
  title(main=toupper(disease), cex.main=0.8)
  
  #ADD TRAP POINTS
  points(loc$long, loc$lat,
         pch=16,
         col=adjustcolor("darkred", alpha.f=0.6),
         cex=temp^(1/4))
  
  legend(x="bottomleft",
         pch=16,
         col=adjustcolor("grey", alpha.f=0.6),
         pt.cex=c(10, 50, 200)^(1/4),
         legend=c(10,50,200), bty="n")
}


#PLOT DISEASE MAPS
par(mfrow=c(3,2))
par(mar=c(0,0,3,0))
par(oma=c(1,1,1,0))
DiseaseMapFun("wnv")
DiseaseMapFun("slev")
DiseaseMapFun("eeev")
DiseaseMapFun("dengue")
DiseaseMapFun("malaria")
DiseaseMapFun("chik")
#DiseaseMapFun("yellow")


############################

#######
# HISTOGRAMS OF HISTORICAL, OBSERVED, PREDICTED
########
library(arm)
bootRain <- rnorm(1000, 
                  mean=coef(best)[1], 
                  sd=se.coef(best)[1] * 
                    sqrt(nrow(master[which(is.na(master$totPer) == FALSE),])))
bootRecent <- rnorm(1000, 
                    mean=coef(best)[2], 
                    sd=se.coef(best)[2] * 
                      sqrt(nrow(master[which(is.na(master$totPer) == FALSE),])))
#Old way
#bootPred <- ((master$rain15.29[which(master$date == "2014-06-16")]^(2)*bootRain) +
#              (master$mostRecent[which(master$date == "2014-06-16")]*bootRecent))^(1/3)

#Experimental
bootPred <- ((mean(master$rain15.29[which(master$date > recent)], na.rm=T)^(2)*bootRain) +
               (mean(master$mostRecent[which(master$date > recent)], na.rm=T)*bootRecent))^(1/3)


breakNum <- 20
histCols <- colorRampPalette(adjustcolor(c("darkgreen", "darkred"), alpha.f=0.5))(1+ breakNum) 

nCols <- 30
col1 <- "darkgreen"
col2 <- "darkred"
alexCols <- colorRampPalette(c(col1, col2))(nCols)

#PLOT
myhist <- 
  hist(master$totPer[which(master$date < "2014-01-01")]^(1/3), 
       breaks=breakNum,
       xlab="Mosquitoes per trap (2008-13)",
       main=NA,
       col=histCols,
       border=NA,
       xaxt="n",
       freq=FALSE,
       ylim=c(0,.4))
axis(side=1,
     at=c(0, 100, 400, 1000, 1600)^(1/3),
     labels=c(0, 100, 400, 1000, 1600))
abline(v=master$totPer[which(master$date == recent)]^(1/3),
       col=adjustcolor("blue", alpha.f=0.2),
       lwd=2)
# text(x= master$totPer[which(master$date == recent)]^(1/3),
#      y= max(myhist$density),
#      labels = "Most recent \n collection",
#      pos=4,
#      col=adjustcolor("blue", alpha.f=0.7))
# # HIST OF PREDICTIONS
# hist(bootPred,
#   col=adjustcolor("black", alpha.f=0.2), add=TRUE,
#   breaks=breakNum,
#   freq=FALSE,
#   border=NA)
# #LINE OF PREDICTIONS
hist(bootPred,
     xaxt="n",
     border=NA,
     freq=FALSE,
     main=NA,
     xlab="Estimated mosquitoes per trap")
axis(side=1,
     at=c(0, 100, 400, 1000, 1600)^(1/3),
     labels=c(0, 100, 400, 1000, 1600))
lines(density(bootPred, adjust=2, na.rm=T), 
      lwd=3,
      col=adjustcolor("darkred", alpha.f=0.4))

#LEGEND
legend("topright",
       fill=c("darkgreen", NA, NA),
       col=c(NA, adjustcolor("blue", alpha.f=0.4), "black"),
       lty=c(NA, 1, 6),
       lwd=c(NA, 2, 1),
       legend=c("Historical observations",
                "Most recent collection",
                "2 week prediction"),
       bty="n",
       border=FALSE)

# # HIST OF PREDICTIONS
# hist(bootPred,
#   col=adjustcolor("black", alpha.f=0.2), add=TRUE,
#   breaks=breakNum,
#   freq=FALSE,
#   border=NA)
# #LINE OF PREDICTIONS


###################
# DISEASE PLOT
###################
DiseaseLinesFun <- function(color, disease){
  temp <- master#[which(is.na(master[,i]) == FALSE),]
  lines(temp$date, temp[,disease]^(1/3),
        col=adjustcolor(color, alpha.f=0.3),
        lwd=3)
  #   points(master$date, master[,disease]^(1/3),
  #          col=adjustcolor(color, alpha.f=0.5),
  #          pch=16)
}
par(mar=c(5,4,2,1))
par(oma=c(1,1,1,1))
plot(master$date, master$wnv^(1/3), type="n",
     yaxt="n",
     xlab="Date",
     ylab="Mosquitoes per trap", 
     ylim=c(0, max(master$wnv^(1/3), na.rm=TRUE)))
axis(side=2,
     at=c(10, 100, 1000, 5000)^(1/3),
     labels=c(10, 100, 1000, 5000))
DiseaseLinesFun("red", "wnv")
DiseaseLinesFun("darkgreen", "dengue")
DiseaseLinesFun("darkblue", "malaria")
DiseaseLinesFun("purple", "slev")
DiseaseLinesFun("brown", "eeev")
DiseaseLinesFun("darkorange", "chik")
DiseaseLinesFun("grey", "yellow")

legend(x="topleft",
       col=adjustcolor(c("red", "darkgreen", "darkblue", "purple", "brown", "darkorange", "grey"), alpha.f=0.3),
       lwd=3,
       legend=c("WNV",
                "Dengue", 
                "Malaria",
                "SLEV",
                "EEEV",
                "Chikungunya",
                "Yellow Fever"),
       bty="n")

############
# SURFACE MAPS (SPATIAL INTERPOLATION)
############
library(gstat)
library(geoR)
library(rgdal)
library(scatterplot3d)
library(RColorBrewer)

# Read in Alachua Countyboundary
setwd("C:/Users/BrewJR/Documents/fdoh/public/mosquito")
boundary <- readOGR("Alachua_Boundary", "Alachua_Boundary")

# Source the code for surface maps
source("SurfaceFun.R")
par(mfrow=c(1,1))
suppressWarnings(SurfaceFun("wnv"))
suppressWarnings(SurfaceFun("dengue"))
suppressWarnings(SurfaceFun("malaria"))
suppressWarnings(SurfaceFun("slev"))
suppressWarnings(SurfaceFun("eeev"))
suppressWarnings(SurfaceFun("yellow"))


############
# SAVE IMAGE FOR REPORTS
############
save.image("C:/Users/BrewJR/Documents/fdoh/public/mosquito/reports/2014-11-07/master.RData")

#SHOW FORECAST

cbind(as.character(master$date[which(is.na(master$predicted) == FALSE)]), 
      master$totPer[which(is.na(master$predicted) == FALSE)],
      master$predicted[which(is.na(master$predicted) == FALSE)], 
      master$lwr[which(is.na(master$predicted) == FALSE)], 
      master$upr[which(is.na(master$predicted) == FALSE)],
      master$rain5.11[which(is.na(master$predicted) == FALSE)])

x <- data.frame("date" = master$date[which(is.na(master$predicted) == FALSE)], 
                "totPer" = master$totPer[which(is.na(master$predicted) == FALSE)],
                "predicted" = master$predicted[which(is.na(master$predicted) == FALSE)], 
                "lwr" = master$lwr[which(is.na(master$predicted) == FALSE)], 
                "upr" = master$upr[which(is.na(master$predicted) == FALSE)],
                "rain5.11" = master$rain5.11[which(is.na(master$predicted) == FALSE)])
x

# library(car)
# best.boot <- Boot(best, R=1999)
# summary(best.boot)
# confint(best.boot, level=0.8, type="norm")
# hist(best.boot)
# boot.ci(best.boot)
############
#
############

##########KNN

oldPreds <- master[which(master$date >= "2013-01-01" &
                           is.na(master$totPer) == FALSE),
                   c("totPer", "rain5.19", "predicted")]

myTrain <- master[which(master$date < "2013-01-01" &
                          is.na(master$totPer) == FALSE &
                          is.na(master$rain5.19) == FALSE),
                  c("totPer", "rain5.19")]

myTrain$totPerFac <- factor(myTrain$totPer)
myTest <- master[which(master$date >= "2013-01-01" &
                         is.na(master$totPer) == FALSE),
                 c("totPer", "rain5.19")]
myTest$totPerFac <- factor(myTest$totPer)

library("class")
z <- 
  knn(train = myTrain,
      test = myTest,
      cl = myTrain$totPerFac,
      k=1,
      prob=TRUE)
x <- as.numeric(as.character(z))

par(mfrow=c(1,1))
plot(myTest$totPer, x)

plot(1:nrow(myTest), myTest$totPer, type="l", col="red") #observed
lines(1:nrow(myTest), x, col="darkgreen") #predicted by knn
#lines(1:nrow(myTest), oldPreds$predicted, col="blue") #predicted by current model

#quantify difference
lm.knn <- lm(myTest$totPer ~ x)
lm.old <- lm(myTest$totPer ~ oldPreds$predicted)

sum(lm.old$residuals^2) - sum(lm.knn$residuals^2)

barplot(c(sum(lm.old$residuals^2),sum(lm.knn$residuals^2)),
        names=c("old", "knn"))


#what about changing k?
plot(1:nrow(myTest), myTest$totPer, type="l", col="red") #observed

a <- as.data.frame(1:20)
colnames(a) <- "k"
a$r.squared <- NA
a$sls <- NA

for (i in a$k){
  z <- 
    knn(train = myTrain,
        test = myTest,
        cl = myTrain$totPerFac,
        k=i,
        prob=TRUE)
  x <- as.numeric(as.character(z))
  
  
  a$r.squared[which(a$k == i)] <-
    summary(lm(myTest$totPer~ x))$r.squared
  
  a$sls[which(a$k == i)] <- 
    sum(lm(myTest$totPer ~ x)$residuals^2)
  
  #lines(1:nrow(myTest), x, col="darkgreen") #predicted by knn
  #Sys.sleep(0.5)
}
#k=1 produces best result

a <- a[order(a$sls),]
a











kmeans(myTrain[,1:2])

#Naive Bayes
library(e1071)

nB_model <- naiveBayes(totPer ~ rain15.29 + mostRecent, data=master)
summary(nB_model)
nB_model$apriori
