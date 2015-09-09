#SET COLOR PALETTE
library(RColorBrewer)
#FOR SYNDROMS
symcols <- colorRampPalette(brewer.pal(8, "Set1"))(8)
#FOR MAP
#Create a vector for county colors
blackcols <- c("gray90",rep("gray75", 67))
symcols[6] <- "darkred"
#
# OVERALL SIMPLE SPREADHSEET#####
par(mfrow=c(1,1))
setwd("C:/Users/BrewJR/Documents/workingdirectory/mosquito")
trap <- read.csv("simple.csv", head=T, sep=",")


trap$daterec <- as.Date(trap$date, "%m/%d/%y")

z <- 143 #number of rows

trap2008 <- trap[which(trap$year==2008),] 
trap2009 <- trap[which(trap$year==2009),] 
trap2010 <- trap[which(trap$year==2010),] 
trap2011 <- trap[which(trap$year==2011),] 
trap2012 <- trap[which(trap$year==2012),] 
trap2013 <- trap[which(trap$year==2013),] 

#A label vector for plots
dates <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
dates2 <- c("Jun","Jul","Aug")

#Average vectors per trap for all entries
Avgvec <- sum(trap$vector)/z
Avgvec

#6 years on x axis
plot(trap$daterec, trap$vector, type="p")
lines(trap$daterec, trap$vector, lty=1, lwd=3)

#1 year ZOOMED IN on x axis
par(mfrow=c(1,1))
library(splines)
plot(trap$dayofyear, trap$vector, type="n",
     main="Vectors per trap (historical comparison)",
     cex.main=1.3,
     ylim=c(0,1050),
     xlim=c(150,280),
     xlab="Date",
     ylab="Average Vectors Per Trap",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, c(91,121,152,182,212,243,274,305), labels=dates, las=1, cex.axis=1)
#lines(loess.smooth(trap$dayofyear, trap$vector), col="black", lty=1, lwd=1)
legend(x="topright", ncol=2,
       legend=c("6 year average","2008","2009","2010","2011","2012","2013","Most recent collection"),
       lty=c(3,1,1,1,1,1,1,6),
       lwd=c(1,1,1,1,1,1,3,1),
       col=c("black",adjustcolor(symcols, alpha.f=0.5),"black"), bor=T, bty="n", cex=0.6)
abline(h=Avgvec, lty=3, col="black")
lines(x=rep(264, 800), y=1:800, lty=3, col=adjustcolor("darkred", alpha.f=0.4))


xspline(trap2008$dayofyear, trap2008$vector, shape=0.4,
        border=adjustcolor(symcols[1], alpha.f=0.5), lty=1, lwd=5)
xspline(trap2009$dayofyear, trap2009$vector, shape=0.4,
        border=adjustcolor(symcols[2], alpha.f=0.5), lty=1, lwd=5)
xspline(trap2010$dayofyear, trap2010$vector, shape=0.4,
        border=adjustcolor(symcols[3], alpha.f=0.5), lty=1, lwd=5)
xspline(trap2011$dayofyear, trap2011$vector, shape=0.4,
        border=adjustcolor(symcols[4], alpha.f=0.5), lty=1, lwd=5)
xspline(trap2012$dayofyear, trap2012$vector, shape=1,
        border=adjustcolor(symcols[5], alpha.f=0.5), lty=1, lwd=5)
#xspline(trap2013$dayofyear, trap2013$vectorjoe, shape=0.4,
#        border="blue", lty=1, lwd=2)

xspline(trap2013$dayofyear, trap2013$vectorjoe, shape=0.6,
        border=adjustcolor(symcols[6], alpha.f=0.8), lty=1, lwd=5)


#READ IN SPREADHSEETS######

jun04
jun12
jun18
jun25
jul04
jul10
jul17
jul24
jul30
aug06
aug13
aug20
sep03
sep10
sep17
sep24

setwd("C:/Users/BrewJR/Documents/workingdirectory/mosquito")
jun04 <- read.csv("2013-06-04.csv", skip=3, header=T, sep=",")
jun12 <- read.csv("2013-06-12.csv", skip=3, header=T, sep=",")
jun18 <- read.csv("2013-06-18.csv", skip=3, header=T, sep=",")
jun25 <- read.csv("2013-06-25.csv", skip=3, header=T, sep=",")
jul04 <- read.csv("2013-07-04.csv", skip=3, header=T, sep=",")
jul10 <- read.csv("2013-07-10.csv", skip=4, header=T, sep=",")
jul17 <- read.csv("2013-07-17.csv", skip=4, header=T, sep=",")
jul24 <- read.csv("2013-07-24.csv", skip=4, header=T, sep=",")
jul30 <- read.csv("2013-07-30.csv", skip=4, header=T, sep=",")
aug06 <- read.csv("2013-08-06.csv", skip=4, header=T, sep=",")
aug13 <- read.csv("2013-08-13.csv", skip=4, header=T, sep=",")
aug20 <- read.csv("2013-08-20.csv", skip=4, header=T, sep=",")
sep03 <- read.csv("2013-09-03.csv", skip=4, header=T, sep=",")
sep10 <- read.csv("2013-09-10.csv", skip=4, header=T, sep=",")
sep17 <- read.csv("2013-09-17.csv", skip=4, header=T, sep=",")
sep24 <- read.csv("2013-09-24.csv", skip=4, header=T, sep=",")


#change column names
names(jun04)[1] <- "site"
names(jun12)[1] <- "site"
names(jun18)[1] <- "site"
names(jun25)[1] <- "site"
names(jul04)[1] <- "site"
names(jul10)[1] <- "site"
names(jul17)[1] <- "site"
names(jul24)[1] <- "site"
names(jul30)[1] <- "site"
names(aug06)[1] <- "site"
names(aug13)[1] <- "site"
names(aug20)[1] <- "site"
names(sep03)[1] <- "site"
names(sep10)[1] <- "site"
names(sep17)[1] <- "site"
names(sep24)[1] <- "site"



names(jun04)[11] <- "total"
names(jun12)[11] <- "total"
names(jun18)[11] <- "total"
names(jun25)[11] <- "total"
names(jul04)[11] <- "total"
names(jul10)[12] <- "total"
names(jul17)[12] <- "total"
names(jul24)[12] <- "total"
names(jul30)[12] <- "total"
names(aug06)[12] <- "total"
names(aug13)[12] <- "total"
names(aug20)[12] <- "total"
names(sep03)[12] <- "total"
names(sep10)[12] <- "total"
names(sep17)[12] <- "total"
names(sep24)[12] <- "total"


#Create an X10 column for those dfs that dont have it
jun04$X10 <- 0
jun12$X10 <- 0
jun18$X10 <- 0
jun25$X10 <- 0
jul04$X10 <- 0

#delete the total row (check row number first for each df)
jun04 <- jun04[-34,]
jun12 <- jun12[-34,]
jun18 <- jun18[-34,]
jun25 <- jun25[-34,]
jul04 <- jul04[-34,]
jul10 <- jul10[-34,]
jul17 <- jul17[-34,]
jul24 <- jul24[-34,]
jul30 <- jul30[-34,]
aug06 <- aug06[-34,]
aug13 <- aug13[-34,]
aug20 <- aug20[-34,]
sep03 <- sep03[-34,]
sep10 <- sep10[-34,]
sep17 <- sep17[-34,]
sep24 <- sep24[-34,]



#delete the code column
jun04$Code.. <- NULL
jun12$Code.. <- NULL
jun18$Code.. <- NULL
jun25$Code.. <- NULL
jul04$Code.. <- NULL
jul10$Code.. <- NULL
jul17$Code.. <- NULL
jul24$Code.. <- NULL
jul30$Code.. <- NULL
aug06$Code.. <- NULL
aug13$Code.. <- NULL
aug20$Code.. <- NULL
sep03$Code.. <- NULL
sep10$Code.. <- NULL
sep17$Code.. <- NULL
sep24$Code.. <- NULL


#CREATE A NEW DATE COLUMN
jun04$date <- as.Date("2013-06-04")
jun12$date <- as.Date("2013-06-12")
jun18$date <- as.Date("2013-06-18")
jun25$date <- as.Date("2013-06-25")
jul04$date <- as.Date("2013-07-04")
jul10$date <- as.Date("2013-07-10")
jul17$date <- as.Date("2013-07-17")
jul24$date <- as.Date("2013-07-24")
jul30$date <- as.Date("2013-07-30")
aug06$date <- as.Date("2013-08-06")
aug13$date <- as.Date("2013-08-13")
aug20$date <- as.Date("2013-08-20")
sep03$date <- as.Date("2013-09-03")
sep10$date <- as.Date("2013-09-10")
sep17$date <- as.Date("2013-09-17")
sep24$date <- as.Date("2013-09-24")


#DELETE THE EXTRANEOUS ROWS (dfs that have more than 33)
jul24 <- jul24[-c(34,35),]
jul30 <- jul30[-c(34,35),]
aug06 <- aug06[-c(34,35),]
aug20 <- aug20[-c(34:90),]
sep03 <- sep03[-c(34:90),]
sep10 <- sep10[-c(34:90),]
sep17 <- sep17[-c(34:90),]
sep24 <- sep24[-c(34:90),]



#DELETE A FEW EXTRANEOUS COLUMNS AS WELL
aug20 <- aug20[,-c(13:21)]
sep10 <- sep10[,-c(13:21)]
sep17 <- sep17[,-c(13:21)]
sep24 <- sep24[,-c(13:21)]



#MERGE THE DATA
merged <- rbind(jun04,
                jun12,
                jun18,
                jun25,
                jul04,
                jul10,
                jul17,
                jul24,
                jul30,
                aug06,
                aug13,
                aug20,
                sep03,
                sep10,
                sep17, 
                sep24)


#CONVERT SOME STUFF TO NUMERIC
merged$X1 <- as.integer(merged$X1)
merged$X2 <- as.integer(merged$X2)
merged$X3 <- as.integer(merged$X3)
merged$X4 <- as.integer(merged$X4)
merged$X5 <- as.integer(merged$X5)
merged$X6 <- as.integer(merged$X6)
merged$X7 <- as.integer(merged$X7)
merged$X8 <- as.integer(merged$X8)
merged$X9 <- as.integer(merged$X9)
merged$X10 <- as.integer(merged$X10)
merged$total <- as.integer(merged$total)



#Rename the "site" column to be "mosq" instead
merged$mosq <- merged$site
merged$site <- NULL

#Create a vector column
library(car)
merged$vector <- merged$mosq
merged$wnv <- merged$mosq
merged$slev <- merged$mosq
merged$eeev <- merged$mosq
merged$chik <- merged$mosq
merged$malaria <- merged$mosq
merged$dengue <- merged$mosq


#Populate the vector column appropriately
merged$vector <- recode(merged$vector, "'Aedes albopictus'='vector'")
merged$vector <- recode(merged$vector, "'A. aegypti'='vector'")
merged$vector <- recode(merged$vector, "'A. vexans'='vector'")
merged$vector <- recode(merged$vector, "'O.  triseriatus'='vector'")
merged$vector <- recode(merged$vector, "'Anopheles crucians'='vector'")
merged$vector <- recode(merged$vector, "'A. quadrimaculatus'='vector'")
merged$vector <- recode(merged$vector, "'Culex erraticus'='vector'")
merged$vector <- recode(merged$vector, "'C. nigrapalpus'='vector'")
merged$vector <- recode(merged$vector, "'C.  nigrapalpus'='vector'")
merged$vector <- recode(merged$vector, "'C. nigripalpus'='vector'")
merged$vector <- recode(merged$vector, "'C. quinquefasciatus'='vector'")
merged$vector <- recode(merged$vector, "'C.  quinquefasciatus'='vector'")
merged$vector <- recode(merged$vector, "'C. restuans'='vector'")
merged$vector <- recode(merged$vector, "'C. salinarius'='vector'")
merged$vector <- recode(merged$vector, "'C.  salinarius'='vector'")
merged$vector <- recode(merged$vector, "'Culiseta  inornata'='vector'")
merged$vector <- recode(merged$vector, "'Culiseta  melanura'='vector'")
merged$vector <- recode(merged$vector, "'Culex  erraticus'='vector'")

merged$vector <- recode(merged$vector, "'Ochlerotatus atlanticus'='nonvector'")
merged$vector <- recode(merged$vector, "'O. canadensis'='nonvector'")
merged$vector <- recode(merged$vector, "'O. dupreei'='nonvector'")
merged$vector <- recode(merged$vector, "'O. fulvus pallens'='nonvector'")
merged$vector <- recode(merged$vector, "'O. infirmatus'='nonvector'")
merged$vector <- recode(merged$vector, "'O. mitchellae'='nonvector'")
merged$vector <- recode(merged$vector, "'O.  mitchellae'='nonvector'")
merged$vector <- recode(merged$vector, "'O.  solicitans'='nonvector'")
merged$vector <- recode(merged$vector, "'O. sollicitans'='nonvector'")
merged$vector <- recode(merged$vector, "'O.  taeniorhynchus'='nonvector'")
merged$vector <- recode(merged$vector, "'Coquillettidia perturbans'='nonvector'")
merged$vector <- recode(merged$vector, "'Coquillettidia  perturbans'='nonvector'")
merged$vector <- recode(merged$vector, "'Mansonia  titillans'='nonvector'")
merged$vector <- recode(merged$vector, "'M. dyari'='nonvector'")
merged$vector <- recode(merged$vector, "'Psorophora  ciliata'='nonvector'")
merged$vector <- recode(merged$vector, "'P. columbiae'='nonvector'")
merged$vector <- recode(merged$vector, "'P. ferox'='nonvector'")
merged$vector <- recode(merged$vector, "'P. howardii'='nonvector'")
merged$vector <- recode(merged$vector, "'Toxorhynchites rutilus'='nonvector'")
merged$vector <- recode(merged$vector, "'Uranotaenia  sapphirina'='nonvector'")
merged$vector <- recode(merged$vector, "'U.  lowii'='nonvector'")
merged$vector <- recode(merged$vector, "'Wyeomyia  mitchellii'='nonvector'")
merged$vector <- recode(merged$vector, "'W.  vanduzeei'='nonvector'")
merged$vector <- recode(merged$vector, "'Other'='nonvector'")
merged$vector <- recode(merged$vector, "'Other (Cx coronator)'='nonvector'")


#Populate the WNV column appropriately
merged$wnv <- recode(merged$wnv, "'Aedes albopictus'='wnv'")
merged$wnv <- recode(merged$wnv, "'A. aegypti'='nownv'")
merged$wnv <- recode(merged$wnv, "'A. vexans'='nownv'")
merged$wnv <- recode(merged$wnv, "'O.  triseriatus'='nownv'")
merged$wnv <- recode(merged$wnv, "'Anopheles crucians'='nownv'")
merged$wnv <- recode(merged$wnv, "'A. quadrimaculatus'='nownv'")
merged$wnv <- recode(merged$wnv, "'Culex erraticus'='wnv'")
merged$wnv <- recode(merged$wnv, "'Culex  erraticus'='wnv'")
merged$wnv <- recode(merged$wnv, "'C. nigrapalpus'='wnv'")
merged$wnv <- recode(merged$wnv, "'C.  nigrapalpus'='wnv'")
merged$wnv <- recode(merged$wnv, "'C. nigripalpus'='wnv'")
merged$wnv <- recode(merged$wnv, "'C. quinquefasciatus'='wnv'")
merged$wnv <- recode(merged$wnv, "'C.  quinquefasciatus'='wnv'")
merged$wnv <- recode(merged$wnv, "'C. restuans'='wnv'")
merged$wnv <- recode(merged$wnv, "'C. salinarius'='wnv'")
merged$wnv <- recode(merged$wnv, "'C.  salinarius'='wnv'")
merged$wnv <- recode(merged$wnv, "'Culiseta  inornata'='nownv'")
merged$wnv <- recode(merged$wnv, "'Culiseta  melanura'='nownv'")

merged$wnv <- recode(merged$wnv, "'Ochlerotatus atlanticus'='wnv'")
merged$wnv <- recode(merged$wnv, "'O. canadensis'='nownv'")
merged$wnv <- recode(merged$wnv, "'O. dupreei'='nownv'")
merged$wnv <- recode(merged$wnv, "'O. fulvus pallens'='nownv'")
merged$wnv <- recode(merged$wnv, "'O. infirmatus'='nownv'")
merged$wnv <- recode(merged$wnv, "'O. mitchellae'='nownv'")
merged$wnv <- recode(merged$wnv, "'O.  mitchellae'='nownv'")
merged$wnv <- recode(merged$wnv, "'O.  solicitans'='nownv'")
merged$wnv <- recode(merged$wnv, "'O. sollicitans'='nownv'")
merged$wnv <- recode(merged$wnv, "'O.  taeniorhynchus'='nownv'")
merged$wnv <- recode(merged$wnv, "'Coquillettidia perturbans'='wnv'")
merged$wnv <- recode(merged$wnv, "'Coquillettidia  perturbans'='wnv'")
merged$wnv <- recode(merged$wnv, "'Mansonia  titillans'='nownv'")
merged$wnv <- recode(merged$wnv, "'M. dyari'='nownv'")
merged$wnv <- recode(merged$wnv, "'Psorophora  ciliata'='nownv'")
merged$wnv <- recode(merged$wnv, "'P. columbiae'='nownv'")
merged$wnv <- recode(merged$wnv, "'P. ferox'='nownv'")
merged$wnv <- recode(merged$wnv, "'P. howardii'='nownv'")
merged$wnv <- recode(merged$wnv, "'Toxorhynchites rutilus'='nownv'")
merged$wnv <- recode(merged$wnv, "'Uranotaenia  sapphirina'='nownv'")
merged$wnv <- recode(merged$wnv, "'U.  lowii'='nownv'")
merged$wnv <- recode(merged$wnv, "'Wyeomyia  mitchellii'='nownv'")
merged$wnv <- recode(merged$wnv, "'W.  vanduzeei'='nownv'")
merged$wnv <- recode(merged$wnv, "'Other'='nownv'")
merged$wnv <- recode(merged$wnv, "'Other (Cx coronator)'='nownv'")


#Populate the slev column appropriately
merged$slev <- recode(merged$slev, "'Aedes albopictus'='slev'")
merged$slev <- recode(merged$slev, "'A. aegypti'='noslev'")
merged$slev <- recode(merged$slev, "'A. vexans'='noslev'")
merged$slev <- recode(merged$slev, "'O.  triseriatus'='noslev'")
merged$slev <- recode(merged$slev, "'Anopheles crucians'='noslev'")
merged$slev <- recode(merged$slev, "'A. quadrimaculatus'='noslev'")
merged$slev <- recode(merged$slev, "'Culex erraticus'='noslev'")
merged$slev <- recode(merged$slev, "'Culex  erraticus'='noslev'")
merged$slev <- recode(merged$slev, "'C. nigrapalpus'='slev'")
merged$slev <- recode(merged$slev, "'C.  nigrapalpus'='slev'")
merged$slev <- recode(merged$slev, "'C. nigripalpus'='slev'")
merged$slev <- recode(merged$slev, "'C. quinquefasciatus'='noslev'")
merged$slev <- recode(merged$slev, "'C.  quinquefasciatus'='noslev'")
merged$slev <- recode(merged$slev, "'C. restuans'='noslev'")
merged$slev <- recode(merged$slev, "'C. salinarius'='noslev'")
merged$slev <- recode(merged$slev, "'C.  salinarius'='noslev'")
merged$slev <- recode(merged$slev, "'Culiseta  inornata'='noslev'")
merged$slev <- recode(merged$slev, "'Culiseta  melanura'='noslev'")

merged$slev <- recode(merged$slev, "'Ochlerotatus atlanticus'='noslev'")
merged$slev <- recode(merged$slev, "'O. canadensis'='noslev'")
merged$slev <- recode(merged$slev, "'O. dupreei'='noslev'")
merged$slev <- recode(merged$slev, "'O. fulvus pallens'='noslev'")
merged$slev <- recode(merged$slev, "'O. infirmatus'='noslev'")
merged$slev <- recode(merged$slev, "'O. mitchellae'='noslev'")
merged$slev <- recode(merged$slev, "'O.  mitchellae'='noslev'")
merged$slev <- recode(merged$slev, "'O.  solicitans'='noslev'")
merged$slev <- recode(merged$slev, "'O. sollicitans'='noslev'")
merged$slev <- recode(merged$slev, "'O.  taeniorhynchus'='slev'")
merged$slev <- recode(merged$slev, "'Coquillettidia perturbans'='noslev'")
merged$slev <- recode(merged$slev, "'Coquillettidia  perturbans'='noslev'")
merged$slev <- recode(merged$slev, "'Mansonia  titillans'='noslev'")
merged$slev <- recode(merged$slev, "'M. dyari'='noslev'")
merged$slev <- recode(merged$slev, "'Psorophora  ciliata'='noslev'")
merged$slev <- recode(merged$slev, "'P. columbiae'='noslev'")
merged$slev <- recode(merged$slev, "'P. ferox'='noslev'")
merged$slev <- recode(merged$slev, "'P. howardii'='noslev'")
merged$slev <- recode(merged$slev, "'Toxorhynchites rutilus'='noslev'")
merged$slev <- recode(merged$slev, "'Uranotaenia  sapphirina'='noslev'")
merged$slev <- recode(merged$slev, "'U.  lowii'='noslev'")
merged$slev <- recode(merged$slev, "'Wyeomyia  mitchellii'='noslev'")
merged$slev <- recode(merged$slev, "'W.  vanduzeei'='noslev'")
merged$slev <- recode(merged$slev, "'Other'='noslev'")
merged$slev <- recode(merged$slev, "'Other (Cx coronator)'='noslev'")



#Populate the eeev column appropriately
merged$eeev <- recode(merged$eeev, "'Aedes albopictus'='eeev'")
merged$eeev <- recode(merged$eeev, "'A. aegypti'='eeev'")
merged$eeev <- recode(merged$eeev, "'A. vexans'='eeev'")
merged$eeev <- recode(merged$eeev, "'O.  triseriatus'='eeev'")
merged$eeev <- recode(merged$eeev, "'Anopheles crucians'='noeeev'")
merged$eeev <- recode(merged$eeev, "'A. quadrimaculatus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Culex erraticus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Culex  erraticus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'C. nigrapalpus'='eeev'")
merged$eeev <- recode(merged$eeev, "'C.  nigrapalpus'='eeev'")
merged$eeev <- recode(merged$eeev, "'C. nigripalpus'='eeev'")
merged$eeev <- recode(merged$eeev, "'C. quinquefasciatus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'C.  quinquefasciatus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'C. restuans'='noeeev'")
merged$eeev <- recode(merged$eeev, "'C. salinarius'='noeeev'")
merged$eeev <- recode(merged$eeev, "'C.  salinarius'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Culiseta  inornata'='eeev'")
merged$eeev <- recode(merged$eeev, "'Culiseta  melanura'='eeev'")

merged$eeev <- recode(merged$eeev, "'Ochlerotatus atlanticus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O. canadensis'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O. dupreei'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O. fulvus pallens'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O. infirmatus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O. mitchellae'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O.  mitchellae'='noeeev'")
merged$eeev <- recode(merged$eeev, "'O.  solicitans'='eeev'")
merged$eeev <- recode(merged$eeev, "'O. sollicitans'='eeev'")
merged$eeev <- recode(merged$eeev, "'O.  taeniorhynchus'='eeev'")
merged$eeev <- recode(merged$eeev, "'Coquillettidia perturbans'='eeev'")
merged$eeev <- recode(merged$eeev, "'Coquillettidia  perturbans'='eeev'")
merged$eeev <- recode(merged$eeev, "'Mansonia  titillans'='noeeev'")
merged$eeev <- recode(merged$eeev, "'M. dyari'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Psorophora  ciliata'='noeeev'")
merged$eeev <- recode(merged$eeev, "'P. columbiae'='noeeev'")
merged$eeev <- recode(merged$eeev, "'P. ferox'='noeeev'")
merged$eeev <- recode(merged$eeev, "'P. howardii'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Toxorhynchites rutilus'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Uranotaenia  sapphirina'='noeeev'")
merged$eeev <- recode(merged$eeev, "'U.  lowii'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Wyeomyia  mitchellii'='noeeev'")
merged$eeev <- recode(merged$eeev, "'W.  vanduzeei'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Other'='noeeev'")
merged$eeev <- recode(merged$eeev, "'Other (Cx coronator)'='noeeev'")



#Populate the chik column appropriately
merged$chik <- recode(merged$chik, "'Aedes albopictus'='chik'")
merged$chik <- recode(merged$chik, "'A. aegypti'='chik'")
merged$chik <- recode(merged$chik, "'A. vexans'='nochik'")
merged$chik <- recode(merged$chik, "'O.  triseriatus'='nochik'")
merged$chik <- recode(merged$chik, "'Anopheles crucians'='nochik'")
merged$chik <- recode(merged$chik, "'A. quadrimaculatus'='nochik'")
merged$chik <- recode(merged$chik, "'Culex erraticus'='nochik'")
merged$chik <- recode(merged$chik, "'Culex  erraticus'='nochik'")
merged$chik <- recode(merged$chik, "'C. nigrapalpus'='nochik'")
merged$chik <- recode(merged$chik, "'C.  nigrapalpus'='nochik'")
merged$chik <- recode(merged$chik, "'C. nigripalpus'='nochik'")
merged$chik <- recode(merged$chik, "'C. quinquefasciatus'='nochik'")
merged$chik <- recode(merged$chik, "'C.  quinquefasciatus'='nochik'")
merged$chik <- recode(merged$chik, "'C. restuans'='nochik'")
merged$chik <- recode(merged$chik, "'C. salinarius'='nochik'")
merged$chik <- recode(merged$chik, "'C.  salinarius'='nochik'")
merged$chik <- recode(merged$chik, "'Culiseta  inornata'='nochik'")
merged$chik <- recode(merged$chik, "'Culiseta  melanura'='nochik'")

merged$chik <- recode(merged$chik, "'Ochlerotatus atlanticus'='nochik'")
merged$chik <- recode(merged$chik, "'O. canadensis'='nochik'")
merged$chik <- recode(merged$chik, "'O. dupreei'='nochik'")
merged$chik <- recode(merged$chik, "'O. fulvus pallens'='nochik'")
merged$chik <- recode(merged$chik, "'O. infirmatus'='nochik'")
merged$chik <- recode(merged$chik, "'O. mitchellae'='nochik'")
merged$chik <- recode(merged$chik, "'O.  mitchellae'='nochik'")
merged$chik <- recode(merged$chik, "'O.  solicitans'='nochik'")
merged$chik <- recode(merged$chik, "'O. sollicitans'='nochik'")
merged$chik <- recode(merged$chik, "'O.  taeniorhynchus'='nochik'")
merged$chik <- recode(merged$chik, "'Coquillettidia perturbans'='nochik'")
merged$chik <- recode(merged$chik, "'Coquillettidia  perturbans'='nochik'")
merged$chik <- recode(merged$chik, "'Mansonia  titillans'='nochik'")
merged$chik <- recode(merged$chik, "'M. dyari'='nochik'")
merged$chik <- recode(merged$chik, "'Psorophora  ciliata'='nochik'")
merged$chik <- recode(merged$chik, "'P. columbiae'='nochik'")
merged$chik <- recode(merged$chik, "'P. ferox'='nochik'")
merged$chik <- recode(merged$chik, "'P. howardii'='nochik'")
merged$chik <- recode(merged$chik, "'Toxorhynchites rutilus'='nochik'")
merged$chik <- recode(merged$chik, "'Uranotaenia  sapphirina'='nochik'")
merged$chik <- recode(merged$chik, "'U.  lowii'='nochik'")
merged$chik <- recode(merged$chik, "'Wyeomyia  mitchellii'='nochik'")
merged$chik <- recode(merged$chik, "'W.  vanduzeei'='nochik'")
merged$chik <- recode(merged$chik, "'Other'='nochik'")
merged$chik <- recode(merged$chik, "'Other (Cx coronator)'='nochik'")


#Populate the malaria column appropriately
merged$malaria <- recode(merged$malaria, "'Aedes albopictus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'A. aegypti'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'A. vexans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O.  triseriatus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Anopheles crucians'='malaria'")
merged$malaria <- recode(merged$malaria, "'A. quadrimaculatus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Culex erraticus'='malaria'")
merged$malaria <- recode(merged$malaria, "'Culex  erraticus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C. nigrapalpus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C.  nigrapalpus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C. nigripalpus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C. quinquefasciatus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C.  quinquefasciatus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C. restuans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C. salinarius'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'C.  salinarius'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Culiseta  inornata'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Culiseta  melanura'='nomalaria'")

merged$malaria <- recode(merged$malaria, "'Ochlerotatus atlanticus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O. canadensis'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O. dupreei'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O. fulvus pallens'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O. infirmatus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O. mitchellae'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O.  mitchellae'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O.  solicitans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O. sollicitans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'O.  taeniorhynchus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Coquillettidia perturbans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Coquillettidia  perturbans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Mansonia  titillans'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'M. dyari'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Psorophora  ciliata'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'P. columbiae'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'P. ferox'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'P. howardii'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Toxorhynchites rutilus'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Uranotaenia  sapphirina'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'U.  lowii'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Wyeomyia  mitchellii'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'W.  vanduzeei'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Other'='nomalaria'")
merged$malaria <- recode(merged$malaria, "'Other (Cx coronator)'='nomalaria'")



#Populate the dengue column appropriately
merged$dengue <- recode(merged$dengue, "'Aedes albopictus'='dengue'")
merged$dengue <- recode(merged$dengue, "'A. aegypti'='nodengue'")
merged$dengue <- recode(merged$dengue, "'A. vexans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O.  triseriatus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Anopheles crucians'='nodengue'")
merged$dengue <- recode(merged$dengue, "'A. quadrimaculatus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Culex erraticus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Culex  erraticus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C. nigrapalpus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C.  nigrapalpus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C. nigripalpus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C. quinquefasciatus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C.  quinquefasciatus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C. restuans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C. salinarius'='nodengue'")
merged$dengue <- recode(merged$dengue, "'C.  salinarius'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Culiseta  inornata'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Culiseta  melanura'='nodengue'")

merged$dengue <- recode(merged$dengue, "'Ochlerotatus atlanticus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O. canadensis'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O. dupreei'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O. fulvus pallens'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O. infirmatus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O. mitchellae'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O.  mitchellae'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O.  solicitans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O. sollicitans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'O.  taeniorhynchus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Coquillettidia perturbans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Coquillettidia  perturbans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Mansonia  titillans'='nodengue'")
merged$dengue <- recode(merged$dengue, "'M. dyari'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Psorophora  ciliata'='nodengue'")
merged$dengue <- recode(merged$dengue, "'P. columbiae'='nodengue'")
merged$dengue <- recode(merged$dengue, "'P. ferox'='nodengue'")
merged$dengue <- recode(merged$dengue, "'P. howardii'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Toxorhynchites rutilus'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Uranotaenia  sapphirina'='nodengue'")
merged$dengue <- recode(merged$dengue, "'U.  lowii'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Wyeomyia  mitchellii'='nodengue'")
merged$dengue <- recode(merged$dengue, "'W.  vanduzeei'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Other'='nodengue'")
merged$dengue <- recode(merged$dengue, "'Other (Cx coronator)'='nodengue'")



#DATE STUFF
merged$date <- as.Date(merged$date, "%Y-%m-%d" )
merged$date2 <- as.Date(as.character(merged$date), "%Y-%m-%d")
date2 <- merged$date2

#SUBSET THE DATA : VECTOR / NONVECTOR
mergedvector <- merged[which(merged$vector=="vector"),]
mergedwnv <- merged[which(merged$wnv=="wnv"),]
mergedslev <- merged[which(merged$slev=="slev"),]
mergedeeev <- merged[which(merged$eeev=="eeev"),]
mergedchik <- merged[which(merged$chik=="chik"),]
mergedmalaria <- merged[which(merged$malaria=="malaria"),]
mergeddengue <- merged[which(merged$dengue=="dengue"),]

mergednonvector <- merged[which(merged$vector=="nonvector"),]

#SUBSET BY DATE and VECTOR / NONVECTOR
jun04vector <- subset(mergedvector, date2  == as.Date("2013-06-04"))
jun12vector <- subset(mergedvector, date2  == as.Date("2013-06-12"))
jun18vector <- subset(mergedvector, date2  == as.Date("2013-06-18"))
jun25vector <- subset(mergedvector, date2  == as.Date("2013-06-25"))
jul04vector <- subset(mergedvector, date2  == as.Date("2013-07-04"))
jul10vector <- subset(mergedvector, date2  == as.Date("2013-07-10"))
jul17vector <- subset(mergedvector, date2  == as.Date("2013-07-17"))
jul24vector <- subset(mergedvector, date2  == as.Date("2013-07-24"))
jul30vector <- subset(mergedvector, date2  == as.Date("2013-07-30"))
aug06vector <- subset(mergedvector, date2  == as.Date("2013-08-06"))
aug13vector <- subset(mergedvector, date2  == as.Date("2013-08-13"))
aug20vector <- subset(mergedvector, date2  == as.Date("2013-08-20"))
sep03vector <- subset(mergedvector, date2  == as.Date("2013-09-03"))
sep10vector <- subset(mergedvector, date2  == as.Date("2013-09-10"))
sep17vector <- subset(mergedvector, date2  == as.Date("2013-09-17"))
sep24vector <- subset(mergedvector, date2  == as.Date("2013-09-24"))



jun04nonvector <- subset(mergednonvector, date2  == as.Date("2013-06-04"))
jun12nonvector <- subset(mergednonvector, date2  == as.Date("2013-06-12"))
jun18nonvector <- subset(mergednonvector, date2  == as.Date("2013-06-18"))
jun25nonvector <- subset(mergednonvector, date2  == as.Date("2013-06-25"))
jul04nonvector <- subset(mergednonvector, date2  == as.Date("2013-07-04"))
jul10nonvector <- subset(mergednonvector, date2  == as.Date("2013-07-10"))
jul17nonvector <- subset(mergednonvector, date2  == as.Date("2013-07-17"))
jul24nonvector <- subset(mergednonvector, date2  == as.Date("2013-07-24"))
jul30nonvector <- subset(mergednonvector, date2  == as.Date("2013-07-30"))
aug06nonvector <- subset(mergednonvector, date2  == as.Date("2013-08-06"))
aug13nonvector <- subset(mergednonvector, date2  == as.Date("2013-08-13"))
aug20nonvector <- subset(mergednonvector, date2  == as.Date("2013-08-20"))
sep03nonvector <- subset(mergednonvector, date2  == as.Date("2013-09-03"))
sep10nonvector <- subset(mergednonvector, date2  == as.Date("2013-09-10"))
sep17nonvector <- subset(mergednonvector, date2  == as.Date("2013-09-17"))
sep24nonvector <- subset(mergednonvector, date2  == as.Date("2013-09-24"))


datevector <- c("2013-06-04",
                "2013-06-012",
                "2013-06-18",
                "2013-06-25",
                "2013-07-04",
                "2013-07-10",
                "2013-07-17",
                "2013-07-24",
                "2013-07-30",
                "2013-08-06",
                "2013-08-13",
                "2013-08-20",
                "2013-09-03",
                "2013-09-10",
                "2013-09-17",
                "2013-09-24")

datevector <- as.Date(datevector)

#SUBSET THE DATA FOR THE DIFFERENT COLLECTION SITES (1:10)
names(merged)
merged1 <- merged[,c(1,12:21)]
merged2 <- merged[,c(2,12:21)]
merged3 <- merged[,c(3,12:21)]
merged4 <- merged[,c(4,12:21)]
merged5 <- merged[,c(5,12:21)]
merged6 <- merged[,c(6,12:21)]
merged7 <- merged[,c(7,12:21)]
merged8 <- merged[,c(8,12:21)]
merged9 <- merged[,c(9,12:21)]
merged10 <- merged[,c(11,12:21)]

#Double subset for vector, disease and non vector by site
#VECTOR
merged1vector <- mergedvector[,c(1,12:21)]
merged2vector <- mergedvector[,c(2,12:21)]
merged3vector <- mergedvector[,c(3,12:21)]
merged4vector <- mergedvector[,c(4,12:21)]
merged5vector <- mergedvector[,c(5,12:21)]
merged6vector <- mergedvector[,c(6,12:21)]
merged7vector <- mergedvector[,c(7,12:21)]
merged8vector <- mergedvector[,c(8,12:21)]
merged9vector <- mergedvector[,c(9,12:21)]
merged10vector <- mergedvector[,c(11,12:21)]

#wnv
merged1wnv <- mergedwnv[,c(1,12:21)]
merged2wnv <- mergedwnv[,c(2,12:21)]
merged3wnv <- mergedwnv[,c(3,12:21)]
merged4wnv <- mergedwnv[,c(4,12:21)]
merged5wnv <- mergedwnv[,c(5,12:21)]
merged6wnv <- mergedwnv[,c(6,12:21)]
merged7wnv <- mergedwnv[,c(7,12:21)]
merged8wnv <- mergedwnv[,c(8,12:21)]
merged9wnv <- mergedwnv[,c(9,12:21)]
merged10wnv <- mergedwnv[,c(11,12:21)]

#slev
merged1slev <- mergedslev[,c(1,12:21)]
merged2slev <- mergedslev[,c(2,12:21)]
merged3slev <- mergedslev[,c(3,12:21)]
merged4slev <- mergedslev[,c(4,12:21)]
merged5slev <- mergedslev[,c(5,12:21)]
merged6slev <- mergedslev[,c(6,12:21)]
merged7slev <- mergedslev[,c(7,12:21)]
merged8slev <- mergedslev[,c(8,12:21)]
merged9slev <- mergedslev[,c(9,12:21)]
merged10slev <- mergedslev[,c(11,12:21)]

#eeev
merged1eeev <- mergedeeev[,c(1,12:21)]
merged2eeev <- mergedeeev[,c(2,12:21)]
merged3eeev <- mergedeeev[,c(3,12:21)]
merged4eeev <- mergedeeev[,c(4,12:21)]
merged5eeev <- mergedeeev[,c(5,12:21)]
merged6eeev <- mergedeeev[,c(6,12:21)]
merged7eeev <- mergedeeev[,c(7,12:21)]
merged8eeev <- mergedeeev[,c(8,12:21)]
merged9eeev <- mergedeeev[,c(9,12:21)]
merged10eeev <- mergedeeev[,c(11,12:21)]

#chik
merged1chik <- mergedchik[,c(1,12:21)]
merged2chik <- mergedchik[,c(2,12:21)]
merged3chik <- mergedchik[,c(3,12:21)]
merged4chik <- mergedchik[,c(4,12:21)]
merged5chik <- mergedchik[,c(5,12:21)]
merged6chik <- mergedchik[,c(6,12:21)]
merged7chik <- mergedchik[,c(7,12:21)]
merged8chik <- mergedchik[,c(8,12:21)]
merged9chik <- mergedchik[,c(9,12:21)]
merged10chik <- mergedchik[,c(11,12:21)]

#malaria
merged1malaria <- mergedmalaria[,c(1,12:21)]
merged2malaria <- mergedmalaria[,c(2,12:21)]
merged3malaria <- mergedmalaria[,c(3,12:21)]
merged4malaria <- mergedmalaria[,c(4,12:21)]
merged5malaria <- mergedmalaria[,c(5,12:21)]
merged6malaria <- mergedmalaria[,c(6,12:21)]
merged7malaria <- mergedmalaria[,c(7,12:21)]
merged8malaria <- mergedmalaria[,c(8,12:21)]
merged9malaria <- mergedmalaria[,c(9,12:21)]
merged10malaria <- mergedmalaria[,c(11,12:21)]

#dengue
merged1dengue <- mergeddengue[,c(1,12:21)]
merged2dengue <- mergeddengue[,c(2,12:21)]
merged3dengue <- mergeddengue[,c(3,12:21)]
merged4dengue <- mergeddengue[,c(4,12:21)]
merged5dengue <- mergeddengue[,c(5,12:21)]
merged6dengue <- mergeddengue[,c(6,12:21)]
merged7dengue <- mergeddengue[,c(7,12:21)]
merged8dengue <- mergeddengue[,c(8,12:21)]
merged9dengue <- mergeddengue[,c(9,12:21)]
merged10dengue <- mergeddengue[,c(11,12:21)]

#NONVECTOR
merged1nonvector <- mergednonvector[,c(1,12:21)]
merged2nonvector <- mergednonvector[,c(2,12:21)]
merged3nonvector <- mergednonvector[,c(3,12:21)]
merged4nonvector <- mergednonvector[,c(4,12:21)]
merged5nonvector <- mergednonvector[,c(5,12:21)]
merged6nonvector <- mergednonvector[,c(6,12:21)]
merged7nonvector <- mergednonvector[,c(7,12:21)]
merged8nonvector <- mergednonvector[,c(8,12:21)]
merged9nonvector <- mergednonvector[,c(9,12:21)]
merged10nonvector <- mergednonvector[,c(11,12:21)]



####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE VECTOR SITE

#JUN 04 Vector
jun04vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-06-04"))
jun04vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-06-04"))
jun04vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-06-04"))
jun04vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-06-04"))
jun04vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-06-04"))
jun04vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-06-04"))
jun04vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-06-04"))
jun04vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-06-04"))
jun04vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-06-04"))
jun04vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-06-04"))
#JUN 04 Nonvector
jun04nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-06-04"))
jun04nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-06-04"))

#jun12 Vector
jun12vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-06-12"))
jun12vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-06-12"))
jun12vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-06-12"))
jun12vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-06-12"))
jun12vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-06-12"))
jun12vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-06-12"))
jun12vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-06-12"))
jun12vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-06-12"))
jun12vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-06-12"))
jun12vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-06-12"))
#jun12 Nonvector
jun12nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-06-12"))
jun12nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-06-12"))

#jun18 Vector
jun18vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-06-18"))
jun18vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-06-18"))
jun18vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-06-18"))
jun18vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-06-18"))
jun18vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-06-18"))
jun18vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-06-18"))
jun18vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-06-18"))
jun18vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-06-18"))
jun18vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-06-18"))
jun18vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-06-18"))
#jun18 Nonvector
jun18nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-06-18"))
jun18nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-06-18"))


#jun25 Vector
jun25vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-06-25"))
jun25vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-06-25"))
jun25vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-06-25"))
jun25vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-06-25"))
jun25vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-06-25"))
jun25vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-06-25"))
jun25vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-06-25"))
jun25vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-06-25"))
jun25vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-06-25"))
jun25vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-06-25"))
#jun25 Nonvector
jun25nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-06-25"))
jun25nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-06-25"))

#jul04 Vector
jul04vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-07-04"))
jul04vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-07-04"))
jul04vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-07-04"))
jul04vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-07-04"))
jul04vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-07-04"))
jul04vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-07-04"))
jul04vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-07-04"))
jul04vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-07-04"))
jul04vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-07-04"))
jul04vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-07-04"))
#jul04 Nonvector
jul04nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-07-04"))
jul04nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-07-04"))

#jul10 Vector
jul10vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-07-10"))
jul10vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-07-10"))
jul10vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-07-10"))
jul10vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-07-10"))
jul10vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-07-10"))
jul10vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-07-10"))
jul10vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-07-10"))
jul10vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-07-10"))
jul10vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-07-10"))
jul10vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-07-10"))
#jul10 Nonvector
jul10nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-07-10"))
jul10nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-07-10"))

#jul17 Vector
jul17vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-07-17"))
jul17vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-07-17"))
jul17vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-07-17"))
jul17vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-07-17"))
jul17vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-07-17"))
jul17vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-07-17"))
jul17vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-07-17"))
jul17vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-07-17"))
jul17vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-07-17"))
jul17vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-07-17"))
#jul17 Nonvector
jul17nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-07-17"))
jul17nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-07-17"))

#jul24 Vector
jul24vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-07-24"))
jul24vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-07-24"))
jul24vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-07-24"))
jul24vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-07-24"))
jul24vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-07-24"))
jul24vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-07-24"))
jul24vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-07-24"))
jul24vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-07-24"))
jul24vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-07-24"))
jul24vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-07-24"))
#jul24 Nonvector
jul24nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-07-24"))
jul24nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-07-24"))

#jul30 Vector
jul30vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-07-30"))
jul30vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-07-30"))
jul30vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-07-30"))
jul30vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-07-30"))
jul30vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-07-30"))
jul30vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-07-30"))
jul30vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-07-30"))
jul30vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-07-30"))
jul30vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-07-30"))
jul30vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-07-30"))
#jul30 Nonvector
jul30nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-07-30"))
jul30nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-07-30"))

#aug06 Vector
aug06vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-08-06"))
aug06vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-08-06"))
aug06vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-08-06"))
aug06vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-08-06"))
aug06vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-08-06"))
aug06vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-08-06"))
aug06vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-08-06"))
aug06vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-08-06"))
aug06vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-08-06"))
aug06vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-08-06"))
#aug06 Nonvector
aug06nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-08-06"))
aug06nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-08-06"))


#aug13 Vector
aug13vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-08-13"))
aug13vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-08-13"))
aug13vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-08-13"))
aug13vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-08-13"))
aug13vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-08-13"))
aug13vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-08-13"))
aug13vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-08-13"))
aug13vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-08-13"))
aug13vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-08-13"))
aug13vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-08-13"))
#aug13 Nonvector
aug13nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-08-13"))
aug13nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-08-13"))



#aug20 Vector
aug20vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-08-20"))
aug20vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-08-20"))
aug20vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-08-20"))
aug20vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-08-20"))
aug20vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-08-20"))
aug20vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-08-20"))
aug20vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-08-20"))
aug20vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-08-20"))
aug20vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-08-20"))
aug20vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-08-20"))
#aug20 Nonvector
aug20nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-08-20"))
aug20nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-08-20"))



#sep03 Vector
sep03vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-09-03"))
sep03vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-09-03"))
sep03vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-09-03"))
sep03vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-09-03"))
sep03vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-09-03"))
sep03vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-09-03"))
sep03vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-09-03"))
sep03vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-09-03"))
sep03vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-09-03"))
sep03vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-09-03"))
#sep03 Nonvector
sep03nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-09-03"))
sep03nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-09-03"))




#sep10 Vector
sep10vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-09-10"))
sep10vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-09-10"))
sep10vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-09-10"))
sep10vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-09-10"))
sep10vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-09-10"))
sep10vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-09-10"))
sep10vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-09-10"))
sep10vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-09-10"))
sep10vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-09-10"))
sep10vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-09-10"))
#sep10 Nonvector
sep10nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-09-10"))
sep10nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-09-10"))



#sep17 Vector
sep17vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-09-17"))
sep17vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-09-17"))
sep17vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-09-17"))
sep17vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-09-17"))
sep17vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-09-17"))
sep17vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-09-17"))
sep17vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-09-17"))
sep17vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-09-17"))
sep17vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-09-17"))
sep17vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-09-17"))
#sep17 Nonvector
sep17nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-09-17"))
sep17nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-09-17"))


#sep24 Vector
sep24vectorsite1 <-subset(merged1vector, date2  == as.Date("2013-09-24"))
sep24vectorsite2 <-subset(merged2vector, date2  == as.Date("2013-09-24"))
sep24vectorsite3 <-subset(merged3vector, date2  == as.Date("2013-09-24"))
sep24vectorsite4 <-subset(merged4vector, date2  == as.Date("2013-09-24"))
sep24vectorsite5 <-subset(merged5vector, date2  == as.Date("2013-09-24"))
sep24vectorsite6 <-subset(merged6vector, date2  == as.Date("2013-09-24"))
sep24vectorsite7 <-subset(merged7vector, date2  == as.Date("2013-09-24"))
sep24vectorsite8 <-subset(merged8vector, date2  == as.Date("2013-09-24"))
sep24vectorsite9 <-subset(merged9vector, date2  == as.Date("2013-09-24"))
sep24vectorsite10 <-subset(merged10vector, date2  == as.Date("2013-09-24"))
#sep24 Nonvector
sep24nonvectorsite1 <-subset(merged1nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite2 <-subset(merged2nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite3 <-subset(merged3nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite4 <-subset(merged4nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite5 <-subset(merged5nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite6 <-subset(merged6nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite7 <-subset(merged7nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite8 <-subset(merged8nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite9 <-subset(merged9nonvector, date2  == as.Date("2013-09-24"))
sep24nonvectorsite10 <-subset(merged10nonvector, date2  == as.Date("2013-09-24"))













#CREATE GRAPHING VECTORS FOR EACH DATAPOINT IN THE FORM OF
#DATE VECTOR SITE


##### ALL SITES COMBINED
#VECTOR
ajun04vectortotal <- sum(jun04vector$total, na.rm=T)
ajun12vectortotal <- sum(jun12vector$total, na.rm=T)
ajun18vectortotal <- sum(jun18vector$total, na.rm=T)
ajun25vectortotal <- sum(jun25vector$total, na.rm=T)
ajul04vectortotal <- sum(jul04vector$total, na.rm=T)
ajul10vectortotal <- sum(jul10vector$total, na.rm=T)
ajul17vectortotal <- sum(jul17vector$total, na.rm=T)
ajul24vectortotal <- sum(jul24vector$total, na.rm=T)
ajul30vectortotal <- sum(jul30vector$total, na.rm=T)
aaug06vectortotal <- sum(aug06vector$total, na.rm=T)
aaug13vectortotal <- sum(aug13vector$total, na.rm=T)
aaug20vectortotal <- sum(aug20vector$total, na.rm=T)
asep03vectortotal <- sum(sep03vector$total, na.rm=T)
asep10vectortotal <- sum(sep10vector$total, na.rm=T)
asep17vectortotal <- sum(sep17vector$total, na.rm=T)
asep24vectortotal <- sum(sep24vector$total, na.rm=T)


#NONVECTOR
ajun04nonvectortotal <- sum(jun04nonvector$total, na.rm=T)
ajun12nonvectortotal <- sum(jun12nonvector$total, na.rm=T)
ajun18nonvectortotal <- sum(jun18nonvector$total, na.rm=T)
ajun25nonvectortotal <- sum(jun25nonvector$total, na.rm=T)
ajul04nonvectortotal <- sum(jul04nonvector$total, na.rm=T)
ajul10nonvectortotal <- sum(jul10nonvector$total, na.rm=T)
ajul17nonvectortotal <- sum(jul17nonvector$total, na.rm=T)
ajul24nonvectortotal <- sum(jul24nonvector$total, na.rm=T)
ajul30nonvectortotal <- sum(jul30nonvector$total, na.rm=T)
aaug06nonvectortotal <- sum(aug06nonvector$total, na.rm=T)
aaug13nonvectortotal <- sum(aug13nonvector$total, na.rm=T)
aaug20nonvectortotal <- sum(aug20nonvector$total, na.rm=T)
asep03nonvectortotal <- sum(sep03nonvector$total, na.rm=T)
asep10nonvectortotal <- sum(sep10nonvector$total, na.rm=T)
asep17nonvectortotal <- sum(sep17nonvector$total, na.rm=T)
asep24nonvectortotal <- sum(sep24nonvector$total, na.rm=T)



##### site1
#VECTOR
ajun04vectorsite1 <- sum(jun04vectorsite1$X1, na.rm=T)
ajun12vectorsite1 <- sum(jun12vectorsite1$X1, na.rm=T)
ajun18vectorsite1 <- sum(jun18vectorsite1$X1, na.rm=T)
ajun25vectorsite1 <- sum(jun25vectorsite1$X1, na.rm=T)
ajul04vectorsite1 <- sum(jul04vectorsite1$X1, na.rm=T)
ajul10vectorsite1 <- sum(jul10vectorsite1$X1, na.rm=T)
ajul17vectorsite1 <- sum(jul17vectorsite1$X1, na.rm=T)
ajul24vectorsite1 <- sum(jul24vectorsite1$X1, na.rm=T)
ajul30vectorsite1 <- sum(jul30vectorsite1$X1, na.rm=T)
aaug06vectorsite1 <- sum(aug06vectorsite1$X1, na.rm=T)
aaug13vectorsite1 <- sum(aug13vectorsite1$X1, na.rm=T)
aaug20vectorsite1 <- sum(aug20vectorsite1$X1, na.rm=T)
asep03vectorsite1 <- sum(sep03vectorsite1$X1, na.rm=T)
asep10vectorsite1 <- sum(sep10vectorsite1$X1, na.rm=T)
asep17vectorsite1 <- sum(sep17vectorsite1$X1, na.rm=T)
asep24vectorsite1 <- sum(sep24vectorsite1$X1, na.rm=T)


#NONVECTOR
ajun04nonvectorsite1 <- sum(jun04nonvectorsite1$X1, na.rm=T)
ajun12nonvectorsite1 <- sum(jun12nonvectorsite1$X1, na.rm=T)
ajun18nonvectorsite1 <- sum(jun18nonvectorsite1$X1, na.rm=T)
ajun25nonvectorsite1 <- sum(jun25nonvectorsite1$X1, na.rm=T)
ajul04nonvectorsite1 <- sum(jul04nonvectorsite1$X1, na.rm=T)
ajul10nonvectorsite1 <- sum(jul10nonvectorsite1$X1, na.rm=T)
ajul17nonvectorsite1 <- sum(jul17nonvectorsite1$X1, na.rm=T)
ajul24nonvectorsite1 <- sum(jul24nonvectorsite1$X1, na.rm=T)
ajul30nonvectorsite1 <- sum(jul30nonvectorsite1$X1, na.rm=T)
aaug06nonvectorsite1 <- sum(aug06nonvectorsite1$X1, na.rm=T)
aaug13nonvectorsite1 <- sum(aug13nonvectorsite1$X1, na.rm=T)
aaug20nonvectorsite1 <- sum(aug20nonvectorsite1$X1, na.rm=T)
asep03nonvectorsite1 <- sum(sep03nonvectorsite1$X1, na.rm=T)
asep10nonvectorsite1 <- sum(sep10nonvectorsite1$X1, na.rm=T)
asep17nonvectorsite1 <- sum(sep17nonvectorsite1$X1, na.rm=T)
asep24nonvectorsite1 <- sum(sep24nonvectorsite1$X1, na.rm=T)



##### site2
#VECTOR
ajun04vectorsite2 <- sum(jun04vectorsite2$X2, na.rm=T)
ajun12vectorsite2 <- sum(jun12vectorsite2$X2, na.rm=T)
ajun18vectorsite2 <- sum(jun18vectorsite2$X2, na.rm=T)
ajun25vectorsite2 <- sum(jun25vectorsite2$X2, na.rm=T)
ajul04vectorsite2 <- sum(jul04vectorsite2$X2, na.rm=T)
ajul10vectorsite2 <- sum(jul10vectorsite2$X2, na.rm=T)
ajul17vectorsite2 <- sum(jul17vectorsite2$X2, na.rm=T)
ajul24vectorsite2 <- sum(jul24vectorsite2$X2, na.rm=T)
ajul30vectorsite2 <- sum(jul30vectorsite2$X2, na.rm=T)
aaug06vectorsite2 <- sum(aug06vectorsite2$X2, na.rm=T)
aaug13vectorsite2 <- sum(aug13vectorsite2$X2, na.rm=T)
aaug20vectorsite2 <- sum(aug20vectorsite2$X2, na.rm=T)
asep03vectorsite2 <- sum(sep03vectorsite2$X2, na.rm=T)
asep10vectorsite2 <- sum(sep10vectorsite2$X2, na.rm=T)
asep17vectorsite2 <- sum(sep17vectorsite2$X2, na.rm=T)
asep24vectorsite2 <- sum(sep24vectorsite2$X2, na.rm=T)


#NONVECTOR
ajun04nonvectorsite2 <- sum(jun04nonvectorsite2$X2, na.rm=T)
ajun12nonvectorsite2 <- sum(jun12nonvectorsite2$X2, na.rm=T)
ajun18nonvectorsite2 <- sum(jun18nonvectorsite2$X2, na.rm=T)
ajun25nonvectorsite2 <- sum(jun25nonvectorsite2$X2, na.rm=T)
ajul04nonvectorsite2 <- sum(jul04nonvectorsite2$X2, na.rm=T)
ajul10nonvectorsite2 <- sum(jul10nonvectorsite2$X2, na.rm=T)
ajul17nonvectorsite2 <- sum(jul17nonvectorsite2$X2, na.rm=T)
ajul24nonvectorsite2 <- sum(jul24nonvectorsite2$X2, na.rm=T)
ajul30nonvectorsite2 <- sum(jul30nonvectorsite2$X2, na.rm=T)
aaug06nonvectorsite2 <- sum(aug06nonvectorsite2$X2, na.rm=T)
aaug13nonvectorsite2 <- sum(aug13nonvectorsite2$X2, na.rm=T)
aaug20nonvectorsite2 <- sum(aug20nonvectorsite2$X2, na.rm=T)
asep03nonvectorsite2 <- sum(sep03nonvectorsite2$X2, na.rm=T)
asep10nonvectorsite2 <- sum(sep10nonvectorsite2$X2, na.rm=T)
asep17nonvectorsite2 <- sum(sep17nonvectorsite2$X2, na.rm=T)
asep24nonvectorsite2 <- sum(sep24nonvectorsite2$X2, na.rm=T)


##### site3
#VECTOR
ajun04vectorsite3 <- sum(jun04vectorsite3$X3, na.rm=T)
ajun12vectorsite3 <- sum(jun12vectorsite3$X3, na.rm=T)
ajun18vectorsite3 <- sum(jun18vectorsite3$X3, na.rm=T)
ajun25vectorsite3 <- sum(jun25vectorsite3$X3, na.rm=T)
ajul04vectorsite3 <- sum(jul04vectorsite3$X3, na.rm=T)
ajul10vectorsite3 <- sum(jul10vectorsite3$X3, na.rm=T)
ajul17vectorsite3 <- sum(jul17vectorsite3$X3, na.rm=T)
ajul24vectorsite3 <- sum(jul24vectorsite3$X3, na.rm=T)
ajul30vectorsite3 <- sum(jul30vectorsite3$X3, na.rm=T)
aaug06vectorsite3 <- sum(aug06vectorsite3$X3, na.rm=T)
aaug13vectorsite3 <- sum(aug13vectorsite3$X3, na.rm=T)
aaug20vectorsite3 <- sum(aug20vectorsite3$X3, na.rm=T)
asep03vectorsite3 <- sum(sep03vectorsite3$X3, na.rm=T)
asep10vectorsite3 <- sum(sep10vectorsite3$X3, na.rm=T)
asep17vectorsite3 <- sum(sep17vectorsite3$X3, na.rm=T)
asep24vectorsite3 <- sum(sep24vectorsite3$X3, na.rm=T)


#NONVECTOR
ajun04nonvectorsite3 <- sum(jun04nonvectorsite3$X3, na.rm=T)
ajun12nonvectorsite3 <- sum(jun12nonvectorsite3$X3, na.rm=T)
ajun18nonvectorsite3 <- sum(jun18nonvectorsite3$X3, na.rm=T)
ajun25nonvectorsite3 <- sum(jun25nonvectorsite3$X3, na.rm=T)
ajul04nonvectorsite3 <- sum(jul04nonvectorsite3$X3, na.rm=T)
ajul10nonvectorsite3 <- sum(jul10nonvectorsite3$X3, na.rm=T)
ajul17nonvectorsite3 <- sum(jul17nonvectorsite3$X3, na.rm=T)
ajul24nonvectorsite3 <- sum(jul24nonvectorsite3$X3, na.rm=T)
ajul30nonvectorsite3 <- sum(jul30nonvectorsite3$X3, na.rm=T)
aaug06nonvectorsite3 <- sum(aug06nonvectorsite3$X3, na.rm=T)
aaug13nonvectorsite3 <- sum(aug13nonvectorsite3$X3, na.rm=T)
aaug20nonvectorsite3 <- sum(aug20nonvectorsite3$X3, na.rm=T)
asep03nonvectorsite3 <- sum(sep03nonvectorsite3$X3, na.rm=T)
asep10nonvectorsite3 <- sum(sep10nonvectorsite3$X3, na.rm=T)
asep17nonvectorsite3 <- sum(sep17nonvectorsite3$X3, na.rm=T)
asep24nonvectorsite3 <- sum(sep24nonvectorsite3$X3, na.rm=T)



##### site4
#VECTOR
ajun04vectorsite4 <- sum(jun04vectorsite4$X4, na.rm=T)
ajun12vectorsite4 <- sum(jun12vectorsite4$X4, na.rm=T)
ajun18vectorsite4 <- sum(jun18vectorsite4$X4, na.rm=T)
ajun25vectorsite4 <- sum(jun25vectorsite4$X4, na.rm=T)
ajul04vectorsite4 <- sum(jul04vectorsite4$X4, na.rm=T)
ajul10vectorsite4 <- sum(jul10vectorsite4$X4, na.rm=T)
ajul17vectorsite4 <- sum(jul17vectorsite4$X4, na.rm=T)
ajul24vectorsite4 <- sum(jul24vectorsite4$X4, na.rm=T)
ajul30vectorsite4 <- sum(jul30vectorsite4$X4, na.rm=T)
aaug06vectorsite4 <- sum(aug06vectorsite4$X4, na.rm=T)
aaug13vectorsite4 <- sum(aug13vectorsite4$X4, na.rm=T)
aaug20vectorsite4 <- sum(aug20vectorsite4$X4, na.rm=T)
asep03vectorsite4 <- sum(sep03vectorsite4$X4, na.rm=T)
asep10vectorsite4 <- sum(sep10vectorsite4$X4, na.rm=T)
asep17vectorsite4 <- sum(sep17vectorsite4$X4, na.rm=T)
asep24vectorsite4 <- sum(sep24vectorsite4$X4, na.rm=T)


#NONVECTOR
ajun04nonvectorsite4 <- sum(jun04nonvectorsite4$X4, na.rm=T)
ajun12nonvectorsite4 <- sum(jun12nonvectorsite4$X4, na.rm=T)
ajun18nonvectorsite4 <- sum(jun18nonvectorsite4$X4, na.rm=T)
ajun25nonvectorsite4 <- sum(jun25nonvectorsite4$X4, na.rm=T)
ajul04nonvectorsite4 <- sum(jul04nonvectorsite4$X4, na.rm=T)
ajul10nonvectorsite4 <- sum(jul10nonvectorsite4$X4, na.rm=T)
ajul17nonvectorsite4 <- sum(jul17nonvectorsite4$X4, na.rm=T)
ajul24nonvectorsite4 <- sum(jul24nonvectorsite4$X4, na.rm=T)
ajul30nonvectorsite4 <- sum(jul30nonvectorsite4$X4, na.rm=T)
aaug06nonvectorsite4 <- sum(aug06nonvectorsite4$X4, na.rm=T)
aaug13nonvectorsite4 <- sum(aug13nonvectorsite4$X4, na.rm=T)
aaug20nonvectorsite4 <- sum(aug20nonvectorsite4$X4, na.rm=T)
asep03nonvectorsite4 <- sum(sep03nonvectorsite4$X4, na.rm=T)
asep10nonvectorsite4 <- sum(sep10nonvectorsite4$X4, na.rm=T)
asep17nonvectorsite4 <- sum(sep17nonvectorsite4$X4, na.rm=T)
asep24nonvectorsite4 <- sum(sep24nonvectorsite4$X4, na.rm=T)


##### site5
#VECTOR
ajun04vectorsite5 <- sum(jun04vectorsite5$X5, na.rm=T)
ajun12vectorsite5 <- sum(jun12vectorsite5$X5, na.rm=T)
ajun18vectorsite5 <- sum(jun18vectorsite5$X5, na.rm=T)
ajun25vectorsite5 <- sum(jun25vectorsite5$X5, na.rm=T)
ajul04vectorsite5 <- sum(jul04vectorsite5$X5, na.rm=T)
ajul10vectorsite5 <- sum(jul10vectorsite5$X5, na.rm=T)
ajul17vectorsite5 <- sum(jul17vectorsite5$X5, na.rm=T)
ajul24vectorsite5 <- sum(jul24vectorsite5$X5, na.rm=T)
ajul30vectorsite5 <- sum(jul30vectorsite5$X5, na.rm=T)
aaug06vectorsite5 <- sum(aug06vectorsite5$X5, na.rm=T)
aaug13vectorsite5 <- sum(aug13vectorsite5$X5, na.rm=T)
aaug20vectorsite5 <- sum(aug20vectorsite5$X5, na.rm=T)
asep03vectorsite5 <- sum(sep03vectorsite5$X5, na.rm=T)
asep10vectorsite5 <- sum(sep10vectorsite5$X5, na.rm=T)
asep17vectorsite5 <- sum(sep17vectorsite5$X5, na.rm=T)
asep24vectorsite5 <- sum(sep24vectorsite5$X5, na.rm=T)


#NONVECTOR
ajun04nonvectorsite5 <- sum(jun04nonvectorsite5$X5, na.rm=T)
ajun12nonvectorsite5 <- sum(jun12nonvectorsite5$X5, na.rm=T)
ajun18nonvectorsite5 <- sum(jun18nonvectorsite5$X5, na.rm=T)
ajun25nonvectorsite5 <- sum(jun25nonvectorsite5$X5, na.rm=T)
ajul04nonvectorsite5 <- sum(jul04nonvectorsite5$X5, na.rm=T)
ajul10nonvectorsite5 <- sum(jul10nonvectorsite5$X5, na.rm=T)
ajul17nonvectorsite5 <- sum(jul17nonvectorsite5$X5, na.rm=T)
ajul24nonvectorsite5 <- sum(jul24nonvectorsite5$X5, na.rm=T)
ajul30nonvectorsite5 <- sum(jul30nonvectorsite5$X5, na.rm=T)
aaug06nonvectorsite5 <- sum(aug06nonvectorsite5$X5, na.rm=T)
aaug13nonvectorsite5 <- sum(aug13nonvectorsite5$X5, na.rm=T)
aaug20nonvectorsite5 <- sum(aug20nonvectorsite5$X5, na.rm=T)
asep03nonvectorsite5 <- sum(sep03nonvectorsite5$X5, na.rm=T)
asep10nonvectorsite5 <- sum(sep10nonvectorsite5$X5, na.rm=T)
asep17nonvectorsite5 <- sum(sep17nonvectorsite5$X5, na.rm=T)
asep24nonvectorsite5 <- sum(sep24nonvectorsite5$X5, na.rm=T)



##### site6
#VECTOR
jun04vectorsite6$X6 <- as.numeric(jun04vectorsite6$X6)
jun12vectorsite6$X6 <- as.numeric(jun12vectorsite6$X6)
jun18vectorsite6$X6 <- as.numeric(jun18vectorsite6$X6)
jun25vectorsite6$X6 <- as.numeric(jun25vectorsite6$X6)
jul04vectorsite6$X6 <- as.numeric(jul04vectorsite6$X6)
jul10vectorsite6$X6 <- as.numeric(jul10vectorsite6$X6)
jul17vectorsite6$X6 <- as.numeric(jul17vectorsite6$X6)
jul24vectorsite6$X6 <- as.numeric(jul24vectorsite6$X6)
jul30vectorsite6$X6 <- as.numeric(jul30vectorsite6$X6)
aug06vectorsite6$X6 <- as.numeric(aug06vectorsite6$X6)
aug13vectorsite6$X6 <- as.numeric(aug13vectorsite6$X6)
aug20vectorsite6$X6 <- as.numeric(aug20vectorsite6$X6)
sep03vectorsite6$X6 <- as.numeric(sep03vectorsite6$X6)
sep10vectorsite6$X6 <- as.numeric(sep10vectorsite6$X6)
sep17vectorsite6$X6 <- as.numeric(sep17vectorsite6$X6)
sep24vectorsite6$X6 <- as.numeric(sep24vectorsite6$X6)


ajun04vectorsite6 <- sum(jun04vectorsite6$X6, na.rm=T)
ajun12vectorsite6 <- sum(jun12vectorsite6$X6, na.rm=T)
ajun18vectorsite6 <- sum(jun18vectorsite6$X6, na.rm=T)
ajun25vectorsite6 <- sum(jun25vectorsite6$X6, na.rm=T)
ajul04vectorsite6 <- sum(jul04vectorsite6$X6, na.rm=T)
ajul10vectorsite6 <- sum(jul10vectorsite6$X6, na.rm=T)
ajul17vectorsite6 <- sum(jul17vectorsite6$X6, na.rm=T)
ajul24vectorsite6 <- sum(jul24vectorsite6$X6, na.rm=T)
ajul30vectorsite6 <- sum(jul30vectorsite6$X6, na.rm=T)
aaug06vectorsite6 <- sum(aug06vectorsite6$X6, na.rm=T)
aaug13vectorsite6 <- sum(aug13vectorsite6$X6, na.rm=T)
aaug20vectorsite6 <- sum(aug20vectorsite6$X6, na.rm=T)
asep03vectorsite6 <- sum(sep03vectorsite6$X6, na.rm=T)
asep10vectorsite6 <- sum(sep10vectorsite6$X6, na.rm=T)
asep17vectorsite6 <- sum(sep17vectorsite6$X6, na.rm=T)
asep24vectorsite6 <- sum(sep24vectorsite6$X6, na.rm=T)


#NONVECTOR
jun04nonvectorsite6$X6 <- as.numeric(jun04nonvectorsite6$X6)
jun12nonvectorsite6$X6 <- as.numeric(jun12nonvectorsite6$X6)
jun18nonvectorsite6$X6 <- as.numeric(jun18nonvectorsite6$X6)
jun25nonvectorsite6$X6 <- as.numeric(jun25nonvectorsite6$X6)
jul04nonvectorsite6$X6 <- as.numeric(jul04nonvectorsite6$X6)
jul10nonvectorsite6$X6 <- as.numeric(jul10nonvectorsite6$X6)
jul17nonvectorsite6$X6 <- as.numeric(jul17nonvectorsite6$X6)
jul24nonvectorsite6$X6 <- as.numeric(jul24nonvectorsite6$X6)
jul30nonvectorsite6$X6 <- as.numeric(jul30nonvectorsite6$X6)
aug06nonvectorsite6$X6 <- as.numeric(aug06nonvectorsite6$X6)
aug13nonvectorsite6$X6 <- as.numeric(aug13nonvectorsite6$X6)
aug20nonvectorsite6$X6 <- as.numeric(aug20nonvectorsite6$X6)
sep03nonvectorsite6$X6 <- as.numeric(sep03nonvectorsite6$X6)
sep10nonvectorsite6$X6 <- as.numeric(sep10nonvectorsite6$X6)
sep17nonvectorsite6$X6 <- as.numeric(sep17nonvectorsite6$X6)
sep24nonvectorsite6$X6 <- as.numeric(sep24nonvectorsite6$X6)


ajun04nonvectorsite6 <- sum(jun04nonvectorsite6$X6, na.rm=T)
ajun12nonvectorsite6 <- sum(jun12nonvectorsite6$X6, na.rm=T)
ajun18nonvectorsite6 <- sum(jun18nonvectorsite6$X6, na.rm=T)
ajun25nonvectorsite6 <- sum(jun25nonvectorsite6$X6, na.rm=T)
ajul04nonvectorsite6 <- sum(jul04nonvectorsite6$X6, na.rm=T)
ajul10nonvectorsite6 <- sum(jul10nonvectorsite6$X6, na.rm=T)
ajul17nonvectorsite6 <- sum(jul17nonvectorsite6$X6, na.rm=T)
ajul24nonvectorsite6 <- sum(jul24nonvectorsite6$X6, na.rm=T)
ajul30nonvectorsite6 <- sum(jul30nonvectorsite6$X6, na.rm=T)
aaug06nonvectorsite6 <- sum(aug06nonvectorsite6$X6, na.rm=T)
aaug13nonvectorsite6 <- sum(aug13nonvectorsite6$X6, na.rm=T)
aaug20nonvectorsite6 <- sum(aug20nonvectorsite6$X6, na.rm=T)
asep03nonvectorsite6 <- sum(sep03nonvectorsite6$X6, na.rm=T)
asep10nonvectorsite6 <- sum(sep10nonvectorsite6$X6, na.rm=T)
asep17nonvectorsite6 <- sum(sep17nonvectorsite6$X6, na.rm=T)
asep24nonvectorsite6 <- sum(sep24nonvectorsite6$X6, na.rm=T)



##### site7
#VECTOR
ajun04vectorsite7 <- sum(jun04vectorsite7$X7, na.rm=T)
ajun12vectorsite7 <- sum(jun12vectorsite7$X7, na.rm=T)
ajun18vectorsite7 <- sum(jun18vectorsite7$X7, na.rm=T)
ajun25vectorsite7 <- sum(jun25vectorsite7$X7, na.rm=T)
ajul04vectorsite7 <- sum(jul04vectorsite7$X7, na.rm=T)
ajul10vectorsite7 <- sum(jul10vectorsite7$X7, na.rm=T)
ajul17vectorsite7 <- sum(jul17vectorsite7$X7, na.rm=T)
ajul24vectorsite7 <- sum(jul24vectorsite7$X7, na.rm=T)
ajul30vectorsite7 <- sum(jul30vectorsite7$X7, na.rm=T)
aaug06vectorsite7 <- sum(aug06vectorsite7$X7, na.rm=T)
aaug13vectorsite7 <- sum(aug13vectorsite7$X7, na.rm=T)
aaug20vectorsite7 <- sum(aug20vectorsite7$X7, na.rm=T)
asep03vectorsite7 <- sum(sep03vectorsite7$X7, na.rm=T)
asep10vectorsite7 <- sum(sep10vectorsite7$X7, na.rm=T)
asep17vectorsite7 <- sum(sep17vectorsite7$X7, na.rm=T)
asep24vectorsite7 <- sum(sep24vectorsite7$X7, na.rm=T)


#NONVECTOR
ajun04nonvectorsite7 <- sum(jun04nonvectorsite7$X7, na.rm=T)
ajun12nonvectorsite7 <- sum(jun12nonvectorsite7$X7, na.rm=T)
ajun18nonvectorsite7 <- sum(jun18nonvectorsite7$X7, na.rm=T)
ajun25nonvectorsite7 <- sum(jun25nonvectorsite7$X7, na.rm=T)
ajul04nonvectorsite7 <- sum(jul04nonvectorsite7$X7, na.rm=T)
ajul10nonvectorsite7 <- sum(jul10nonvectorsite7$X7, na.rm=T)
ajul17nonvectorsite7 <- sum(jul17nonvectorsite7$X7, na.rm=T)
ajul24nonvectorsite7 <- sum(jul24nonvectorsite7$X7, na.rm=T)
ajul30nonvectorsite7 <- sum(jul30nonvectorsite7$X7, na.rm=T)
aaug06nonvectorsite7 <- sum(aug06nonvectorsite7$X7, na.rm=T)
aaug13nonvectorsite7 <- sum(aug13nonvectorsite7$X7, na.rm=T)
aaug20nonvectorsite7 <- sum(aug20nonvectorsite7$X7, na.rm=T)
asep03nonvectorsite7 <- sum(sep03nonvectorsite7$X7, na.rm=T)
asep10nonvectorsite7 <- sum(sep10nonvectorsite7$X7, na.rm=T)
asep17nonvectorsite7 <- sum(sep17nonvectorsite7$X7, na.rm=T)
asep24nonvectorsite7 <- sum(sep24nonvectorsite7$X7, na.rm=T)



##### site8
#VECTOR
ajun04vectorsite8 <- sum(jun04vectorsite8$X8, na.rm=T)
ajun12vectorsite8 <- sum(jun12vectorsite8$X8, na.rm=T)
ajun18vectorsite8 <- sum(jun18vectorsite8$X8, na.rm=T)
ajun25vectorsite8 <- sum(jun25vectorsite8$X8, na.rm=T)
ajul04vectorsite8 <- sum(jul04vectorsite8$X8, na.rm=T)
ajul10vectorsite8 <- sum(jul10vectorsite8$X8, na.rm=T)
ajul17vectorsite8 <- sum(jul17vectorsite8$X8, na.rm=T)
ajul24vectorsite8 <- sum(jul24vectorsite8$X8, na.rm=T)
ajul30vectorsite8 <- sum(jul30vectorsite8$X8, na.rm=T)
aaug06vectorsite8 <- sum(aug06vectorsite8$X8, na.rm=T)
aaug13vectorsite8 <- sum(aug13vectorsite8$X8, na.rm=T)
aaug20vectorsite8 <- sum(aug20vectorsite8$X8, na.rm=T)
asep03vectorsite8 <- sum(sep03vectorsite8$X8, na.rm=T)
asep10vectorsite8 <- sum(sep10vectorsite8$X8, na.rm=T)
asep17vectorsite8 <- sum(sep17vectorsite8$X8, na.rm=T)
asep24vectorsite8 <- sum(sep24vectorsite8$X8, na.rm=T)


#NONVECTOR
ajun04nonvectorsite8 <- sum(jun04nonvectorsite8$X8, na.rm=T)
ajun12nonvectorsite8 <- sum(jun12nonvectorsite8$X8, na.rm=T)
ajun18nonvectorsite8 <- sum(jun18nonvectorsite8$X8, na.rm=T)
ajun25nonvectorsite8 <- sum(jun25nonvectorsite8$X8, na.rm=T)
ajul04nonvectorsite8 <- sum(jul04nonvectorsite8$X8, na.rm=T)
ajul10nonvectorsite8 <- sum(jul10nonvectorsite8$X8, na.rm=T)
ajul17nonvectorsite8 <- sum(jul17nonvectorsite8$X8, na.rm=T)
ajul24nonvectorsite8 <- sum(jul24nonvectorsite8$X8, na.rm=T)
ajul30nonvectorsite8 <- sum(jul30nonvectorsite8$X8, na.rm=T)
aaug06nonvectorsite8 <- sum(aug06nonvectorsite8$X8, na.rm=T)
aaug13nonvectorsite8 <- sum(aug13nonvectorsite8$X8, na.rm=T)
aaug20nonvectorsite8 <- sum(aug20nonvectorsite8$X8, na.rm=T)
asep03nonvectorsite8 <- sum(sep03nonvectorsite8$X8, na.rm=T)
asep10nonvectorsite8 <- sum(sep10nonvectorsite8$X8, na.rm=T)
asep17nonvectorsite8 <- sum(sep17nonvectorsite8$X8, na.rm=T)
asep24nonvectorsite8 <- sum(sep24nonvectorsite8$X8, na.rm=T)



##### site9
#VECTOR

jun04vectorsite9$X9 <- as.numeric(jun04vectorsite9$X9)
jun12vectorsite9$X9 <- as.numeric(jun12vectorsite9$X9)
jun18vectorsite9$X9 <- as.numeric(jun18vectorsite9$X9)
jun25vectorsite9$X9 <- as.numeric(jun25vectorsite9$X9)
jul04vectorsite9$X9 <- as.numeric(jul04vectorsite9$X9)
jul10vectorsite9$X9 <- as.numeric(jul10vectorsite9$X9)
jul17vectorsite9$X9 <- as.numeric(jul17vectorsite9$X9)
jul24vectorsite9$X9 <- as.numeric(jul24vectorsite9$X9)
jul30vectorsite9$X9 <- as.numeric(jul30vectorsite9$X9)
aug06vectorsite9$X9 <- as.numeric(aug06vectorsite9$X9)
aug13vectorsite9$X9 <- as.numeric(aug13vectorsite9$X9)
aug20vectorsite9$X9 <- as.numeric(aug20vectorsite9$X9)
sep03vectorsite9$X9 <- as.numeric(sep03vectorsite9$X9)
sep10vectorsite9$X9 <- as.numeric(sep10vectorsite9$X9)
sep17vectorsite9$X9 <- as.numeric(sep17vectorsite9$X9)
sep24vectorsite9$X9 <- as.numeric(sep24vectorsite9$X9)


ajun04vectorsite9 <- sum(jun04vectorsite9$X9, na.rm=T)
ajun12vectorsite9 <- sum(jun12vectorsite9$X9, na.rm=T)
ajun18vectorsite9 <- sum(jun18vectorsite9$X9, na.rm=T)
ajun25vectorsite9 <- sum(jun25vectorsite9$X9, na.rm=T)
ajul04vectorsite9 <- sum(jul04vectorsite9$X9, na.rm=T)
ajul10vectorsite9 <- sum(jul10vectorsite9$X9, na.rm=T)
ajul17vectorsite9 <- sum(jul17vectorsite9$X9, na.rm=T)
ajul24vectorsite9 <- sum(jul24vectorsite9$X9, na.rm=T)
ajul30vectorsite9 <- sum(jul30vectorsite9$X9, na.rm=T)
aaug06vectorsite9 <- sum(aug06vectorsite9$X9, na.rm=T)
aaug13vectorsite9 <- sum(aug13vectorsite9$X9, na.rm=T)
aaug20vectorsite9 <- sum(aug20vectorsite9$X9, na.rm=T)
asep03vectorsite9 <- sum(sep03vectorsite9$X9, na.rm=T)
asep10vectorsite9 <- sum(sep10vectorsite9$X9, na.rm=T)
asep17vectorsite9 <- sum(sep17vectorsite9$X9, na.rm=T)
asep24vectorsite9 <- sum(sep24vectorsite9$X9, na.rm=T)


#NONVECTOR
jun04nonvectorsite9$X9 <- as.numeric(jun04nonvectorsite9$X9)
jun12nonvectorsite9$X9 <- as.numeric(jun12nonvectorsite9$X9)
jun18nonvectorsite9$X9 <- as.numeric(jun18nonvectorsite9$X9)
jun25nonvectorsite9$X9 <- as.numeric(jun25nonvectorsite9$X9)
jul04nonvectorsite9$X9 <- as.numeric(jul04nonvectorsite9$X9)
jul10nonvectorsite9$X9 <- as.numeric(jul10nonvectorsite9$X9)
jul17nonvectorsite9$X9 <- as.numeric(jul17nonvectorsite9$X9)
jul24nonvectorsite9$X9 <- as.numeric(jul24nonvectorsite9$X9)
jul30nonvectorsite9$X9 <- as.numeric(jul30nonvectorsite9$X9)
aug06nonvectorsite9$X9 <- as.numeric(aug06nonvectorsite9$X9)
aug13nonvectorsite9$X9 <- as.numeric(aug13nonvectorsite9$X9)
aug20nonvectorsite9$X9 <- as.numeric(aug20nonvectorsite9$X9)
sep03nonvectorsite9$X9 <- as.numeric(sep03nonvectorsite9$X9)
sep10nonvectorsite9$X9 <- as.numeric(sep10nonvectorsite9$X9)
sep17nonvectorsite9$X9 <- as.numeric(sep17nonvectorsite9$X9)
sep24nonvectorsite9$X9 <- as.numeric(sep24nonvectorsite9$X9)


ajun04nonvectorsite9 <- sum(jun04nonvectorsite9$X9, na.rm=T)
ajun12nonvectorsite9 <- sum(jun12nonvectorsite9$X9, na.rm=T)
ajun18nonvectorsite9 <- sum(jun18nonvectorsite9$X9, na.rm=T)
ajun25nonvectorsite9 <- sum(jun25nonvectorsite9$X9, na.rm=T)
ajul04nonvectorsite9 <- sum(jul04nonvectorsite9$X9, na.rm=T)
ajul10nonvectorsite9 <- sum(jul10nonvectorsite9$X9, na.rm=T)
ajul17nonvectorsite9 <- sum(jul17nonvectorsite9$X9, na.rm=T)
ajul24nonvectorsite9 <- sum(jul24nonvectorsite9$X9, na.rm=T)
ajul30nonvectorsite9 <- sum(jul30nonvectorsite9$X9, na.rm=T)
aaug06nonvectorsite9 <- sum(aug06nonvectorsite9$X9, na.rm=T)
aaug13nonvectorsite9 <- sum(aug13nonvectorsite9$X9, na.rm=T)
aaug20nonvectorsite9 <- sum(aug20nonvectorsite9$X9, na.rm=T)
asep03nonvectorsite9 <- sum(sep03nonvectorsite9$X9, na.rm=T)
asep10nonvectorsite9 <- sum(sep10nonvectorsite9$X9, na.rm=T)
asep17nonvectorsite9 <- sum(sep17nonvectorsite9$X9, na.rm=T)
asep24nonvectorsite9 <- sum(sep24nonvectorsite9$X9, na.rm=T)


##### site10
#VECTOR
jun04vectorsite10$X10 <- as.numeric(jun04vectorsite10$X10)
jun12vectorsite10$X10 <- as.numeric(jun12vectorsite10$X10)
jun18vectorsite10$X10 <- as.numeric(jun18vectorsite10$X10)
jun25vectorsite10$X10 <- as.numeric(jun25vectorsite10$X10)
jul04vectorsite10$X10 <- as.numeric(jul04vectorsite10$X10)
jul10vectorsite10$X10 <- as.numeric(jul10vectorsite10$X10)
jul17vectorsite10$X10 <- as.numeric(jul17vectorsite10$X10)
jul24vectorsite10$X10 <- as.numeric(jul24vectorsite10$X10)
jul30vectorsite10$X10 <- as.numeric(jul30vectorsite10$X10)
aug06vectorsite10$X10 <- as.numeric(aug06vectorsite10$X10)
aug13vectorsite10$X10 <- as.numeric(aug13vectorsite10$X10)
aug20vectorsite10$X10 <- as.numeric(aug20vectorsite10$X10)
sep03vectorsite10$X10 <- as.numeric(sep03vectorsite10$X10)
sep10vectorsite10$X10 <- as.numeric(sep10vectorsite10$X10)
sep17vectorsite10$X10 <- as.numeric(sep17vectorsite10$X10)
sep24vectorsite10$X10 <- as.numeric(sep24vectorsite10$X10)


ajun04vectorsite10 <- sum(jun04vectorsite10$X10, na.rm=T)
ajun12vectorsite10 <- sum(jun12vectorsite10$X10, na.rm=T)
ajun18vectorsite10 <- sum(jun18vectorsite10$X10, na.rm=T)
ajun25vectorsite10 <- sum(jun25vectorsite10$X10, na.rm=T)
ajul04vectorsite10 <- sum(jul04vectorsite10$X10, na.rm=T)
ajul10vectorsite10 <- sum(jul10vectorsite10$X10, na.rm=T)
ajul17vectorsite10 <- sum(jul17vectorsite10$X10, na.rm=T)
ajul24vectorsite10 <- sum(jul24vectorsite10$X10, na.rm=T)
ajul30vectorsite10 <- sum(jul30vectorsite10$X10, na.rm=T)
aaug06vectorsite10 <- sum(aug06vectorsite10$X10, na.rm=T)
aaug13vectorsite10 <- sum(aug13vectorsite10$X10, na.rm=T)
aaug20vectorsite10 <- sum(aug20vectorsite10$X10, na.rm=T)
asep03vectorsite10 <- sum(sep03vectorsite10$X10, na.rm=T)
asep10vectorsite10 <- sum(sep10vectorsite10$X10, na.rm=T)
asep17vectorsite10 <- sum(sep17vectorsite10$X10, na.rm=T)
asep24vectorsite10 <- sum(sep24vectorsite10$X10, na.rm=T)


#NONVECTOR
jun04nonvectorsite10$X10 <- as.numeric(jun04nonvectorsite10$X10)
jun12nonvectorsite10$X10 <- as.numeric(jun12nonvectorsite10$X10)
jun18nonvectorsite10$X10 <- as.numeric(jun18nonvectorsite10$X10)
jun25nonvectorsite10$X10 <- as.numeric(jun25nonvectorsite10$X10)
jul04nonvectorsite10$X10 <- as.numeric(jul04nonvectorsite10$X10)
jul10nonvectorsite10$X10 <- as.numeric(jul10nonvectorsite10$X10)
jul17nonvectorsite10$X10 <- as.numeric(jul17nonvectorsite10$X10)
jul24nonvectorsite10$X10 <- as.numeric(jul24nonvectorsite10$X10)
jul30nonvectorsite10$X10 <- as.numeric(jul30nonvectorsite10$X10)
aug06nonvectorsite10$X10 <- as.numeric(aug06nonvectorsite10$X10)
aug13nonvectorsite10$X10 <- as.numeric(aug13nonvectorsite10$X10)
aug20nonvectorsite10$X10 <- as.numeric(aug20nonvectorsite10$X10)
sep03nonvectorsite10$X10 <- as.numeric(sep03nonvectorsite10$X10)
sep10nonvectorsite10$X10 <- as.numeric(sep10nonvectorsite10$X10)
sep17nonvectorsite10$X10 <- as.numeric(sep17nonvectorsite10$X10)
sep24nonvectorsite10$X10 <- as.numeric(sep24nonvectorsite10$X10)


ajun04nonvectorsite10 <- sum(jun04nonvectorsite10$X10, na.rm=T)
ajun12nonvectorsite10 <- sum(jun12nonvectorsite10$X10, na.rm=T)
ajun18nonvectorsite10 <- sum(jun18nonvectorsite10$X10, na.rm=T)
ajun25nonvectorsite10 <- sum(jun25nonvectorsite10$X10, na.rm=T)
ajul04nonvectorsite10 <- sum(jul04nonvectorsite10$X10, na.rm=T)
ajul10nonvectorsite10 <- sum(jul10nonvectorsite10$X10, na.rm=T)
ajul17nonvectorsite10 <- sum(jul17nonvectorsite10$X10, na.rm=T)
ajul24nonvectorsite10 <- sum(jul24nonvectorsite10$X10, na.rm=T)
ajul30nonvectorsite10 <- sum(jul30nonvectorsite10$X10, na.rm=T)
aaug06nonvectorsite10 <- sum(aug06nonvectorsite10$X10, na.rm=T)
aaug13nonvectorsite10 <- sum(aug13nonvectorsite10$X10, na.rm=T)
aaug20nonvectorsite10 <- sum(aug20nonvectorsite10$X10, na.rm=T)
asep03nonvectorsite10 <- sum(sep03nonvectorsite10$X10, na.rm=T)
asep10nonvectorsite10 <- sum(sep10nonvectorsite10$X10, na.rm=T)
asep17nonvectorsite10 <- sum(sep17nonvectorsite10$X10, na.rm=T)
asep24nonvectorsite10 <- sum(sep24nonvectorsite10$X10, na.rm=T)



##
#
#
#
#
#



#CREATE VECTORS FOR PLOTTING
#Site 1
vec1 <- c(ajun04vectorsite1,
          ajun12vectorsite1,
          ajun18vectorsite1,
          ajun25vectorsite1,
          ajul04vectorsite1,
          ajul10vectorsite1,
          ajul17vectorsite1,
          ajul24vectorsite1,
          ajul30vectorsite1,
          aaug06vectorsite1,
          aaug13vectorsite1,
          aaug20vectorsite1,
          asep03vectorsite1,
          asep10vectorsite1,
          asep17vectorsite1,
          asep24vectorsite1)
nonvec1 <- c(ajun04nonvectorsite1,
             ajun12nonvectorsite1,
             ajun18nonvectorsite1,
             ajun25nonvectorsite1,
             ajul04nonvectorsite1,
             ajul10nonvectorsite1,
             ajul17nonvectorsite1,
             ajul24nonvectorsite1,
             ajul30nonvectorsite1,
             aaug06nonvectorsite1,
             aaug13nonvectorsite1,
             aaug20nonvectorsite1,
             asep03nonvectorsite1,
             asep10nonvectorsite1,
             asep17nonvectorsite1,
             asep24nonvectorsite1)

#Site 2
vec2 <- c(ajun04vectorsite2,
          ajun12vectorsite2,
          ajun18vectorsite2,
          ajun25vectorsite2,
          ajul04vectorsite2,
          ajul10vectorsite2,
          ajul17vectorsite2,
          ajul24vectorsite2,
          ajul30vectorsite2,
          aaug06vectorsite2,
          aaug13vectorsite2,
          aaug20vectorsite2,
          asep03vectorsite2,
          asep10vectorsite2,
          asep17vectorsite2,
          asep24vectorsite2)
nonvec2 <- c(ajun04nonvectorsite2,
             ajun12nonvectorsite2,
             ajun18nonvectorsite2,
             ajun25nonvectorsite2,
             ajul04nonvectorsite2,
             ajul10nonvectorsite2,
             ajul17nonvectorsite2,
             ajul24nonvectorsite2,
             ajul30nonvectorsite2,
             aaug06nonvectorsite2,
             aaug13nonvectorsite2,
             aaug20nonvectorsite2,
             asep03nonvectorsite2,
             asep10nonvectorsite2,
             asep17nonvectorsite2,
             asep24nonvectorsite2)

#Site 3
vec3 <- c(ajun04vectorsite3,
          ajun12vectorsite3,
          ajun18vectorsite3,
          ajun25vectorsite3,
          ajul04vectorsite3,
          ajul10vectorsite3,
          ajul17vectorsite3,
          ajul24vectorsite3,
          ajul30vectorsite3,
          aaug06vectorsite3,
          aaug13vectorsite3,
          aaug20vectorsite3,
          asep03vectorsite3,
          asep10vectorsite3,
          asep17vectorsite3,
          asep24vectorsite3)
nonvec3 <- c(ajun04nonvectorsite3,
             ajun12nonvectorsite3,
             ajun18nonvectorsite3,
             ajun25nonvectorsite3,
             ajul04nonvectorsite3,
             ajul10nonvectorsite3,
             ajul17nonvectorsite3,
             ajul24nonvectorsite3,
             ajul30nonvectorsite3,
             aaug06nonvectorsite3,
             aaug13nonvectorsite3,
             aaug20nonvectorsite3,
             asep03nonvectorsite3,
             asep10nonvectorsite3,
             asep17nonvectorsite3,
             asep24vectorsite3)

#Site 4
vec4 <- c(ajun04vectorsite4,
          ajun12vectorsite4,
          ajun18vectorsite4,
          ajun25vectorsite4,
          ajul04vectorsite4,
          ajul10vectorsite4,
          ajul17vectorsite4,
          ajul24vectorsite4,
          ajul30vectorsite4,
          aaug06vectorsite4,
          aaug13vectorsite4,
          aaug20vectorsite4,
          asep03vectorsite4,
          asep10vectorsite4,
          asep17vectorsite4,
          asep24vectorsite4)
nonvec4 <- c(ajun04nonvectorsite4,
             ajun12nonvectorsite4,
             ajun18nonvectorsite4,
             ajun25nonvectorsite4,
             ajul04nonvectorsite4,
             ajul10nonvectorsite4,
             ajul17nonvectorsite4,
             ajul24nonvectorsite4,
             ajul30nonvectorsite4,
             aaug06nonvectorsite4,
             aaug13nonvectorsite4,
             aaug20nonvectorsite4,
             asep03nonvectorsite4,
             asep10nonvectorsite4,
             asep17nonvectorsite4,
             asep24nonvectorsite4)

#Site 5
vec5 <- c(ajun04vectorsite5,
          ajun12vectorsite5,
          ajun18vectorsite5,
          ajun25vectorsite5,
          ajul04vectorsite5,
          ajul10vectorsite5,
          ajul17vectorsite5,
          ajul24vectorsite5,
          ajul30vectorsite5,
          aaug06vectorsite5,
          aaug13vectorsite5,
          aaug20vectorsite5,
          asep03vectorsite5,
          asep10vectorsite5,
          asep17vectorsite5,
          asep24vectorsite5)
nonvec5 <- c(ajun04nonvectorsite5,
             ajun12nonvectorsite5,
             ajun18nonvectorsite5,
             ajun25nonvectorsite5,
             ajul04nonvectorsite5,
             ajul10nonvectorsite5,
             ajul17nonvectorsite5,
             ajul24nonvectorsite5,
             ajul30nonvectorsite5,
             aaug06nonvectorsite5,
             aaug13nonvectorsite5,
             aaug20nonvectorsite5,
             asep03nonvectorsite5,
             asep10nonvectorsite5,
             asep17nonvectorsite5,
             asep24nonvectorsite5)

#Site 6
vec6 <- c(ajun04vectorsite6,
          ajun12vectorsite6,
          ajun18vectorsite6,
          ajun25vectorsite6,
          ajul04vectorsite6,
          ajul10vectorsite6,
          ajul17vectorsite6,
          ajul24vectorsite6,
          ajul30vectorsite6,
          aaug06vectorsite6,
          aaug13vectorsite6,
          aaug20vectorsite6,
          asep03vectorsite6,
          asep10vectorsite6,
          asep17vectorsite6,
          asep24vectorsite6)
nonvec6 <- c(ajun04nonvectorsite6,
             ajun12nonvectorsite6,
             ajun18nonvectorsite6,
             ajun25nonvectorsite6,
             ajul04nonvectorsite6,
             ajul10nonvectorsite6,
             ajul17nonvectorsite6,
             ajul24nonvectorsite6,
             ajul30nonvectorsite6,
             aaug06nonvectorsite6,
             aaug13nonvectorsite6,
             aaug20nonvectorsite6,
             asep03nonvectorsite6,
             asep10nonvectorsite6,
             asep17nonvectorsite6,
             asep24nonvectorsite6)

#Site 7
vec7 <- c(ajun04vectorsite7,
          ajun12vectorsite7,
          ajun18vectorsite7,
          ajun25vectorsite7,
          ajul04vectorsite7,
          ajul10vectorsite7,
          ajul17vectorsite7,
          ajul24vectorsite7,
          ajul30vectorsite7,
          aaug06vectorsite7,
          aaug13vectorsite7,
          aaug20vectorsite7,
          asep03vectorsite7,
          asep10vectorsite7,
          asep17vectorsite7,
          asep24vectorsite7)
nonvec7 <- c(ajun04nonvectorsite7,
             ajun12nonvectorsite7,
             ajun18nonvectorsite7,
             ajun25nonvectorsite7,
             ajul04nonvectorsite7,
             ajul10nonvectorsite7,
             ajul17nonvectorsite7,
             ajul24nonvectorsite7,
             ajul30nonvectorsite7,
             aaug06nonvectorsite7,
             aaug13nonvectorsite7,
             aaug20nonvectorsite7,
             asep03nonvectorsite7,
             asep10nonvectorsite7,
             asep17nonvectorsite7,
             asep24nonvectorsite7)

#Site 8
vec8 <- c(ajun04vectorsite8,
          ajun12vectorsite8,
          ajun18vectorsite8,
          ajun25vectorsite8,
          ajul04vectorsite8,
          ajul10vectorsite8,
          ajul17vectorsite8,
          ajul24vectorsite8,
          ajul30vectorsite8,
          aaug06vectorsite8,
          aaug13vectorsite8,
          aaug20vectorsite8,
          asep03vectorsite8,
          asep10vectorsite8,
          asep17vectorsite8,
          asep24vectorsite8)
nonvec8 <- c(ajun04nonvectorsite8,
             ajun12nonvectorsite8,
             ajun18nonvectorsite8,
             ajun25nonvectorsite8,
             ajul04nonvectorsite8,
             ajul10nonvectorsite8,
             ajul17nonvectorsite8,
             ajul24nonvectorsite8,
             ajul30nonvectorsite8,
             aaug06nonvectorsite8,
             aaug13nonvectorsite8,
             aaug20nonvectorsite8,
             asep03nonvectorsite8,
             asep10nonvectorsite8,
             asep17nonvectorsite8,
             asep24nonvectorsite8)

#Site 9
vec9 <- c(ajun04vectorsite9,
          ajun12vectorsite9,
          ajun18vectorsite9,
          ajun25vectorsite9,
          ajul04vectorsite9,
          ajul10vectorsite9,
          ajul17vectorsite9,
          ajul24vectorsite9,
          ajul30vectorsite9,
          aaug06vectorsite9,
          aaug13vectorsite9,
          aaug20vectorsite9,
          asep03vectorsite9,
          asep10vectorsite9,
          asep17vectorsite9,
          asep24vectorsite9)
nonvec9 <- c(ajun04nonvectorsite9,
             ajun12nonvectorsite9,
             ajun18nonvectorsite9,
             ajun25nonvectorsite9,
             ajul04nonvectorsite9,
             ajul10nonvectorsite9,
             ajul17nonvectorsite9,
             ajul24nonvectorsite9,
             ajul30nonvectorsite9,
             aaug06nonvectorsite9,
             aaug13nonvectorsite9,
             aaug20nonvectorsite9,
             asep03nonvectorsite9,
             asep10nonvectorsite9,
             asep17nonvectorsite9,
             asep24nonvectorsite9)

#Site 10
vec10 <- c(ajun04vectorsite10,
           ajun12vectorsite10,
           ajun18vectorsite10,
           ajun25vectorsite10,
           ajul04vectorsite10,
           ajul10vectorsite10,
           ajul17vectorsite10,
           ajul24vectorsite10,
           ajul30vectorsite10,
           aaug06vectorsite10,
           aaug13vectorsite10,
           aaug20vectorsite10,
           asep03vectorsite10,
           asep10vectorsite10,
           asep17vectorsite10,
           asep24vectorsite10)

nonvec10 <- c(ajun04nonvectorsite10,
              ajun12nonvectorsite10,
              ajun18nonvectorsite10,
              ajun25nonvectorsite10,
              ajul04nonvectorsite10,
              ajul10nonvectorsite10,
              ajul17nonvectorsite10,
              ajul24nonvectorsite10,
              ajul30nonvectorsite10,
              aaug06nonvectorsite10,
              aaug13nonvectorsite10,
              aaug20nonvectorsite10,
              asep03nonvectorsite10,
              asep10nonvectorsite10,
              asep17nonvectorsite10,
              asep24nonvectorsite10)

##
###FOR ALL OTHER DISEASES
#(NEXT 5000 lines or so)

##### wnv 1 - WNV
#SUBSET BY DATE and wnv
jun04wnv <- subset(mergedwnv, date2  == as.Date("2013-06-04"))
jun12wnv <- subset(mergedwnv, date2  == as.Date("2013-06-12"))
jun18wnv <- subset(mergedwnv, date2  == as.Date("2013-06-18"))
jun25wnv <- subset(mergedwnv, date2  == as.Date("2013-06-25"))
jul04wnv <- subset(mergedwnv, date2  == as.Date("2013-07-04"))
jul10wnv <- subset(mergedwnv, date2  == as.Date("2013-07-10"))
jul17wnv <- subset(mergedwnv, date2  == as.Date("2013-07-17"))
jul24wnv <- subset(mergedwnv, date2  == as.Date("2013-07-24"))
jul30wnv <- subset(mergedwnv, date2  == as.Date("2013-07-30"))
aug06wnv <- subset(mergedwnv, date2  == as.Date("2013-08-06"))
aug13wnv <- subset(mergedwnv, date2  == as.Date("2013-08-13"))
aug20wnv <- subset(mergedwnv, date2  == as.Date("2013-08-20"))
sep03wnv <- subset(mergedwnv, date2  == as.Date("2013-09-03"))
sep10wnv <- subset(mergedwnv, date2  == as.Date("2013-09-10"))
sep17wnv <- subset(mergedwnv, date2  == as.Date("2013-09-17"))
sep24wnv <- subset(mergedwnv, date2  == as.Date("2013-09-24"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE wnv SITE

#JUN 04 wnv
jun04wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-04"))


#jun12 wnv
jun12wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-12"))

#jun18 wnv
jun18wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-18"))

#jun25 wnv
jun25wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-25"))

#jul04 wnv
jul04wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-04"))

#jul10 wnv
jul10wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-10"))

#jul17 wnv
jul17wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-17"))

#jul24 wnv
jul24wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-24"))

#jul30 wnv
jul30wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-30"))

#aug06 wnv
aug06wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-08-06"))

#aug13 wnv
aug13wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-08-13"))

#aug20 wnv
aug20wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-08-20"))

#sep03 wnv
sep03wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-09-03"))

#sep10 wnv
sep10wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-09-10"))
sep10wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-09-10"))


#sep17 wnv
sep17wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-09-17"))
sep17wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-09-17"))


#sep24 wnv
sep24wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-09-24"))
sep24wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-09-24"))

##### ALL SITES COMBINED
#wnv
ajun04wnvtotal <- sum(jun04wnv$total, na.rm=T)
ajun12wnvtotal <- sum(jun12wnv$total, na.rm=T)
ajun18wnvtotal <- sum(jun18wnv$total, na.rm=T)
ajun25wnvtotal <- sum(jun25wnv$total, na.rm=T)
ajul04wnvtotal <- sum(jul04wnv$total, na.rm=T)
ajul10wnvtotal <- sum(jul10wnv$total, na.rm=T)
ajul17wnvtotal <- sum(jul17wnv$total, na.rm=T)
ajul24wnvtotal <- sum(jul24wnv$total, na.rm=T)
ajul30wnvtotal <- sum(jul30wnv$total, na.rm=T)
aaug06wnvtotal <- sum(aug06wnv$total, na.rm=T)
aaug13wnvtotal <- sum(aug13wnv$total, na.rm=T)
aaug20wnvtotal <- sum(aug20wnv$total, na.rm=T)
asep03wnvtotal <- sum(sep03wnv$total, na.rm=T)
asep10wnvtotal <- sum(sep10wnv$total, na.rm=T)
asep17wnvtotal <- sum(sep17wnv$total, na.rm=T)
asep24wnvtotal <- sum(sep24wnv$total, na.rm=T)




##### site1
#wnv
ajun04wnvsite1 <- sum(jun04wnvsite1$X1, na.rm=T)
ajun12wnvsite1 <- sum(jun12wnvsite1$X1, na.rm=T)
ajun18wnvsite1 <- sum(jun18wnvsite1$X1, na.rm=T)
ajun25wnvsite1 <- sum(jun25wnvsite1$X1, na.rm=T)
ajul04wnvsite1 <- sum(jul04wnvsite1$X1, na.rm=T)
ajul10wnvsite1 <- sum(jul10wnvsite1$X1, na.rm=T)
ajul17wnvsite1 <- sum(jul17wnvsite1$X1, na.rm=T)
ajul24wnvsite1 <- sum(jul24wnvsite1$X1, na.rm=T)
ajul30wnvsite1 <- sum(jul30wnvsite1$X1, na.rm=T)
aaug06wnvsite1 <- sum(aug06wnvsite1$X1, na.rm=T)
aaug13wnvsite1 <- sum(aug13wnvsite1$X1, na.rm=T)
aaug20wnvsite1 <- sum(aug20wnvsite1$X1, na.rm=T)
asep03wnvsite1 <- sum(sep03wnvsite1$X1, na.rm=T)
asep10wnvsite1 <- sum(sep10wnvsite1$X1, na.rm=T)
asep17wnvsite1 <- sum(sep17wnvsite1$X1, na.rm=T)
asep24wnvsite1 <- sum(sep24wnvsite1$X1, na.rm=T)



##### site2
#wnv
ajun04wnvsite2 <- sum(jun04wnvsite2$X2, na.rm=T)
ajun12wnvsite2 <- sum(jun12wnvsite2$X2, na.rm=T)
ajun18wnvsite2 <- sum(jun18wnvsite2$X2, na.rm=T)
ajun25wnvsite2 <- sum(jun25wnvsite2$X2, na.rm=T)
ajul04wnvsite2 <- sum(jul04wnvsite2$X2, na.rm=T)
ajul10wnvsite2 <- sum(jul10wnvsite2$X2, na.rm=T)
ajul17wnvsite2 <- sum(jul17wnvsite2$X2, na.rm=T)
ajul24wnvsite2 <- sum(jul24wnvsite2$X2, na.rm=T)
ajul30wnvsite2 <- sum(jul30wnvsite2$X2, na.rm=T)
aaug06wnvsite2 <- sum(aug06wnvsite2$X2, na.rm=T)
aaug13wnvsite2 <- sum(aug13wnvsite2$X2, na.rm=T)
aaug20wnvsite2 <- sum(aug20wnvsite2$X2, na.rm=T)
asep03wnvsite2 <- sum(sep03wnvsite2$X2, na.rm=T)
asep10wnvsite2 <- sum(sep10wnvsite2$X2, na.rm=T)
asep17wnvsite2 <- sum(sep17wnvsite2$X2, na.rm=T)
asep24wnvsite2 <- sum(sep24wnvsite2$X2, na.rm=T)



##### site3
#wnv
ajun04wnvsite3 <- sum(jun04wnvsite3$X3, na.rm=T)
ajun12wnvsite3 <- sum(jun12wnvsite3$X3, na.rm=T)
ajun18wnvsite3 <- sum(jun18wnvsite3$X3, na.rm=T)
ajun25wnvsite3 <- sum(jun25wnvsite3$X3, na.rm=T)
ajul04wnvsite3 <- sum(jul04wnvsite3$X3, na.rm=T)
ajul10wnvsite3 <- sum(jul10wnvsite3$X3, na.rm=T)
ajul17wnvsite3 <- sum(jul17wnvsite3$X3, na.rm=T)
ajul24wnvsite3 <- sum(jul24wnvsite3$X3, na.rm=T)
ajul30wnvsite3 <- sum(jul30wnvsite3$X3, na.rm=T)
aaug06wnvsite3 <- sum(aug06wnvsite3$X3, na.rm=T)
aaug13wnvsite3 <- sum(aug13wnvsite3$X3, na.rm=T)
aaug20wnvsite3 <- sum(aug20wnvsite3$X3, na.rm=T)
asep03wnvsite3 <- sum(sep03wnvsite3$X3, na.rm=T)
asep10wnvsite3 <- sum(sep10wnvsite3$X3, na.rm=T)
asep17wnvsite3 <- sum(sep17wnvsite3$X3, na.rm=T)
asep24wnvsite3 <- sum(sep24wnvsite3$X3, na.rm=T)



##### site4
#wnv
ajun04wnvsite4 <- sum(jun04wnvsite4$X4, na.rm=T)
ajun12wnvsite4 <- sum(jun12wnvsite4$X4, na.rm=T)
ajun18wnvsite4 <- sum(jun18wnvsite4$X4, na.rm=T)
ajun25wnvsite4 <- sum(jun25wnvsite4$X4, na.rm=T)
ajul04wnvsite4 <- sum(jul04wnvsite4$X4, na.rm=T)
ajul10wnvsite4 <- sum(jul10wnvsite4$X4, na.rm=T)
ajul17wnvsite4 <- sum(jul17wnvsite4$X4, na.rm=T)
ajul24wnvsite4 <- sum(jul24wnvsite4$X4, na.rm=T)
ajul30wnvsite4 <- sum(jul30wnvsite4$X4, na.rm=T)
aaug06wnvsite4 <- sum(aug06wnvsite4$X4, na.rm=T)
aaug13wnvsite4 <- sum(aug13wnvsite4$X4, na.rm=T)
aaug20wnvsite4 <- sum(aug20wnvsite4$X4, na.rm=T)
asep03wnvsite4 <- sum(sep03wnvsite4$X4, na.rm=T)
asep10wnvsite4 <- sum(sep10wnvsite4$X4, na.rm=T)
asep17wnvsite4 <- sum(sep17wnvsite4$X4, na.rm=T)
asep24wnvsite4 <- sum(sep24wnvsite4$X4, na.rm=T)




##### site5
#wnv
ajun04wnvsite5 <- sum(jun04wnvsite5$X5, na.rm=T)
ajun12wnvsite5 <- sum(jun12wnvsite5$X5, na.rm=T)
ajun18wnvsite5 <- sum(jun18wnvsite5$X5, na.rm=T)
ajun25wnvsite5 <- sum(jun25wnvsite5$X5, na.rm=T)
ajul04wnvsite5 <- sum(jul04wnvsite5$X5, na.rm=T)
ajul10wnvsite5 <- sum(jul10wnvsite5$X5, na.rm=T)
ajul17wnvsite5 <- sum(jul17wnvsite5$X5, na.rm=T)
ajul24wnvsite5 <- sum(jul24wnvsite5$X5, na.rm=T)
ajul30wnvsite5 <- sum(jul30wnvsite5$X5, na.rm=T)
aaug06wnvsite5 <- sum(aug06wnvsite5$X5, na.rm=T)
aaug13wnvsite5 <- sum(aug13wnvsite5$X5, na.rm=T)
aaug20wnvsite5 <- sum(aug20wnvsite5$X5, na.rm=T)
asep03wnvsite5 <- sum(sep03wnvsite5$X5, na.rm=T)
asep10wnvsite5 <- sum(sep10wnvsite5$X5, na.rm=T)
asep17wnvsite5 <- sum(sep17wnvsite5$X5, na.rm=T)
asep24wnvsite5 <- sum(sep24wnvsite5$X5, na.rm=T)



##### site6
#wnv
jun04wnvsite6$X6 <- as.numeric(jun04wnvsite6$X6)
jun12wnvsite6$X6 <- as.numeric(jun12wnvsite6$X6)
jun18wnvsite6$X6 <- as.numeric(jun18wnvsite6$X6)
jun25wnvsite6$X6 <- as.numeric(jun25wnvsite6$X6)
jul04wnvsite6$X6 <- as.numeric(jul04wnvsite6$X6)
jul10wnvsite6$X6 <- as.numeric(jul10wnvsite6$X6)
jul17wnvsite6$X6 <- as.numeric(jul17wnvsite6$X6)
jul24wnvsite6$X6 <- as.numeric(jul24wnvsite6$X6)
jul30wnvsite6$X6 <- as.numeric(jul30wnvsite6$X6)
aug06wnvsite6$X6 <- as.numeric(aug06wnvsite6$X6)
aug13wnvsite6$X6 <- as.numeric(aug13wnvsite6$X6)
aug20wnvsite6$X6 <- as.numeric(aug20wnvsite6$X6)
sep03wnvsite6$X6 <- as.numeric(sep03wnvsite6$X6)
sep10wnvsite6$X6 <- as.numeric(sep10wnvsite6$X6)
sep17wnvsite6$X6 <- as.numeric(sep17wnvsite6$X6)
sep24wnvsite6$X6 <- as.numeric(sep24wnvsite6$X6)


ajun04wnvsite6 <- sum(jun04wnvsite6$X6, na.rm=T)
ajun12wnvsite6 <- sum(jun12wnvsite6$X6, na.rm=T)
ajun18wnvsite6 <- sum(jun18wnvsite6$X6, na.rm=T)
ajun25wnvsite6 <- sum(jun25wnvsite6$X6, na.rm=T)
ajul04wnvsite6 <- sum(jul04wnvsite6$X6, na.rm=T)
ajul10wnvsite6 <- sum(jul10wnvsite6$X6, na.rm=T)
ajul17wnvsite6 <- sum(jul17wnvsite6$X6, na.rm=T)
ajul24wnvsite6 <- sum(jul24wnvsite6$X6, na.rm=T)
ajul30wnvsite6 <- sum(jul30wnvsite6$X6, na.rm=T)
aaug06wnvsite6 <- sum(aug06wnvsite6$X6, na.rm=T)
aaug13wnvsite6 <- sum(aug13wnvsite6$X6, na.rm=T)
aaug20wnvsite6 <- sum(aug20wnvsite6$X6, na.rm=T)
asep03wnvsite6 <- sum(sep03wnvsite6$X6, na.rm=T)
asep10wnvsite6 <- sum(sep10wnvsite6$X6, na.rm=T)
asep17wnvsite6 <- sum(sep17wnvsite6$X6, na.rm=T)
asep24wnvsite6 <- sum(sep24wnvsite6$X6, na.rm=T)


##### site7
#wnv
ajun04wnvsite7 <- sum(jun04wnvsite7$X7, na.rm=T)
ajun12wnvsite7 <- sum(jun12wnvsite7$X7, na.rm=T)
ajun18wnvsite7 <- sum(jun18wnvsite7$X7, na.rm=T)
ajun25wnvsite7 <- sum(jun25wnvsite7$X7, na.rm=T)
ajul04wnvsite7 <- sum(jul04wnvsite7$X7, na.rm=T)
ajul10wnvsite7 <- sum(jul10wnvsite7$X7, na.rm=T)
ajul17wnvsite7 <- sum(jul17wnvsite7$X7, na.rm=T)
ajul24wnvsite7 <- sum(jul24wnvsite7$X7, na.rm=T)
ajul30wnvsite7 <- sum(jul30wnvsite7$X7, na.rm=T)
aaug06wnvsite7 <- sum(aug06wnvsite7$X7, na.rm=T)
aaug13wnvsite7 <- sum(aug13wnvsite7$X7, na.rm=T)
aaug20wnvsite7 <- sum(aug20wnvsite7$X7, na.rm=T)
asep03wnvsite7 <- sum(sep03wnvsite7$X7, na.rm=T)
asep10wnvsite7 <- sum(sep10wnvsite7$X7, na.rm=T)
asep17wnvsite7 <- sum(sep17wnvsite7$X7, na.rm=T)
asep24wnvsite7 <- sum(sep24wnvsite7$X7, na.rm=T)



##### site8
#wnv
ajun04wnvsite8 <- sum(jun04wnvsite8$X8, na.rm=T)
ajun12wnvsite8 <- sum(jun12wnvsite8$X8, na.rm=T)
ajun18wnvsite8 <- sum(jun18wnvsite8$X8, na.rm=T)
ajun25wnvsite8 <- sum(jun25wnvsite8$X8, na.rm=T)
ajul04wnvsite8 <- sum(jul04wnvsite8$X8, na.rm=T)
ajul10wnvsite8 <- sum(jul10wnvsite8$X8, na.rm=T)
ajul17wnvsite8 <- sum(jul17wnvsite8$X8, na.rm=T)
ajul24wnvsite8 <- sum(jul24wnvsite8$X8, na.rm=T)
ajul30wnvsite8 <- sum(jul30wnvsite8$X8, na.rm=T)
aaug06wnvsite8 <- sum(aug06wnvsite8$X8, na.rm=T)
aaug13wnvsite8 <- sum(aug13wnvsite8$X8, na.rm=T)
aaug20wnvsite8 <- sum(aug20wnvsite8$X8, na.rm=T)
asep03wnvsite8 <- sum(sep03wnvsite8$X8, na.rm=T)
asep10wnvsite8 <- sum(sep10wnvsite8$X8, na.rm=T)
asep17wnvsite8 <- sum(sep17wnvsite8$X8, na.rm=T)
asep24wnvsite8 <- sum(sep24wnvsite8$X8, na.rm=T)



##### site9
#wnv

jun04wnvsite9$X9 <- as.numeric(jun04wnvsite9$X9)
jun12wnvsite9$X9 <- as.numeric(jun12wnvsite9$X9)
jun18wnvsite9$X9 <- as.numeric(jun18wnvsite9$X9)
jun25wnvsite9$X9 <- as.numeric(jun25wnvsite9$X9)
jul04wnvsite9$X9 <- as.numeric(jul04wnvsite9$X9)
jul10wnvsite9$X9 <- as.numeric(jul10wnvsite9$X9)
jul17wnvsite9$X9 <- as.numeric(jul17wnvsite9$X9)
jul24wnvsite9$X9 <- as.numeric(jul24wnvsite9$X9)
jul30wnvsite9$X9 <- as.numeric(jul30wnvsite9$X9)
aug06wnvsite9$X9 <- as.numeric(aug06wnvsite9$X9)
aug13wnvsite9$X9 <- as.numeric(aug13wnvsite9$X9)
aug20wnvsite9$X9 <- as.numeric(aug20wnvsite9$X9)
sep03wnvsite9$X9 <- as.numeric(sep03wnvsite9$X9)
sep10wnvsite9$X9 <- as.numeric(sep10wnvsite9$X9)
sep17wnvsite9$X9 <- as.numeric(sep17wnvsite9$X9)
sep24wnvsite9$X9 <- as.numeric(sep24wnvsite9$X9)


ajun04wnvsite9 <- sum(jun04wnvsite9$X9, na.rm=T)
ajun12wnvsite9 <- sum(jun12wnvsite9$X9, na.rm=T)
ajun18wnvsite9 <- sum(jun18wnvsite9$X9, na.rm=T)
ajun25wnvsite9 <- sum(jun25wnvsite9$X9, na.rm=T)
ajul04wnvsite9 <- sum(jul04wnvsite9$X9, na.rm=T)
ajul10wnvsite9 <- sum(jul10wnvsite9$X9, na.rm=T)
ajul17wnvsite9 <- sum(jul17wnvsite9$X9, na.rm=T)
ajul24wnvsite9 <- sum(jul24wnvsite9$X9, na.rm=T)
ajul30wnvsite9 <- sum(jul30wnvsite9$X9, na.rm=T)
aaug06wnvsite9 <- sum(aug06wnvsite9$X9, na.rm=T)
aaug13wnvsite9 <- sum(aug13wnvsite9$X9, na.rm=T)
aaug20wnvsite9 <- sum(aug20wnvsite9$X9, na.rm=T)
asep03wnvsite9 <- sum(sep03wnvsite9$X9, na.rm=T)
asep10wnvsite9 <- sum(sep10wnvsite9$X9, na.rm=T)
asep17wnvsite9 <- sum(sep17wnvsite9$X9, na.rm=T)
asep24wnvsite9 <- sum(sep24wnvsite9$X9, na.rm=T)



##### site10
#wnv
jun04wnvsite10$X10 <- as.numeric(jun04wnvsite10$X10)
jun12wnvsite10$X10 <- as.numeric(jun12wnvsite10$X10)
jun18wnvsite10$X10 <- as.numeric(jun18wnvsite10$X10)
jun25wnvsite10$X10 <- as.numeric(jun25wnvsite10$X10)
jul04wnvsite10$X10 <- as.numeric(jul04wnvsite10$X10)
jul10wnvsite10$X10 <- as.numeric(jul10wnvsite10$X10)
jul17wnvsite10$X10 <- as.numeric(jul17wnvsite10$X10)
jul24wnvsite10$X10 <- as.numeric(jul24wnvsite10$X10)
jul30wnvsite10$X10 <- as.numeric(jul30wnvsite10$X10)
aug06wnvsite10$X10 <- as.numeric(aug06wnvsite10$X10)
aug13wnvsite10$X10 <- as.numeric(aug13wnvsite10$X10)
aug20wnvsite10$X10 <- as.numeric(aug20wnvsite10$X10)
sep03wnvsite10$X10 <- as.numeric(sep03wnvsite10$X10)
sep10wnvsite10$X10 <- as.numeric(sep10wnvsite10$X10)
sep17wnvsite10$X10 <- as.numeric(sep17wnvsite10$X10)
sep24wnvsite10$X10 <- as.numeric(sep24wnvsite10$X10)


ajun04wnvsite10 <- sum(jun04wnvsite10$X10, na.rm=T)
ajun12wnvsite10 <- sum(jun12wnvsite10$X10, na.rm=T)
ajun18wnvsite10 <- sum(jun18wnvsite10$X10, na.rm=T)
ajun25wnvsite10 <- sum(jun25wnvsite10$X10, na.rm=T)
ajul04wnvsite10 <- sum(jul04wnvsite10$X10, na.rm=T)
ajul10wnvsite10 <- sum(jul10wnvsite10$X10, na.rm=T)
ajul17wnvsite10 <- sum(jul17wnvsite10$X10, na.rm=T)
ajul24wnvsite10 <- sum(jul24wnvsite10$X10, na.rm=T)
ajul30wnvsite10 <- sum(jul30wnvsite10$X10, na.rm=T)
aaug06wnvsite10 <- sum(aug06wnvsite10$X10, na.rm=T)
aaug13wnvsite10 <- sum(aug13wnvsite10$X10, na.rm=T)
aaug20wnvsite10 <- sum(aug20wnvsite10$X10, na.rm=T)
asep03wnvsite10 <- sum(sep03wnvsite10$X10, na.rm=T)
asep10wnvsite10 <- sum(sep10wnvsite10$X10, na.rm=T)
asep17wnvsite10 <- sum(sep17wnvsite10$X10, na.rm=T)
asep24wnvsite10 <- sum(sep24wnvsite10$X10, na.rm=T)



##
#
#
#
#
#



#CREATE wnvS FOR PLOTTING
#Site 1
wnv1 <- c(ajun04wnvsite1,
          ajun12wnvsite1,
          ajun18wnvsite1,
          ajun25wnvsite1,
          ajul04wnvsite1,
          ajul10wnvsite1,
          ajul17wnvsite1,
          ajul24wnvsite1,
          ajul30wnvsite1,
          aaug06wnvsite1,
          aaug13wnvsite1,
          aaug20wnvsite1,
          asep03wnvsite1,
          asep10wnvsite1,
          asep17wnvsite1,
          asep24wnvsite1)

#Site 2
wnv2 <- c(ajun04wnvsite2,
          ajun12wnvsite2,
          ajun18wnvsite2,
          ajun25wnvsite2,
          ajul04wnvsite2,
          ajul10wnvsite2,
          ajul17wnvsite2,
          ajul24wnvsite2,
          ajul30wnvsite2,
          aaug06wnvsite2,
          aaug13wnvsite2,
          aaug20wnvsite2,
          asep03wnvsite2,
          asep10wnvsite2,
          asep17wnvsite2,
          asep24wnvsite2)


#Site 3
wnv3 <- c(ajun04wnvsite3,
          ajun12wnvsite3,
          ajun18wnvsite3,
          ajun25wnvsite3,
          ajul04wnvsite3,
          ajul10wnvsite3,
          ajul17wnvsite3,
          ajul24wnvsite3,
          ajul30wnvsite3,
          aaug06wnvsite3,
          aaug13wnvsite3,
          aaug20wnvsite3,
          asep03wnvsite3,
          asep10wnvsite3,
          asep17wnvsite3,
          asep24wnvsite3)


#Site 4
wnv4 <- c(ajun04wnvsite4,
          ajun12wnvsite4,
          ajun18wnvsite4,
          ajun25wnvsite4,
          ajul04wnvsite4,
          ajul10wnvsite4,
          ajul17wnvsite4,
          ajul24wnvsite4,
          ajul30wnvsite4,
          aaug06wnvsite4,
          aaug13wnvsite4,
          aaug20wnvsite4,
          asep03wnvsite4,
          asep10wnvsite4,
          asep17wnvsite4,
          asep24wnvsite4)


#Site 5
wnv5 <- c(ajun04wnvsite5,
          ajun12wnvsite5,
          ajun18wnvsite5,
          ajun25wnvsite5,
          ajul04wnvsite5,
          ajul10wnvsite5,
          ajul17wnvsite5,
          ajul24wnvsite5,
          ajul30wnvsite5,
          aaug06wnvsite5,
          aaug13wnvsite5,
          aaug20wnvsite5,
          asep03wnvsite5,
          asep10wnvsite5,
          asep17wnvsite5,
          asep24wnvsite5)


#Site 6
wnv6 <- c(ajun04wnvsite6,
          ajun12wnvsite6,
          ajun18wnvsite6,
          ajun25wnvsite6,
          ajul04wnvsite6,
          ajul10wnvsite6,
          ajul17wnvsite6,
          ajul24wnvsite6,
          ajul30wnvsite6,
          aaug06wnvsite6,
          aaug13wnvsite6,
          aaug20wnvsite6,
          asep03wnvsite6,
          asep10wnvsite6,
          asep17wnvsite6,
          asep24wnvsite6)


#Site 7
wnv7 <- c(ajun04wnvsite7,
          ajun12wnvsite7,
          ajun18wnvsite7,
          ajun25wnvsite7,
          ajul04wnvsite7,
          ajul10wnvsite7,
          ajul17wnvsite7,
          ajul24wnvsite7,
          ajul30wnvsite7,
          aaug06wnvsite7,
          aaug13wnvsite7,
          aaug20wnvsite7,
          asep03wnvsite7,
          asep10wnvsite7,
          asep17wnvsite7,
          asep24wnvsite7)


#Site 8
wnv8 <- c(ajun04wnvsite8,
          ajun12wnvsite8,
          ajun18wnvsite8,
          ajun25wnvsite8,
          ajul04wnvsite8,
          ajul10wnvsite8,
          ajul17wnvsite8,
          ajul24wnvsite8,
          ajul30wnvsite8,
          aaug06wnvsite8,
          aaug13wnvsite8,
          aaug20wnvsite8,
          asep03wnvsite8,
          asep10wnvsite8,
          asep17wnvsite8,
          asep24wnvsite8)


#Site 9
wnv9 <- c(ajun04wnvsite9,
          ajun12wnvsite9,
          ajun18wnvsite9,
          ajun25wnvsite9,
          ajul04wnvsite9,
          ajul10wnvsite9,
          ajul17wnvsite9,
          ajul24wnvsite9,
          ajul30wnvsite9,
          aaug06wnvsite9,
          aaug13wnvsite9,
          aaug20wnvsite9,
          asep03wnvsite9,
          asep10wnvsite9,
          asep17wnvsite9,
          asep24wnvsite9)


#Site 10
wnv10 <- c(ajun04wnvsite10,
           ajun12wnvsite10,
           ajun18wnvsite10,
           ajun25wnvsite10,
           ajul04wnvsite10,
           ajul10wnvsite10,
           ajul17wnvsite10,
           ajul24wnvsite10,
           ajul30wnvsite10,
           aaug06wnvsite10,
           aaug13wnvsite10,
           aaug20wnvsite10,
           asep03wnvsite10,
           asep10wnvsite10,
           asep17wnvsite10,
           asep24wnvsite10)



wnvtotal <- wnv1+wnv2+wnv3+wnv4+wnv5+wnv6+wnv7+wnv8+wnv9+wnv10


##### slev 2 - SLEV
#SUBSET BY DATE and slev
jun04slev <- subset(mergedslev, date2  == as.Date("2013-06-04"))
jun12slev <- subset(mergedslev, date2  == as.Date("2013-06-12"))
jun18slev <- subset(mergedslev, date2  == as.Date("2013-06-18"))
jun25slev <- subset(mergedslev, date2  == as.Date("2013-06-25"))
jul04slev <- subset(mergedslev, date2  == as.Date("2013-07-04"))
jul10slev <- subset(mergedslev, date2  == as.Date("2013-07-10"))
jul17slev <- subset(mergedslev, date2  == as.Date("2013-07-17"))
jul24slev <- subset(mergedslev, date2  == as.Date("2013-07-24"))
jul30slev <- subset(mergedslev, date2  == as.Date("2013-07-30"))
aug06slev <- subset(mergedslev, date2  == as.Date("2013-08-06"))
aug13slev <- subset(mergedslev, date2  == as.Date("2013-08-13"))
aug20slev <- subset(mergedslev, date2  == as.Date("2013-08-20"))
sep03slev <- subset(mergedslev, date2  == as.Date("2013-09-03"))
sep10slev <- subset(mergedslev, date2  == as.Date("2013-09-10"))
sep17slev <- subset(mergedslev, date2  == as.Date("2013-09-17"))
sep24slev <- subset(mergedslev, date2  == as.Date("2013-09-24"))



####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE slev SITE

#JUN 04 slev
jun04slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-04"))
jun04slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-04"))
jun04slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-04"))
jun04slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-04"))
jun04slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-04"))
jun04slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-04"))
jun04slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-04"))
jun04slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-04"))
jun04slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-04"))
jun04slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-04"))


#jun12 slev
jun12slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-12"))
jun12slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-12"))
jun12slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-12"))
jun12slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-12"))
jun12slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-12"))
jun12slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-12"))
jun12slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-12"))
jun12slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-12"))
jun12slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-12"))
jun12slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-12"))

#jun18 slev
jun18slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-18"))
jun18slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-18"))
jun18slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-18"))
jun18slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-18"))
jun18slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-18"))
jun18slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-18"))
jun18slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-18"))
jun18slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-18"))
jun18slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-18"))
jun18slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-18"))

#jun25 slev
jun25slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-25"))
jun25slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-25"))
jun25slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-25"))
jun25slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-25"))
jun25slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-25"))
jun25slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-25"))
jun25slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-25"))
jun25slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-25"))
jun25slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-25"))
jun25slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-25"))

#jul04 slev
jul04slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-04"))
jul04slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-04"))
jul04slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-04"))
jul04slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-04"))
jul04slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-04"))
jul04slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-04"))
jul04slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-04"))
jul04slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-04"))
jul04slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-04"))
jul04slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-04"))

#jul10 slev
jul10slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-10"))
jul10slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-10"))
jul10slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-10"))
jul10slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-10"))
jul10slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-10"))
jul10slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-10"))
jul10slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-10"))
jul10slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-10"))
jul10slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-10"))
jul10slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-10"))

#jul17 slev
jul17slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-17"))
jul17slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-17"))
jul17slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-17"))
jul17slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-17"))
jul17slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-17"))
jul17slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-17"))
jul17slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-17"))
jul17slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-17"))
jul17slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-17"))
jul17slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-17"))

#jul24 slev
jul24slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-24"))
jul24slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-24"))
jul24slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-24"))
jul24slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-24"))
jul24slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-24"))
jul24slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-24"))
jul24slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-24"))
jul24slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-24"))
jul24slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-24"))
jul24slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-24"))

#jul30 slev
jul30slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-30"))
jul30slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-30"))
jul30slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-30"))
jul30slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-30"))
jul30slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-30"))
jul30slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-30"))
jul30slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-30"))
jul30slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-30"))
jul30slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-30"))
jul30slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-30"))

#aug06 slev
aug06slevsite1 <-subset(merged1slev, date2  == as.Date("2013-08-06"))
aug06slevsite2 <-subset(merged2slev, date2  == as.Date("2013-08-06"))
aug06slevsite3 <-subset(merged3slev, date2  == as.Date("2013-08-06"))
aug06slevsite4 <-subset(merged4slev, date2  == as.Date("2013-08-06"))
aug06slevsite5 <-subset(merged5slev, date2  == as.Date("2013-08-06"))
aug06slevsite6 <-subset(merged6slev, date2  == as.Date("2013-08-06"))
aug06slevsite7 <-subset(merged7slev, date2  == as.Date("2013-08-06"))
aug06slevsite8 <-subset(merged8slev, date2  == as.Date("2013-08-06"))
aug06slevsite9 <-subset(merged9slev, date2  == as.Date("2013-08-06"))
aug06slevsite10 <-subset(merged10slev, date2  == as.Date("2013-08-06"))

#aug13 slev
aug13slevsite1 <-subset(merged1slev, date2  == as.Date("2013-08-13"))
aug13slevsite2 <-subset(merged2slev, date2  == as.Date("2013-08-13"))
aug13slevsite3 <-subset(merged3slev, date2  == as.Date("2013-08-13"))
aug13slevsite4 <-subset(merged4slev, date2  == as.Date("2013-08-13"))
aug13slevsite5 <-subset(merged5slev, date2  == as.Date("2013-08-13"))
aug13slevsite6 <-subset(merged6slev, date2  == as.Date("2013-08-13"))
aug13slevsite7 <-subset(merged7slev, date2  == as.Date("2013-08-13"))
aug13slevsite8 <-subset(merged8slev, date2  == as.Date("2013-08-13"))
aug13slevsite9 <-subset(merged9slev, date2  == as.Date("2013-08-13"))
aug13slevsite10 <-subset(merged10slev, date2  == as.Date("2013-08-13"))

#aug20 slev
aug20slevsite1 <-subset(merged1slev, date2  == as.Date("2013-08-20"))
aug20slevsite2 <-subset(merged2slev, date2  == as.Date("2013-08-20"))
aug20slevsite3 <-subset(merged3slev, date2  == as.Date("2013-08-20"))
aug20slevsite4 <-subset(merged4slev, date2  == as.Date("2013-08-20"))
aug20slevsite5 <-subset(merged5slev, date2  == as.Date("2013-08-20"))
aug20slevsite6 <-subset(merged6slev, date2  == as.Date("2013-08-20"))
aug20slevsite7 <-subset(merged7slev, date2  == as.Date("2013-08-20"))
aug20slevsite8 <-subset(merged8slev, date2  == as.Date("2013-08-20"))
aug20slevsite9 <-subset(merged9slev, date2  == as.Date("2013-08-20"))
aug20slevsite10 <-subset(merged10slev, date2  == as.Date("2013-08-20"))

#sep03 slev
sep03slevsite1 <-subset(merged1slev, date2  == as.Date("2013-09-03"))
sep03slevsite2 <-subset(merged2slev, date2  == as.Date("2013-09-03"))
sep03slevsite3 <-subset(merged3slev, date2  == as.Date("2013-09-03"))
sep03slevsite4 <-subset(merged4slev, date2  == as.Date("2013-09-03"))
sep03slevsite5 <-subset(merged5slev, date2  == as.Date("2013-09-03"))
sep03slevsite6 <-subset(merged6slev, date2  == as.Date("2013-09-03"))
sep03slevsite7 <-subset(merged7slev, date2  == as.Date("2013-09-03"))
sep03slevsite8 <-subset(merged8slev, date2  == as.Date("2013-09-03"))
sep03slevsite9 <-subset(merged9slev, date2  == as.Date("2013-09-03"))
sep03slevsite10 <-subset(merged10slev, date2  == as.Date("2013-09-03"))

#sep10 slev
sep10slevsite1 <-subset(merged1slev, date2  == as.Date("2013-09-10"))
sep10slevsite2 <-subset(merged2slev, date2  == as.Date("2013-09-10"))
sep10slevsite3 <-subset(merged3slev, date2  == as.Date("2013-09-10"))
sep10slevsite4 <-subset(merged4slev, date2  == as.Date("2013-09-10"))
sep10slevsite5 <-subset(merged5slev, date2  == as.Date("2013-09-10"))
sep10slevsite6 <-subset(merged6slev, date2  == as.Date("2013-09-10"))
sep10slevsite7 <-subset(merged7slev, date2  == as.Date("2013-09-10"))
sep10slevsite8 <-subset(merged8slev, date2  == as.Date("2013-09-10"))
sep10slevsite9 <-subset(merged9slev, date2  == as.Date("2013-09-10"))
sep10slevsite10 <-subset(merged10slev, date2  == as.Date("2013-09-10"))

#sep17 slev
sep17slevsite1 <-subset(merged1slev, date2  == as.Date("2013-09-17"))
sep17slevsite2 <-subset(merged2slev, date2  == as.Date("2013-09-17"))
sep17slevsite3 <-subset(merged3slev, date2  == as.Date("2013-09-17"))
sep17slevsite4 <-subset(merged4slev, date2  == as.Date("2013-09-17"))
sep17slevsite5 <-subset(merged5slev, date2  == as.Date("2013-09-17"))
sep17slevsite6 <-subset(merged6slev, date2  == as.Date("2013-09-17"))
sep17slevsite7 <-subset(merged7slev, date2  == as.Date("2013-09-17"))
sep17slevsite8 <-subset(merged8slev, date2  == as.Date("2013-09-17"))
sep17slevsite9 <-subset(merged9slev, date2  == as.Date("2013-09-17"))
sep17slevsite10 <-subset(merged10slev, date2  == as.Date("2013-09-17"))

#sep24 slev
sep24slevsite1 <-subset(merged1slev, date2  == as.Date("2013-09-24"))
sep24slevsite2 <-subset(merged2slev, date2  == as.Date("2013-09-24"))
sep24slevsite3 <-subset(merged3slev, date2  == as.Date("2013-09-24"))
sep24slevsite4 <-subset(merged4slev, date2  == as.Date("2013-09-24"))
sep24slevsite5 <-subset(merged5slev, date2  == as.Date("2013-09-24"))
sep24slevsite6 <-subset(merged6slev, date2  == as.Date("2013-09-24"))
sep24slevsite7 <-subset(merged7slev, date2  == as.Date("2013-09-24"))
sep24slevsite8 <-subset(merged8slev, date2  == as.Date("2013-09-24"))
sep24slevsite9 <-subset(merged9slev, date2  == as.Date("2013-09-24"))
sep24slevsite10 <-subset(merged10slev, date2  == as.Date("2013-09-24"))

##### ALL SITES COMBINED
#slev
ajun04slevtotal <- sum(jun04slev$total, na.rm=T)
ajun12slevtotal <- sum(jun12slev$total, na.rm=T)
ajun18slevtotal <- sum(jun18slev$total, na.rm=T)
ajun25slevtotal <- sum(jun25slev$total, na.rm=T)
ajul04slevtotal <- sum(jul04slev$total, na.rm=T)
ajul10slevtotal <- sum(jul10slev$total, na.rm=T)
ajul17slevtotal <- sum(jul17slev$total, na.rm=T)
ajul24slevtotal <- sum(jul24slev$total, na.rm=T)
ajul30slevtotal <- sum(jul30slev$total, na.rm=T)
aaug06slevtotal <- sum(aug06slev$total, na.rm=T)
aaug13slevtotal <- sum(aug13slev$total, na.rm=T)
aaug20slevtotal <- sum(aug20slev$total, na.rm=T)
asep03slevtotal <- sum(sep03slev$total, na.rm=T)
asep10slevtotal <- sum(sep10slev$total, na.rm=T)
asep17slevtotal <- sum(sep17slev$total, na.rm=T)
asep24slevtotal <- sum(sep24slev$total, na.rm=T)

##### site1
#slev
ajun04slevsite1 <- sum(jun04slevsite1$X1, na.rm=T)
ajun12slevsite1 <- sum(jun12slevsite1$X1, na.rm=T)
ajun18slevsite1 <- sum(jun18slevsite1$X1, na.rm=T)
ajun25slevsite1 <- sum(jun25slevsite1$X1, na.rm=T)
ajul04slevsite1 <- sum(jul04slevsite1$X1, na.rm=T)
ajul10slevsite1 <- sum(jul10slevsite1$X1, na.rm=T)
ajul17slevsite1 <- sum(jul17slevsite1$X1, na.rm=T)
ajul24slevsite1 <- sum(jul24slevsite1$X1, na.rm=T)
ajul30slevsite1 <- sum(jul30slevsite1$X1, na.rm=T)
aaug06slevsite1 <- sum(aug06slevsite1$X1, na.rm=T)
aaug13slevsite1 <- sum(aug13slevsite1$X1, na.rm=T)
aaug20slevsite1 <- sum(aug20slevsite1$X1, na.rm=T)
asep03slevsite1 <- sum(sep03slevsite1$X1, na.rm=T)
asep10slevsite1 <- sum(sep10slevsite1$X1, na.rm=T)
asep17slevsite1 <- sum(sep17slevsite1$X1, na.rm=T)
asep24slevsite1 <- sum(sep24slevsite1$X1, na.rm=T)



##### site2
#slev
ajun04slevsite2 <- sum(jun04slevsite2$X2, na.rm=T)
ajun12slevsite2 <- sum(jun12slevsite2$X2, na.rm=T)
ajun18slevsite2 <- sum(jun18slevsite2$X2, na.rm=T)
ajun25slevsite2 <- sum(jun25slevsite2$X2, na.rm=T)
ajul04slevsite2 <- sum(jul04slevsite2$X2, na.rm=T)
ajul10slevsite2 <- sum(jul10slevsite2$X2, na.rm=T)
ajul17slevsite2 <- sum(jul17slevsite2$X2, na.rm=T)
ajul24slevsite2 <- sum(jul24slevsite2$X2, na.rm=T)
ajul30slevsite2 <- sum(jul30slevsite2$X2, na.rm=T)
aaug06slevsite2 <- sum(aug06slevsite2$X2, na.rm=T)
aaug13slevsite2 <- sum(aug13slevsite2$X2, na.rm=T)
aaug20slevsite2 <- sum(aug20slevsite2$X2, na.rm=T)
asep03slevsite2 <- sum(sep03slevsite2$X2, na.rm=T)
asep10slevsite2 <- sum(sep10slevsite2$X2, na.rm=T)
asep17slevsite2 <- sum(sep17slevsite2$X2, na.rm=T)
asep24slevsite2 <- sum(sep24slevsite2$X2, na.rm=T)



##### site3
#slev
ajun04slevsite3 <- sum(jun04slevsite3$X3, na.rm=T)
ajun12slevsite3 <- sum(jun12slevsite3$X3, na.rm=T)
ajun18slevsite3 <- sum(jun18slevsite3$X3, na.rm=T)
ajun25slevsite3 <- sum(jun25slevsite3$X3, na.rm=T)
ajul04slevsite3 <- sum(jul04slevsite3$X3, na.rm=T)
ajul10slevsite3 <- sum(jul10slevsite3$X3, na.rm=T)
ajul17slevsite3 <- sum(jul17slevsite3$X3, na.rm=T)
ajul24slevsite3 <- sum(jul24slevsite3$X3, na.rm=T)
ajul30slevsite3 <- sum(jul30slevsite3$X3, na.rm=T)
aaug06slevsite3 <- sum(aug06slevsite3$X3, na.rm=T)
aaug13slevsite3 <- sum(aug13slevsite3$X3, na.rm=T)
aaug20slevsite3 <- sum(aug20slevsite3$X3, na.rm=T)
asep03slevsite3 <- sum(sep03slevsite3$X3, na.rm=T)
asep10slevsite3 <- sum(sep10slevsite3$X3, na.rm=T)
asep17slevsite3 <- sum(sep17slevsite3$X3, na.rm=T)
asep24slevsite3 <- sum(sep24slevsite3$X3, na.rm=T)




##### site4
#slev
ajun04slevsite4 <- sum(jun04slevsite4$X4, na.rm=T)
ajun12slevsite4 <- sum(jun12slevsite4$X4, na.rm=T)
ajun18slevsite4 <- sum(jun18slevsite4$X4, na.rm=T)
ajun25slevsite4 <- sum(jun25slevsite4$X4, na.rm=T)
ajul04slevsite4 <- sum(jul04slevsite4$X4, na.rm=T)
ajul10slevsite4 <- sum(jul10slevsite4$X4, na.rm=T)
ajul17slevsite4 <- sum(jul17slevsite4$X4, na.rm=T)
ajul24slevsite4 <- sum(jul24slevsite4$X4, na.rm=T)
ajul30slevsite4 <- sum(jul30slevsite4$X4, na.rm=T)
aaug06slevsite4 <- sum(aug06slevsite4$X4, na.rm=T)
aaug13slevsite4 <- sum(aug13slevsite4$X4, na.rm=T)
aaug20slevsite4 <- sum(aug20slevsite4$X4, na.rm=T)
asep03slevsite4 <- sum(sep03slevsite4$X4, na.rm=T)
asep10slevsite4 <- sum(sep10slevsite4$X4, na.rm=T)
asep17slevsite4 <- sum(sep17slevsite4$X4, na.rm=T)
asep24slevsite4 <- sum(sep24slevsite4$X4, na.rm=T)




##### site5
#slev
ajun04slevsite5 <- sum(jun04slevsite5$X5, na.rm=T)
ajun12slevsite5 <- sum(jun12slevsite5$X5, na.rm=T)
ajun18slevsite5 <- sum(jun18slevsite5$X5, na.rm=T)
ajun25slevsite5 <- sum(jun25slevsite5$X5, na.rm=T)
ajul04slevsite5 <- sum(jul04slevsite5$X5, na.rm=T)
ajul10slevsite5 <- sum(jul10slevsite5$X5, na.rm=T)
ajul17slevsite5 <- sum(jul17slevsite5$X5, na.rm=T)
ajul24slevsite5 <- sum(jul24slevsite5$X5, na.rm=T)
ajul30slevsite5 <- sum(jul30slevsite5$X5, na.rm=T)
aaug06slevsite5 <- sum(aug06slevsite5$X5, na.rm=T)
aaug13slevsite5 <- sum(aug13slevsite5$X5, na.rm=T)
aaug20slevsite5 <- sum(aug20slevsite5$X5, na.rm=T)
asep03slevsite5 <- sum(sep03slevsite5$X5, na.rm=T)
asep10slevsite5 <- sum(sep10slevsite5$X5, na.rm=T)
asep17slevsite5 <- sum(sep17slevsite5$X5, na.rm=T)
asep24slevsite5 <- sum(sep24slevsite5$X5, na.rm=T)




##### site6
#slev
jun04slevsite6$X6 <- as.numeric(jun04slevsite6$X6)
jun12slevsite6$X6 <- as.numeric(jun12slevsite6$X6)
jun18slevsite6$X6 <- as.numeric(jun18slevsite6$X6)
jun25slevsite6$X6 <- as.numeric(jun25slevsite6$X6)
jul04slevsite6$X6 <- as.numeric(jul04slevsite6$X6)
jul10slevsite6$X6 <- as.numeric(jul10slevsite6$X6)
jul17slevsite6$X6 <- as.numeric(jul17slevsite6$X6)
jul24slevsite6$X6 <- as.numeric(jul24slevsite6$X6)
jul30slevsite6$X6 <- as.numeric(jul30slevsite6$X6)
aug06slevsite6$X6 <- as.numeric(aug06slevsite6$X6)
aug13slevsite6$X6 <- as.numeric(aug13slevsite6$X6)
aug20slevsite6$X6 <- as.numeric(aug20slevsite6$X6)
sep03slevsite6$X6 <- as.numeric(sep03slevsite6$X6)
sep10slevsite6$X6 <- as.numeric(sep10slevsite6$X6)
sep17slevsite6$X6 <- as.numeric(sep17slevsite6$X6)
sep24slevsite6$X6 <- as.numeric(sep24slevsite6$X6)


ajun04slevsite6 <- sum(jun04slevsite6$X6, na.rm=T)
ajun12slevsite6 <- sum(jun12slevsite6$X6, na.rm=T)
ajun18slevsite6 <- sum(jun18slevsite6$X6, na.rm=T)
ajun25slevsite6 <- sum(jun25slevsite6$X6, na.rm=T)
ajul04slevsite6 <- sum(jul04slevsite6$X6, na.rm=T)
ajul10slevsite6 <- sum(jul10slevsite6$X6, na.rm=T)
ajul17slevsite6 <- sum(jul17slevsite6$X6, na.rm=T)
ajul24slevsite6 <- sum(jul24slevsite6$X6, na.rm=T)
ajul30slevsite6 <- sum(jul30slevsite6$X6, na.rm=T)
aaug06slevsite6 <- sum(aug06slevsite6$X6, na.rm=T)
aaug13slevsite6 <- sum(aug13slevsite6$X6, na.rm=T)
aaug20slevsite6 <- sum(aug20slevsite6$X6, na.rm=T)
asep03slevsite6 <- sum(sep03slevsite6$X6, na.rm=T)
asep10slevsite6 <- sum(sep10slevsite6$X6, na.rm=T)
asep17slevsite6 <- sum(sep17slevsite6$X6, na.rm=T)
asep24slevsite6 <- sum(sep24slevsite6$X6, na.rm=T)



##### site7
#slev
ajun04slevsite7 <- sum(jun04slevsite7$X7, na.rm=T)
ajun12slevsite7 <- sum(jun12slevsite7$X7, na.rm=T)
ajun18slevsite7 <- sum(jun18slevsite7$X7, na.rm=T)
ajun25slevsite7 <- sum(jun25slevsite7$X7, na.rm=T)
ajul04slevsite7 <- sum(jul04slevsite7$X7, na.rm=T)
ajul10slevsite7 <- sum(jul10slevsite7$X7, na.rm=T)
ajul17slevsite7 <- sum(jul17slevsite7$X7, na.rm=T)
ajul24slevsite7 <- sum(jul24slevsite7$X7, na.rm=T)
ajul30slevsite7 <- sum(jul30slevsite7$X7, na.rm=T)
aaug06slevsite7 <- sum(aug06slevsite7$X7, na.rm=T)
aaug13slevsite7 <- sum(aug13slevsite7$X7, na.rm=T)
aaug20slevsite7 <- sum(aug20slevsite7$X7, na.rm=T)
asep03slevsite7 <- sum(sep03slevsite7$X7, na.rm=T)
asep10slevsite7 <- sum(sep10slevsite7$X7, na.rm=T)
asep17slevsite7 <- sum(sep17slevsite7$X7, na.rm=T)
asep24slevsite7 <- sum(sep24slevsite7$X7, na.rm=T)




##### site8
#slev
ajun04slevsite8 <- sum(jun04slevsite8$X8, na.rm=T)
ajun12slevsite8 <- sum(jun12slevsite8$X8, na.rm=T)
ajun18slevsite8 <- sum(jun18slevsite8$X8, na.rm=T)
ajun25slevsite8 <- sum(jun25slevsite8$X8, na.rm=T)
ajul04slevsite8 <- sum(jul04slevsite8$X8, na.rm=T)
ajul10slevsite8 <- sum(jul10slevsite8$X8, na.rm=T)
ajul17slevsite8 <- sum(jul17slevsite8$X8, na.rm=T)
ajul24slevsite8 <- sum(jul24slevsite8$X8, na.rm=T)
ajul30slevsite8 <- sum(jul30slevsite8$X8, na.rm=T)
aaug06slevsite8 <- sum(aug06slevsite8$X8, na.rm=T)
aaug13slevsite8 <- sum(aug13slevsite8$X8, na.rm=T)
aaug20slevsite8 <- sum(aug20slevsite8$X8, na.rm=T)
asep03slevsite8 <- sum(sep03slevsite8$X8, na.rm=T)
asep10slevsite8 <- sum(sep10slevsite8$X8, na.rm=T)
asep17slevsite8 <- sum(sep17slevsite8$X8, na.rm=T)
asep24slevsite8 <- sum(sep24slevsite8$X8, na.rm=T)


##### site9
#slev

jun04slevsite9$X9 <- as.numeric(jun04slevsite9$X9)
jun12slevsite9$X9 <- as.numeric(jun12slevsite9$X9)
jun18slevsite9$X9 <- as.numeric(jun18slevsite9$X9)
jun25slevsite9$X9 <- as.numeric(jun25slevsite9$X9)
jul04slevsite9$X9 <- as.numeric(jul04slevsite9$X9)
jul10slevsite9$X9 <- as.numeric(jul10slevsite9$X9)
jul17slevsite9$X9 <- as.numeric(jul17slevsite9$X9)
jul24slevsite9$X9 <- as.numeric(jul24slevsite9$X9)
jul30slevsite9$X9 <- as.numeric(jul30slevsite9$X9)
aug06slevsite9$X9 <- as.numeric(aug06slevsite9$X9)
aug13slevsite9$X9 <- as.numeric(aug13slevsite9$X9)
aug20slevsite9$X9 <- as.numeric(aug20slevsite9$X9)
sep03slevsite9$X9 <- as.numeric(sep03slevsite9$X9)
sep10slevsite9$X9 <- as.numeric(sep10slevsite9$X9)
sep17slevsite9$X9 <- as.numeric(sep17slevsite9$X9)
sep24slevsite9$X9 <- as.numeric(sep24slevsite9$X9)


ajun04slevsite9 <- sum(jun04slevsite9$X9, na.rm=T)
ajun12slevsite9 <- sum(jun12slevsite9$X9, na.rm=T)
ajun18slevsite9 <- sum(jun18slevsite9$X9, na.rm=T)
ajun25slevsite9 <- sum(jun25slevsite9$X9, na.rm=T)
ajul04slevsite9 <- sum(jul04slevsite9$X9, na.rm=T)
ajul10slevsite9 <- sum(jul10slevsite9$X9, na.rm=T)
ajul17slevsite9 <- sum(jul17slevsite9$X9, na.rm=T)
ajul24slevsite9 <- sum(jul24slevsite9$X9, na.rm=T)
ajul30slevsite9 <- sum(jul30slevsite9$X9, na.rm=T)
aaug06slevsite9 <- sum(aug06slevsite9$X9, na.rm=T)
aaug13slevsite9 <- sum(aug13slevsite9$X9, na.rm=T)
aaug20slevsite9 <- sum(aug20slevsite9$X9, na.rm=T)
asep03slevsite9 <- sum(sep03slevsite9$X9, na.rm=T)
asep10slevsite9 <- sum(sep10slevsite9$X9, na.rm=T)
asep17slevsite9 <- sum(sep17slevsite9$X9, na.rm=T)
asep24slevsite9 <- sum(sep24slevsite9$X9, na.rm=T)




##### site10
#slev
jun04slevsite10$X10 <- as.numeric(jun04slevsite10$X10)
jun12slevsite10$X10 <- as.numeric(jun12slevsite10$X10)
jun18slevsite10$X10 <- as.numeric(jun18slevsite10$X10)
jun25slevsite10$X10 <- as.numeric(jun25slevsite10$X10)
jul04slevsite10$X10 <- as.numeric(jul04slevsite10$X10)
jul10slevsite10$X10 <- as.numeric(jul10slevsite10$X10)
jul17slevsite10$X10 <- as.numeric(jul17slevsite10$X10)
jul24slevsite10$X10 <- as.numeric(jul24slevsite10$X10)
jul30slevsite10$X10 <- as.numeric(jul30slevsite10$X10)
aug06slevsite10$X10 <- as.numeric(aug06slevsite10$X10)
aug13slevsite10$X10 <- as.numeric(aug13slevsite10$X10)
aug20slevsite10$X10 <- as.numeric(aug20slevsite10$X10)
sep03slevsite10$X10 <- as.numeric(sep03slevsite10$X10)
sep10slevsite10$X10 <- as.numeric(sep10slevsite10$X10)
sep17slevsite10$X10 <- as.numeric(sep17slevsite10$X10)
sep24slevsite10$X10 <- as.numeric(sep24slevsite10$X10)


ajun04slevsite10 <- sum(jun04slevsite10$X10, na.rm=T)
ajun12slevsite10 <- sum(jun12slevsite10$X10, na.rm=T)
ajun18slevsite10 <- sum(jun18slevsite10$X10, na.rm=T)
ajun25slevsite10 <- sum(jun25slevsite10$X10, na.rm=T)
ajul04slevsite10 <- sum(jul04slevsite10$X10, na.rm=T)
ajul10slevsite10 <- sum(jul10slevsite10$X10, na.rm=T)
ajul17slevsite10 <- sum(jul17slevsite10$X10, na.rm=T)
ajul24slevsite10 <- sum(jul24slevsite10$X10, na.rm=T)
ajul30slevsite10 <- sum(jul30slevsite10$X10, na.rm=T)
aaug06slevsite10 <- sum(aug06slevsite10$X10, na.rm=T)
aaug13slevsite10 <- sum(aug13slevsite10$X10, na.rm=T)
aaug20slevsite10 <- sum(aug20slevsite10$X10, na.rm=T)
asep03slevsite10 <- sum(sep03slevsite10$X10, na.rm=T)
asep10slevsite10 <- sum(sep10slevsite10$X10, na.rm=T)
asep17slevsite10 <- sum(sep17slevsite10$X10, na.rm=T)
asep24slevsite10 <- sum(sep24slevsite10$X10, na.rm=T)


##
#
#
#
#
#



#CREATE slevS FOR PLOTTING
#Site 1
slev1 <- c(ajun04slevsite1,
           ajun12slevsite1,
           ajun18slevsite1,
           ajun25slevsite1,
           ajul04slevsite1,
           ajul10slevsite1,
           ajul17slevsite1,
           ajul24slevsite1,
           ajul30slevsite1,
           aaug06slevsite1,
           aaug13slevsite1,
           aaug20slevsite1,
           asep03slevsite1,
           asep10slevsite1,
           asep17slevsite1,
           asep24slevsite1)

#Site 2
slev2 <- c(ajun04slevsite2,
           ajun12slevsite2,
           ajun18slevsite2,
           ajun25slevsite2,
           ajul04slevsite2,
           ajul10slevsite2,
           ajul17slevsite2,
           ajul24slevsite2,
           ajul30slevsite2,
           aaug06slevsite2,
           aaug13slevsite2,
           aaug20slevsite2,
           asep03slevsite2,
           asep10slevsite2,
           asep17slevsite2,
           asep24slevsite2)


#Site 3
slev3 <- c(ajun04slevsite3,
           ajun12slevsite3,
           ajun18slevsite3,
           ajun25slevsite3,
           ajul04slevsite3,
           ajul10slevsite3,
           ajul17slevsite3,
           ajul24slevsite3,
           ajul30slevsite3,
           aaug06slevsite3,
           aaug13slevsite3,
           aaug20slevsite3,
           asep03slevsite3,
           asep10slevsite3,
           asep17slevsite3,
           asep24slevsite3)


#Site 4
slev4 <- c(ajun04slevsite4,
           ajun12slevsite4,
           ajun18slevsite4,
           ajun25slevsite4,
           ajul04slevsite4,
           ajul10slevsite4,
           ajul17slevsite4,
           ajul24slevsite4,
           ajul30slevsite4,
           aaug06slevsite4,
           aaug13slevsite4,
           aaug20slevsite4,
           asep03slevsite4,
           asep10slevsite4,
           asep17slevsite4,
           asep24slevsite4)


#Site 5
slev5 <- c(ajun04slevsite5,
           ajun12slevsite5,
           ajun18slevsite5,
           ajun25slevsite5,
           ajul04slevsite5,
           ajul10slevsite5,
           ajul17slevsite5,
           ajul24slevsite5,
           ajul30slevsite5,
           aaug06slevsite5,
           aaug13slevsite5,
           aaug20slevsite5,
           asep03slevsite5,
           asep10slevsite5,
           asep17slevsite5,
           asep24slevsite5)


#Site 6
slev6 <- c(ajun04slevsite6,
           ajun12slevsite6,
           ajun18slevsite6,
           ajun25slevsite6,
           ajul04slevsite6,
           ajul10slevsite6,
           ajul17slevsite6,
           ajul24slevsite6,
           ajul30slevsite6,
           aaug06slevsite6,
           aaug13slevsite6,
           aaug20slevsite6,
           asep03slevsite6,
           asep10slevsite6,
           asep17slevsite6,
           asep24slevsite6)


#Site 7
slev7 <- c(ajun04slevsite7,
           ajun12slevsite7,
           ajun18slevsite7,
           ajun25slevsite7,
           ajul04slevsite7,
           ajul10slevsite7,
           ajul17slevsite7,
           ajul24slevsite7,
           ajul30slevsite7,
           aaug06slevsite7,
           aaug13slevsite7,
           aaug20slevsite7,
           asep03slevsite7,
           asep10slevsite7,
           asep17slevsite7,
           asep24slevsite7)


#Site 8
slev8 <- c(ajun04slevsite8,
           ajun12slevsite8,
           ajun18slevsite8,
           ajun25slevsite8,
           ajul04slevsite8,
           ajul10slevsite8,
           ajul17slevsite8,
           ajul24slevsite8,
           ajul30slevsite8,
           aaug06slevsite8,
           aaug13slevsite8,
           aaug20slevsite8,
           asep03slevsite8,
           asep10slevsite8,
           asep17slevsite8,
           asep24slevsite8)


#Site 9
slev9 <- c(ajun04slevsite9,
           ajun12slevsite9,
           ajun18slevsite9,
           ajun25slevsite9,
           ajul04slevsite9,
           ajul10slevsite9,
           ajul17slevsite9,
           ajul24slevsite9,
           ajul30slevsite9,
           aaug06slevsite9,
           aaug13slevsite9,
           aaug20slevsite9,
           asep03slevsite9,
           asep10slevsite9,
           asep17slevsite9,
           asep24slevsite9)


#Site 10
slev10 <- c(ajun04slevsite10,
            ajun12slevsite10,
            ajun18slevsite10,
            ajun25slevsite10,
            ajul04slevsite10,
            ajul10slevsite10,
            ajul17slevsite10,
            ajul24slevsite10,
            ajul30slevsite10,
            aaug06slevsite10,
            aaug13slevsite10,
            aaug20slevsite10,
            asep03slevsite10,
            asep10slevsite10,
            asep17slevsite10,
            asep24slevsite10)



slevtotal <- slev1+slev2+slev3+slev4+slev5+slev6+slev7+slev8+slev9+slev10


##### eeev 3 - EEEV
#SUBSET BY DATE and eeev
jun04eeev <- subset(mergedeeev, date2  == as.Date("2013-06-04"))
jun12eeev <- subset(mergedeeev, date2  == as.Date("2013-06-12"))
jun18eeev <- subset(mergedeeev, date2  == as.Date("2013-06-18"))
jun25eeev <- subset(mergedeeev, date2  == as.Date("2013-06-25"))
jul04eeev <- subset(mergedeeev, date2  == as.Date("2013-07-04"))
jul10eeev <- subset(mergedeeev, date2  == as.Date("2013-07-10"))
jul17eeev <- subset(mergedeeev, date2  == as.Date("2013-07-17"))
jul24eeev <- subset(mergedeeev, date2  == as.Date("2013-07-24"))
jul30eeev <- subset(mergedeeev, date2  == as.Date("2013-07-30"))
aug06eeev <- subset(mergedeeev, date2  == as.Date("2013-08-06"))
aug13eeev <- subset(mergedeeev, date2  == as.Date("2013-08-13"))
aug20eeev <- subset(mergedeeev, date2  == as.Date("2013-08-20"))
sep03eeev <- subset(mergedeeev, date2  == as.Date("2013-09-03"))
sep10eeev <- subset(mergedeeev, date2  == as.Date("2013-09-10"))
sep17eeev <- subset(mergedeeev, date2  == as.Date("2013-09-17"))
sep24eeev <- subset(mergedeeev, date2  == as.Date("2013-09-24"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE eeev SITE

#JUN 04 eeev
jun04eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-04"))


#jun12 eeev
jun12eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-12"))

#jun18 eeev
jun18eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-18"))

#jun25 eeev
jun25eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-25"))

#jul04 eeev
jul04eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-04"))

#jul10 eeev
jul10eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-10"))

#jul17 eeev
jul17eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-17"))

#jul24 eeev
jul24eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-24"))

#jul30 eeev
jul30eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-30"))

#aug06 eeev
aug06eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-08-06"))

#aug13 eeev
aug13eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-08-13"))

#aug20 eeev
aug20eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-08-20"))

#sep03 eeev
sep03eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-09-03"))

#sep10 eeev
sep10eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-09-10"))
sep10eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-09-10"))

#sep17 eeev
sep17eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-09-17"))
sep17eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-09-17"))


#sep24 eeev
sep24eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-09-24"))
sep24eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-09-24"))


##### ALL SITES COMBINED
#eeev
ajun04eeevtotal <- sum(jun04eeev$total, na.rm=T)
ajun12eeevtotal <- sum(jun12eeev$total, na.rm=T)
ajun18eeevtotal <- sum(jun18eeev$total, na.rm=T)
ajun25eeevtotal <- sum(jun25eeev$total, na.rm=T)
ajul04eeevtotal <- sum(jul04eeev$total, na.rm=T)
ajul10eeevtotal <- sum(jul10eeev$total, na.rm=T)
ajul17eeevtotal <- sum(jul17eeev$total, na.rm=T)
ajul24eeevtotal <- sum(jul24eeev$total, na.rm=T)
ajul30eeevtotal <- sum(jul30eeev$total, na.rm=T)
aaug06eeevtotal <- sum(aug06eeev$total, na.rm=T)
aaug13eeevtotal <- sum(aug13eeev$total, na.rm=T)
aaug20eeevtotal <- sum(aug20eeev$total, na.rm=T)
asep03eeevtotal <- sum(sep03eeev$total, na.rm=T)
asep10eeevtotal <- sum(sep10eeev$total, na.rm=T)
asep17eeevtotal <- sum(sep17eeev$total, na.rm=T)
asep24eeevtotal <- sum(sep24eeev$total, na.rm=T)





##### site1
#eeev
ajun04eeevsite1 <- sum(jun04eeevsite1$X1, na.rm=T)
ajun12eeevsite1 <- sum(jun12eeevsite1$X1, na.rm=T)
ajun18eeevsite1 <- sum(jun18eeevsite1$X1, na.rm=T)
ajun25eeevsite1 <- sum(jun25eeevsite1$X1, na.rm=T)
ajul04eeevsite1 <- sum(jul04eeevsite1$X1, na.rm=T)
ajul10eeevsite1 <- sum(jul10eeevsite1$X1, na.rm=T)
ajul17eeevsite1 <- sum(jul17eeevsite1$X1, na.rm=T)
ajul24eeevsite1 <- sum(jul24eeevsite1$X1, na.rm=T)
ajul30eeevsite1 <- sum(jul30eeevsite1$X1, na.rm=T)
aaug06eeevsite1 <- sum(aug06eeevsite1$X1, na.rm=T)
aaug13eeevsite1 <- sum(aug13eeevsite1$X1, na.rm=T)
aaug20eeevsite1 <- sum(aug20eeevsite1$X1, na.rm=T)
asep03eeevsite1 <- sum(sep03eeevsite1$X1, na.rm=T)
asep10eeevsite1 <- sum(sep10eeevsite1$X1, na.rm=T)
asep17eeevsite1 <- sum(sep17eeevsite1$X1, na.rm=T)
asep24eeevsite1 <- sum(sep24eeevsite1$X1, na.rm=T)



##### site2
#eeev
ajun04eeevsite2 <- sum(jun04eeevsite2$X2, na.rm=T)
ajun12eeevsite2 <- sum(jun12eeevsite2$X2, na.rm=T)
ajun18eeevsite2 <- sum(jun18eeevsite2$X2, na.rm=T)
ajun25eeevsite2 <- sum(jun25eeevsite2$X2, na.rm=T)
ajul04eeevsite2 <- sum(jul04eeevsite2$X2, na.rm=T)
ajul10eeevsite2 <- sum(jul10eeevsite2$X2, na.rm=T)
ajul17eeevsite2 <- sum(jul17eeevsite2$X2, na.rm=T)
ajul24eeevsite2 <- sum(jul24eeevsite2$X2, na.rm=T)
ajul30eeevsite2 <- sum(jul30eeevsite2$X2, na.rm=T)
aaug06eeevsite2 <- sum(aug06eeevsite2$X2, na.rm=T)
aaug13eeevsite2 <- sum(aug13eeevsite2$X2, na.rm=T)
aaug20eeevsite2 <- sum(aug20eeevsite2$X2, na.rm=T)
asep03eeevsite2 <- sum(sep03eeevsite2$X2, na.rm=T)
asep10eeevsite2 <- sum(sep10eeevsite2$X2, na.rm=T)
asep17eeevsite2 <- sum(sep17eeevsite2$X2, na.rm=T)
asep24eeevsite2 <- sum(sep24eeevsite2$X2, na.rm=T)




##### site3
#eeev
ajun04eeevsite3 <- sum(jun04eeevsite3$X3, na.rm=T)
ajun12eeevsite3 <- sum(jun12eeevsite3$X3, na.rm=T)
ajun18eeevsite3 <- sum(jun18eeevsite3$X3, na.rm=T)
ajun25eeevsite3 <- sum(jun25eeevsite3$X3, na.rm=T)
ajul04eeevsite3 <- sum(jul04eeevsite3$X3, na.rm=T)
ajul10eeevsite3 <- sum(jul10eeevsite3$X3, na.rm=T)
ajul17eeevsite3 <- sum(jul17eeevsite3$X3, na.rm=T)
ajul24eeevsite3 <- sum(jul24eeevsite3$X3, na.rm=T)
ajul30eeevsite3 <- sum(jul30eeevsite3$X3, na.rm=T)
aaug06eeevsite3 <- sum(aug06eeevsite3$X3, na.rm=T)
aaug13eeevsite3 <- sum(aug13eeevsite3$X3, na.rm=T)
aaug20eeevsite3 <- sum(aug20eeevsite3$X3, na.rm=T)
asep03eeevsite3 <- sum(sep03eeevsite3$X3, na.rm=T)
asep10eeevsite3 <- sum(sep10eeevsite3$X3, na.rm=T)
asep17eeevsite3 <- sum(sep17eeevsite3$X3, na.rm=T)
asep24eeevsite3 <- sum(sep24eeevsite3$X3, na.rm=T)





##### site4
#eeev
ajun04eeevsite4 <- sum(jun04eeevsite4$X4, na.rm=T)
ajun12eeevsite4 <- sum(jun12eeevsite4$X4, na.rm=T)
ajun18eeevsite4 <- sum(jun18eeevsite4$X4, na.rm=T)
ajun25eeevsite4 <- sum(jun25eeevsite4$X4, na.rm=T)
ajul04eeevsite4 <- sum(jul04eeevsite4$X4, na.rm=T)
ajul10eeevsite4 <- sum(jul10eeevsite4$X4, na.rm=T)
ajul17eeevsite4 <- sum(jul17eeevsite4$X4, na.rm=T)
ajul24eeevsite4 <- sum(jul24eeevsite4$X4, na.rm=T)
ajul30eeevsite4 <- sum(jul30eeevsite4$X4, na.rm=T)
aaug06eeevsite4 <- sum(aug06eeevsite4$X4, na.rm=T)
aaug13eeevsite4 <- sum(aug13eeevsite4$X4, na.rm=T)
aaug20eeevsite4 <- sum(aug20eeevsite4$X4, na.rm=T)
asep03eeevsite4 <- sum(sep03eeevsite4$X4, na.rm=T)
asep10eeevsite4 <- sum(sep10eeevsite4$X4, na.rm=T)
asep17eeevsite4 <- sum(sep17eeevsite4$X4, na.rm=T)
asep24eeevsite4 <- sum(sep24eeevsite4$X4, na.rm=T)




##### site5
#eeev
ajun04eeevsite5 <- sum(jun04eeevsite5$X5, na.rm=T)
ajun12eeevsite5 <- sum(jun12eeevsite5$X5, na.rm=T)
ajun18eeevsite5 <- sum(jun18eeevsite5$X5, na.rm=T)
ajun25eeevsite5 <- sum(jun25eeevsite5$X5, na.rm=T)
ajul04eeevsite5 <- sum(jul04eeevsite5$X5, na.rm=T)
ajul10eeevsite5 <- sum(jul10eeevsite5$X5, na.rm=T)
ajul17eeevsite5 <- sum(jul17eeevsite5$X5, na.rm=T)
ajul24eeevsite5 <- sum(jul24eeevsite5$X5, na.rm=T)
ajul30eeevsite5 <- sum(jul30eeevsite5$X5, na.rm=T)
aaug06eeevsite5 <- sum(aug06eeevsite5$X5, na.rm=T)
aaug13eeevsite5 <- sum(aug13eeevsite5$X5, na.rm=T)
aaug20eeevsite5 <- sum(aug20eeevsite5$X5, na.rm=T)
asep03eeevsite5 <- sum(sep03eeevsite5$X5, na.rm=T)
asep10eeevsite5 <- sum(sep10eeevsite5$X5, na.rm=T)
asep17eeevsite5 <- sum(sep17eeevsite5$X5, na.rm=T)
asep24eeevsite5 <- sum(sep24eeevsite5$X5, na.rm=T)



##### site6
#eeev
jun04eeevsite6$X6 <- as.numeric(jun04eeevsite6$X6)
jun12eeevsite6$X6 <- as.numeric(jun12eeevsite6$X6)
jun18eeevsite6$X6 <- as.numeric(jun18eeevsite6$X6)
jun25eeevsite6$X6 <- as.numeric(jun25eeevsite6$X6)
jul04eeevsite6$X6 <- as.numeric(jul04eeevsite6$X6)
jul10eeevsite6$X6 <- as.numeric(jul10eeevsite6$X6)
jul17eeevsite6$X6 <- as.numeric(jul17eeevsite6$X6)
jul24eeevsite6$X6 <- as.numeric(jul24eeevsite6$X6)
jul30eeevsite6$X6 <- as.numeric(jul30eeevsite6$X6)
aug06eeevsite6$X6 <- as.numeric(aug06eeevsite6$X6)
aug13eeevsite6$X6 <- as.numeric(aug13eeevsite6$X6)
aug20eeevsite6$X6 <- as.numeric(aug20eeevsite6$X6)
sep03eeevsite6$X6 <- as.numeric(sep03eeevsite6$X6)
sep10eeevsite6$X6 <- as.numeric(sep10eeevsite6$X6)
sep17eeevsite6$X6 <- as.numeric(sep17eeevsite6$X6)
sep24eeevsite6$X6 <- as.numeric(sep24eeevsite6$X6)


ajun04eeevsite6 <- sum(jun04eeevsite6$X6, na.rm=T)
ajun12eeevsite6 <- sum(jun12eeevsite6$X6, na.rm=T)
ajun18eeevsite6 <- sum(jun18eeevsite6$X6, na.rm=T)
ajun25eeevsite6 <- sum(jun25eeevsite6$X6, na.rm=T)
ajul04eeevsite6 <- sum(jul04eeevsite6$X6, na.rm=T)
ajul10eeevsite6 <- sum(jul10eeevsite6$X6, na.rm=T)
ajul17eeevsite6 <- sum(jul17eeevsite6$X6, na.rm=T)
ajul24eeevsite6 <- sum(jul24eeevsite6$X6, na.rm=T)
ajul30eeevsite6 <- sum(jul30eeevsite6$X6, na.rm=T)
aaug06eeevsite6 <- sum(aug06eeevsite6$X6, na.rm=T)
aaug13eeevsite6 <- sum(aug13eeevsite6$X6, na.rm=T)
aaug20eeevsite6 <- sum(aug20eeevsite6$X6, na.rm=T)
asep03eeevsite6 <- sum(sep03eeevsite6$X6, na.rm=T)
asep10eeevsite6 <- sum(sep10eeevsite6$X6, na.rm=T)
asep17eeevsite6 <- sum(sep17eeevsite6$X6, na.rm=T)
asep24eeevsite6 <- sum(sep24eeevsite6$X6, na.rm=T)




##### site7
#eeev
ajun04eeevsite7 <- sum(jun04eeevsite7$X7, na.rm=T)
ajun12eeevsite7 <- sum(jun12eeevsite7$X7, na.rm=T)
ajun18eeevsite7 <- sum(jun18eeevsite7$X7, na.rm=T)
ajun25eeevsite7 <- sum(jun25eeevsite7$X7, na.rm=T)
ajul04eeevsite7 <- sum(jul04eeevsite7$X7, na.rm=T)
ajul10eeevsite7 <- sum(jul10eeevsite7$X7, na.rm=T)
ajul17eeevsite7 <- sum(jul17eeevsite7$X7, na.rm=T)
ajul24eeevsite7 <- sum(jul24eeevsite7$X7, na.rm=T)
ajul30eeevsite7 <- sum(jul30eeevsite7$X7, na.rm=T)
aaug06eeevsite7 <- sum(aug06eeevsite7$X7, na.rm=T)
aaug13eeevsite7 <- sum(aug13eeevsite7$X7, na.rm=T)
aaug20eeevsite7 <- sum(aug20eeevsite7$X7, na.rm=T)
asep03eeevsite7 <- sum(sep03eeevsite7$X7, na.rm=T)
asep10eeevsite7 <- sum(sep10eeevsite7$X7, na.rm=T)
asep17eeevsite7 <- sum(sep17eeevsite7$X7, na.rm=T)
asep24eeevsite7 <- sum(sep24eeevsite7$X7, na.rm=T)




##### site8
#eeev
ajun04eeevsite8 <- sum(jun04eeevsite8$X8, na.rm=T)
ajun12eeevsite8 <- sum(jun12eeevsite8$X8, na.rm=T)
ajun18eeevsite8 <- sum(jun18eeevsite8$X8, na.rm=T)
ajun25eeevsite8 <- sum(jun25eeevsite8$X8, na.rm=T)
ajul04eeevsite8 <- sum(jul04eeevsite8$X8, na.rm=T)
ajul10eeevsite8 <- sum(jul10eeevsite8$X8, na.rm=T)
ajul17eeevsite8 <- sum(jul17eeevsite8$X8, na.rm=T)
ajul24eeevsite8 <- sum(jul24eeevsite8$X8, na.rm=T)
ajul30eeevsite8 <- sum(jul30eeevsite8$X8, na.rm=T)
aaug06eeevsite8 <- sum(aug06eeevsite8$X8, na.rm=T)
aaug13eeevsite8 <- sum(aug13eeevsite8$X8, na.rm=T)
aaug20eeevsite8 <- sum(aug20eeevsite8$X8, na.rm=T)
asep03eeevsite8 <- sum(sep03eeevsite8$X8, na.rm=T)
asep10eeevsite8 <- sum(sep10eeevsite8$X8, na.rm=T)
asep17eeevsite8 <- sum(sep17eeevsite8$X8, na.rm=T)
asep24eeevsite8 <- sum(sep24eeevsite8$X8, na.rm=T)



##### site9
#eeev

jun04eeevsite9$X9 <- as.numeric(jun04eeevsite9$X9)
jun12eeevsite9$X9 <- as.numeric(jun12eeevsite9$X9)
jun18eeevsite9$X9 <- as.numeric(jun18eeevsite9$X9)
jun25eeevsite9$X9 <- as.numeric(jun25eeevsite9$X9)
jul04eeevsite9$X9 <- as.numeric(jul04eeevsite9$X9)
jul10eeevsite9$X9 <- as.numeric(jul10eeevsite9$X9)
jul17eeevsite9$X9 <- as.numeric(jul17eeevsite9$X9)
jul24eeevsite9$X9 <- as.numeric(jul24eeevsite9$X9)
jul30eeevsite9$X9 <- as.numeric(jul30eeevsite9$X9)
aug06eeevsite9$X9 <- as.numeric(aug06eeevsite9$X9)
aug13eeevsite9$X9 <- as.numeric(aug13eeevsite9$X9)
aug20eeevsite9$X9 <- as.numeric(aug20eeevsite9$X9)
sep03eeevsite9$X9 <- as.numeric(sep03eeevsite9$X9)
sep10eeevsite9$X9 <- as.numeric(sep10eeevsite9$X9)
sep17eeevsite9$X9 <- as.numeric(sep17eeevsite9$X9)
sep24eeevsite9$X9 <- as.numeric(sep24eeevsite9$X9)


ajun04eeevsite9 <- sum(jun04eeevsite9$X9, na.rm=T)
ajun12eeevsite9 <- sum(jun12eeevsite9$X9, na.rm=T)
ajun18eeevsite9 <- sum(jun18eeevsite9$X9, na.rm=T)
ajun25eeevsite9 <- sum(jun25eeevsite9$X9, na.rm=T)
ajul04eeevsite9 <- sum(jul04eeevsite9$X9, na.rm=T)
ajul10eeevsite9 <- sum(jul10eeevsite9$X9, na.rm=T)
ajul17eeevsite9 <- sum(jul17eeevsite9$X9, na.rm=T)
ajul24eeevsite9 <- sum(jul24eeevsite9$X9, na.rm=T)
ajul30eeevsite9 <- sum(jul30eeevsite9$X9, na.rm=T)
aaug06eeevsite9 <- sum(aug06eeevsite9$X9, na.rm=T)
aaug13eeevsite9 <- sum(aug13eeevsite9$X9, na.rm=T)
aaug20eeevsite9 <- sum(aug20eeevsite9$X9, na.rm=T)
asep03eeevsite9 <- sum(sep03eeevsite9$X9, na.rm=T)
asep10eeevsite9 <- sum(sep10eeevsite9$X9, na.rm=T)
asep17eeevsite9 <- sum(sep17eeevsite9$X9, na.rm=T)
asep24eeevsite9 <- sum(sep24eeevsite9$X9, na.rm=T)



##### site10
#eeev
jun04eeevsite10$X10 <- as.numeric(jun04eeevsite10$X10)
jun12eeevsite10$X10 <- as.numeric(jun12eeevsite10$X10)
jun18eeevsite10$X10 <- as.numeric(jun18eeevsite10$X10)
jun25eeevsite10$X10 <- as.numeric(jun25eeevsite10$X10)
jul04eeevsite10$X10 <- as.numeric(jul04eeevsite10$X10)
jul10eeevsite10$X10 <- as.numeric(jul10eeevsite10$X10)
jul17eeevsite10$X10 <- as.numeric(jul17eeevsite10$X10)
jul24eeevsite10$X10 <- as.numeric(jul24eeevsite10$X10)
jul30eeevsite10$X10 <- as.numeric(jul30eeevsite10$X10)
aug06eeevsite10$X10 <- as.numeric(aug06eeevsite10$X10)
aug13eeevsite10$X10 <- as.numeric(aug13eeevsite10$X10)
aug20eeevsite10$X10 <- as.numeric(aug20eeevsite10$X10)
sep03eeevsite10$X10 <- as.numeric(sep03eeevsite10$X10)
sep10eeevsite10$X10 <- as.numeric(sep10eeevsite10$X10)
sep17eeevsite10$X10 <- as.numeric(sep17eeevsite10$X10)
sep24eeevsite10$X10 <- as.numeric(sep24eeevsite10$X10)


ajun04eeevsite10 <- sum(jun04eeevsite10$X10, na.rm=T)
ajun12eeevsite10 <- sum(jun12eeevsite10$X10, na.rm=T)
ajun18eeevsite10 <- sum(jun18eeevsite10$X10, na.rm=T)
ajun25eeevsite10 <- sum(jun25eeevsite10$X10, na.rm=T)
ajul04eeevsite10 <- sum(jul04eeevsite10$X10, na.rm=T)
ajul10eeevsite10 <- sum(jul10eeevsite10$X10, na.rm=T)
ajul17eeevsite10 <- sum(jul17eeevsite10$X10, na.rm=T)
ajul24eeevsite10 <- sum(jul24eeevsite10$X10, na.rm=T)
ajul30eeevsite10 <- sum(jul30eeevsite10$X10, na.rm=T)
aaug06eeevsite10 <- sum(aug06eeevsite10$X10, na.rm=T)
aaug13eeevsite10 <- sum(aug13eeevsite10$X10, na.rm=T)
aaug20eeevsite10 <- sum(aug20eeevsite10$X10, na.rm=T)
asep03eeevsite10 <- sum(sep03eeevsite10$X10, na.rm=T)
asep10eeevsite10 <- sum(sep10eeevsite10$X10, na.rm=T)
asep17eeevsite10 <- sum(sep17eeevsite10$X10, na.rm=T)
asep24eeevsite10 <- sum(sep24eeevsite10$X10, na.rm=T)



##
#
#
#
#
#



#CREATE eeevS FOR PLOTTING
#Site 1
eeev1 <- c(ajun04eeevsite1,
           ajun12eeevsite1,
           ajun18eeevsite1,
           ajun25eeevsite1,
           ajul04eeevsite1,
           ajul10eeevsite1,
           ajul17eeevsite1,
           ajul24eeevsite1,
           ajul30eeevsite1,
           aaug06eeevsite1,
           aaug13eeevsite1,
           aaug20eeevsite1,
           asep03eeevsite1,
           asep10eeevsite1,
           asep17eeevsite1,
           asep24eeevsite1)

#Site 2
eeev2 <- c(ajun04eeevsite2,
           ajun12eeevsite2,
           ajun18eeevsite2,
           ajun25eeevsite2,
           ajul04eeevsite2,
           ajul10eeevsite2,
           ajul17eeevsite2,
           ajul24eeevsite2,
           ajul30eeevsite2,
           aaug06eeevsite2,
           aaug13eeevsite2,
           aaug20eeevsite2,
           asep03eeevsite2,
           asep10eeevsite2,
           asep17eeevsite2,
           asep24eeevsite2)


#Site 3
eeev3 <- c(ajun04eeevsite3,
           ajun12eeevsite3,
           ajun18eeevsite3,
           ajun25eeevsite3,
           ajul04eeevsite3,
           ajul10eeevsite3,
           ajul17eeevsite3,
           ajul24eeevsite3,
           ajul30eeevsite3,
           aaug06eeevsite3,
           aaug13eeevsite3,
           aaug20eeevsite3,
           asep03eeevsite3,
           asep10eeevsite3,
           asep17eeevsite3,
           asep24eeevsite3)


#Site 4
eeev4 <- c(ajun04eeevsite4,
           ajun12eeevsite4,
           ajun18eeevsite4,
           ajun25eeevsite4,
           ajul04eeevsite4,
           ajul10eeevsite4,
           ajul17eeevsite4,
           ajul24eeevsite4,
           ajul30eeevsite4,
           aaug06eeevsite4,
           aaug13eeevsite4,
           aaug20eeevsite4,
           asep03eeevsite4,
           asep10eeevsite4,
           asep17eeevsite4,
           asep24eeevsite4)


#Site 5
eeev5 <- c(ajun04eeevsite5,
           ajun12eeevsite5,
           ajun18eeevsite5,
           ajun25eeevsite5,
           ajul04eeevsite5,
           ajul10eeevsite5,
           ajul17eeevsite5,
           ajul24eeevsite5,
           ajul30eeevsite5,
           aaug06eeevsite5,
           aaug13eeevsite5,
           aaug20eeevsite5,
           asep03eeevsite5,
           asep10eeevsite5,
           asep17eeevsite5,
           asep24eeevsite5)


#Site 6
eeev6 <- c(ajun04eeevsite6,
           ajun12eeevsite6,
           ajun18eeevsite6,
           ajun25eeevsite6,
           ajul04eeevsite6,
           ajul10eeevsite6,
           ajul17eeevsite6,
           ajul24eeevsite6,
           ajul30eeevsite6,
           aaug06eeevsite6,
           aaug13eeevsite6,
           aaug20eeevsite6,
           asep03eeevsite6,
           asep10eeevsite6,
           asep17eeevsite6,
           asep24eeevsite6)


#Site 7
eeev7 <- c(ajun04eeevsite7,
           ajun12eeevsite7,
           ajun18eeevsite7,
           ajun25eeevsite7,
           ajul04eeevsite7,
           ajul10eeevsite7,
           ajul17eeevsite7,
           ajul24eeevsite7,
           ajul30eeevsite7,
           aaug06eeevsite7,
           aaug13eeevsite7,
           aaug20eeevsite7,
           asep03eeevsite7,
           asep10eeevsite7,
           asep17eeevsite7,
           asep24eeevsite7)


#Site 8
eeev8 <- c(ajun04eeevsite8,
           ajun12eeevsite8,
           ajun18eeevsite8,
           ajun25eeevsite8,
           ajul04eeevsite8,
           ajul10eeevsite8,
           ajul17eeevsite8,
           ajul24eeevsite8,
           ajul30eeevsite8,
           aaug06eeevsite8,
           aaug13eeevsite8,
           aaug20eeevsite8,
           asep03eeevsite8,
           asep10eeevsite8,
           asep17eeevsite8,
           asep24eeevsite8)


#Site 9
eeev9 <- c(ajun04eeevsite9,
           ajun12eeevsite9,
           ajun18eeevsite9,
           ajun25eeevsite9,
           ajul04eeevsite9,
           ajul10eeevsite9,
           ajul17eeevsite9,
           ajul24eeevsite9,
           ajul30eeevsite9,
           aaug06eeevsite9,
           aaug13eeevsite9,
           aaug20eeevsite9,
           asep03eeevsite9,
           asep10eeevsite9,
           asep17eeevsite9,
           asep24eeevsite9)


#Site 10
eeev10 <- c(ajun04eeevsite10,
            ajun12eeevsite10,
            ajun18eeevsite10,
            ajun25eeevsite10,
            ajul04eeevsite10,
            ajul10eeevsite10,
            ajul17eeevsite10,
            ajul24eeevsite10,
            ajul30eeevsite10,
            aaug06eeevsite10,
            aaug13eeevsite10,
            aaug20eeevsite10,
            asep03eeevsite10,
            asep10eeevsite10,
            asep17eeevsite10,
            asep24eeevsite10)



eeevtotal <- eeev1+eeev2+eeev3+eeev4+eeev5+eeev6+eeev7+eeev8+eeev9+eeev10


##### chik 4 - CHIK
#SUBSET BY DATE and chik
jun04chik <- subset(mergedchik, date2  == as.Date("2013-06-04"))
jun12chik <- subset(mergedchik, date2  == as.Date("2013-06-12"))
jun18chik <- subset(mergedchik, date2  == as.Date("2013-06-18"))
jun25chik <- subset(mergedchik, date2  == as.Date("2013-06-25"))
jul04chik <- subset(mergedchik, date2  == as.Date("2013-07-04"))
jul10chik <- subset(mergedchik, date2  == as.Date("2013-07-10"))
jul17chik <- subset(mergedchik, date2  == as.Date("2013-07-17"))
jul24chik <- subset(mergedchik, date2  == as.Date("2013-07-24"))
jul30chik <- subset(mergedchik, date2  == as.Date("2013-07-30"))
aug06chik <- subset(mergedchik, date2  == as.Date("2013-08-06"))
aug13chik <- subset(mergedchik, date2  == as.Date("2013-08-13"))
aug20chik <- subset(mergedchik, date2  == as.Date("2013-08-20"))
sep03chik <- subset(mergedchik, date2  == as.Date("2013-09-03"))
sep10chik <- subset(mergedchik, date2  == as.Date("2013-09-10"))
sep17chik <- subset(mergedchik, date2  == as.Date("2013-09-17"))
sep24chik <- subset(mergedchik, date2  == as.Date("2013-09-24"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE chik SITE

#JUN 04 chik
jun04chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-04"))
jun04chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-04"))
jun04chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-04"))
jun04chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-04"))
jun04chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-04"))
jun04chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-04"))
jun04chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-04"))
jun04chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-04"))
jun04chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-04"))
jun04chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-04"))


#jun12 chik
jun12chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-12"))
jun12chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-12"))
jun12chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-12"))
jun12chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-12"))
jun12chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-12"))
jun12chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-12"))
jun12chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-12"))
jun12chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-12"))
jun12chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-12"))
jun12chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-12"))

#jun18 chik
jun18chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-18"))
jun18chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-18"))
jun18chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-18"))
jun18chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-18"))
jun18chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-18"))
jun18chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-18"))
jun18chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-18"))
jun18chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-18"))
jun18chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-18"))
jun18chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-18"))

#jun25 chik
jun25chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-25"))
jun25chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-25"))
jun25chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-25"))
jun25chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-25"))
jun25chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-25"))
jun25chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-25"))
jun25chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-25"))
jun25chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-25"))
jun25chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-25"))
jun25chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-25"))

#jul04 chik
jul04chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-04"))
jul04chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-04"))
jul04chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-04"))
jul04chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-04"))
jul04chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-04"))
jul04chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-04"))
jul04chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-04"))
jul04chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-04"))
jul04chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-04"))
jul04chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-04"))

#jul10 chik
jul10chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-10"))
jul10chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-10"))
jul10chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-10"))
jul10chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-10"))
jul10chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-10"))
jul10chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-10"))
jul10chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-10"))
jul10chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-10"))
jul10chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-10"))
jul10chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-10"))

#jul17 chik
jul17chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-17"))
jul17chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-17"))
jul17chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-17"))
jul17chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-17"))
jul17chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-17"))
jul17chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-17"))
jul17chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-17"))
jul17chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-17"))
jul17chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-17"))
jul17chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-17"))

#jul24 chik
jul24chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-24"))
jul24chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-24"))
jul24chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-24"))
jul24chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-24"))
jul24chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-24"))
jul24chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-24"))
jul24chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-24"))
jul24chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-24"))
jul24chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-24"))
jul24chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-24"))

#jul30 chik
jul30chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-30"))
jul30chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-30"))
jul30chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-30"))
jul30chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-30"))
jul30chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-30"))
jul30chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-30"))
jul30chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-30"))
jul30chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-30"))
jul30chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-30"))
jul30chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-30"))

#aug06 chik
aug06chiksite1 <-subset(merged1chik, date2  == as.Date("2013-08-06"))
aug06chiksite2 <-subset(merged2chik, date2  == as.Date("2013-08-06"))
aug06chiksite3 <-subset(merged3chik, date2  == as.Date("2013-08-06"))
aug06chiksite4 <-subset(merged4chik, date2  == as.Date("2013-08-06"))
aug06chiksite5 <-subset(merged5chik, date2  == as.Date("2013-08-06"))
aug06chiksite6 <-subset(merged6chik, date2  == as.Date("2013-08-06"))
aug06chiksite7 <-subset(merged7chik, date2  == as.Date("2013-08-06"))
aug06chiksite8 <-subset(merged8chik, date2  == as.Date("2013-08-06"))
aug06chiksite9 <-subset(merged9chik, date2  == as.Date("2013-08-06"))
aug06chiksite10 <-subset(merged10chik, date2  == as.Date("2013-08-06"))

#aug13 chik
aug13chiksite1 <-subset(merged1chik, date2  == as.Date("2013-08-13"))
aug13chiksite2 <-subset(merged2chik, date2  == as.Date("2013-08-13"))
aug13chiksite3 <-subset(merged3chik, date2  == as.Date("2013-08-13"))
aug13chiksite4 <-subset(merged4chik, date2  == as.Date("2013-08-13"))
aug13chiksite5 <-subset(merged5chik, date2  == as.Date("2013-08-13"))
aug13chiksite6 <-subset(merged6chik, date2  == as.Date("2013-08-13"))
aug13chiksite7 <-subset(merged7chik, date2  == as.Date("2013-08-13"))
aug13chiksite8 <-subset(merged8chik, date2  == as.Date("2013-08-13"))
aug13chiksite9 <-subset(merged9chik, date2  == as.Date("2013-08-13"))
aug13chiksite10 <-subset(merged10chik, date2  == as.Date("2013-08-13"))

#aug20 chik
aug20chiksite1 <-subset(merged1chik, date2  == as.Date("2013-08-20"))
aug20chiksite2 <-subset(merged2chik, date2  == as.Date("2013-08-20"))
aug20chiksite3 <-subset(merged3chik, date2  == as.Date("2013-08-20"))
aug20chiksite4 <-subset(merged4chik, date2  == as.Date("2013-08-20"))
aug20chiksite5 <-subset(merged5chik, date2  == as.Date("2013-08-20"))
aug20chiksite6 <-subset(merged6chik, date2  == as.Date("2013-08-20"))
aug20chiksite7 <-subset(merged7chik, date2  == as.Date("2013-08-20"))
aug20chiksite8 <-subset(merged8chik, date2  == as.Date("2013-08-20"))
aug20chiksite9 <-subset(merged9chik, date2  == as.Date("2013-08-20"))
aug20chiksite10 <-subset(merged10chik, date2  == as.Date("2013-08-20"))

#sep03 chik
sep03chiksite1 <-subset(merged1chik, date2  == as.Date("2013-09-03"))
sep03chiksite2 <-subset(merged2chik, date2  == as.Date("2013-09-03"))
sep03chiksite3 <-subset(merged3chik, date2  == as.Date("2013-09-03"))
sep03chiksite4 <-subset(merged4chik, date2  == as.Date("2013-09-03"))
sep03chiksite5 <-subset(merged5chik, date2  == as.Date("2013-09-03"))
sep03chiksite6 <-subset(merged6chik, date2  == as.Date("2013-09-03"))
sep03chiksite7 <-subset(merged7chik, date2  == as.Date("2013-09-03"))
sep03chiksite8 <-subset(merged8chik, date2  == as.Date("2013-09-03"))
sep03chiksite9 <-subset(merged9chik, date2  == as.Date("2013-09-03"))
sep03chiksite10 <-subset(merged10chik, date2  == as.Date("2013-09-03"))

#sep10 chik
sep10chiksite1 <-subset(merged1chik, date2  == as.Date("2013-09-10"))
sep10chiksite2 <-subset(merged2chik, date2  == as.Date("2013-09-10"))
sep10chiksite3 <-subset(merged3chik, date2  == as.Date("2013-09-10"))
sep10chiksite4 <-subset(merged4chik, date2  == as.Date("2013-09-10"))
sep10chiksite5 <-subset(merged5chik, date2  == as.Date("2013-09-10"))
sep10chiksite6 <-subset(merged6chik, date2  == as.Date("2013-09-10"))
sep10chiksite7 <-subset(merged7chik, date2  == as.Date("2013-09-10"))
sep10chiksite8 <-subset(merged8chik, date2  == as.Date("2013-09-10"))
sep10chiksite9 <-subset(merged9chik, date2  == as.Date("2013-09-10"))
sep10chiksite10 <-subset(merged10chik, date2  == as.Date("2013-09-10"))

#sep17 chik
sep17chiksite1 <-subset(merged1chik, date2  == as.Date("2013-09-17"))
sep17chiksite2 <-subset(merged2chik, date2  == as.Date("2013-09-17"))
sep17chiksite3 <-subset(merged3chik, date2  == as.Date("2013-09-17"))
sep17chiksite4 <-subset(merged4chik, date2  == as.Date("2013-09-17"))
sep17chiksite5 <-subset(merged5chik, date2  == as.Date("2013-09-17"))
sep17chiksite6 <-subset(merged6chik, date2  == as.Date("2013-09-17"))
sep17chiksite7 <-subset(merged7chik, date2  == as.Date("2013-09-17"))
sep17chiksite8 <-subset(merged8chik, date2  == as.Date("2013-09-17"))
sep17chiksite9 <-subset(merged9chik, date2  == as.Date("2013-09-17"))
sep17chiksite10 <-subset(merged10chik, date2  == as.Date("2013-09-17"))



#sep24 chik
sep24chiksite1 <-subset(merged1chik, date2  == as.Date("2013-09-24"))
sep24chiksite2 <-subset(merged2chik, date2  == as.Date("2013-09-24"))
sep24chiksite3 <-subset(merged3chik, date2  == as.Date("2013-09-24"))
sep24chiksite4 <-subset(merged4chik, date2  == as.Date("2013-09-24"))
sep24chiksite5 <-subset(merged5chik, date2  == as.Date("2013-09-24"))
sep24chiksite6 <-subset(merged6chik, date2  == as.Date("2013-09-24"))
sep24chiksite7 <-subset(merged7chik, date2  == as.Date("2013-09-24"))
sep24chiksite8 <-subset(merged8chik, date2  == as.Date("2013-09-24"))
sep24chiksite9 <-subset(merged9chik, date2  == as.Date("2013-09-24"))
sep24chiksite10 <-subset(merged10chik, date2  == as.Date("2013-09-24"))

##### ALL SITES COMBINED
#chik
ajun04chiktotal <- sum(jun04chik$total, na.rm=T)
ajun12chiktotal <- sum(jun12chik$total, na.rm=T)
ajun18chiktotal <- sum(jun18chik$total, na.rm=T)
ajun25chiktotal <- sum(jun25chik$total, na.rm=T)
ajul04chiktotal <- sum(jul04chik$total, na.rm=T)
ajul10chiktotal <- sum(jul10chik$total, na.rm=T)
ajul17chiktotal <- sum(jul17chik$total, na.rm=T)
ajul24chiktotal <- sum(jul24chik$total, na.rm=T)
ajul30chiktotal <- sum(jul30chik$total, na.rm=T)
aaug06chiktotal <- sum(aug06chik$total, na.rm=T)
aaug13chiktotal <- sum(aug13chik$total, na.rm=T)
aaug20chiktotal <- sum(aug20chik$total, na.rm=T)
asep03chiktotal <- sum(sep03chik$total, na.rm=T)
asep10chiktotal <- sum(sep10chik$total, na.rm=T)
asep17chiktotal <- sum(sep17chik$total, na.rm=T)
asep24chiktotal <- sum(sep24chik$total, na.rm=T)




##### site1
#chik
ajun04chiksite1 <- sum(jun04chiksite1$X1, na.rm=T)
ajun12chiksite1 <- sum(jun12chiksite1$X1, na.rm=T)
ajun18chiksite1 <- sum(jun18chiksite1$X1, na.rm=T)
ajun25chiksite1 <- sum(jun25chiksite1$X1, na.rm=T)
ajul04chiksite1 <- sum(jul04chiksite1$X1, na.rm=T)
ajul10chiksite1 <- sum(jul10chiksite1$X1, na.rm=T)
ajul17chiksite1 <- sum(jul17chiksite1$X1, na.rm=T)
ajul24chiksite1 <- sum(jul24chiksite1$X1, na.rm=T)
ajul30chiksite1 <- sum(jul30chiksite1$X1, na.rm=T)
aaug06chiksite1 <- sum(aug06chiksite1$X1, na.rm=T)
aaug13chiksite1 <- sum(aug13chiksite1$X1, na.rm=T)
aaug20chiksite1 <- sum(aug20chiksite1$X1, na.rm=T)
asep03chiksite1 <- sum(sep03chiksite1$X1, na.rm=T)
asep10chiksite1 <- sum(sep10chiksite1$X1, na.rm=T)
asep17chiksite1 <- sum(sep17chiksite1$X1, na.rm=T)
asep24chiksite1 <- sum(sep24chiksite1$X1, na.rm=T)



##### site2
#chik
ajun04chiksite2 <- sum(jun04chiksite2$X2, na.rm=T)
ajun12chiksite2 <- sum(jun12chiksite2$X2, na.rm=T)
ajun18chiksite2 <- sum(jun18chiksite2$X2, na.rm=T)
ajun25chiksite2 <- sum(jun25chiksite2$X2, na.rm=T)
ajul04chiksite2 <- sum(jul04chiksite2$X2, na.rm=T)
ajul10chiksite2 <- sum(jul10chiksite2$X2, na.rm=T)
ajul17chiksite2 <- sum(jul17chiksite2$X2, na.rm=T)
ajul24chiksite2 <- sum(jul24chiksite2$X2, na.rm=T)
ajul30chiksite2 <- sum(jul30chiksite2$X2, na.rm=T)
aaug06chiksite2 <- sum(aug06chiksite2$X2, na.rm=T)
aaug13chiksite2 <- sum(aug13chiksite2$X2, na.rm=T)
aaug20chiksite2 <- sum(aug20chiksite2$X2, na.rm=T)
asep03chiksite2 <- sum(sep03chiksite2$X2, na.rm=T)
asep10chiksite2 <- sum(sep10chiksite2$X2, na.rm=T)
asep17chiksite2 <- sum(sep17chiksite2$X2, na.rm=T)
asep24chiksite2 <- sum(sep24chiksite2$X2, na.rm=T)



##### site3
#chik
ajun04chiksite3 <- sum(jun04chiksite3$X3, na.rm=T)
ajun12chiksite3 <- sum(jun12chiksite3$X3, na.rm=T)
ajun18chiksite3 <- sum(jun18chiksite3$X3, na.rm=T)
ajun25chiksite3 <- sum(jun25chiksite3$X3, na.rm=T)
ajul04chiksite3 <- sum(jul04chiksite3$X3, na.rm=T)
ajul10chiksite3 <- sum(jul10chiksite3$X3, na.rm=T)
ajul17chiksite3 <- sum(jul17chiksite3$X3, na.rm=T)
ajul24chiksite3 <- sum(jul24chiksite3$X3, na.rm=T)
ajul30chiksite3 <- sum(jul30chiksite3$X3, na.rm=T)
aaug06chiksite3 <- sum(aug06chiksite3$X3, na.rm=T)
aaug13chiksite3 <- sum(aug13chiksite3$X3, na.rm=T)
aaug20chiksite3 <- sum(aug20chiksite3$X3, na.rm=T)
asep03chiksite3 <- sum(sep03chiksite3$X3, na.rm=T)
asep10chiksite3 <- sum(sep10chiksite3$X3, na.rm=T)
asep17chiksite3 <- sum(sep17chiksite3$X3, na.rm=T)
asep24chiksite3 <- sum(sep24chiksite3$X3, na.rm=T)


##### site4
#chik
ajun04chiksite4 <- sum(jun04chiksite4$X4, na.rm=T)
ajun12chiksite4 <- sum(jun12chiksite4$X4, na.rm=T)
ajun18chiksite4 <- sum(jun18chiksite4$X4, na.rm=T)
ajun25chiksite4 <- sum(jun25chiksite4$X4, na.rm=T)
ajul04chiksite4 <- sum(jul04chiksite4$X4, na.rm=T)
ajul10chiksite4 <- sum(jul10chiksite4$X4, na.rm=T)
ajul17chiksite4 <- sum(jul17chiksite4$X4, na.rm=T)
ajul24chiksite4 <- sum(jul24chiksite4$X4, na.rm=T)
ajul30chiksite4 <- sum(jul30chiksite4$X4, na.rm=T)
aaug06chiksite4 <- sum(aug06chiksite4$X4, na.rm=T)
aaug13chiksite4 <- sum(aug13chiksite4$X4, na.rm=T)
aaug20chiksite4 <- sum(aug20chiksite4$X4, na.rm=T)
asep03chiksite4 <- sum(sep03chiksite4$X4, na.rm=T)
asep10chiksite4 <- sum(sep10chiksite4$X4, na.rm=T)
asep17chiksite4 <- sum(sep17chiksite4$X4, na.rm=T)
asep24chiksite4 <- sum(sep24chiksite4$X4, na.rm=T)


##### site5
#chik
ajun04chiksite5 <- sum(jun04chiksite5$X5, na.rm=T)
ajun12chiksite5 <- sum(jun12chiksite5$X5, na.rm=T)
ajun18chiksite5 <- sum(jun18chiksite5$X5, na.rm=T)
ajun25chiksite5 <- sum(jun25chiksite5$X5, na.rm=T)
ajul04chiksite5 <- sum(jul04chiksite5$X5, na.rm=T)
ajul10chiksite5 <- sum(jul10chiksite5$X5, na.rm=T)
ajul17chiksite5 <- sum(jul17chiksite5$X5, na.rm=T)
ajul24chiksite5 <- sum(jul24chiksite5$X5, na.rm=T)
ajul30chiksite5 <- sum(jul30chiksite5$X5, na.rm=T)
aaug06chiksite5 <- sum(aug06chiksite5$X5, na.rm=T)
aaug13chiksite5 <- sum(aug13chiksite5$X5, na.rm=T)
aaug20chiksite5 <- sum(aug20chiksite5$X5, na.rm=T)
asep03chiksite5 <- sum(sep03chiksite5$X5, na.rm=T)
asep10chiksite5 <- sum(sep10chiksite5$X5, na.rm=T)
asep17chiksite5 <- sum(sep17chiksite5$X5, na.rm=T)
asep24chiksite5 <- sum(sep24chiksite5$X5, na.rm=T)




##### site6
#chik
jun04chiksite6$X6 <- as.numeric(jun04chiksite6$X6)
jun12chiksite6$X6 <- as.numeric(jun12chiksite6$X6)
jun18chiksite6$X6 <- as.numeric(jun18chiksite6$X6)
jun25chiksite6$X6 <- as.numeric(jun25chiksite6$X6)
jul04chiksite6$X6 <- as.numeric(jul04chiksite6$X6)
jul10chiksite6$X6 <- as.numeric(jul10chiksite6$X6)
jul17chiksite6$X6 <- as.numeric(jul17chiksite6$X6)
jul24chiksite6$X6 <- as.numeric(jul24chiksite6$X6)
jul30chiksite6$X6 <- as.numeric(jul30chiksite6$X6)
aug06chiksite6$X6 <- as.numeric(aug06chiksite6$X6)
aug13chiksite6$X6 <- as.numeric(aug13chiksite6$X6)
aug20chiksite6$X6 <- as.numeric(aug20chiksite6$X6)
sep03chiksite6$X6 <- as.numeric(sep03chiksite6$X6)
sep10chiksite6$X6 <- as.numeric(sep10chiksite6$X6)
sep17chiksite6$X6 <- as.numeric(sep17chiksite6$X6)
sep24chiksite6$X6 <- as.numeric(sep24chiksite6$X6)


ajun04chiksite6 <- sum(jun04chiksite6$X6, na.rm=T)
ajun12chiksite6 <- sum(jun12chiksite6$X6, na.rm=T)
ajun18chiksite6 <- sum(jun18chiksite6$X6, na.rm=T)
ajun25chiksite6 <- sum(jun25chiksite6$X6, na.rm=T)
ajul04chiksite6 <- sum(jul04chiksite6$X6, na.rm=T)
ajul10chiksite6 <- sum(jul10chiksite6$X6, na.rm=T)
ajul17chiksite6 <- sum(jul17chiksite6$X6, na.rm=T)
ajul24chiksite6 <- sum(jul24chiksite6$X6, na.rm=T)
ajul30chiksite6 <- sum(jul30chiksite6$X6, na.rm=T)
aaug06chiksite6 <- sum(aug06chiksite6$X6, na.rm=T)
aaug13chiksite6 <- sum(aug13chiksite6$X6, na.rm=T)
aaug20chiksite6 <- sum(aug20chiksite6$X6, na.rm=T)
asep03chiksite6 <- sum(sep03chiksite6$X6, na.rm=T)
asep10chiksite6 <- sum(sep10chiksite6$X6, na.rm=T)
asep17chiksite6 <- sum(sep17chiksite6$X6, na.rm=T)
asep24chiksite6 <- sum(sep24chiksite6$X6, na.rm=T)




##### site7
#chik
ajun04chiksite7 <- sum(jun04chiksite7$X7, na.rm=T)
ajun12chiksite7 <- sum(jun12chiksite7$X7, na.rm=T)
ajun18chiksite7 <- sum(jun18chiksite7$X7, na.rm=T)
ajun25chiksite7 <- sum(jun25chiksite7$X7, na.rm=T)
ajul04chiksite7 <- sum(jul04chiksite7$X7, na.rm=T)
ajul10chiksite7 <- sum(jul10chiksite7$X7, na.rm=T)
ajul17chiksite7 <- sum(jul17chiksite7$X7, na.rm=T)
ajul24chiksite7 <- sum(jul24chiksite7$X7, na.rm=T)
ajul30chiksite7 <- sum(jul30chiksite7$X7, na.rm=T)
aaug06chiksite7 <- sum(aug06chiksite7$X7, na.rm=T)
aaug13chiksite7 <- sum(aug13chiksite7$X7, na.rm=T)
aaug20chiksite7 <- sum(aug20chiksite7$X7, na.rm=T)
asep03chiksite7 <- sum(sep03chiksite7$X7, na.rm=T)
asep10chiksite7 <- sum(sep10chiksite7$X7, na.rm=T)
asep17chiksite7 <- sum(sep17chiksite7$X7, na.rm=T)
asep24chiksite7 <- sum(sep24chiksite7$X7, na.rm=T)




##### site8
#chik
ajun04chiksite8 <- sum(jun04chiksite8$X8, na.rm=T)
ajun12chiksite8 <- sum(jun12chiksite8$X8, na.rm=T)
ajun18chiksite8 <- sum(jun18chiksite8$X8, na.rm=T)
ajun25chiksite8 <- sum(jun25chiksite8$X8, na.rm=T)
ajul04chiksite8 <- sum(jul04chiksite8$X8, na.rm=T)
ajul10chiksite8 <- sum(jul10chiksite8$X8, na.rm=T)
ajul17chiksite8 <- sum(jul17chiksite8$X8, na.rm=T)
ajul24chiksite8 <- sum(jul24chiksite8$X8, na.rm=T)
ajul30chiksite8 <- sum(jul30chiksite8$X8, na.rm=T)
aaug06chiksite8 <- sum(aug06chiksite8$X8, na.rm=T)
aaug13chiksite8 <- sum(aug13chiksite8$X8, na.rm=T)
aaug20chiksite8 <- sum(aug20chiksite8$X8, na.rm=T)
asep03chiksite8 <- sum(sep03chiksite8$X8, na.rm=T)
asep10chiksite8 <- sum(sep10chiksite8$X8, na.rm=T)
asep17chiksite8 <- sum(sep17chiksite8$X8, na.rm=T)
asep24chiksite8 <- sum(sep24chiksite8$X8, na.rm=T)




##### site9
#chik

jun04chiksite9$X9 <- as.numeric(jun04chiksite9$X9)
jun12chiksite9$X9 <- as.numeric(jun12chiksite9$X9)
jun18chiksite9$X9 <- as.numeric(jun18chiksite9$X9)
jun25chiksite9$X9 <- as.numeric(jun25chiksite9$X9)
jul04chiksite9$X9 <- as.numeric(jul04chiksite9$X9)
jul10chiksite9$X9 <- as.numeric(jul10chiksite9$X9)
jul17chiksite9$X9 <- as.numeric(jul17chiksite9$X9)
jul24chiksite9$X9 <- as.numeric(jul24chiksite9$X9)
jul30chiksite9$X9 <- as.numeric(jul30chiksite9$X9)
aug06chiksite9$X9 <- as.numeric(aug06chiksite9$X9)
aug13chiksite9$X9 <- as.numeric(aug13chiksite9$X9)
aug20chiksite9$X9 <- as.numeric(aug20chiksite9$X9)
sep03chiksite9$X9 <- as.numeric(sep03chiksite9$X9)
sep10chiksite9$X9 <- as.numeric(sep10chiksite9$X9)
sep17chiksite9$X9 <- as.numeric(sep17chiksite9$X9)
sep24chiksite9$X9 <- as.numeric(sep24chiksite9$X9)


ajun04chiksite9 <- sum(jun04chiksite9$X9, na.rm=T)
ajun12chiksite9 <- sum(jun12chiksite9$X9, na.rm=T)
ajun18chiksite9 <- sum(jun18chiksite9$X9, na.rm=T)
ajun25chiksite9 <- sum(jun25chiksite9$X9, na.rm=T)
ajul04chiksite9 <- sum(jul04chiksite9$X9, na.rm=T)
ajul10chiksite9 <- sum(jul10chiksite9$X9, na.rm=T)
ajul17chiksite9 <- sum(jul17chiksite9$X9, na.rm=T)
ajul24chiksite9 <- sum(jul24chiksite9$X9, na.rm=T)
ajul30chiksite9 <- sum(jul30chiksite9$X9, na.rm=T)
aaug06chiksite9 <- sum(aug06chiksite9$X9, na.rm=T)
aaug13chiksite9 <- sum(aug13chiksite9$X9, na.rm=T)
aaug20chiksite9 <- sum(aug20chiksite9$X9, na.rm=T)
asep03chiksite9 <- sum(sep03chiksite9$X9, na.rm=T)
asep10chiksite9 <- sum(sep10chiksite9$X9, na.rm=T)
asep17chiksite9 <- sum(sep17chiksite9$X9, na.rm=T)
asep24chiksite9 <- sum(sep24chiksite9$X9, na.rm=T)




##### site10
#chik
jun04chiksite10$X10 <- as.numeric(jun04chiksite10$X10)
jun12chiksite10$X10 <- as.numeric(jun12chiksite10$X10)
jun18chiksite10$X10 <- as.numeric(jun18chiksite10$X10)
jun25chiksite10$X10 <- as.numeric(jun25chiksite10$X10)
jul04chiksite10$X10 <- as.numeric(jul04chiksite10$X10)
jul10chiksite10$X10 <- as.numeric(jul10chiksite10$X10)
jul17chiksite10$X10 <- as.numeric(jul17chiksite10$X10)
jul24chiksite10$X10 <- as.numeric(jul24chiksite10$X10)
jul30chiksite10$X10 <- as.numeric(jul30chiksite10$X10)
aug06chiksite10$X10 <- as.numeric(aug06chiksite10$X10)
aug13chiksite10$X10 <- as.numeric(aug13chiksite10$X10)
aug20chiksite10$X10 <- as.numeric(aug20chiksite10$X10)
sep03chiksite10$X10 <- as.numeric(sep03chiksite10$X10)
sep10chiksite10$X10 <- as.numeric(sep10chiksite10$X10)
sep17chiksite10$X10 <- as.numeric(sep17chiksite10$X10)
sep24chiksite10$X10 <- as.numeric(sep24chiksite10$X10)


ajun04chiksite10 <- sum(jun04chiksite10$X10, na.rm=T)
ajun12chiksite10 <- sum(jun12chiksite10$X10, na.rm=T)
ajun18chiksite10 <- sum(jun18chiksite10$X10, na.rm=T)
ajun25chiksite10 <- sum(jun25chiksite10$X10, na.rm=T)
ajul04chiksite10 <- sum(jul04chiksite10$X10, na.rm=T)
ajul10chiksite10 <- sum(jul10chiksite10$X10, na.rm=T)
ajul17chiksite10 <- sum(jul17chiksite10$X10, na.rm=T)
ajul24chiksite10 <- sum(jul24chiksite10$X10, na.rm=T)
ajul30chiksite10 <- sum(jul30chiksite10$X10, na.rm=T)
aaug06chiksite10 <- sum(aug06chiksite10$X10, na.rm=T)
aaug13chiksite10 <- sum(aug13chiksite10$X10, na.rm=T)
aaug20chiksite10 <- sum(aug20chiksite10$X10, na.rm=T)
asep03chiksite10 <- sum(sep03chiksite10$X10, na.rm=T)
asep10chiksite10 <- sum(sep10chiksite10$X10, na.rm=T)
asep17chiksite10 <- sum(sep17chiksite10$X10, na.rm=T)
asep24chiksite10 <- sum(sep24chiksite10$X10, na.rm=T)

##
#
#
#
#
#



#CREATE chikS FOR PLOTTING
#Site 1
chik1 <- c(ajun04chiksite1,
           ajun12chiksite1,
           ajun18chiksite1,
           ajun25chiksite1,
           ajul04chiksite1,
           ajul10chiksite1,
           ajul17chiksite1,
           ajul24chiksite1,
           ajul30chiksite1,
           aaug06chiksite1,
           aaug13chiksite1,
           aaug20chiksite1,
           asep03chiksite1,
           asep10chiksite1,
           asep17chiksite1,
           asep24chiksite1)

#Site 2
chik2 <- c(ajun04chiksite2,
           ajun12chiksite2,
           ajun18chiksite2,
           ajun25chiksite2,
           ajul04chiksite2,
           ajul10chiksite2,
           ajul17chiksite2,
           ajul24chiksite2,
           ajul30chiksite2,
           aaug06chiksite2,
           aaug13chiksite2,
           aaug20chiksite2,
           asep03chiksite2,
           asep10chiksite2,
           asep17chiksite2,
           asep24chiksite2)


#Site 3
chik3 <- c(ajun04chiksite3,
           ajun12chiksite3,
           ajun18chiksite3,
           ajun25chiksite3,
           ajul04chiksite3,
           ajul10chiksite3,
           ajul17chiksite3,
           ajul24chiksite3,
           ajul30chiksite3,
           aaug06chiksite3,
           aaug13chiksite3,
           aaug20chiksite3,
           asep03chiksite3,
           asep10chiksite3,
           asep17chiksite3,
           asep24chiksite3)


#Site 4
chik4 <- c(ajun04chiksite4,
           ajun12chiksite4,
           ajun18chiksite4,
           ajun25chiksite4,
           ajul04chiksite4,
           ajul10chiksite4,
           ajul17chiksite4,
           ajul24chiksite4,
           ajul30chiksite4,
           aaug06chiksite4,
           aaug13chiksite4,
           aaug20chiksite4,
           asep03chiksite4,
           asep10chiksite4,
           asep17chiksite4,
           asep24chiksite4)


#Site 5
chik5 <- c(ajun04chiksite5,
           ajun12chiksite5,
           ajun18chiksite5,
           ajun25chiksite5,
           ajul04chiksite5,
           ajul10chiksite5,
           ajul17chiksite5,
           ajul24chiksite5,
           ajul30chiksite5,
           aaug06chiksite5,
           aaug13chiksite5,
           aaug20chiksite5,
           asep03chiksite5,
           asep10chiksite5,
           asep17chiksite5,
           asep24chiksite5)


#Site 6
chik6 <- c(ajun04chiksite6,
           ajun12chiksite6,
           ajun18chiksite6,
           ajun25chiksite6,
           ajul04chiksite6,
           ajul10chiksite6,
           ajul17chiksite6,
           ajul24chiksite6,
           ajul30chiksite6,
           aaug06chiksite6,
           aaug13chiksite6,
           aaug20chiksite6,
           asep03chiksite6,
           asep10chiksite6,
           asep17chiksite6,
           asep24chiksite6)


#Site 7
chik7 <- c(ajun04chiksite7,
           ajun12chiksite7,
           ajun18chiksite7,
           ajun25chiksite7,
           ajul04chiksite7,
           ajul10chiksite7,
           ajul17chiksite7,
           ajul24chiksite7,
           ajul30chiksite7,
           aaug06chiksite7,
           aaug13chiksite7,
           aaug20chiksite7,
           asep03chiksite7,
           asep10chiksite7,
           asep17chiksite7,
           asep24chiksite7)


#Site 8
chik8 <- c(ajun04chiksite8,
           ajun12chiksite8,
           ajun18chiksite8,
           ajun25chiksite8,
           ajul04chiksite8,
           ajul10chiksite8,
           ajul17chiksite8,
           ajul24chiksite8,
           ajul30chiksite8,
           aaug06chiksite8,
           aaug13chiksite8,
           aaug20chiksite8,
           asep03chiksite8,
           asep10chiksite8,
           asep17chiksite8,
           asep24chiksite8)


#Site 9
chik9 <- c(ajun04chiksite9,
           ajun12chiksite9,
           ajun18chiksite9,
           ajun25chiksite9,
           ajul04chiksite9,
           ajul10chiksite9,
           ajul17chiksite9,
           ajul24chiksite9,
           ajul30chiksite9,
           aaug06chiksite9,
           aaug13chiksite9,
           aaug20chiksite9,
           asep03chiksite9,
           asep10chiksite9,
           asep17chiksite9,
           asep24chiksite9)


#Site 10
chik10 <- c(ajun04chiksite10,
            ajun12chiksite10,
            ajun18chiksite10,
            ajun25chiksite10,
            ajul04chiksite10,
            ajul10chiksite10,
            ajul17chiksite10,
            ajul24chiksite10,
            ajul30chiksite10,
            aaug06chiksite10,
            aaug13chiksite10,
            aaug20chiksite10,
            asep03chiksite10,
            asep10chiksite10,
            asep17chiksite10,
            asep24chiksite10)



chiktotal <- chik1+chik2+chik3+chik4+chik5+chik6+chik7+chik8+chik9+chik10


##### MALARIA 5 - MALARIA
#SUBSET BY DATE and malaria
jun04malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-04"))
jun12malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-12"))
jun18malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-18"))
jun25malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-25"))
jul04malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-04"))
jul10malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-10"))
jul17malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-17"))
jul24malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-24"))
jul30malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-30"))
aug06malaria <- subset(mergedmalaria, date2  == as.Date("2013-08-06"))
aug13malaria <- subset(mergedmalaria, date2  == as.Date("2013-08-13"))
aug20malaria <- subset(mergedmalaria, date2  == as.Date("2013-08-20"))
sep03malaria <- subset(mergedmalaria, date2  == as.Date("2013-09-03"))
sep10malaria <- subset(mergedmalaria, date2  == as.Date("2013-09-10"))
sep17malaria <- subset(mergedmalaria, date2  == as.Date("2013-09-17"))
sep24malaria <- subset(mergedmalaria, date2  == as.Date("2013-09-24"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE malaria SITE

#JUN 04 malaria
jun04malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-04"))
jun04malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-04"))
jun04malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-04"))
jun04malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-04"))
jun04malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-04"))
jun04malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-04"))
jun04malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-04"))
jun04malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-04"))
jun04malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-04"))
jun04malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-04"))


#jun12 malaria
jun12malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-12"))
jun12malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-12"))
jun12malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-12"))
jun12malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-12"))
jun12malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-12"))
jun12malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-12"))
jun12malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-12"))
jun12malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-12"))
jun12malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-12"))
jun12malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-12"))

#jun18 malaria
jun18malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-18"))
jun18malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-18"))
jun18malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-18"))
jun18malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-18"))
jun18malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-18"))
jun18malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-18"))
jun18malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-18"))
jun18malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-18"))
jun18malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-18"))
jun18malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-18"))

#jun25 malaria
jun25malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-25"))
jun25malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-25"))
jun25malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-25"))
jun25malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-25"))
jun25malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-25"))
jun25malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-25"))
jun25malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-25"))
jun25malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-25"))
jun25malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-25"))
jun25malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-25"))

#jul04 malaria
jul04malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-04"))
jul04malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-04"))
jul04malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-04"))
jul04malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-04"))
jul04malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-04"))
jul04malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-04"))
jul04malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-04"))
jul04malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-04"))
jul04malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-04"))
jul04malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-04"))

#jul10 malaria
jul10malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-10"))
jul10malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-10"))
jul10malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-10"))
jul10malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-10"))
jul10malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-10"))
jul10malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-10"))
jul10malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-10"))
jul10malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-10"))
jul10malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-10"))
jul10malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-10"))

#jul17 malaria
jul17malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-17"))
jul17malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-17"))
jul17malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-17"))
jul17malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-17"))
jul17malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-17"))
jul17malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-17"))
jul17malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-17"))
jul17malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-17"))
jul17malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-17"))
jul17malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-17"))

#jul24 malaria
jul24malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-24"))
jul24malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-24"))
jul24malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-24"))
jul24malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-24"))
jul24malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-24"))
jul24malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-24"))
jul24malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-24"))
jul24malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-24"))
jul24malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-24"))
jul24malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-24"))

#jul30 malaria
jul30malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-30"))
jul30malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-30"))
jul30malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-30"))
jul30malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-30"))
jul30malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-30"))
jul30malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-30"))
jul30malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-30"))
jul30malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-30"))
jul30malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-30"))
jul30malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-30"))

#aug06 malaria
aug06malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-08-06"))
aug06malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-08-06"))
aug06malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-08-06"))
aug06malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-08-06"))
aug06malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-08-06"))
aug06malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-08-06"))
aug06malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-08-06"))
aug06malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-08-06"))
aug06malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-08-06"))
aug06malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-08-06"))

#aug13 malaria
aug13malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-08-13"))
aug13malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-08-13"))
aug13malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-08-13"))
aug13malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-08-13"))
aug13malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-08-13"))
aug13malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-08-13"))
aug13malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-08-13"))
aug13malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-08-13"))
aug13malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-08-13"))
aug13malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-08-13"))

#aug20 malaria
aug20malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-08-20"))
aug20malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-08-20"))
aug20malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-08-20"))
aug20malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-08-20"))
aug20malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-08-20"))
aug20malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-08-20"))
aug20malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-08-20"))
aug20malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-08-20"))
aug20malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-08-20"))
aug20malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-08-20"))

#sep03 malaria
sep03malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-09-03"))
sep03malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-09-03"))
sep03malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-09-03"))
sep03malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-09-03"))
sep03malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-09-03"))
sep03malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-09-03"))
sep03malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-09-03"))
sep03malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-09-03"))
sep03malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-09-03"))
sep03malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-09-03"))

#sep10 malaria
sep10malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-09-10"))
sep10malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-09-10"))
sep10malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-09-10"))
sep10malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-09-10"))
sep10malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-09-10"))
sep10malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-09-10"))
sep10malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-09-10"))
sep10malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-09-10"))
sep10malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-09-10"))
sep10malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-09-10"))

#sep17 malaria
sep17malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-09-17"))
sep17malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-09-17"))
sep17malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-09-17"))
sep17malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-09-17"))
sep17malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-09-17"))
sep17malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-09-17"))
sep17malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-09-17"))
sep17malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-09-17"))
sep17malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-09-17"))
sep17malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-09-17"))

#sep24 malaria
sep24malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-09-24"))
sep24malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-09-24"))
sep24malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-09-24"))
sep24malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-09-24"))
sep24malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-09-24"))
sep24malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-09-24"))
sep24malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-09-24"))
sep24malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-09-24"))
sep24malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-09-24"))
sep24malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-09-24"))


##### ALL SITES COMBINED
#malaria
ajun04malariatotal <- sum(jun04malaria$total, na.rm=T)
ajun12malariatotal <- sum(jun12malaria$total, na.rm=T)
ajun18malariatotal <- sum(jun18malaria$total, na.rm=T)
ajun25malariatotal <- sum(jun25malaria$total, na.rm=T)
ajul04malariatotal <- sum(jul04malaria$total, na.rm=T)
ajul10malariatotal <- sum(jul10malaria$total, na.rm=T)
ajul17malariatotal <- sum(jul17malaria$total, na.rm=T)
ajul24malariatotal <- sum(jul24malaria$total, na.rm=T)
ajul30malariatotal <- sum(jul30malaria$total, na.rm=T)
aaug06malariatotal <- sum(aug06malaria$total, na.rm=T)
aaug13malariatotal <- sum(aug13malaria$total, na.rm=T)
aaug20malariatotal <- sum(aug20malaria$total, na.rm=T)
asep03malariatotal <- sum(sep03malaria$total, na.rm=T)
asep10malariatotal <- sum(sep10malaria$total, na.rm=T)
asep17malariatotal <- sum(sep17malaria$total, na.rm=T)
asep24malariatotal <- sum(sep24malaria$total, na.rm=T)




##### site1
#malaria
ajun04malariasite1 <- sum(jun04malariasite1$X1, na.rm=T)
ajun12malariasite1 <- sum(jun12malariasite1$X1, na.rm=T)
ajun18malariasite1 <- sum(jun18malariasite1$X1, na.rm=T)
ajun25malariasite1 <- sum(jun25malariasite1$X1, na.rm=T)
ajul04malariasite1 <- sum(jul04malariasite1$X1, na.rm=T)
ajul10malariasite1 <- sum(jul10malariasite1$X1, na.rm=T)
ajul17malariasite1 <- sum(jul17malariasite1$X1, na.rm=T)
ajul24malariasite1 <- sum(jul24malariasite1$X1, na.rm=T)
ajul30malariasite1 <- sum(jul30malariasite1$X1, na.rm=T)
aaug06malariasite1 <- sum(aug06malariasite1$X1, na.rm=T)
aaug13malariasite1 <- sum(aug13malariasite1$X1, na.rm=T)
aaug20malariasite1 <- sum(aug20malariasite1$X1, na.rm=T)
asep03malariasite1 <- sum(sep03malariasite1$X1, na.rm=T)
asep10malariasite1 <- sum(sep10malariasite1$X1, na.rm=T)
asep17malariasite1 <- sum(sep17malariasite1$X1, na.rm=T)
asep24malariasite1 <- sum(sep24malariasite1$X1, na.rm=T)



##### site2
#malaria
ajun04malariasite2 <- sum(jun04malariasite2$X2, na.rm=T)
ajun12malariasite2 <- sum(jun12malariasite2$X2, na.rm=T)
ajun18malariasite2 <- sum(jun18malariasite2$X2, na.rm=T)
ajun25malariasite2 <- sum(jun25malariasite2$X2, na.rm=T)
ajul04malariasite2 <- sum(jul04malariasite2$X2, na.rm=T)
ajul10malariasite2 <- sum(jul10malariasite2$X2, na.rm=T)
ajul17malariasite2 <- sum(jul17malariasite2$X2, na.rm=T)
ajul24malariasite2 <- sum(jul24malariasite2$X2, na.rm=T)
ajul30malariasite2 <- sum(jul30malariasite2$X2, na.rm=T)
aaug06malariasite2 <- sum(aug06malariasite2$X2, na.rm=T)
aaug13malariasite2 <- sum(aug13malariasite2$X2, na.rm=T)
aaug20malariasite2 <- sum(aug20malariasite2$X2, na.rm=T)
asep03malariasite2 <- sum(sep03malariasite2$X2, na.rm=T)
asep10malariasite2 <- sum(sep10malariasite2$X2, na.rm=T)
asep17malariasite2 <- sum(sep17malariasite2$X2, na.rm=T)
asep24malariasite2 <- sum(sep24malariasite2$X2, na.rm=T)



##### site3
#malaria
ajun04malariasite3 <- sum(jun04malariasite3$X3, na.rm=T)
ajun12malariasite3 <- sum(jun12malariasite3$X3, na.rm=T)
ajun18malariasite3 <- sum(jun18malariasite3$X3, na.rm=T)
ajun25malariasite3 <- sum(jun25malariasite3$X3, na.rm=T)
ajul04malariasite3 <- sum(jul04malariasite3$X3, na.rm=T)
ajul10malariasite3 <- sum(jul10malariasite3$X3, na.rm=T)
ajul17malariasite3 <- sum(jul17malariasite3$X3, na.rm=T)
ajul24malariasite3 <- sum(jul24malariasite3$X3, na.rm=T)
ajul30malariasite3 <- sum(jul30malariasite3$X3, na.rm=T)
aaug06malariasite3 <- sum(aug06malariasite3$X3, na.rm=T)
aaug13malariasite3 <- sum(aug13malariasite3$X3, na.rm=T)
aaug20malariasite3 <- sum(aug20malariasite3$X3, na.rm=T)
asep03malariasite3 <- sum(sep03malariasite3$X3, na.rm=T)
asep10malariasite3 <- sum(sep10malariasite3$X3, na.rm=T)
asep17malariasite3 <- sum(sep17malariasite3$X3, na.rm=T)
asep24malariasite3 <- sum(sep24malariasite3$X3, na.rm=T)


##### site4
#malaria
ajun04malariasite4 <- sum(jun04malariasite4$X4, na.rm=T)
ajun12malariasite4 <- sum(jun12malariasite4$X4, na.rm=T)
ajun18malariasite4 <- sum(jun18malariasite4$X4, na.rm=T)
ajun25malariasite4 <- sum(jun25malariasite4$X4, na.rm=T)
ajul04malariasite4 <- sum(jul04malariasite4$X4, na.rm=T)
ajul10malariasite4 <- sum(jul10malariasite4$X4, na.rm=T)
ajul17malariasite4 <- sum(jul17malariasite4$X4, na.rm=T)
ajul24malariasite4 <- sum(jul24malariasite4$X4, na.rm=T)
ajul30malariasite4 <- sum(jul30malariasite4$X4, na.rm=T)
aaug06malariasite4 <- sum(aug06malariasite4$X4, na.rm=T)
aaug13malariasite4 <- sum(aug13malariasite4$X4, na.rm=T)
aaug20malariasite4 <- sum(aug20malariasite4$X4, na.rm=T)
asep03malariasite4 <- sum(sep03malariasite4$X4, na.rm=T)
asep10malariasite4 <- sum(sep10malariasite4$X4, na.rm=T)
asep17malariasite4 <- sum(sep17malariasite4$X4, na.rm=T)
asep24malariasite4 <- sum(sep24malariasite4$X4, na.rm=T)



##### site5
#malaria
ajun04malariasite5 <- sum(jun04malariasite5$X5, na.rm=T)
ajun12malariasite5 <- sum(jun12malariasite5$X5, na.rm=T)
ajun18malariasite5 <- sum(jun18malariasite5$X5, na.rm=T)
ajun25malariasite5 <- sum(jun25malariasite5$X5, na.rm=T)
ajul04malariasite5 <- sum(jul04malariasite5$X5, na.rm=T)
ajul10malariasite5 <- sum(jul10malariasite5$X5, na.rm=T)
ajul17malariasite5 <- sum(jul17malariasite5$X5, na.rm=T)
ajul24malariasite5 <- sum(jul24malariasite5$X5, na.rm=T)
ajul30malariasite5 <- sum(jul30malariasite5$X5, na.rm=T)
aaug06malariasite5 <- sum(aug06malariasite5$X5, na.rm=T)
aaug13malariasite5 <- sum(aug13malariasite5$X5, na.rm=T)
aaug20malariasite5 <- sum(aug20malariasite5$X5, na.rm=T)
asep03malariasite5 <- sum(sep03malariasite5$X5, na.rm=T)
asep10malariasite5 <- sum(sep10malariasite5$X5, na.rm=T)
asep17malariasite5 <- sum(sep17malariasite5$X5, na.rm=T)
asep24malariasite5 <- sum(sep24malariasite5$X5, na.rm=T)




##### site6
#malaria
jun04malariasite6$X6 <- as.numeric(jun04malariasite6$X6)
jun12malariasite6$X6 <- as.numeric(jun12malariasite6$X6)
jun18malariasite6$X6 <- as.numeric(jun18malariasite6$X6)
jun25malariasite6$X6 <- as.numeric(jun25malariasite6$X6)
jul04malariasite6$X6 <- as.numeric(jul04malariasite6$X6)
jul10malariasite6$X6 <- as.numeric(jul10malariasite6$X6)
jul17malariasite6$X6 <- as.numeric(jul17malariasite6$X6)
jul24malariasite6$X6 <- as.numeric(jul24malariasite6$X6)
jul30malariasite6$X6 <- as.numeric(jul30malariasite6$X6)
aug06malariasite6$X6 <- as.numeric(aug06malariasite6$X6)
aug13malariasite6$X6 <- as.numeric(aug13malariasite6$X6)
aug20malariasite6$X6 <- as.numeric(aug20malariasite6$X6)
sep03malariasite6$X6 <- as.numeric(sep03malariasite6$X6)
sep10malariasite6$X6 <- as.numeric(sep10malariasite6$X6)
sep17malariasite6$X6 <- as.numeric(sep17malariasite6$X6)
sep24malariasite6$X6 <- as.numeric(sep24malariasite6$X6)


ajun04malariasite6 <- sum(jun04malariasite6$X6, na.rm=T)
ajun12malariasite6 <- sum(jun12malariasite6$X6, na.rm=T)
ajun18malariasite6 <- sum(jun18malariasite6$X6, na.rm=T)
ajun25malariasite6 <- sum(jun25malariasite6$X6, na.rm=T)
ajul04malariasite6 <- sum(jul04malariasite6$X6, na.rm=T)
ajul10malariasite6 <- sum(jul10malariasite6$X6, na.rm=T)
ajul17malariasite6 <- sum(jul17malariasite6$X6, na.rm=T)
ajul24malariasite6 <- sum(jul24malariasite6$X6, na.rm=T)
ajul30malariasite6 <- sum(jul30malariasite6$X6, na.rm=T)
aaug06malariasite6 <- sum(aug06malariasite6$X6, na.rm=T)
aaug13malariasite6 <- sum(aug13malariasite6$X6, na.rm=T)
aaug20malariasite6 <- sum(aug20malariasite6$X6, na.rm=T)
asep03malariasite6 <- sum(sep03malariasite6$X6, na.rm=T)
asep10malariasite6 <- sum(sep10malariasite6$X6, na.rm=T)
asep17malariasite6 <- sum(sep17malariasite6$X6, na.rm=T)
asep24malariasite6 <- sum(sep24malariasite6$X6, na.rm=T)



##### site7
#malaria
ajun04malariasite7 <- sum(jun04malariasite7$X7, na.rm=T)
ajun12malariasite7 <- sum(jun12malariasite7$X7, na.rm=T)
ajun18malariasite7 <- sum(jun18malariasite7$X7, na.rm=T)
ajun25malariasite7 <- sum(jun25malariasite7$X7, na.rm=T)
ajul04malariasite7 <- sum(jul04malariasite7$X7, na.rm=T)
ajul10malariasite7 <- sum(jul10malariasite7$X7, na.rm=T)
ajul17malariasite7 <- sum(jul17malariasite7$X7, na.rm=T)
ajul24malariasite7 <- sum(jul24malariasite7$X7, na.rm=T)
ajul30malariasite7 <- sum(jul30malariasite7$X7, na.rm=T)
aaug06malariasite7 <- sum(aug06malariasite7$X7, na.rm=T)
aaug13malariasite7 <- sum(aug13malariasite7$X7, na.rm=T)
aaug20malariasite7 <- sum(aug20malariasite7$X7, na.rm=T)
asep03malariasite7 <- sum(sep03malariasite7$X7, na.rm=T)
asep10malariasite7 <- sum(sep10malariasite7$X7, na.rm=T)
asep17malariasite7 <- sum(sep17malariasite7$X7, na.rm=T)
asep24malariasite7 <- sum(sep24malariasite7$X7, na.rm=T)




##### site8
#malaria
ajun04malariasite8 <- sum(jun04malariasite8$X8, na.rm=T)
ajun12malariasite8 <- sum(jun12malariasite8$X8, na.rm=T)
ajun18malariasite8 <- sum(jun18malariasite8$X8, na.rm=T)
ajun25malariasite8 <- sum(jun25malariasite8$X8, na.rm=T)
ajul04malariasite8 <- sum(jul04malariasite8$X8, na.rm=T)
ajul10malariasite8 <- sum(jul10malariasite8$X8, na.rm=T)
ajul17malariasite8 <- sum(jul17malariasite8$X8, na.rm=T)
ajul24malariasite8 <- sum(jul24malariasite8$X8, na.rm=T)
ajul30malariasite8 <- sum(jul30malariasite8$X8, na.rm=T)
aaug06malariasite8 <- sum(aug06malariasite8$X8, na.rm=T)
aaug13malariasite8 <- sum(aug13malariasite8$X8, na.rm=T)
aaug20malariasite8 <- sum(aug20malariasite8$X8, na.rm=T)
asep03malariasite8 <- sum(sep03malariasite8$X8, na.rm=T)
asep10malariasite8 <- sum(sep10malariasite8$X8, na.rm=T)
asep17malariasite8 <- sum(sep17malariasite8$X8, na.rm=T)
asep24malariasite8 <- sum(sep24malariasite8$X8, na.rm=T)


##### site9
#malaria

jun04malariasite9$X9 <- as.numeric(jun04malariasite9$X9)
jun12malariasite9$X9 <- as.numeric(jun12malariasite9$X9)
jun18malariasite9$X9 <- as.numeric(jun18malariasite9$X9)
jun25malariasite9$X9 <- as.numeric(jun25malariasite9$X9)
jul04malariasite9$X9 <- as.numeric(jul04malariasite9$X9)
jul10malariasite9$X9 <- as.numeric(jul10malariasite9$X9)
jul17malariasite9$X9 <- as.numeric(jul17malariasite9$X9)
jul24malariasite9$X9 <- as.numeric(jul24malariasite9$X9)
jul30malariasite9$X9 <- as.numeric(jul30malariasite9$X9)
aug06malariasite9$X9 <- as.numeric(aug06malariasite9$X9)
aug13malariasite9$X9 <- as.numeric(aug13malariasite9$X9)
aug20malariasite9$X9 <- as.numeric(aug20malariasite9$X9)
sep03malariasite9$X9 <- as.numeric(sep03malariasite9$X9)
sep10malariasite9$X9 <- as.numeric(sep10malariasite9$X9)
sep17malariasite9$X9 <- as.numeric(sep17malariasite9$X9)
sep24malariasite9$X9 <- as.numeric(sep24malariasite9$X9)


ajun04malariasite9 <- sum(jun04malariasite9$X9, na.rm=T)
ajun12malariasite9 <- sum(jun12malariasite9$X9, na.rm=T)
ajun18malariasite9 <- sum(jun18malariasite9$X9, na.rm=T)
ajun25malariasite9 <- sum(jun25malariasite9$X9, na.rm=T)
ajul04malariasite9 <- sum(jul04malariasite9$X9, na.rm=T)
ajul10malariasite9 <- sum(jul10malariasite9$X9, na.rm=T)
ajul17malariasite9 <- sum(jul17malariasite9$X9, na.rm=T)
ajul24malariasite9 <- sum(jul24malariasite9$X9, na.rm=T)
ajul30malariasite9 <- sum(jul30malariasite9$X9, na.rm=T)
aaug06malariasite9 <- sum(aug06malariasite9$X9, na.rm=T)
aaug13malariasite9 <- sum(aug13malariasite9$X9, na.rm=T)
aaug20malariasite9 <- sum(aug20malariasite9$X9, na.rm=T)
asep03malariasite9 <- sum(sep03malariasite9$X9, na.rm=T)
asep10malariasite9 <- sum(sep10malariasite9$X9, na.rm=T)
asep17malariasite9 <- sum(sep17malariasite9$X9, na.rm=T)
asep24malariasite9 <- sum(sep24malariasite9$X9, na.rm=T)



##### site10
#malaria
jun04malariasite10$X10 <- as.numeric(jun04malariasite10$X10)
jun12malariasite10$X10 <- as.numeric(jun12malariasite10$X10)
jun18malariasite10$X10 <- as.numeric(jun18malariasite10$X10)
jun25malariasite10$X10 <- as.numeric(jun25malariasite10$X10)
jul04malariasite10$X10 <- as.numeric(jul04malariasite10$X10)
jul10malariasite10$X10 <- as.numeric(jul10malariasite10$X10)
jul17malariasite10$X10 <- as.numeric(jul17malariasite10$X10)
jul24malariasite10$X10 <- as.numeric(jul24malariasite10$X10)
jul30malariasite10$X10 <- as.numeric(jul30malariasite10$X10)
aug06malariasite10$X10 <- as.numeric(aug06malariasite10$X10)
aug13malariasite10$X10 <- as.numeric(aug13malariasite10$X10)
aug20malariasite10$X10 <- as.numeric(aug20malariasite10$X10)
sep03malariasite10$X10 <- as.numeric(sep03malariasite10$X10)
sep10malariasite10$X10 <- as.numeric(sep10malariasite10$X10)
sep17malariasite10$X10 <- as.numeric(sep17malariasite10$X10)
sep24malariasite10$X10 <- as.numeric(sep24malariasite10$X10)


ajun04malariasite10 <- sum(jun04malariasite10$X10, na.rm=T)
ajun12malariasite10 <- sum(jun12malariasite10$X10, na.rm=T)
ajun18malariasite10 <- sum(jun18malariasite10$X10, na.rm=T)
ajun25malariasite10 <- sum(jun25malariasite10$X10, na.rm=T)
ajul04malariasite10 <- sum(jul04malariasite10$X10, na.rm=T)
ajul10malariasite10 <- sum(jul10malariasite10$X10, na.rm=T)
ajul17malariasite10 <- sum(jul17malariasite10$X10, na.rm=T)
ajul24malariasite10 <- sum(jul24malariasite10$X10, na.rm=T)
ajul30malariasite10 <- sum(jul30malariasite10$X10, na.rm=T)
aaug06malariasite10 <- sum(aug06malariasite10$X10, na.rm=T)
aaug13malariasite10 <- sum(aug13malariasite10$X10, na.rm=T)
aaug20malariasite10 <- sum(aug20malariasite10$X10, na.rm=T)
asep03malariasite10 <- sum(sep03malariasite10$X10, na.rm=T)
asep10malariasite10 <- sum(sep10malariasite10$X10, na.rm=T)
asep17malariasite10 <- sum(sep17malariasite10$X10, na.rm=T)
asep24malariasite10 <- sum(sep24malariasite10$X10, na.rm=T)




##
#
#
#
#
#



#CREATE malariaS FOR PLOTTING
#Site 1
malaria1 <- c(ajun04malariasite1,
              ajun12malariasite1,
              ajun18malariasite1,
              ajun25malariasite1,
              ajul04malariasite1,
              ajul10malariasite1,
              ajul17malariasite1,
              ajul24malariasite1,
              ajul30malariasite1,
              aaug06malariasite1,
              aaug13malariasite1,
              aaug20malariasite1,
              asep03malariasite1,
              asep10malariasite1,
              asep17malariasite1,
              asep24malariasite1)

#Site 2
malaria2 <- c(ajun04malariasite2,
              ajun12malariasite2,
              ajun18malariasite2,
              ajun25malariasite2,
              ajul04malariasite2,
              ajul10malariasite2,
              ajul17malariasite2,
              ajul24malariasite2,
              ajul30malariasite2,
              aaug06malariasite2,
              aaug13malariasite2,
              aaug20malariasite2,
              asep03malariasite2,
              asep10malariasite2,
              asep17malariasite2,
              asep24malariasite2)


#Site 3
malaria3 <- c(ajun04malariasite3,
              ajun12malariasite3,
              ajun18malariasite3,
              ajun25malariasite3,
              ajul04malariasite3,
              ajul10malariasite3,
              ajul17malariasite3,
              ajul24malariasite3,
              ajul30malariasite3,
              aaug06malariasite3,
              aaug13malariasite3,
              aaug20malariasite3,
              asep03malariasite3,
              asep10malariasite3,
              asep17malariasite3,
              asep24malariasite3)


#Site 4
malaria4 <- c(ajun04malariasite4,
              ajun12malariasite4,
              ajun18malariasite4,
              ajun25malariasite4,
              ajul04malariasite4,
              ajul10malariasite4,
              ajul17malariasite4,
              ajul24malariasite4,
              ajul30malariasite4,
              aaug06malariasite4,
              aaug13malariasite4,
              aaug20malariasite4,
              asep03malariasite4,
              asep10malariasite4,
              asep17malariasite4,
              asep24malariasite4)


#Site 5
malaria5 <- c(ajun04malariasite5,
              ajun12malariasite5,
              ajun18malariasite5,
              ajun25malariasite5,
              ajul04malariasite5,
              ajul10malariasite5,
              ajul17malariasite5,
              ajul24malariasite5,
              ajul30malariasite5,
              aaug06malariasite5,
              aaug13malariasite5,
              aaug20malariasite5,
              asep03malariasite5,
              asep10malariasite5,
              asep17malariasite5,
              asep24malariasite5)


#Site 6
malaria6 <- c(ajun04malariasite6,
              ajun12malariasite6,
              ajun18malariasite6,
              ajun25malariasite6,
              ajul04malariasite6,
              ajul10malariasite6,
              ajul17malariasite6,
              ajul24malariasite6,
              ajul30malariasite6,
              aaug06malariasite6,
              aaug13malariasite6,
              aaug20malariasite6,
              asep03malariasite6,
              asep10malariasite6,
              asep17malariasite6,
              asep24malariasite6)


#Site 7
malaria7 <- c(ajun04malariasite7,
              ajun12malariasite7,
              ajun18malariasite7,
              ajun25malariasite7,
              ajul04malariasite7,
              ajul10malariasite7,
              ajul17malariasite7,
              ajul24malariasite7,
              ajul30malariasite7,
              aaug06malariasite7,
              aaug13malariasite7,
              aaug20malariasite7,
              asep03malariasite7,
              asep10malariasite7,
              asep17malariasite7,
              asep24malariasite7)


#Site 8
malaria8 <- c(ajun04malariasite8,
              ajun12malariasite8,
              ajun18malariasite8,
              ajun25malariasite8,
              ajul04malariasite8,
              ajul10malariasite8,
              ajul17malariasite8,
              ajul24malariasite8,
              ajul30malariasite8,
              aaug06malariasite8,
              aaug13malariasite8,
              aaug20malariasite8,
              asep03malariasite8,
              asep10malariasite8,
              asep17malariasite8,
              asep24malariasite8)


#Site 9
malaria9 <- c(ajun04malariasite9,
              ajun12malariasite9,
              ajun18malariasite9,
              ajun25malariasite9,
              ajul04malariasite9,
              ajul10malariasite9,
              ajul17malariasite9,
              ajul24malariasite9,
              ajul30malariasite9,
              aaug06malariasite9,
              aaug13malariasite9,
              aaug20malariasite9,
              asep03malariasite9,
              asep10malariasite9,
              asep17malariasite9,
              asep24malariasite9)


#Site 10
malaria10 <- c(ajun04malariasite10,
               ajun12malariasite10,
               ajun18malariasite10,
               ajun25malariasite10,
               ajul04malariasite10,
               ajul10malariasite10,
               ajul17malariasite10,
               ajul24malariasite10,
               ajul30malariasite10,
               aaug06malariasite10,
               aaug13malariasite10,
               aaug20malariasite10,
               asep03malariasite10,
               asep10malariasite10,
               asep17malariasite10,
               asep24malariasite10)



malariatotal <- malaria1+malaria2+malaria3+malaria4+malaria5+malaria6+malaria7+malaria8+malaria9+malaria10


##### dengue 6 - DENGUE
#SUBSET BY DATE and dengue
jun04dengue <- subset(mergeddengue, date2  == as.Date("2013-06-04"))
jun12dengue <- subset(mergeddengue, date2  == as.Date("2013-06-12"))
jun18dengue <- subset(mergeddengue, date2  == as.Date("2013-06-18"))
jun25dengue <- subset(mergeddengue, date2  == as.Date("2013-06-25"))
jul04dengue <- subset(mergeddengue, date2  == as.Date("2013-07-04"))
jul10dengue <- subset(mergeddengue, date2  == as.Date("2013-07-10"))
jul17dengue <- subset(mergeddengue, date2  == as.Date("2013-07-17"))
jul24dengue <- subset(mergeddengue, date2  == as.Date("2013-07-24"))
jul30dengue <- subset(mergeddengue, date2  == as.Date("2013-07-30"))
aug06dengue <- subset(mergeddengue, date2  == as.Date("2013-08-06"))
aug13dengue <- subset(mergeddengue, date2  == as.Date("2013-08-13"))
aug20dengue <- subset(mergeddengue, date2  == as.Date("2013-08-20"))
sep03dengue <- subset(mergeddengue, date2  == as.Date("2013-09-03"))
sep10dengue <- subset(mergeddengue, date2  == as.Date("2013-09-10"))
sep17dengue <- subset(mergeddengue, date2  == as.Date("2013-09-17"))
sep24dengue <- subset(mergeddengue, date2  == as.Date("2013-09-24"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE dengue SITE

#JUN 04 dengue
jun04denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-04"))
jun04denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-04"))
jun04denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-04"))
jun04denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-04"))
jun04denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-04"))
jun04denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-04"))
jun04denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-04"))
jun04denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-04"))
jun04denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-04"))
jun04denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-04"))


#jun12 dengue
jun12denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-12"))
jun12denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-12"))
jun12denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-12"))
jun12denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-12"))
jun12denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-12"))
jun12denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-12"))
jun12denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-12"))
jun12denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-12"))
jun12denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-12"))
jun12denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-12"))

#jun18 dengue
jun18denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-18"))
jun18denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-18"))
jun18denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-18"))
jun18denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-18"))
jun18denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-18"))
jun18denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-18"))
jun18denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-18"))
jun18denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-18"))
jun18denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-18"))
jun18denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-18"))

#jun25 dengue
jun25denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-25"))
jun25denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-25"))
jun25denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-25"))
jun25denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-25"))
jun25denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-25"))
jun25denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-25"))
jun25denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-25"))
jun25denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-25"))
jun25denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-25"))
jun25denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-25"))

#jul04 dengue
jul04denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-04"))
jul04denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-04"))
jul04denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-04"))
jul04denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-04"))
jul04denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-04"))
jul04denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-04"))
jul04denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-04"))
jul04denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-04"))
jul04denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-04"))
jul04denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-04"))

#jul10 dengue
jul10denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-10"))
jul10denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-10"))
jul10denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-10"))
jul10denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-10"))
jul10denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-10"))
jul10denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-10"))
jul10denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-10"))
jul10denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-10"))
jul10denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-10"))
jul10denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-10"))

#jul17 dengue
jul17denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-17"))
jul17denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-17"))
jul17denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-17"))
jul17denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-17"))
jul17denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-17"))
jul17denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-17"))
jul17denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-17"))
jul17denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-17"))
jul17denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-17"))
jul17denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-17"))

#jul24 dengue
jul24denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-24"))
jul24denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-24"))
jul24denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-24"))
jul24denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-24"))
jul24denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-24"))
jul24denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-24"))
jul24denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-24"))
jul24denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-24"))
jul24denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-24"))
jul24denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-24"))

#jul30 dengue
jul30denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-30"))
jul30denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-30"))
jul30denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-30"))
jul30denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-30"))
jul30denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-30"))
jul30denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-30"))
jul30denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-30"))
jul30denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-30"))
jul30denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-30"))
jul30denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-30"))

#aug06 dengue
aug06denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-08-06"))
aug06denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-08-06"))
aug06denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-08-06"))
aug06denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-08-06"))
aug06denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-08-06"))
aug06denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-08-06"))
aug06denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-08-06"))
aug06denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-08-06"))
aug06denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-08-06"))
aug06denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-08-06"))

#aug13 dengue
aug13denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-08-13"))
aug13denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-08-13"))
aug13denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-08-13"))
aug13denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-08-13"))
aug13denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-08-13"))
aug13denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-08-13"))
aug13denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-08-13"))
aug13denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-08-13"))
aug13denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-08-13"))
aug13denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-08-13"))

#aug20 dengue
aug20denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-08-20"))
aug20denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-08-20"))
aug20denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-08-20"))
aug20denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-08-20"))
aug20denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-08-20"))
aug20denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-08-20"))
aug20denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-08-20"))
aug20denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-08-20"))
aug20denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-08-20"))
aug20denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-08-20"))

#sep03 dengue
sep03denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-09-03"))
sep03denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-09-03"))
sep03denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-09-03"))
sep03denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-09-03"))
sep03denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-09-03"))
sep03denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-09-03"))
sep03denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-09-03"))
sep03denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-09-03"))
sep03denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-09-03"))
sep03denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-09-03"))

#sep10 dengue
sep10denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-09-10"))
sep10denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-09-10"))
sep10denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-09-10"))
sep10denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-09-10"))
sep10denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-09-10"))
sep10denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-09-10"))
sep10denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-09-10"))
sep10denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-09-10"))
sep10denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-09-10"))
sep10denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-09-10"))

#sep17 dengue
sep17denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-09-17"))
sep17denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-09-17"))
sep17denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-09-17"))
sep17denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-09-17"))
sep17denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-09-17"))
sep17denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-09-17"))
sep17denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-09-17"))
sep17denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-09-17"))
sep17denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-09-17"))
sep17denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-09-17"))


#sep24 dengue
sep24denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-09-24"))
sep24denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-09-24"))
sep24denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-09-24"))
sep24denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-09-24"))
sep24denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-09-24"))
sep24denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-09-24"))
sep24denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-09-24"))
sep24denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-09-24"))
sep24denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-09-24"))
sep24denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-09-24"))

##### ALL SITES COMBINED
#dengue
ajun04denguetotal <- sum(jun04dengue$total, na.rm=T)
ajun12denguetotal <- sum(jun12dengue$total, na.rm=T)
ajun18denguetotal <- sum(jun18dengue$total, na.rm=T)
ajun25denguetotal <- sum(jun25dengue$total, na.rm=T)
ajul04denguetotal <- sum(jul04dengue$total, na.rm=T)
ajul10denguetotal <- sum(jul10dengue$total, na.rm=T)
ajul17denguetotal <- sum(jul17dengue$total, na.rm=T)
ajul24denguetotal <- sum(jul24dengue$total, na.rm=T)
ajul30denguetotal <- sum(jul30dengue$total, na.rm=T)
aaug06denguetotal <- sum(aug06dengue$total, na.rm=T)
aaug13denguetotal <- sum(aug13dengue$total, na.rm=T)
aaug20denguetotal <- sum(aug20dengue$total, na.rm=T)
asep03denguetotal <- sum(sep03dengue$total, na.rm=T)
asep10denguetotal <- sum(sep10dengue$total, na.rm=T)
asep17denguetotal <- sum(sep17dengue$total, na.rm=T)
asep24denguetotal <- sum(sep24dengue$total, na.rm=T)




##### site1
#dengue
ajun04denguesite1 <- sum(jun04denguesite1$X1, na.rm=T)
ajun12denguesite1 <- sum(jun12denguesite1$X1, na.rm=T)
ajun18denguesite1 <- sum(jun18denguesite1$X1, na.rm=T)
ajun25denguesite1 <- sum(jun25denguesite1$X1, na.rm=T)
ajul04denguesite1 <- sum(jul04denguesite1$X1, na.rm=T)
ajul10denguesite1 <- sum(jul10denguesite1$X1, na.rm=T)
ajul17denguesite1 <- sum(jul17denguesite1$X1, na.rm=T)
ajul24denguesite1 <- sum(jul24denguesite1$X1, na.rm=T)
ajul30denguesite1 <- sum(jul30denguesite1$X1, na.rm=T)
aaug06denguesite1 <- sum(aug06denguesite1$X1, na.rm=T)
aaug13denguesite1 <- sum(aug13denguesite1$X1, na.rm=T)
aaug20denguesite1 <- sum(aug20denguesite1$X1, na.rm=T)
asep03denguesite1 <- sum(sep03denguesite1$X1, na.rm=T)
asep10denguesite1 <- sum(sep10denguesite1$X1, na.rm=T)
asep17denguesite1 <- sum(sep17denguesite1$X1, na.rm=T)
asep24denguesite1 <- sum(sep24denguesite1$X1, na.rm=T)



##### site2
#dengue
ajun04denguesite2 <- sum(jun04denguesite2$X2, na.rm=T)
ajun12denguesite2 <- sum(jun12denguesite2$X2, na.rm=T)
ajun18denguesite2 <- sum(jun18denguesite2$X2, na.rm=T)
ajun25denguesite2 <- sum(jun25denguesite2$X2, na.rm=T)
ajul04denguesite2 <- sum(jul04denguesite2$X2, na.rm=T)
ajul10denguesite2 <- sum(jul10denguesite2$X2, na.rm=T)
ajul17denguesite2 <- sum(jul17denguesite2$X2, na.rm=T)
ajul24denguesite2 <- sum(jul24denguesite2$X2, na.rm=T)
ajul30denguesite2 <- sum(jul30denguesite2$X2, na.rm=T)
aaug06denguesite2 <- sum(aug06denguesite2$X2, na.rm=T)
aaug13denguesite2 <- sum(aug13denguesite2$X2, na.rm=T)
aaug20denguesite2 <- sum(aug20denguesite2$X2, na.rm=T)
asep03denguesite2 <- sum(sep03denguesite2$X2, na.rm=T)
asep10denguesite2 <- sum(sep10denguesite2$X2, na.rm=T)
asep17denguesite2 <- sum(sep17denguesite2$X2, na.rm=T)
asep24denguesite2 <- sum(sep24denguesite2$X2, na.rm=T)


##### site3
#dengue
ajun04denguesite3 <- sum(jun04denguesite3$X3, na.rm=T)
ajun12denguesite3 <- sum(jun12denguesite3$X3, na.rm=T)
ajun18denguesite3 <- sum(jun18denguesite3$X3, na.rm=T)
ajun25denguesite3 <- sum(jun25denguesite3$X3, na.rm=T)
ajul04denguesite3 <- sum(jul04denguesite3$X3, na.rm=T)
ajul10denguesite3 <- sum(jul10denguesite3$X3, na.rm=T)
ajul17denguesite3 <- sum(jul17denguesite3$X3, na.rm=T)
ajul24denguesite3 <- sum(jul24denguesite3$X3, na.rm=T)
ajul30denguesite3 <- sum(jul30denguesite3$X3, na.rm=T)
aaug06denguesite3 <- sum(aug06denguesite3$X3, na.rm=T)
aaug13denguesite3 <- sum(aug13denguesite3$X3, na.rm=T)
aaug20denguesite3 <- sum(aug20denguesite3$X3, na.rm=T)
asep03denguesite3 <- sum(sep03denguesite3$X3, na.rm=T)
asep10denguesite3 <- sum(sep10denguesite3$X3, na.rm=T)
asep17denguesite3 <- sum(sep17denguesite3$X3, na.rm=T)
asep24denguesite3 <- sum(sep24denguesite3$X3, na.rm=T)



##### site4
#dengue
ajun04denguesite4 <- sum(jun04denguesite4$X4, na.rm=T)
ajun12denguesite4 <- sum(jun12denguesite4$X4, na.rm=T)
ajun18denguesite4 <- sum(jun18denguesite4$X4, na.rm=T)
ajun25denguesite4 <- sum(jun25denguesite4$X4, na.rm=T)
ajul04denguesite4 <- sum(jul04denguesite4$X4, na.rm=T)
ajul10denguesite4 <- sum(jul10denguesite4$X4, na.rm=T)
ajul17denguesite4 <- sum(jul17denguesite4$X4, na.rm=T)
ajul24denguesite4 <- sum(jul24denguesite4$X4, na.rm=T)
ajul30denguesite4 <- sum(jul30denguesite4$X4, na.rm=T)
aaug06denguesite4 <- sum(aug06denguesite4$X4, na.rm=T)
aaug13denguesite4 <- sum(aug13denguesite4$X4, na.rm=T)
aaug20denguesite4 <- sum(aug20denguesite4$X4, na.rm=T)
asep03denguesite4 <- sum(sep03denguesite4$X4, na.rm=T)
asep10denguesite4 <- sum(sep10denguesite4$X4, na.rm=T)
asep17denguesite4 <- sum(sep17denguesite4$X4, na.rm=T)
asep24denguesite4 <- sum(sep24denguesite4$X4, na.rm=T)



##### site5
#dengue
ajun04denguesite5 <- sum(jun04denguesite5$X5, na.rm=T)
ajun12denguesite5 <- sum(jun12denguesite5$X5, na.rm=T)
ajun18denguesite5 <- sum(jun18denguesite5$X5, na.rm=T)
ajun25denguesite5 <- sum(jun25denguesite5$X5, na.rm=T)
ajul04denguesite5 <- sum(jul04denguesite5$X5, na.rm=T)
ajul10denguesite5 <- sum(jul10denguesite5$X5, na.rm=T)
ajul17denguesite5 <- sum(jul17denguesite5$X5, na.rm=T)
ajul24denguesite5 <- sum(jul24denguesite5$X5, na.rm=T)
ajul30denguesite5 <- sum(jul30denguesite5$X5, na.rm=T)
aaug06denguesite5 <- sum(aug06denguesite5$X5, na.rm=T)
aaug13denguesite5 <- sum(aug13denguesite5$X5, na.rm=T)
aaug20denguesite5 <- sum(aug20denguesite5$X5, na.rm=T)
asep03denguesite5 <- sum(sep03denguesite5$X5, na.rm=T)
asep10denguesite5 <- sum(sep10denguesite5$X5, na.rm=T)
asep17denguesite5 <- sum(sep17denguesite5$X5, na.rm=T)
asep24denguesite5 <- sum(sep24denguesite5$X5, na.rm=T)




##### site6
#dengue
jun04denguesite6$X6 <- as.numeric(jun04denguesite6$X6)
jun12denguesite6$X6 <- as.numeric(jun12denguesite6$X6)
jun18denguesite6$X6 <- as.numeric(jun18denguesite6$X6)
jun25denguesite6$X6 <- as.numeric(jun25denguesite6$X6)
jul04denguesite6$X6 <- as.numeric(jul04denguesite6$X6)
jul10denguesite6$X6 <- as.numeric(jul10denguesite6$X6)
jul17denguesite6$X6 <- as.numeric(jul17denguesite6$X6)
jul24denguesite6$X6 <- as.numeric(jul24denguesite6$X6)
jul30denguesite6$X6 <- as.numeric(jul30denguesite6$X6)
aug06denguesite6$X6 <- as.numeric(aug06denguesite6$X6)
aug13denguesite6$X6 <- as.numeric(aug13denguesite6$X6)
aug20denguesite6$X6 <- as.numeric(aug20denguesite6$X6)
sep03denguesite6$X6 <- as.numeric(sep03denguesite6$X6)
sep10denguesite6$X6 <- as.numeric(sep10denguesite6$X6)
sep17denguesite6$X6 <- as.numeric(sep17denguesite6$X6)
sep24denguesite6$X6 <- as.numeric(sep24denguesite6$X6)


ajun04denguesite6 <- sum(jun04denguesite6$X6, na.rm=T)
ajun12denguesite6 <- sum(jun12denguesite6$X6, na.rm=T)
ajun18denguesite6 <- sum(jun18denguesite6$X6, na.rm=T)
ajun25denguesite6 <- sum(jun25denguesite6$X6, na.rm=T)
ajul04denguesite6 <- sum(jul04denguesite6$X6, na.rm=T)
ajul10denguesite6 <- sum(jul10denguesite6$X6, na.rm=T)
ajul17denguesite6 <- sum(jul17denguesite6$X6, na.rm=T)
ajul24denguesite6 <- sum(jul24denguesite6$X6, na.rm=T)
ajul30denguesite6 <- sum(jul30denguesite6$X6, na.rm=T)
aaug06denguesite6 <- sum(aug06denguesite6$X6, na.rm=T)
aaug13denguesite6 <- sum(aug13denguesite6$X6, na.rm=T)
aaug20denguesite6 <- sum(aug20denguesite6$X6, na.rm=T)
asep03denguesite6 <- sum(sep03denguesite6$X6, na.rm=T)
asep10denguesite6 <- sum(sep10denguesite6$X6, na.rm=T)
asep17denguesite6 <- sum(sep17denguesite6$X6, na.rm=T)
asep24denguesite6 <- sum(sep24denguesite6$X6, na.rm=T)




##### site7
#dengue
ajun04denguesite7 <- sum(jun04denguesite7$X7, na.rm=T)
ajun12denguesite7 <- sum(jun12denguesite7$X7, na.rm=T)
ajun18denguesite7 <- sum(jun18denguesite7$X7, na.rm=T)
ajun25denguesite7 <- sum(jun25denguesite7$X7, na.rm=T)
ajul04denguesite7 <- sum(jul04denguesite7$X7, na.rm=T)
ajul10denguesite7 <- sum(jul10denguesite7$X7, na.rm=T)
ajul17denguesite7 <- sum(jul17denguesite7$X7, na.rm=T)
ajul24denguesite7 <- sum(jul24denguesite7$X7, na.rm=T)
ajul30denguesite7 <- sum(jul30denguesite7$X7, na.rm=T)
aaug06denguesite7 <- sum(aug06denguesite7$X7, na.rm=T)
aaug13denguesite7 <- sum(aug13denguesite7$X7, na.rm=T)
aaug20denguesite7 <- sum(aug20denguesite7$X7, na.rm=T)
asep03denguesite7 <- sum(sep03denguesite7$X7, na.rm=T)
asep10denguesite7 <- sum(sep10denguesite7$X7, na.rm=T)
asep17denguesite7 <- sum(sep17denguesite7$X7, na.rm=T)
asep24denguesite7 <- sum(sep24denguesite7$X7, na.rm=T)


##### site8
#dengue
ajun04denguesite8 <- sum(jun04denguesite8$X8, na.rm=T)
ajun12denguesite8 <- sum(jun12denguesite8$X8, na.rm=T)
ajun18denguesite8 <- sum(jun18denguesite8$X8, na.rm=T)
ajun25denguesite8 <- sum(jun25denguesite8$X8, na.rm=T)
ajul04denguesite8 <- sum(jul04denguesite8$X8, na.rm=T)
ajul10denguesite8 <- sum(jul10denguesite8$X8, na.rm=T)
ajul17denguesite8 <- sum(jul17denguesite8$X8, na.rm=T)
ajul24denguesite8 <- sum(jul24denguesite8$X8, na.rm=T)
ajul30denguesite8 <- sum(jul30denguesite8$X8, na.rm=T)
aaug06denguesite8 <- sum(aug06denguesite8$X8, na.rm=T)
aaug13denguesite8 <- sum(aug13denguesite8$X8, na.rm=T)
aaug20denguesite8 <- sum(aug20denguesite8$X8, na.rm=T)
asep03denguesite8 <- sum(sep03denguesite8$X8, na.rm=T)
asep10denguesite8 <- sum(sep10denguesite8$X8, na.rm=T)
asep17denguesite8 <- sum(sep17denguesite8$X8, na.rm=T)
asep24denguesite8 <- sum(sep24denguesite8$X8, na.rm=T)



##### site9
#dengue

jun04denguesite9$X9 <- as.numeric(jun04denguesite9$X9)
jun12denguesite9$X9 <- as.numeric(jun12denguesite9$X9)
jun18denguesite9$X9 <- as.numeric(jun18denguesite9$X9)
jun25denguesite9$X9 <- as.numeric(jun25denguesite9$X9)
jul04denguesite9$X9 <- as.numeric(jul04denguesite9$X9)
jul10denguesite9$X9 <- as.numeric(jul10denguesite9$X9)
jul17denguesite9$X9 <- as.numeric(jul17denguesite9$X9)
jul24denguesite9$X9 <- as.numeric(jul24denguesite9$X9)
jul30denguesite9$X9 <- as.numeric(jul30denguesite9$X9)
aug06denguesite9$X9 <- as.numeric(aug06denguesite9$X9)
aug13denguesite9$X9 <- as.numeric(aug13denguesite9$X9)
aug20denguesite9$X9 <- as.numeric(aug20denguesite9$X9)
sep03denguesite9$X9 <- as.numeric(sep03denguesite9$X9)
sep10denguesite9$X9 <- as.numeric(sep10denguesite9$X9)
sep17denguesite9$X9 <- as.numeric(sep17denguesite9$X9)
sep24denguesite9$X9 <- as.numeric(sep24denguesite9$X9)


ajun04denguesite9 <- sum(jun04denguesite9$X9, na.rm=T)
ajun12denguesite9 <- sum(jun12denguesite9$X9, na.rm=T)
ajun18denguesite9 <- sum(jun18denguesite9$X9, na.rm=T)
ajun25denguesite9 <- sum(jun25denguesite9$X9, na.rm=T)
ajul04denguesite9 <- sum(jul04denguesite9$X9, na.rm=T)
ajul10denguesite9 <- sum(jul10denguesite9$X9, na.rm=T)
ajul17denguesite9 <- sum(jul17denguesite9$X9, na.rm=T)
ajul24denguesite9 <- sum(jul24denguesite9$X9, na.rm=T)
ajul30denguesite9 <- sum(jul30denguesite9$X9, na.rm=T)
aaug06denguesite9 <- sum(aug06denguesite9$X9, na.rm=T)
aaug13denguesite9 <- sum(aug13denguesite9$X9, na.rm=T)
aaug20denguesite9 <- sum(aug20denguesite9$X9, na.rm=T)
asep03denguesite9 <- sum(sep03denguesite9$X9, na.rm=T)
asep10denguesite9 <- sum(sep10denguesite9$X9, na.rm=T)
asep17denguesite9 <- sum(sep17denguesite9$X9, na.rm=T)
asep24denguesite9 <- sum(sep24denguesite9$X9, na.rm=T)



##### site10
#dengue
jun04denguesite10$X10 <- as.numeric(jun04denguesite10$X10)
jun12denguesite10$X10 <- as.numeric(jun12denguesite10$X10)
jun18denguesite10$X10 <- as.numeric(jun18denguesite10$X10)
jun25denguesite10$X10 <- as.numeric(jun25denguesite10$X10)
jul04denguesite10$X10 <- as.numeric(jul04denguesite10$X10)
jul10denguesite10$X10 <- as.numeric(jul10denguesite10$X10)
jul17denguesite10$X10 <- as.numeric(jul17denguesite10$X10)
jul24denguesite10$X10 <- as.numeric(jul24denguesite10$X10)
jul30denguesite10$X10 <- as.numeric(jul30denguesite10$X10)
aug06denguesite10$X10 <- as.numeric(aug06denguesite10$X10)
aug13denguesite10$X10 <- as.numeric(aug13denguesite10$X10)
aug20denguesite10$X10 <- as.numeric(aug20denguesite10$X10)
sep03denguesite10$X10 <- as.numeric(sep03denguesite10$X10)
sep10denguesite10$X10 <- as.numeric(sep10denguesite10$X10)
sep17denguesite10$X10 <- as.numeric(sep17denguesite10$X10)
sep24denguesite10$X10 <- as.numeric(sep24denguesite10$X10)


ajun04denguesite10 <- sum(jun04denguesite10$X10, na.rm=T)
ajun12denguesite10 <- sum(jun12denguesite10$X10, na.rm=T)
ajun18denguesite10 <- sum(jun18denguesite10$X10, na.rm=T)
ajun25denguesite10 <- sum(jun25denguesite10$X10, na.rm=T)
ajul04denguesite10 <- sum(jul04denguesite10$X10, na.rm=T)
ajul10denguesite10 <- sum(jul10denguesite10$X10, na.rm=T)
ajul17denguesite10 <- sum(jul17denguesite10$X10, na.rm=T)
ajul24denguesite10 <- sum(jul24denguesite10$X10, na.rm=T)
ajul30denguesite10 <- sum(jul30denguesite10$X10, na.rm=T)
aaug06denguesite10 <- sum(aug06denguesite10$X10, na.rm=T)
aaug13denguesite10 <- sum(aug13denguesite10$X10, na.rm=T)
aaug20denguesite10 <- sum(aug20denguesite10$X10, na.rm=T)
asep03denguesite10 <- sum(sep03denguesite10$X10, na.rm=T)
asep10denguesite10 <- sum(sep10denguesite10$X10, na.rm=T)
asep17denguesite10 <- sum(sep17denguesite10$X10, na.rm=T)
asep24denguesite10 <- sum(sep24denguesite10$X10, na.rm=T)


##
#
#
#
#
#



#CREATE dengueS FOR PLOTTING
#Site 1
dengue1 <- c(ajun04denguesite1,
             ajun12denguesite1,
             ajun18denguesite1,
             ajun25denguesite1,
             ajul04denguesite1,
             ajul10denguesite1,
             ajul17denguesite1,
             ajul24denguesite1,
             ajul30denguesite1,
             aaug06denguesite1,
             aaug13denguesite1,
             aaug20denguesite1,
             asep03denguesite1,
             asep10denguesite1,
             asep17denguesite1,
             asep24denguesite1)

#Site 2
dengue2 <- c(ajun04denguesite2,
             ajun12denguesite2,
             ajun18denguesite2,
             ajun25denguesite2,
             ajul04denguesite2,
             ajul10denguesite2,
             ajul17denguesite2,
             ajul24denguesite2,
             ajul30denguesite2,
             aaug06denguesite2,
             aaug13denguesite2,
             aaug20denguesite2,
             asep03denguesite2,
             asep10denguesite2,
             asep17denguesite2,
             asep24denguesite2)


#Site 3
dengue3 <- c(ajun04denguesite3,
             ajun12denguesite3,
             ajun18denguesite3,
             ajun25denguesite3,
             ajul04denguesite3,
             ajul10denguesite3,
             ajul17denguesite3,
             ajul24denguesite3,
             ajul30denguesite3,
             aaug06denguesite3,
             aaug13denguesite3,
             aaug20denguesite3,
             asep03denguesite3,
             asep10denguesite3,
             asep17denguesite3,
             asep24denguesite3)


#Site 4
dengue4 <- c(ajun04denguesite4,
             ajun12denguesite4,
             ajun18denguesite4,
             ajun25denguesite4,
             ajul04denguesite4,
             ajul10denguesite4,
             ajul17denguesite4,
             ajul24denguesite4,
             ajul30denguesite4,
             aaug06denguesite4,
             aaug13denguesite4,
             aaug20denguesite4,
             asep03denguesite4,
             asep10denguesite4,
             asep17denguesite4,
             asep24denguesite4)


#Site 5
dengue5 <- c(ajun04denguesite5,
             ajun12denguesite5,
             ajun18denguesite5,
             ajun25denguesite5,
             ajul04denguesite5,
             ajul10denguesite5,
             ajul17denguesite5,
             ajul24denguesite5,
             ajul30denguesite5,
             aaug06denguesite5,
             aaug13denguesite5,
             aaug20denguesite5,
             asep03denguesite5,
             asep10denguesite5,
             asep17denguesite5,
             asep24denguesite5)


#Site 6
dengue6 <- c(ajun04denguesite6,
             ajun12denguesite6,
             ajun18denguesite6,
             ajun25denguesite6,
             ajul04denguesite6,
             ajul10denguesite6,
             ajul17denguesite6,
             ajul24denguesite6,
             ajul30denguesite6,
             aaug06denguesite6,
             aaug13denguesite6,
             aaug20denguesite6,
             asep03denguesite6,
             asep10denguesite6,
             asep17denguesite6,
             asep24denguesite6)


#Site 7
dengue7 <- c(ajun04denguesite7,
             ajun12denguesite7,
             ajun18denguesite7,
             ajun25denguesite7,
             ajul04denguesite7,
             ajul10denguesite7,
             ajul17denguesite7,
             ajul24denguesite7,
             ajul30denguesite7,
             aaug06denguesite7,
             aaug13denguesite7,
             aaug20denguesite7,
             asep03denguesite7,
             asep10denguesite7,
             asep17denguesite7,
             asep24denguesite7)


#Site 8
dengue8 <- c(ajun04denguesite8,
             ajun12denguesite8,
             ajun18denguesite8,
             ajun25denguesite8,
             ajul04denguesite8,
             ajul10denguesite8,
             ajul17denguesite8,
             ajul24denguesite8,
             ajul30denguesite8,
             aaug06denguesite8,
             aaug13denguesite8,
             aaug20denguesite8,
             asep03denguesite8,
             asep10denguesite8,
             asep17denguesite8,
             asep24denguesite8)


#Site 9
dengue9 <- c(ajun04denguesite9,
             ajun12denguesite9,
             ajun18denguesite9,
             ajun25denguesite9,
             ajul04denguesite9,
             ajul10denguesite9,
             ajul17denguesite9,
             ajul24denguesite9,
             ajul30denguesite9,
             aaug06denguesite9,
             aaug13denguesite9,
             aaug20denguesite9,
             asep03denguesite9,
             asep10denguesite9,
             asep17denguesite9,
             asep24denguesite9)


#Site 10
dengue10 <- c(ajun04denguesite10,
              ajun12denguesite10,
              ajun18denguesite10,
              ajun25denguesite10,
              ajul04denguesite10,
              ajul10denguesite10,
              ajul17denguesite10,
              ajul24denguesite10,
              ajul30denguesite10,
              aaug06denguesite10,
              aaug13denguesite10,
              aaug20denguesite10,
              asep03denguesite10,
              asep10denguesite10,
              asep17denguesite10,
              asep24denguesite10)



denguetotal <- dengue1+dengue2+dengue3+dengue4+dengue5+dengue6+dengue7+dengue8+dengue9+dengue10

#




###
#MAPPING SECTION - DATA PREPARATION

#CREATE mean VECTOR NUMBERS PER SITE (FOR COMPARISON)
vectorbl <- c(mean(vec1),
              mean(vec2),
              mean(vec3),
              mean(vec4),
              mean(vec5),
              mean(vec6),
              mean(vec7),
              mean(vec8),
              mean(vec9),
              mean(vec10))
#DO THE SAME FOR ALL SIX DISEASES
wnvbl <- c(mean(wnv1),
           mean(wnv2),
           mean(wnv3),
           mean(wnv4),
           mean(wnv5),
           mean(wnv6),
           mean(wnv7),
           mean(wnv8),
           mean(wnv9),
           mean(wnv10))
slevbl <- c(mean(slev1),
            mean(slev2),
            mean(slev3),
            mean(slev4),
            mean(slev5),
            mean(slev6),
            mean(slev7),
            mean(slev8),
            mean(slev9),
            mean(slev10))
eeevbl <- c(mean(eeev1),
            mean(eeev2),
            mean(eeev3),
            mean(eeev4),
            mean(eeev5),
            mean(eeev6),
            mean(eeev7),
            mean(eeev8),
            mean(eeev9),
            mean(eeev10))
chikbl <- c(mean(chik1),
            mean(chik2),
            mean(chik3),
            mean(chik4),
            mean(chik5),
            mean(chik6),
            mean(chik7),
            mean(chik8),
            mean(chik9),
            mean(chik10))
malariabl <- c(mean(malaria1),
               mean(malaria2),
               mean(malaria3),
               mean(malaria4),
               mean(malaria5),
               mean(malaria6),
               mean(malaria7),
               mean(malaria8),
               mean(malaria9),
               mean(malaria10))
denguebl <- c(mean(dengue1),
              mean(dengue2),
              mean(dengue3),
              mean(dengue4),
              mean(dengue5),
              mean(dengue6),
              mean(dengue7),
              mean(dengue8),
              mean(dengue9),
              mean(dengue10))


#CREATE A VECTOR OF THE NUMBER OF VECTORS PER SITE PER DATE
#THIS WILL BE A 10 number series, going in order of site
#Note that the number in brackets corresponds to the DATE, not the trap number
#The vec# is the trap number
cexvectorjun04 <- c(vec1[1],
                    vec2[1],
                    vec3[1],
                    vec4[1],
                    vec5[1],
                    vec6[1],
                    vec7[1],
                    vec8[1],
                    vec9[1],
                    vec10[1])

cexvectorjun12 <- c(vec1[2],
                    vec2[2],
                    vec3[2],
                    vec4[2],
                    vec5[2],
                    vec6[2],
                    vec7[2],
                    vec8[2],
                    vec9[2],
                    vec10[2])

cexvectorjun18 <- c(vec1[3],
                    vec2[3],
                    vec3[3],
                    vec4[3],
                    vec5[3],
                    vec6[3],
                    vec7[3],
                    vec8[3],
                    vec9[3],
                    vec10[3])

cexvectorjun25 <- c(vec1[4],
                    vec2[4],
                    vec3[4],
                    vec4[4],
                    vec5[4],
                    vec6[4],
                    vec7[4],
                    vec8[4],
                    vec9[4],
                    vec10[4])

cexvectorjul04 <- c(vec1[5],
                    vec2[5],
                    vec3[5],
                    vec4[5],
                    vec5[5],
                    vec6[5],
                    vec7[5],
                    vec8[5],
                    vec9[5],
                    vec10[5])

cexvectorjul10 <- c(vec1[6],
                    vec2[6],
                    vec3[6],
                    vec4[6],
                    vec5[6],
                    vec6[6],
                    vec7[6],
                    vec8[6],
                    vec9[6],
                    vec10[6])

cexvectorjul17 <- c(vec1[7],
                    vec2[7],
                    vec3[7],
                    vec4[7],
                    vec5[7],
                    vec6[7],
                    vec7[7],
                    vec8[7],
                    vec9[7],
                    vec10[7])

cexvectorjul24 <- c(vec1[8],
                    vec2[8],
                    vec3[8],
                    vec4[8],
                    vec5[8],
                    vec6[8],
                    vec7[8],
                    vec8[8],
                    vec9[8],
                    vec10[8])

cexvectorjul30 <- c(vec1[9],
                    vec2[9],
                    vec3[9],
                    vec4[9],
                    vec5[9],
                    vec6[9],
                    vec7[9],
                    vec8[9],
                    vec9[9],
                    vec10[9])

cexvectoraug06 <- c(vec1[10],
                    vec2[10],
                    vec3[10],
                    vec4[10],
                    vec5[10],
                    vec6[10],
                    vec7[10],
                    vec8[10],
                    vec9[10],
                    vec10[10])

cexvectoraug13 <- c(vec1[11],
                    vec2[11],
                    vec3[11],
                    vec4[11],
                    vec5[11],
                    vec6[11],
                    vec7[11],
                    vec8[11],
                    vec9[11],
                    vec10[11])

cexvectoraug20 <- c(vec1[12],
                    vec2[12],
                    vec3[12],
                    vec4[12],
                    vec5[12],
                    vec6[12],
                    vec7[12],
                    vec8[12],
                    vec9[12],
                    vec10[12])

cexvectorsep03 <- c(vec1[13],
                    vec2[13],
                    vec3[13],
                    vec4[13],
                    vec5[13],
                    vec6[13],
                    vec7[13],
                    vec8[13],
                    vec9[13],
                    vec10[13])

cexvectorsep10 <- c(vec1[14],
                    vec2[14],
                    vec3[14],
                    vec4[14],
                    vec5[14],
                    vec6[14],
                    vec7[14],
                    vec8[14],
                    vec9[14],
                    vec10[14])

cexvectorsep17 <- c(vec1[15],
                    vec2[15],
                    vec3[15],
                    vec4[15],
                    vec5[15],
                    vec6[15],
                    vec7[15],
                    vec8[15],
                    vec9[15],
                    vec10[15])

cexvectorsep24 <- c(vec1[16],
                    vec2[16],
                    vec3[16],
                    vec4[16],
                    vec5[16],
                    vec6[16],
                    vec7[16],
                    vec8[16],
                    vec9[16],
                    vec10[16])

#
cexwnvsep24 <- c(wnv1[16],
                 wnv2[16],
                 wnv3[16],
                 wnv4[16],
                 wnv5[16],
                 wnv6[16],
                 wnv7[16],
                 wnv8[16],
                 wnv9[16],
                 wnv10[16])

cexslevsep24 <- c(slev1[16],
                  slev2[16],
                  slev3[16],
                  slev4[16],
                  slev5[16],
                  slev6[16],
                  slev7[16],
                  slev8[16],
                  slev9[16],
                  slev10[16])

cexeeevsep24 <- c(eeev1[16],
                  eeev2[16],
                  eeev3[16],
                  eeev4[16],
                  eeev5[16],
                  eeev6[16],
                  eeev7[16],
                  eeev8[16],
                  eeev9[16],
                  eeev10[16])

cexchiksep24 <- c(chik1[16],
                  chik2[16],
                  chik3[16],
                  chik4[16],
                  chik5[16],
                  chik6[16],
                  chik7[16],
                  chik8[16],
                  chik9[16],
                  chik10[16])

cexmalariasep24 <- c(malaria1[16],
                     malaria2[16],
                     malaria3[16],
                     malaria4[16],
                     malaria5[16],
                     malaria6[16],
                     malaria7[16],
                     malaria8[16],
                     malaria9[16],
                     malaria10[16])

cexdenguesep24 <- c(dengue1[16],
                    dengue2[16],
                    dengue3[16],
                    dengue4[16],
                    dengue5[16],
                    dengue6[16],
                    dengue7[16],
                    dengue8[16],
                    dengue9[16],
                    dengue10[16])






#For Plugging into the OVERALL simple spreadsheet


sep10totalpertrap <- sum(sep10$total)/10
sep10totalpertrap
sep10vectorpertrap <- sum(sep10vector$total)/10
sep10vectorpertrap

sep17totalpertrap <- sum(sep17$total)/10
sep17totalpertrap
sep17vectorpertrap <- sum(sep17vector$total)/10
sep17vectorpertrap


sep24totalpertrap <- sum(sep24$total)/10
sep24totalpertrap
sep24vectorpertrap <- sum(sep24vector$total)/10
sep24vectorpertrap

#





#PLOT######





jour <- c(1:16) #Change this in function of the number of total collections...
jour2 <-jour*2
jourlab <- c ("6/4", "6/12", "6/18", "6/25", "7/4",
              "7/10", "7/17", "7/24", "7/30", "8/6", "8/13", "8/20", "9/3",
              "9/10","9/17","9/24")
vectorstotal <- vec1+vec2+vec3+vec4+vec5+vec6+vec7+vec8+vec9+vec10

mosquitoestotal <- vec1+vec2+vec3+vec4+vec5+vec6+vec7+vec8+vec9+vec10+
  nonvec1+nonvec2+nonvec3+nonvec4+nonvec5+nonvec6+nonvec7+nonvec8+nonvec9+nonvec10

par(mfrow=c(5,2))
par(mar=c(2.2,2.2,1,1))



 
#SITE 1
library(splines)
plot(jour, jour2, type="n",
     main="Transfer Station NE 63rd Ave.",
     ylim=c(0,max(vec1)+100),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec1, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec1, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec1), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 2
library(splines)
plot(jour, jour, type="n",
     main="Monteocha Park",
     ylim=c(0,max(vec2)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec2, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec2, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec2), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)


#SITE 3
library(splines)
plot(jour, jour, type="n",
     main="CR 1491 High Springs",
     ylim=c(0,max(vec3)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec3, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec3, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec3), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 4
library(splines)
plot(jour, jour, type="n",
     main="Laperriere Home (High Springs)",
     ylim=c(0,max(vec4)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec4, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec4, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec4), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 5
library(splines)
plot(jour, jour, type="n",
     main="Woodlands Subdivision (Tower Rd.)",
     ylim=c(0,max(vec5)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec5, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec5, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec5), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 6
library(splines)
plot(jour, jour, type="n",
     main="SE 171 St. (Hawthorne)",
     ylim=c(0,max(vec6)+250),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec6, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec6, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec6), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 7
library(splines)
plot(jour, jour, type="n",
     main="Bowman Residence Archer",
     ylim=c(0,max(vec7)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec7, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec7, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec7), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 8
library(splines)
plot(jour, jour, type="n",
     main="Micanopy",
     ylim=c(0,max(nonvec8)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec8, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec8, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec8), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#SITE 9
library(splines)
plot(jour, jour, type="n",
     main="Hawthorne Chix Site",
     ylim=c(0,max(vec9)+150),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec9, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec9, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec9), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)



#SITE 10
library(splines)
plot(jour, jour, type="n",
     main="UF AES NW 71st St.",
     ylim=c(0,max(vec10)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=1,
       legend=c("Vectors", "Non-vectors"),
       lty=c(1,3),
       col=c("darkred","dodgerblue3"), bor=T, bty="n", cex=0.6)
xspline(jour, vec10, shape=0.2,
        border="darkred", lty=1, lwd=2)
xspline(jour, nonvec10, shape=0.2,
        border="dodgerblue3", lty=3, lwd=2)
abline(h=mean(vec10), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=10)
legend(x="topright", ncol=1,
       legend=c("Average"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#OVERALL VECTORS


library(splines)
plot(jour, jour, type="n",
     main="VECTORS (all locations)",
     ylim=c(0,max(vectorstotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="Vectors",
       lty=1,
       col="darkred", bor=T, bty="n", cex=0.6)
xspline(jour, vectorstotal, shape=0.2,
        border="darkred", lty=1, lwd=3)
abline(h=mean(vectorstotal), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=5)
legend(x="bottomright", ncol=1,
       legend=c("Normal number of\nvectors overall"),
       lty=1,
       lwd=3,
       col=adjustcolor("darkred", alpha.f=0.3), bor=T, bty="n", cex=0.6)

#OVERALL ALL MOSQUITOES

plot(jour, jour, type="n",
     main="All Mosquitoes, All Locations",
     ylim=c(0,max(mosquitoestotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="Non-Vectors",
       lty=1,
       col="dodgerblue3", bor=T, bty="n", cex=0.6)
xspline(jour, mosquitoestotal, shape=0.2,
        border="dodgerblue3", lty=1, lwd=3)
abline(h=mean(mosquitoestotal), col=adjustcolor("dodgerblue3", alpha.f=0.3),
       lwd=5)
legend(x="bottomright", ncol=1,
       legend=c("Normal number of\nnon-vectors for this trap"),
       lty=1,
       lwd=3,
       col=adjustcolor("dodgerblue3", alpha.f=0.3), bor=T, bty="n", cex=0.6)


####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####
####



## DISEASE SPECIFIC PLOTS
#

####
####
####
####
####
####
####
####
####


par(mfrow=c(5,2))
discols <- colorRampPalette(brewer.pal(6, "Set1"))(6)
discols[6] <- "darkgrey"
disnames <- c("WNV", "SLEV", "EEEV", "Chik.", "Malaria", "Dengue")
#SITE 1
library(splines)
plot(jour, jour2, type="n",
     main="Transfer Station NE 63rd Ave.",
     ylim=c(0,max(vec1)+100),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv1, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev1, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev1, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik1, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria1, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue1, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)



#SITE 2
library(splines)
plot(jour, jour, type="n",
     main="Monteocha Park",
     ylim=c(0,max(vec2)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv2, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev2, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev2, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik2, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria2, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue2, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 3
library(splines)
plot(jour, jour, type="n",
     main="CR 1491 High Springs",
     ylim=c(0,max(vec3)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv3, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev3, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev3, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik3, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria3, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue3, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 4
library(splines)
plot(jour, jour, type="n",
     main="Laperriere Home (High Springs)",
     ylim=c(0,max(vec4)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv4, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev4, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev4, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik4, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria4, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue4, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 5
library(splines)
plot(jour, jour, type="n",
     main="Woodlands Subdivision (Tower Rd.)",
     ylim=c(0,max(vec5)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv5, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev5, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev5, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik5, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria5, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue5, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 6
library(splines)
plot(jour, jour, type="n",
     main="SE 171 St. (Hawthorne)",
     ylim=c(0,max(vec6)+250),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv6, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev6, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev6, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik6, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria6, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue6, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 7
library(splines)
plot(jour, jour, type="n",
     main="Bowman Residence Archer",
     ylim=c(0,max(vec7)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv7, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev7, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev7, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik7, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria7, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue7, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 8
library(splines)
plot(jour, jour, type="n",
     main="Micanopy",
     ylim=c(0,max(nonvec8)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv8, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev8, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev8, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik8, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria8, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue8, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 9
library(splines)
plot(jour, jour, type="n",
     main="Hawthorne Chix Site",
     ylim=c(0,max(vec9)+150),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv9, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev9, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev9, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik9, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria9, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue9, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)


#SITE 10
library(splines)
plot(jour, jour, type="n",
     main="UF AES NW 71st St.",
     ylim=c(0,max(vec10)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft", ncol=3,
       legend=disnames,
       lty=1,
       col=adjustcolor(discols, alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnv10, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, slev10, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, eeev10, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, chik10, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, malaria10, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=2)
xspline(jour, dengue10, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=2)



####DISEASE TOTALS
par(mfrow=c(3,1))

#PLOT 11 - WNV TOTAL
library(splines)
plot(jour, jour, type="n",
     main="WNV (all locations)",
     ylim=c(0,max(wnvtotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="WNV",
       lty=1,
       col=adjustcolor(discols[1], alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, wnvtotal, shape=0.2,
        border=adjustcolor(discols[1], alpha.f=0.9), lty=1, lwd=3)
abline(h=mean(wnvtotal), col=adjustcolor(discols[1], alpha.f=0.3),
       lwd=15)

#PLOT 12 - SLEV TOTAL
library(splines)
plot(jour, jour, type="n",
     main="slev (all locations)",
     ylim=c(0,max(slevtotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="slev",
       lty=1,
       col=adjustcolor(discols[2], alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, slevtotal, shape=0.2,
        border=adjustcolor(discols[2], alpha.f=0.9), lty=1, lwd=3)
abline(h=mean(slevtotal), col=adjustcolor(discols[2], alpha.f=0.3),
       lwd=15)

#PLOT 13 - EEEV TOTAL
library(splines)
plot(jour, jour, type="n",
     main="eeev (all locations)",
     ylim=c(0,max(eeevtotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="eeev",
       lty=1,
       col=adjustcolor(discols[3], alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, eeevtotal, shape=0.2,
        border=adjustcolor(discols[3], alpha.f=0.9), lty=1, lwd=3)
abline(h=mean(eeevtotal), col=adjustcolor(discols[3], alpha.f=0.3),
       lwd=15)

#PLOT 14 - CHIK TOTAL
library(splines)
plot(jour, jour, type="n",
     main="chik (all locations)",
     ylim=c(0,max(chiktotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="chik",
       lty=1,
       col=adjustcolor(discols[4], alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, chiktotal, shape=0.2,
        border=adjustcolor(discols[4], alpha.f=0.9), lty=1, lwd=3)
abline(h=mean(chiktotal), col=adjustcolor(discols[4], alpha.f=0.3),
       lwd=15)

#PLOT 15 - MALARIA TOTAL
library(splines)
plot(jour, jour, type="n",
     main="malaria (all locations)",
     ylim=c(0,max(malariatotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="malaria",
       lty=1,
       col=adjustcolor(discols[5], alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, malariatotal, shape=0.2,
        border=adjustcolor(discols[5], alpha.f=0.9), lty=1, lwd=3)
abline(h=mean(malariatotal), col=adjustcolor(discols[5], alpha.f=0.3),
       lwd=15)

#PLOT 16 - DENGUE TOTAL
library(splines)
plot(jour, jour, type="n",
     main="dengue (all locations)",
     ylim=c(0,max(denguetotal)+20),
     cex.main=1,
     xlab="Date",
     ylab="Mosquitoes captured",
     cex.lab=0.75,
     cex.axis=0.75,
     xaxt="n")
axis(1, jour, labels=jourlab, las=1, cex.axis=0.7)
legend(x="topleft",
       legend="dengue",
       lty=1,
       col=adjustcolor(discols[6], alpha.f=0.9), bor=T, bty="n", cex=0.6)
xspline(jour, denguetotal, shape=0.2,
        border=adjustcolor(discols[6], alpha.f=0.9), lty=1, lwd=3)
abline(h=mean(denguetotal), col=adjustcolor(discols[6], alpha.f=0.3),
       lwd=15)


par(mfrow=c(1,1))




#
#


###


#
#

####BEGIN MAPPING
library(maps)
library(mapdata)
library(maptools)
library(RColorBrewer)
display.brewer.all()

#READ TRAP SITES
setwd("C:/Users/BrewJR/Documents/workingdirectory/mosquito")
traplatlong <- read.csv("traplatlong.csv", header=TRUE, sep=",")


#TRAP COLLECTION SITES

#Make colors
countynames <- map("county", "florida", names=TRUE, plot=FALSE)
countynames
countycols <- colorRampPalette(brewer.pal(9, "Spectral"))(68)
countycols2 <- colorRampPalette(brewer.pal(5, "Pastel2"))(67)
countycols3 <- rep(c("grey30", "grey40", "grey50", "grey10"),30)

alachuacol <- adjustcolor("beige", alpha.f=0.2)

#Create a names vector for the labelling
names <- traplatlong$name
names <- as.character(names)
names[4] <- NA
names[9] <- "Hawthorne"
names[6] <- NA
names[3] <- "High Springs"
names[7] <- "Archer"



library(rJava)
library(OpenStreetMap)
library(rgdal)
#mapsat <- openmap(c(30.05, -82.8), c(29.3,-81.9), type="bing")
#plot(mapsat)

#mapcit <- openmap(c(30.05, -82.8), c(29.3,-81.9))
#plot(mapcit)
#CONVERT TO LAT LONG PROJECTION (Raster?)
#mapsatll <- openproj(mapsat, projection = "+proj=longlat")
#plot(mapsatll, raster=TRUE)

###### MAKE A SERIES OF MAPS SHOWING THE AMOUNT OF MOSQUITOES PER SITE
par(mfrow=c(2,2))
par(mar=c(0,0,1,0))
#jun04
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjun04/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("June 4th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jun12
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjun12/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("June 12th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jun18
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjun18/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Jun 18th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jun25
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjun25/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Jun 25th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jul04
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjul04/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("July 4th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jul10
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjul10/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("July 10th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jul17
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjul17/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("July 17th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jul24
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjul24/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("July 24th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#jul30
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorjul30/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("July 30th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#aug06
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectoraug06/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("August 6th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#aug13
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectoraug13/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("August 13th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#aug20
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectoraug20/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("August 20th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#sep03
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorsep03/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("September 3rd")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#sep10
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorsep10/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("September 10th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#sep17
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorsep17/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("September 17th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)

#sep24
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols2),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorsep24/vectorbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("September 24th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)


#DISEASE SPECIFIC MAPS
#DISEASE SPECIFIC MAPS
#DISEASE SPECIFIC MAPS

par(mfrow=c(2,3))
#sep24 - WNV
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor(discols[1], alpha.f=0.5),
       cex=1.2*(cexwnvsep24/wnvbl))
points(-82.392093, 29.809798, cex=2, col=adjustcolor("blue", alpha.f=0.5), pch=17)
text(-82.392093, 29.78, labels="Patient's \n Residence", cex=0.5)

text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("West Nile vectors, September 24th")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor(discols[1], alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n")
#sep24 - slev
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor(discols[2], alpha.f=0.5),
       cex=1.2*(cexslevsep24/slevbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("St. Louis Encephalitis vectors")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor(discols[2], alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)
#sep24 - eeev
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor(discols[3], alpha.f=0.5),
       cex=1.2*(cexeeevsep24/eeevbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Eastern Equine Encephalitis vectors")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor(discols[3], alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)
#sep24 - chik
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor(discols[4], alpha.f=0.5),
       cex=1.2*(cexchiksep24/chikbl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Chikungunya vectors")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor(discols[4], alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)
#sep24 - malaria
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor(discols[5], alpha.f=0.5),
       cex=1.2*(cexmalariasep24/malariabl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Malaria vectors")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor(discols[5], alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)
#sep24 - dengue
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor(discols[6], alpha.f=0.5),
       cex=1.2*(cexdenguesep24/denguebl))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Dengue vectors")
legend(-82.7,29.46,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor(discols[6], alpha.f=0.5),
       pt.cex=c(0.6,1.2,2.4),
       cex=0.6,
       x.intersp=1.3, y.intersp=1,
       bor=T,bty="n",)
par(mfrow=c(1,1))

#

##### 

#### PRETTIER MAPS

###WATER COLOR
joewatercolor <- openmap(c(29.9, -82.65), c(29.4,-82.0),
                         type="stamen-watercolor")
joemapwatercolor <- openproj(joewatercolor, projection = "+proj=longlat")
plot(joemapwatercolor, raster=TRUE)
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=0.6+(2*(cexvectorsep17/vectorbl)))
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.9,
     #adj=c(0.5,2),
     las=3)
legend(-82.1,29.49,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=19, 
       col=adjustcolor("red", alpha.f=0.5),
       pt.cex=c(1.6,2.6,4.6),
       cex=0.6,
       x.intersp=2, y.intersp=1,
       bor=T,bty="n",)



#TONER
joetoner <- openmap(c(30.05, -82.8), c(29.3,-81.9),
                    type="stamen-toner")
joemaptoner <- openproj(joetoner, projection = "+proj=longlat")
plot(joemaptoner, raster=TRUE)
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=1.2*(cexvectorsep17/vectorbl))

#TRAP SITES IN ALACHUA COUNTY
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9),
    ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("dodgerblue3", alpha.f=0.5),
       cex=4)
text(traplatlong$long, traplatlong$lat, 
     labels=names,
     cex=0.4,
     adj=c(0.5,2),
     las=3)
title("Trap sites in Alachua County")

