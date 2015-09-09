#SET COLOR PALETTE
library(RColorBrewer)
#FOR SYNDROMS
symcols <- colorRampPalette(brewer.pal(8, "Set1"))(8)
#FOR MAP
#Create a vector for county colors
blackcols <- c("gray90",rep("gray75", 67))
symcols[6] <- "darkred"
###################################################

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
abline(v=243, lty=6, col="black")

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


############################################################

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


#delete the code row
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


#DELETE THE EXTRANEOUS ROWS (dfs that have more than 33)
jul24 <- jul24[-c(34,35),]
jul30 <- jul30[-c(34,35),]
aug06 <- aug06[-c(34,35),]
aug20 <- aug20[-c(34:90),]
sep03 <- sep03[-c(34:90),]


#DELETE A FEW EXTRANEOUS COLUMNS AS WELL
aug20 <- aug20[,-c(13:21)]


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
                sep03)


#Rename the "site" column to be "mosq" instead
merged$mosq <- merged$site
merged$site <- NULL

#Create a vector column
library(car)
merged$vector <- merged$mosq

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


#DATE STUFF
merged$date <- as.Date(merged$date, "%Y-%m-%d" )
merged$date2 <- as.Date(as.character(merged$date), "%Y-%m-%d")

#SUBSET THE DATA : VECTOR / NONVECTOR
mergedvector <- merged[which(merged$vector=="vector"),]
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
                "2013-09-03")

datevector <- as.Date(datevector)

#SUBSET THE DATA FOR THE DIFFERENT COLLECTION SITES (1:10)
names(merged)
merged1 <- merged[,c(1,12,13,14,15)]
merged2 <- merged[,c(2,12,13,14,15)]
merged3 <- merged[,c(3,12,13,14,15)]
merged4 <- merged[,c(4,12,13,14,15)]
merged5 <- merged[,c(5,12,13,14,15)]
merged6 <- merged[,c(6,12,13,14,15)]
merged7 <- merged[,c(7,12,13,14,15)]
merged8 <- merged[,c(8,12,13,14,15)]
merged9 <- merged[,c(9,12,13,14,15)]
merged10 <- merged[,c(11,12,13,14,15)]

#Double subset for vector and non vector by site
#VECTOR
merged1vector <- mergedvector[,c(1,12,13,14,15)]
merged2vector <- mergedvector[,c(2,12,13,14,15)]
merged3vector <- mergedvector[,c(3,12,13,14,15)]
merged4vector <- mergedvector[,c(4,12,13,14,15)]
merged5vector <- mergedvector[,c(5,12,13,14,15)]
merged6vector <- mergedvector[,c(6,12,13,14,15)]
merged7vector <- mergedvector[,c(7,12,13,14,15)]
merged8vector <- mergedvector[,c(8,12,13,14,15)]
merged9vector <- mergedvector[,c(9,12,13,14,15)]
merged10vector <- mergedvector[,c(11,12,13,14,15)]

#NONVECTOR
merged1nonvector <- mergednonvector[,c(1,12,13,14,15)]
merged2nonvector <- mergednonvector[,c(2,12,13,14,15)]
merged3nonvector <- mergednonvector[,c(3,12,13,14,15)]
merged4nonvector <- mergednonvector[,c(4,12,13,14,15)]
merged5nonvector <- mergednonvector[,c(5,12,13,14,15)]
merged6nonvector <- mergednonvector[,c(6,12,13,14,15)]
merged7nonvector <- mergednonvector[,c(7,12,13,14,15)]
merged8nonvector <- mergednonvector[,c(8,12,13,14,15)]
merged9nonvector <- mergednonvector[,c(9,12,13,14,15)]
merged10nonvector <- mergednonvector[,c(11,12,13,14,15)]



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
#########################
#########################


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
#########################


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
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################
#########################

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


#####################################
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

#####################################
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

#####################################
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


#####################################
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

#####################################
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


#####################################
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


#####################################
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


#####################################
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


#####################################
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

#####################################
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



#####################
#
#
#
#
#
################################


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
          asep03vectorsite1)
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
             asep03nonvectorsite1)

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
          asep03vectorsite2)
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
             asep03nonvectorsite2)

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
          asep03vectorsite3)
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
             asep03nonvectorsite3)

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
          asep03vectorsite4)
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
             asep03nonvectorsite4)

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
          asep03vectorsite5)
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
             asep03nonvectorsite5)

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
          asep03vectorsite6)
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
             asep03nonvectorsite6)

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
          asep03vectorsite7)
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
             asep03nonvectorsite7)

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
          asep03vectorsite8)
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
             asep03nonvectorsite8)

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
          asep03vectorsite9)
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
             asep03nonvectorsite9)

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
           asep03vectorsite10)

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
              asep03nonvectorsite10)

#############PLOT
jour <- c(1:13) #Change this in function of the number of total collections...
jour2 <-jour*2
jourlab <- c ("6/4", "6/12", "6/18", "6/25", "7/4",
              "7/10", "7/17", "7/24", "7/30", "8/6", "8/13", "8/20", "9/3")
vectorstotal <- vec1+vec2+vec3+vec4+vec5+vec6+vec7+vec8+vec9+vec10

mosquitoestotal <- vec1+vec2+vec3+vec4+vec5+vec6+vec7+vec8+vec9+vec10+
  nonvec1+nonvec2+nonvec3+nonvec4+nonvec5+nonvec6+nonvec7+nonvec8+nonvec9+nonvec10

par(mfrow=c(3,4))
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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec1, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec1), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec2, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec2), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)


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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec3, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec3), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec4, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec4), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec5, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec5), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec6, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec6), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec7, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec7), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec8, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec8), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec9, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec9), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)



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
        border="darkred", lty=1, lwd=1)
xspline(jour, nonvec10, shape=0.2,
        border="dodgerblue3", lty=3, lwd=1)
abline(h=mean(vec10), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)

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
       lwd=15)

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
abline(h=mean(mosquitoestotal), col=adjustcolor("darkred", alpha.f=0.3),
       lwd=15)


par(mfrow=c(1,1))

#For Plugging into the OVERALL simple spreadsheet
sep03totalpertrap <- sum(sep03$total)/10
sep03totalpertrap
sep03vectorpertrap <- sum(sep03vector$total)/10
sep03vectorpertrap

##################################


#############
########


########################################


########
##############


###################################
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
#Monteocha park mean not including latest
mean(vec2[-12])
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


################BEGIN MAPPING
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
countycols3 <- c("grey30", "grey40", "grey50", "grey10")

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
par(mfrow=c(4,4))
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

#MAP 15

#MAP 16

par(mfrow=c(1,1))

#############

############################## 

#### PRETTIER MAPS

###WATER COLOR
joewatercolor <- openmap(c(29.9, -82.65), c(29.4,-82.0),
                  type="stamen-watercolor")
joemapwatercolor <- openproj(joewatercolor, projection = "+proj=longlat")
plot(joemapwatercolor, raster=TRUE)
points(traplatlong$long, traplatlong$lat,
       pch=19,
       col=adjustcolor("red", alpha.f=0.5),
       cex=0.6+(2*(cexvectorsep03/vectorbl)))
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
       cex=1.2*(cexvectoraug13/vectorbl))

