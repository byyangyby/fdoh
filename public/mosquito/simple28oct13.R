#SET COLOR PALETTE
library(RColorBrewer)
symcols <- colorRampPalette(brewer.pal(8, "Set1"))(8)
blackcols <- c("gray90",rep("gray75", 67))
symcols[6] <- "darkred"
countynames <- map("county", "florida", names=TRUE, plot=FALSE)
countynames
countycols <- colorRampPalette(brewer.pal(9, "Spectral"))(68)
countycols2 <- colorRampPalette(brewer.pal(5, "Pastel2"))(67)
countycols3 <- rep(c("grey30", "grey40", "grey50", "grey10"),30)
yearcols <- colorRampPalette(brewer.pal(6, "Set1"))(6)
yearcols <- adjustcolor(yearcols, alpha.f=0.8)
yearcols[6] <- adjustcolor("grey", alpha.f=0.8)

alachuacol <- adjustcolor("beige", alpha.f=0.2)

#Set site names
sitenames <- c("Transfer Station NE 63 Ave.",
               "Monteocha Park",
               "CR 1491 High Springs",
               "LaPierre Home (High Springs)",
               "Woodlands Subdivision (Tower Rd.)",
               "SE 171 St. (Hawthorne)",
               "Bowman Residence (Archer)",
               "Micanopy",
               "Hawthorne Chix Site",
               "UF AES NW 71 St.")



#READ IN DATA
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
oct01 <- read.csv("2013-10-01.csv", skip=4, header=T, sep=",")
oct08 <- read.csv("2013-10-08.csv", skip=4, header=T, sep=",")
oct16 <- read.csv("2013-10-16.csv", skip=4, header=T, sep=",")
oct22 <- read.csv("2013-10-22.csv", skip=4, header=T, sep=",")

#Prepare data for merging
names(jun04)[1] <- "site"
names(jun04)[11] <- "total"
jun04$X10 <- 0
jun04 <- jun04[-34,]
jun04$Code.. <- NULL
jun04$date <- as.Date("2013-06-04")

names(jun12)[1] <- "site"
names(jun12)[11] <- "total"
jun12$X10 <- 0
jun12 <- jun12[-34,]
jun12$Code.. <- NULL
jun12$date <- as.Date("2013-06-12")

names(jun18)[1] <- "site"
names(jun18)[11] <- "total"
jun18$X10 <- 0
jun18 <- jun18[-34,]
jun18$Code.. <- NULL
jun18$date <- as.Date("2013-06-18")

names(jun25)[1] <- "site"
names(jun25)[11] <- "total"
jun25$X10 <- 0
jun25 <- jun25[-34,]
jun25$Code.. <- NULL
jun25$date <- as.Date("2013-06-25")

names(jul04)[1] <- "site"
names(jul04)[11] <- "total"
jul04$X10 <- 0
jul04 <- jul04[-34,]
jul04$Code.. <- NULL
jul04$date <- as.Date("2013-07-04")

names(jul10)[1] <- "site"
names(jul10)[12] <- "total"
jul10 <- jul10[-34,]
jul10$Code.. <- NULL
jul10$date <- as.Date("2013-07-10")

names(jul17)[1] <- "site"
names(jul17)[12] <- "total"
jul17 <- jul17[-34,]
jul17$Code.. <- NULL
jul17$date <- as.Date("2013-07-17")

names(jul24)[1] <- "site"
names(jul24)[12] <- "total"
jul24 <- jul24[-34,]
jul24$Code.. <- NULL
jul24$date <- as.Date("2013-07-24")
jul24 <- jul24[-c(34,35),]

names(jul30)[1] <- "site"
names(jul30)[12] <- "total"
jul30 <- jul30[-34,]
jul30$Code.. <- NULL
jul30$date <- as.Date("2013-07-30")
jul30 <- jul30[-c(34,35),]

names(aug06)[1] <- "site"
names(aug06)[12] <- "total"
aug06 <- aug06[-34,]
aug06$Code.. <- NULL
aug06$date <- as.Date("2013-08-06")
aug06 <- aug06[-c(34,35),]

names(aug13)[1] <- "site"
names(aug13)[12] <- "total"
aug13 <- aug13[-34,]
aug13$Code.. <- NULL
aug13$date <- as.Date("2013-08-13")

names(aug20)[1] <- "site"
names(aug20)[12] <- "total"
aug20 <- aug20[-34,]
aug20$Code.. <- NULL
aug20$date <- as.Date("2013-08-20")
aug20 <- aug20[-c(34:90),]
aug20 <- aug20[,-c(13:21)]

names(sep03)[1] <- "site"
names(sep03)[12] <- "total"
sep03 <- sep03[-34,]
sep03$Code.. <- NULL
sep03$date <- as.Date("2013-09-03")
sep03 <- sep03[-c(34:90),]


names(sep10)[1] <- "site"
names(sep10)[12] <- "total"
sep10 <- sep10[-34,]
sep10$Code.. <- NULL
sep10$date <- as.Date("2013-09-10")
sep10 <- sep10[-c(34:90),]
sep10 <- sep10[,-c(13:21)]

names(sep17)[1] <- "site"
names(sep17)[12] <- "total"
sep17 <- sep17[-34,]
sep17$Code.. <- NULL
sep17$date <- as.Date("2013-09-17")
sep17 <- sep17[-c(34:90),]
sep17 <- sep17[,-c(13:21)]

names(sep24)[1] <- "site"
names(sep24)[12] <- "total"
sep24 <- sep24[-34,]
sep24$Code.. <- NULL
sep24$date <- as.Date("2013-09-24")
sep24 <- sep24[-c(34:90),]
sep24 <- sep24[,-c(13:21)]

names(oct01)[1] <- "site"
names(oct01)[12] <- "total"
oct01 <- oct01[-34,]
oct01$Code.. <- NULL
oct01$date <- as.Date("2013-10-01")
oct01 <- oct01[-c(34:90),]
oct01 <- oct01[,-c(13:21)]

names(oct08)[1] <- "site"
names(oct08)[12] <- "total"
oct08 <- oct08[-34,]
oct08$Code.. <- NULL
oct08$date <- as.Date("2013-10-08")
oct08 <- oct08[-c(34:90),]
oct08 <- oct08[,-c(13:21)]

names(oct16)[1] <- "site"
names(oct16)[12] <- "total"
oct16 <- oct16[-34,]
oct16$Code.. <- NULL
oct16$date <- as.Date("2013-10-16")
oct16 <- oct16[-c(34:90),]
oct16 <- oct16[,-c(13:21)]

names(oct22)[1] <- "site"
names(oct22)[12] <- "total"
oct22 <- oct22[-34,]
oct22$Code.. <- NULL
oct22$date <- as.Date("2013-10-22")
oct22 <- oct22[-c(34:90),]
oct22 <- oct22[,-c(13:21)]

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
                sep24,
                oct01,
                oct08,
                oct16,
                oct22)

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
merged$dengue <- recode(merged$dengue, "'A. aegypti'='dengue'")
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

mosquito <- as.data.frame(unique(sort(merged$date)))
colnames(mosquito) <- "date"

#wnv
for(i in mosquito$date){mosquito$wnv[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$wnv=="wnv" &
                                                   merged$date == i)])}
#slev
for(i in mosquito$date){mosquito$slev[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$slev=="slev" &
                                                   merged$date == i)])}
#eeev
for(i in mosquito$date){mosquito$eeev[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$eeev=="eeev" &
                                                   merged$date == i)])}
#chik
for(i in mosquito$date){mosquito$chik[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$chik=="chik" &
                                                   merged$date == i)])}
#malaria
for(i in mosquito$date){mosquito$malaria[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$malaria=="malaria" &
                                                   merged$date == i)])}
#dengue
for(i in mosquito$date){mosquito$dengue[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$dengue=="dengue" &
                                                   merged$date == i)])}
#vector
for(i in mosquito$date){mosquito$vector[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$vector=="vector" &
                                                   merged$date == i)])}
#nonvector
for(i in mosquito$date){mosquito$nonvector[which(mosquito$date == i)] <- 
                          sum(merged$total[which(merged$vector=="nonvector" &
                                                   merged$date == i)])}
#total
mosquito$total <- mosquito$vector +mosquito$nonvector
###################





# OVERALL SIMPLE SPREADHSEET#####
par(mfrow=c(1,1))
setwd("C:/Users/BrewJR/Documents/workingdirectory/mosquito")
trap12 <- read.csv("simple.csv", head=T, sep=",")
trap12$date <- as.Date(trap12$date, "%m/%d/%Y")
trap12 <- trap12[which(trap12$date < as.Date("2013-01-01")),]

trap13 <- as.data.frame(mosquito$date)
colnames(trap13) <- "date"

for (i in trap13$date){
  trap13$total[which(trap13$date == i)] <- mosquito$total[which(mosquito$date == i)]/10}

for (i in trap13$date){
  trap13$vector[which(trap13$date == i)] <- mosquito$vector[which(mosquito$date == i)]/10}

trap <- rbind(trap12, trap13)
trap$dayofyear <- format(trap$date, format="%j")
trap$year <- format(trap$date, format= "%Y")

#####
#SITE-SPECIFIC DATA
site <- trap13
#wnv
site$X1wnv <- NA
for (i in site$date){site$X1wnv[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                           merged$wnv=="wnv")], na.rm=T)}
site$X2wnv <- NA
for (i in site$date){site$X2wnv[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X3wnv <- NA
for (i in site$date){site$X3wnv[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X4wnv <- NA
for (i in site$date){site$X4wnv[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X5wnv <- NA
for (i in site$date){site$X5wnv[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X6wnv <- NA
for (i in site$date){site$X6wnv[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X7wnv <- NA
for (i in site$date){site$X7wnv[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X8wnv <- NA
for (i in site$date){site$X8wnv[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X9wnv <- NA
for (i in site$date){site$X9wnv[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}
site$X10wnv <- NA
for (i in site$date){site$X10wnv[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                              merged$wnv=="wnv")], na.rm=T)}

#slev
site$X1slev <- NA
for (i in site$date){site$X1slev[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X2slev <- NA
for (i in site$date){site$X2slev[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X3slev <- NA
for (i in site$date){site$X3slev[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X4slev <- NA
for (i in site$date){site$X4slev[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X5slev <- NA
for (i in site$date){site$X5slev[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X6slev <- NA
for (i in site$date){site$X6slev[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X7slev <- NA
for (i in site$date){site$X7slev[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X8slev <- NA
for (i in site$date){site$X8slev[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X9slev <- NA
for (i in site$date){site$X9slev[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$slev=="slev")], na.rm=T)}
site$X10slev <- NA
for (i in site$date){site$X10slev[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                                merged$slev=="slev")], na.rm=T)}


#eeev
site$X1eeev <- NA
for (i in site$date){site$X1eeev[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X2eeev <- NA
for (i in site$date){site$X2eeev[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X3eeev <- NA
for (i in site$date){site$X3eeev[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X4eeev <- NA
for (i in site$date){site$X4eeev[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X5eeev <- NA
for (i in site$date){site$X5eeev[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X6eeev <- NA
for (i in site$date){site$X6eeev[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X7eeev <- NA
for (i in site$date){site$X7eeev[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X8eeev <- NA
for (i in site$date){site$X8eeev[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X9eeev <- NA
for (i in site$date){site$X9eeev[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$eeev=="eeev")], na.rm=T)}
site$X10eeev <- NA
for (i in site$date){site$X10eeev[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                                merged$eeev=="eeev")], na.rm=T)}



#chik
site$X1chik <- NA
for (i in site$date){site$X1chik[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X2chik <- NA
for (i in site$date){site$X2chik[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X3chik <- NA
for (i in site$date){site$X3chik[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X4chik <- NA
for (i in site$date){site$X4chik[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X5chik <- NA
for (i in site$date){site$X5chik[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X6chik <- NA
for (i in site$date){site$X6chik[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X7chik <- NA
for (i in site$date){site$X7chik[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X8chik <- NA
for (i in site$date){site$X8chik[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X9chik <- NA
for (i in site$date){site$X9chik[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$chik=="chik")], na.rm=T)}
site$X10chik <- NA
for (i in site$date){site$X10chik[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                                merged$chik=="chik")], na.rm=T)}



#malaria
site$X1malaria <- NA
for (i in site$date){site$X1malaria[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X2malaria <- NA
for (i in site$date){site$X2malaria[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X3malaria <- NA
for (i in site$date){site$X3malaria[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X4malaria <- NA
for (i in site$date){site$X4malaria[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X5malaria <- NA
for (i in site$date){site$X5malaria[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X6malaria <- NA
for (i in site$date){site$X6malaria[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X7malaria <- NA
for (i in site$date){site$X7malaria[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X8malaria <- NA
for (i in site$date){site$X8malaria[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X9malaria <- NA
for (i in site$date){site$X9malaria[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$malaria=="malaria")], na.rm=T)}
site$X10malaria <- NA
for (i in site$date){site$X10malaria[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                                merged$malaria=="malaria")], na.rm=T)}



#dengue
site$X1dengue <- NA
for (i in site$date){site$X1dengue[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X2dengue <- NA
for (i in site$date){site$X2dengue[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X3dengue <- NA
for (i in site$date){site$X3dengue[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X4dengue <- NA
for (i in site$date){site$X4dengue[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X5dengue <- NA
for (i in site$date){site$X5dengue[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X6dengue <- NA
for (i in site$date){site$X6dengue[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X7dengue <- NA
for (i in site$date){site$X7dengue[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X8dengue <- NA
for (i in site$date){site$X8dengue[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X9dengue <- NA
for (i in site$date){site$X9dengue[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$dengue=="dengue")], na.rm=T)}
site$X10dengue <- NA
for (i in site$date){site$X10dengue[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                                merged$dengue=="dengue")], na.rm=T)}



#vector
site$X1vector <- NA
for (i in site$date){site$X1vector[which(site$date==i)] <- sum(merged$X1[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X2vector <- NA
for (i in site$date){site$X2vector[which(site$date==i)] <- sum(merged$X2[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X3vector <- NA
for (i in site$date){site$X3vector[which(site$date==i)] <- sum(merged$X3[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X4vector <- NA
for (i in site$date){site$X4vector[which(site$date==i)] <- sum(merged$X4[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X5vector <- NA
for (i in site$date){site$X5vector[which(site$date==i)] <- sum(merged$X5[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X6vector <- NA
for (i in site$date){site$X6vector[which(site$date==i)] <- sum(merged$X6[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X7vector <- NA
for (i in site$date){site$X7vector[which(site$date==i)] <- sum(merged$X7[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X8vector <- NA
for (i in site$date){site$X8vector[which(site$date==i)] <- sum(merged$X8[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X9vector <- NA
for (i in site$date){site$X9vector[which(site$date==i)] <- sum(merged$X9[which(merged$date== i &
                                                                              merged$vector=="vector")], na.rm=T)}
site$X10vector <- NA
for (i in site$date){site$X10vector[which(site$date==i)] <- sum(merged$X10[which(merged$date== i &
                                                                                merged$vector=="vector")], na.rm=T)}




################ PLOT
par(mfrow=c(2,1))
# OVERALL
plot(trap$dayofyear[which(trap$year==2012)], trap$vector[which(trap$year==2012)], type="n", xaxt="n",
     xlab="Month", ylab="Average vectors per trap", ylim=c(0,1100))
axis(side=1, at=trap$dayofyear[which(trap$year==2012)], 
     labels=format(as.Date(trap$dayofyear[which(trap$year==2012)],
                           format="%j"),format="%b\n%d"), cex.axis=0.2)

xspline(trap$dayofyear[which(trap$year==2013)], trap$vector[which(trap$year==2013)], 
        border=yearcols[1], shape=0.5, lwd=3)
xspline(trap$dayofyear[which(trap$year==2012)], trap$vector[which(trap$year==2012)], 
        border=yearcols[2], shape=0.5)
xspline(trap$dayofyear[which(trap$year==2011)], trap$vector[which(trap$year==2011)], 
        border=yearcols[3], shape=0.5)
xspline(trap$dayofyear[which(trap$year==2010)], trap$vector[which(trap$year==2010)], 
        border=yearcols[4], shape=0.5)
xspline(trap$dayofyear[which(trap$year==2009)], trap$vector[which(trap$year==2009)], 
        border=yearcols[5], shape=0.5)
xspline(trap$dayofyear[which(trap$year==2008)], trap$vector[which(trap$year==2008)], 
        border=yearcols[6], shape=0.5)
        
abline(v=max(trap$dayofyear[which(trap$year==2013)]), lty=1, lwd=3, col=adjustcolor("grey", alpha.f=0.4))
abline(h=max(trap$vector[which(trap$dayofyear==max(trap$dayofyear[which(trap$year==2013)]))]), lty=1, lwd=3, col=adjustcolor("grey", alpha.f=0.4))

legend(x="topright", lty=1, col=yearcols[1:6], legend=c("2013", "2012", "2011", "2010", "2009", "2008"),
       border=FALSE, bty="n", cex=0.6, lwd=c(3,1,1,1,1,1))


# For website 
plot(trap$dayofyear[which(trap$year==2013)], trap$vector[which(trap$year==2013)], type="n", xaxt="n",
     xlab="Date", ylab="Mosquitoes", ylim=c(0,650),
     main="Average vector mosquitoes per trap, 2013")
axis(side=1, at=trap$dayofyear[which(trap$year==2013)], 
     labels=format(as.Date(trap$dayofyear[which(trap$year==2013)],
                           format="%j"),format="%b\n%d"), cex.axis=0.4)

xspline(trap$dayofyear[which(trap$year==2013)], trap$vector[which(trap$year==2013)], 
        border=adjustcolor(yearcols[1], alpha.f=0.6), shape=0, lwd=3)
points(trap$dayofyear[which(trap$year==2013)], trap$vector[which(trap$year==2013)],
        pch=16, col="grey")
points(trap$dayofyear[which(trap$year==2013)], trap$vector[which(trap$year==2013)],
       pch=21, col=adjustcolor("red", alpha.f=0.6))

# BY DISEASE TYPE
par(mfrow=c(3,2))
#wnv
plot(mosquito$date, mosquito$wnv, type="n", xlab="Date", ylab="Msqts. captured")
points(mosquito$date, mosquito$wnv, pch=16, col=adjustcolor("grey", alpha.f=0.6))
points(mosquito$date, mosquito$wnv, pch=21, col=yearcols[1])
xspline(mosquito$date, mosquito$wnv, border=adjustcolor(yearcols[1],alpha.f=0.5), shape=0, lwd=3)
title(main="Mosquitoes capable of carrying WNV captured this summer", cex.main=0.6)

#slev
plot(mosquito$date, mosquito$slev, type="n", xlab="Date", ylab="Msqts. captured")
points(mosquito$date, mosquito$slev, pch=16, col=adjustcolor("grey", alpha.f=0.6))
points(mosquito$date, mosquito$slev, pch=21, col=yearcols[2])
xspline(mosquito$date, mosquito$slev, border=adjustcolor(yearcols[2],alpha.f=0.5), shape=0, lwd=3)
title(main="Mosquitoes capable of carrying SLEV captured this summer", cex.main=0.6)

#eeev
plot(mosquito$date, mosquito$eeev, type="n", xlab="Date", ylab="Msqts. captured")
points(mosquito$date, mosquito$eeev, pch=16, col=adjustcolor("grey", alpha.f=0.6))
points(mosquito$date, mosquito$eeev, pch=21, col=yearcols[3])
xspline(mosquito$date, mosquito$eeev, border=adjustcolor(yearcols[3],alpha.f=0.5), shape=0, lwd=3)
title(main="Mosquitoes capable of carrying EEEV captured this summer", cex.main=0.6)

#chik
plot(mosquito$date, mosquito$chik, type="n", xlab="Date", ylab="Msqts. captured")
points(mosquito$date, mosquito$chik, pch=16, col=adjustcolor("grey", alpha.f=0.6))
points(mosquito$date, mosquito$chik, pch=21, col=yearcols[4])
xspline(mosquito$date, mosquito$chik, border=adjustcolor(yearcols[4],alpha.f=0.5), shape=0, lwd=3)
title(main="Mosquitoes capable of carrying Chikungunya captured this summer", cex.main=0.6)

#malaria
plot(mosquito$date, mosquito$malaria, type="n", xlab="Date", ylab="Msqts. captured")
points(mosquito$date, mosquito$malaria, pch=16, col=adjustcolor("grey", alpha.f=0.6))
points(mosquito$date, mosquito$malaria, pch=21, col=yearcols[5])
xspline(mosquito$date, mosquito$malaria, border=adjustcolor(yearcols[5],alpha.f=0.5), shape=0, lwd=3)
title(main="Mosquitoes capable of carrying Malaria captured this summer", cex.main=0.6)

#dengue
plot(mosquito$date, mosquito$dengue, type="n", xlab="Date", ylab="Msqts. captured")
points(mosquito$date, mosquito$dengue, pch=16, col=adjustcolor("grey", alpha.f=0.6))
points(mosquito$date, mosquito$dengue, pch=21, col=yearcols[6])
xspline(mosquito$date, mosquito$dengue, border=adjustcolor(yearcols[6],alpha.f=0.5), shape=0, lwd=3)
title(main="Mosquitoes capable of carrying Dengue captured this summer", cex.main=0.6)

#WNV vectors CAPTURED
par(mfrow=c(5,2))
#By SITE
#X1
plot(site$date, site$X1wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1wnv, col=symcols[1], lwd=1)
points(site$date, site$X1wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X1wnv, col=symcols[1], pch=21)

#X2
plot(site$date, site$X2wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2wnv, col=symcols[1])
points(site$date, site$X2wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X2wnv, col=symcols[1], pch=21)

#X3
plot(site$date, site$X3wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3wnv, col=symcols[1])
points(site$date, site$X3wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X3wnv, col=symcols[1], pch=21)

#X4
plot(site$date, site$X4wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4wnv, col=symcols[1])
points(site$date, site$X4wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X4wnv, col=symcols[1], pch=21)

#X5
plot(site$date, site$X5wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5wnv, col=symcols[1])
points(site$date, site$X5wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X5wnv, col=symcols[1], pch=21)

#X6
plot(site$date, site$X6wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6wnv, col=symcols[1])
points(site$date, site$X6wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X6wnv, col=symcols[1], pch=21)

#X7
plot(site$date, site$X7wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7wnv, col=symcols[1])
points(site$date, site$X7wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X7wnv, col=symcols[1], pch=21)

#X8
plot(site$date, site$X8wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8wnv, col=symcols[1])
points(site$date, site$X8wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X8wnv, col=symcols[1], pch=21)

#X9
plot(site$date, site$X9wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9wnv, col=symcols[1])
points(site$date, site$X9wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X9wnv, col=symcols[1], pch=21)

#X10
plot(site$date, site$X10wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10wnv, col=symcols[1])
points(site$date, site$X10wnv, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X10wnv, col=symcols[1], pch=21)

title(main="WNV vectors", outer=TRUE, line=-1)


#slev vectors CAPTURED
par(mfrow=c(5,2))
#By SITE
#X1
plot(site$date, site$X1slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1slev, col=symcols[2], lwd=1)
points(site$date, site$X1slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X1slev, col=symcols[2], pch=21)

#X2
plot(site$date, site$X2slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2slev, col=symcols[2])
points(site$date, site$X2slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X2slev, col=symcols[2], pch=21)

#X3
plot(site$date, site$X3slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3slev, col=symcols[2])
points(site$date, site$X3slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X3slev, col=symcols[2], pch=21)

#X4
plot(site$date, site$X4slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4slev, col=symcols[2])
points(site$date, site$X4slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X4slev, col=symcols[2], pch=21)

#X5
plot(site$date, site$X5slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5slev, col=symcols[2])
points(site$date, site$X5slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X5slev, col=symcols[2], pch=21)

#X6
plot(site$date, site$X6slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6slev, col=symcols[2])
points(site$date, site$X6slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X6slev, col=symcols[2], pch=21)

#X7
plot(site$date, site$X7slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7slev, col=symcols[2])
points(site$date, site$X7slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X7slev, col=symcols[2], pch=21)

#X8
plot(site$date, site$X8slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8slev, col=symcols[2])
points(site$date, site$X8slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X8slev, col=symcols[2], pch=21)

#X9
plot(site$date, site$X9slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9slev, col=symcols[2])
points(site$date, site$X9slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X9slev, col=symcols[2], pch=21)

#X10
plot(site$date, site$X10slev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10slev, col=symcols[2])
points(site$date, site$X10slev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X10slev, col=symcols[2], pch=21)

title(main="SLEV vectors", outer=TRUE, line=-1)

#eeev vectors CAPTURED
par(mfrow=c(5,2))
#By SITE
#X1
plot(site$date, site$X1eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1eeev, col=symcols[3], lwd=1)
points(site$date, site$X1eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X1eeev, col=symcols[3], pch=21)

#X2
plot(site$date, site$X2eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2eeev, col=symcols[3])
points(site$date, site$X2eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X2eeev, col=symcols[3], pch=21)

#X3
plot(site$date, site$X3eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3eeev, col=symcols[3])
points(site$date, site$X3eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X3eeev, col=symcols[3], pch=21)

#X4
plot(site$date, site$X4eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4eeev, col=symcols[3])
points(site$date, site$X4eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X4eeev, col=symcols[3], pch=21)

#X5
plot(site$date, site$X5eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5eeev, col=symcols[3])
points(site$date, site$X5eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X5eeev, col=symcols[3], pch=21)

#X6
plot(site$date, site$X6eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6eeev, col=symcols[3])
points(site$date, site$X6eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X6eeev, col=symcols[3], pch=21)

#X7
plot(site$date, site$X7eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7eeev, col=symcols[3])
points(site$date, site$X7eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X7eeev, col=symcols[3], pch=21)

#X8
plot(site$date, site$X8eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8eeev, col=symcols[3])
points(site$date, site$X8eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X8eeev, col=symcols[3], pch=21)

#X9
plot(site$date, site$X9eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9eeev, col=symcols[3])
points(site$date, site$X9eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X9eeev, col=symcols[3], pch=21)

#X10
plot(site$date, site$X10eeev, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10eeev, col=symcols[3])
points(site$date, site$X10eeev, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X10eeev, col=symcols[3], pch=21)

title(main="EEEV vectors", outer=TRUE, line=-1)

#chik vectors CAPTURED
par(mfrow=c(5,2))
#By SITE
#X1
plot(site$date, site$X1chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1chik, col=symcols[4], lwd=1)
points(site$date, site$X1chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X1chik, col=symcols[4], pch=21)

#X2
plot(site$date, site$X2chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2chik, col=symcols[4])
points(site$date, site$X2chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X2chik, col=symcols[4], pch=21)

#X3
plot(site$date, site$X3chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3chik, col=symcols[4])
points(site$date, site$X3chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X3chik, col=symcols[4], pch=21)

#X4
plot(site$date, site$X4chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4chik, col=symcols[4])
points(site$date, site$X4chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X4chik, col=symcols[4], pch=21)

#X5
plot(site$date, site$X5chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5chik, col=symcols[4])
points(site$date, site$X5chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X5chik, col=symcols[4], pch=21)

#X6
plot(site$date, site$X6chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6chik, col=symcols[4])
points(site$date, site$X6chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X6chik, col=symcols[4], pch=21)

#X7
plot(site$date, site$X7chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7chik, col=symcols[4])
points(site$date, site$X7chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X7chik, col=symcols[4], pch=21)

#X8
plot(site$date, site$X8chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8chik, col=symcols[4])
points(site$date, site$X8chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X8chik, col=symcols[4], pch=21)

#X9
plot(site$date, site$X9chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9chik, col=symcols[4])
points(site$date, site$X9chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X9chik, col=symcols[4], pch=21)

#X10
plot(site$date, site$X10chik, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10chik, col=symcols[4])
points(site$date, site$X10chik, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X10chik, col=symcols[4], pch=21)

title(main="Chikungunya vectors", outer=TRUE, line=-1)

#malaria vectors CAPTURED
par(mfrow=c(5,2))
#By SITE
#X1
plot(site$date, site$X1malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1malaria, col=symcols[5], lwd=1)
points(site$date, site$X1malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X1malaria, col=symcols[5], pch=21)

#X2
plot(site$date, site$X2malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2malaria, col=symcols[5])
points(site$date, site$X2malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X2malaria, col=symcols[5], pch=21)

#X3
plot(site$date, site$X3malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3malaria, col=symcols[5])
points(site$date, site$X3malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X3malaria, col=symcols[5], pch=21)

#X4
plot(site$date, site$X4malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4malaria, col=symcols[5])
points(site$date, site$X4malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X4malaria, col=symcols[5], pch=21)

#X5
plot(site$date, site$X5malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5malaria, col=symcols[5])
points(site$date, site$X5malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X5malaria, col=symcols[5], pch=21)

#X6
plot(site$date, site$X6malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6malaria, col=symcols[5])
points(site$date, site$X6malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X6malaria, col=symcols[5], pch=21)

#X7
plot(site$date, site$X7malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7malaria, col=symcols[5])
points(site$date, site$X7malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X7malaria, col=symcols[5], pch=21)

#X8
plot(site$date, site$X8malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8malaria, col=symcols[5])
points(site$date, site$X8malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X8malaria, col=symcols[5], pch=21)

#X9
plot(site$date, site$X9malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9malaria, col=symcols[5])
points(site$date, site$X9malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X9malaria, col=symcols[5], pch=21)

#X10
plot(site$date, site$X10malaria, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10malaria, col=symcols[5])
points(site$date, site$X10malaria, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X10malaria, col=symcols[5], pch=21)

title(main="Malaria vectors", outer=TRUE, line=-1)

#dengue vectors CAPTURED
par(mfrow=c(5,2))
#By SITE
#X1
plot(site$date, site$X1dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1dengue, col=symcols[6], lwd=1)
points(site$date, site$X1dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X1dengue, col=symcols[6], pch=21)

#X2
plot(site$date, site$X2dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2dengue, col=symcols[6])
points(site$date, site$X2dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X2dengue, col=symcols[6], pch=21)

#X3
plot(site$date, site$X3dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3dengue, col=symcols[6])
points(site$date, site$X3dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X3dengue, col=symcols[6], pch=21)

#X4
plot(site$date, site$X4dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4dengue, col=symcols[6])
points(site$date, site$X4dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X4dengue, col=symcols[6], pch=21)

#X5
plot(site$date, site$X5dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5dengue, col=symcols[6])
points(site$date, site$X5dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X5dengue, col=symcols[6], pch=21)

#X6
plot(site$date, site$X6dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6dengue, col=symcols[6])
points(site$date, site$X6dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X6dengue, col=symcols[6], pch=21)

#X7
plot(site$date, site$X7dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7dengue, col=symcols[6])
points(site$date, site$X7dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X7dengue, col=symcols[6], pch=21)

#X8
plot(site$date, site$X8dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8dengue, col=symcols[6])
points(site$date, site$X8dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X8dengue, col=symcols[6], pch=21)

#X9
plot(site$date, site$X9dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9dengue, col=symcols[6])
points(site$date, site$X9dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X9dengue, col=symcols[6], pch=21)

#X10
plot(site$date, site$X10dengue, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10dengue, col=symcols[6])
points(site$date, site$X10dengue, col=adjustcolor("grey", alpha.f=0.6), pch=16)
points(site$date, site$X10dengue, col=symcols[6], pch=21)

title(main="Dengue vectors", outer=TRUE, line=-1)


#### ALL DISEASES COMBINED
symnames <- c("WNV", "SLEV", "EEEV", "Chik", "Malaria", "Dengue")
par(mfrow=c(5,2))
#X1
plot(site$date, site$X1wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[1])
lines(site$date, site$X1wnv, col=symcols[1])
lines(site$date, site$X1slev, col=symcols[2])
lines(site$date, site$X1eeev, col=symcols[3])
lines(site$date, site$X1chik, col=symcols[4])
lines(site$date, site$X1malaria, col=symcols[5])
lines(site$date, site$X1dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X2
plot(site$date, site$X2wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[2])
lines(site$date, site$X2wnv, col=symcols[1])
lines(site$date, site$X2slev, col=symcols[2])
lines(site$date, site$X2eeev, col=symcols[3])
lines(site$date, site$X2chik, col=symcols[4])
lines(site$date, site$X2malaria, col=symcols[5])
lines(site$date, site$X2dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X3
plot(site$date, site$X3wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[3])
lines(site$date, site$X3wnv, col=symcols[1])
lines(site$date, site$X3slev, col=symcols[2])
lines(site$date, site$X3eeev, col=symcols[3])
lines(site$date, site$X3chik, col=symcols[4])
lines(site$date, site$X3malaria, col=symcols[5])
lines(site$date, site$X3dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X4
plot(site$date, site$X4wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[4])
lines(site$date, site$X4wnv, col=symcols[1])
lines(site$date, site$X4slev, col=symcols[2])
lines(site$date, site$X4eeev, col=symcols[3])
lines(site$date, site$X4chik, col=symcols[4])
lines(site$date, site$X4malaria, col=symcols[5])
lines(site$date, site$X4dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X5
plot(site$date, site$X5wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[5])
lines(site$date, site$X5wnv, col=symcols[1])
lines(site$date, site$X5slev, col=symcols[2])
lines(site$date, site$X5eeev, col=symcols[3])
lines(site$date, site$X5chik, col=symcols[4])
lines(site$date, site$X5malaria, col=symcols[5])
lines(site$date, site$X5dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X6
plot(site$date, site$X6wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[6])
lines(site$date, site$X6wnv, col=symcols[1])
lines(site$date, site$X6slev, col=symcols[2])
lines(site$date, site$X6eeev, col=symcols[3])
lines(site$date, site$X6chik, col=symcols[4])
lines(site$date, site$X6malaria, col=symcols[5])
lines(site$date, site$X6dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X7
plot(site$date, site$X7wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[7])
lines(site$date, site$X7wnv, col=symcols[1])
lines(site$date, site$X7slev, col=symcols[2])
lines(site$date, site$X7eeev, col=symcols[3])
lines(site$date, site$X7chik, col=symcols[4])
lines(site$date, site$X7malaria, col=symcols[5])
lines(site$date, site$X7dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X8
plot(site$date, site$X8wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[8])
lines(site$date, site$X8wnv, col=symcols[1])
lines(site$date, site$X8slev, col=symcols[2])
lines(site$date, site$X8eeev, col=symcols[3])
lines(site$date, site$X8chik, col=symcols[4])
lines(site$date, site$X8malaria, col=symcols[5])
lines(site$date, site$X8dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X9
plot(site$date, site$X9wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[9])
lines(site$date, site$X9wnv, col=symcols[1])
lines(site$date, site$X9slev, col=symcols[2])
lines(site$date, site$X9eeev, col=symcols[3])
lines(site$date, site$X9chik, col=symcols[4])
lines(site$date, site$X9malaria, col=symcols[5])
lines(site$date, site$X9dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)
#X10
plot(site$date, site$X10wnv, type="n", xlab="Date", ylab="Mosquitoes",
     main=sitenames[10])
lines(site$date, site$X10wnv, col=symcols[1])
lines(site$date, site$X10slev, col=symcols[2])
lines(site$date, site$X10eeev, col=symcols[3])
lines(site$date, site$X10chik, col=symcols[4])
lines(site$date, site$X10malaria, col=symcols[5])
lines(site$date, site$X10dengue, col=symcols[6])
legend(x="topleft", lty=1, col=symcols[1:6], legend=symnames, border=F, bty="n", cex=0.25)

title(main="All vectors", outer=TRUE, line=-1)



####BEGIN MAPPING
par(mfrow=c(1,1))
library(maps)
library(mapdata)
library(maptools)
library(RColorBrewer)

#READ TRAP SITES
setwd("C:/Users/BrewJR/Documents/workingdirectory/mosquito")
traplatlong <- read.csv("traplatlong.csv", header=TRUE, sep=",")


#TRAP COLLECTION SITES

#Make colors

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


#DISEASE SPECIFIC MAPS
#DISEASE SPECIFIC MAPS
par(mfrow=c(3,2))
par(oma=c(1,1,1,1))
par(mar=c(2,1,1,1))
#WNV
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9), ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[1], alpha.f=0.5),
       cex= 2*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("wnv",names(site)) >1)]/
         apply(site[,which(regexpr("wnv",names(site)) >1)],2, mean))))
text(traplatlong$long, traplatlong$lat, labels=names, cex=0.4, adj=c(0.5,2), las=3)
title("WNV vectors")
legend(-82.7,29.5, ncol=1, legend=c("below normal", "normal", "above normal"),
       pch=16, col=adjustcolor(symcols[1], alpha.f=0.5),
       pt.cex=c(1,2,4), cex=0.4, x.intersp=1.3, y.intersp=1.2, bor=T,bty="n")

#slev
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9), ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[2], alpha.f=0.5),
       cex= 2*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("slev",names(site)) >1)]/
                                     apply(site[,which(regexpr("slev",names(site)) >1)],2, mean))))
text(traplatlong$long, traplatlong$lat, labels=names, cex=0.4, adj=c(0.5,2), las=3)
title("SLEV vectors")
legend(-82.7,29.5, ncol=1, legend=c("below normal", "normal", "above normal"),
       pch=16, col=adjustcolor(symcols[2], alpha.f=0.5),
       pt.cex=c(1,2,4), cex=0.4, x.intersp=1.3, y.intersp=1.2, bor=T,bty="n")

#eeev
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9), ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[3], alpha.f=0.5),
       cex= 2*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("eeev",names(site)) >1)]/
                                     apply(site[,which(regexpr("eeev",names(site)) >1)],2, mean))))
text(traplatlong$long, traplatlong$lat, labels=names, cex=0.4, adj=c(0.5,2), las=3)
title("EEEV vectors")
legend(-82.7,29.5, ncol=1, legend=c("below normal", "normal", "above normal"),
       pch=16, col=adjustcolor(symcols[3], alpha.f=0.5),
       pt.cex=c(1,2,4), cex=0.4, x.intersp=1.3, y.intersp=1.2, bor=T,bty="n")

#chik
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9), ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[4], alpha.f=0.5),
       cex= 2*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("chik",names(site)) >1)]/
                                     apply(site[,which(regexpr("chik",names(site)) >1)],2, mean))))
text(traplatlong$long, traplatlong$lat, labels=names, cex=0.4, adj=c(0.5,2), las=3)
title("Chikungunya vectors")
legend(-82.7,29.5, ncol=1, legend=c("below normal", "normal", "above normal"),
       pch=16, col=adjustcolor(symcols[4], alpha.f=0.5),
       pt.cex=c(1,2,4), cex=0.4, x.intersp=1.3, y.intersp=1.2, bor=T,bty="n")

#malaria
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9), ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[5], alpha.f=0.5),
       cex= 2*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("malaria",names(site)) >1)]/
                                     apply(site[,which(regexpr("malaria",names(site)) >1)],2, mean))))
text(traplatlong$long, traplatlong$lat, labels=names, cex=0.4, adj=c(0.5,2), las=3)
title("Malaria vectors")
legend(-82.7,29.5, ncol=1, legend=c("below normal", "normal", "above normal"),
       pch=16, col=adjustcolor(symcols[5], alpha.f=0.5),
       pt.cex=c(1,2,4), cex=0.4, x.intersp=1.3, y.intersp=1.2, bor=T,bty="n")

#dengue
map("county", "florida", fill=TRUE, col=c(alachuacol,countycols3),
    xlim=c(-82.8,-81.9), ylim=c(29.3,30.05))
points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[6], alpha.f=0.5),
       cex= 2*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("dengue",names(site)) >1)]/
                                     apply(site[,which(regexpr("dengue",names(site)) >1)],2, mean))))
text(traplatlong$long, traplatlong$lat, labels=names, cex=0.4, adj=c(0.5,2), las=3)
title("Dengue vectors")
legend(-82.7,29.5, ncol=1, legend=c("below normal", "normal", "above normal"),
       pch=16, col=adjustcolor(symcols[6], alpha.f=0.5),
       pt.cex=c(1,2,4), cex=0.4, x.intersp=1.3, y.intersp=1.2, bor=T,bty="n")
title(main="Disease vectors by location", lines=-1, outer=T)
##### 

#### PRETTIER MAPS

###WATER COLOR
par(mfrow=c(1,1))
joewatercolor <- openmap(c(29.9, -82.65), c(29.4,-82.0),
                         type="stamen-watercolor")
joemapwatercolor <- openproj(joewatercolor, projection = "+proj=longlat")
plot(joemapwatercolor, raster=TRUE)



points(traplatlong$long, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[1], alpha.f=0.5),
       cex= 4*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("wnv",names(site)) >1)]/
                                     apply(site[,which(regexpr("wnv",names(site)) >1)],2, mean))))

points(traplatlong$long+0.008, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[2], alpha.f=0.5),
       cex= 4*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("slev",names(site)) >1)]/
                                     apply(site[,which(regexpr("slev",names(site)) >1)],2, mean))))

points(traplatlong$long, traplatlong$lat+0.0085,
       pch=16, col=adjustcolor(symcols[3], alpha.f=0.5),
       cex= 4*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("eeev",names(site)) >1)]/
                                     apply(site[,which(regexpr("eeev",names(site)) >1)],2, mean))))

points(traplatlong$long, traplatlong$lat+0.001,
       pch=16, col=adjustcolor(symcols[4], alpha.f=0.5),
       cex= 4*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("chik",names(site)) >1)]/
                                     apply(site[,which(regexpr("chik",names(site)) >1)],2, mean))))

points(traplatlong$long+0.001, traplatlong$lat,
       pch=16, col=adjustcolor(symcols[5], alpha.f=0.5),
       cex= 4*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("malaria",names(site)) >1)]/
                                     apply(site[,which(regexpr("malaria",names(site)) >1)],2, mean))))

points(traplatlong$long-0.002, traplatlong$lat+0.0023,
       pch=16, col=adjustcolor(symcols[6], alpha.f=0.5),
       cex= 4*as.numeric(as.vector(site[which(site$date== max(site$date)),which(regexpr("dengue",names(site)) >1)]/
                                     apply(site[,which(regexpr("dengue",names(site)) >1)],2, mean))))



#text(traplatlong$long, traplatlong$lat, 
 #    labels=names,
  #   cex=0.9,
     #adj=c(0.5,2),
   #  las=3)
legend(-82.1,29.49,
       ncol=1,
       legend=c("below normal", "normal", 
                "above normal"),
       pch=16, 
       col=adjustcolor("black", alpha.f=0.5),
       pt.cex=c(2,4,8),
       cex=0.6,
       x.intersp=2, y.intersp=1,
       bor=T,bty="n",)
legend(-82.6,29.49,
       ncol=2,
       legend=symnames,
       pch=16, 
       col=adjustcolor(symcols, alpha.f=0.5),
       cex=0.8,
       pt.cex=3,
       x.intersp=2, y.intersp=1,
       bor=T,bty="n",)



#Breakdown by mosquito type
dennis2 <- as.data.frame(matrix(rep(999, 700), 
                                nrow=length(unique(sort(merged$date)))))
colnames(dennis2)[1] <- "date"
colnames(dennis2)[2:35] <- unique(sort(as.character(merged$mosq)))
dennis2$date <- unique(sort(merged$date))

for (i in dennis2$date){
  for (j in colnames(dennis2)[2:35]){
    dennis2[which(dennis2$date == i), j] <-
      sum(merged$total[which(merged$date == i &
                               merged$mosq == j)])
  }                              
}
library(splines)
zapcols <- colorRampPalette(brewer.pal(9, "Set1"))(34)

plot(dennis2$date, dennis2[,27], type="n", xlab="Date", ylab="Mosquitoes",
     main="Mosquito types this summer")
for (i in 2:34){
  xspline(dennis2$date, dennis2[,i], shape=0.5, border=adjustcolor(
    zapcols[i], alpha.f=0.5),
          lwd=3)}
legend(x="topleft", lwd=2, col=zapcols[2:34],
       legend=colnames(dennis2)[2:34],
       cex=0.5, y.intersp=0.8)

text(x=max(dennis2$date), y=1000, labels="M. dyari", cex=0.5)
text(x=dennis2$date[14]+5, y=2500, labels="M. dyari", cex=0.5)

text(x=max(dennis2$date-15), y=2500, labels="Culex erraticus", cex=0.5)
text(x=max(dennis2$date-75), y=3300, labels="C. nigripalpus", cex=0.5)
text(x=max(dennis2$date-75), y=2400, labels="P. colubmiae", cex=0.5)
text(x=max(dennis2$date-70), y=2600, labels="M. dyari", cex=0.5)

text(x=max(dennis2$date), y=dennis2[which(dennis2$date==max(dennis2$date)),2:35],
     labels=names(dennis2[which(dennis2$date==max(dennis2$date)),2:35]),
     cex=0.1)
text(x=min(dennis2$date), y=dennis2[which(dennis2$date==min(dennis2$date)),2:35],
     labels=names(dennis2[which(dennis2$date==min(dennis2$date)),2:35]),
     cex=0.1)
text(x=dennis2$date[11], y=dennis2[which(dennis2$date==dennis2$date[11]),2:35],
     labels=names(dennis2[which(dennis2$date==dennis2$date[11]),2:35]),
     cex=0.1)

library(xtable)
dennis2$date <- as.character(dennis2$date)
dennis3 <- xtable(dennis2[c(1,5,9,12,15,27)], tabular.environment="longtable", floating=FALSE)
print(dennis3)

save.image("~/workingdirectory/mosquito/mosq28oct13.RData")
write.csv(dennis2, "mosquitospecies.csv")