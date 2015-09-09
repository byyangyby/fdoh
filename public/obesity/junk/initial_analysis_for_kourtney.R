library(dplyr)

public <- '/home/joebrew/Documents/fdoh/public/obesity/data201415/'
private <- '/media/joebrew/JB/fdoh/private/obesity/data201415/'

df <- read.csv(paste0(private, 'FDOH_Obesity.csv'))
linkage <- read.csv(paste0(private, 'FDOH_Obesity_Linkage.csv'))

#####
# JOIN TOGETHER
#####
screen14 <- left_join(x = df,
                y = linkage)

# Clean up column names
names(screen14) <- c("PersonID", "Growth_Height", "Growth_Weight",
                     "BMI", "Race", "Lunch_Status", "School_Name", "Homeroom_Teacher")

############################################
######### Read in the old data
############################################
library(gdata)
#setwd("J:/Shared/progEval/obesity")
setwd("/media/joebrew/JB/fdoh/private/obesity/screen")
screen13 <- read.xls("screen2.xlsx", sheet=7)
screen12 <- read.xls("screen2.xlsx", sheet=6)
screen11 <- read.xls("screen2.xlsx", sheet=5)
screen10 <- read.xls("screen2.xlsx", sheet=4)
screen09 <- read.xls("screen2.xlsx", sheet=3)
screen08 <- read.xls("screen2.xlsx", sheet=2)
screen07 <- read.xls("screen2.xlsx", sheet=1)

############################################
######### Delete any student with multiple entries
############################################
delFun <- function(x){
  for (i in unique(x$PersonID)){
    x$rep[which(x$PersonID == i)] <- nrow(x[which(x$PersonID == i),])}
  x <- x[which(x$rep <=1),]
  x}

screen14 <- delFun(screen14)
screen13 <- delFun(screen13)
screen12 <- delFun(screen12)
screen11 <- delFun(screen11)
screen10 <- delFun(screen10)
screen09 <- delFun(screen09)
screen08 <- delFun(screen08)
screen07 <- delFun(screen07)

########
# Check column names to see if all the same
################
colnames(screen07)  == colnames(screen13)

############################################
######### Fix column names in order to match up previous years
############################################
#Fix the column names of 2013 in order to match the previous years
colnames(screen13)[13] <- "Lunch_Status"

######################
# DOWNSIZE
#######################
screen14$Homeroom_Teacher <- NULL

screen07 <- screen07[,colnames(screen14)]
screen08 <- screen08[,colnames(screen14)]
screen09 <- screen09[,colnames(screen14)]
screen10 <- screen10[,colnames(screen14)]
screen11 <- screen11[,colnames(screen14)]
screen12 <- screen12[,colnames(screen14)]
screen13 <- screen13[,colnames(screen14)]


############################################
######### Make one master df
############################################
screen <- as.data.frame(rbind(screen07,
                              screen08,
                              screen09,
                              screen10,
                              screen11,
                              screen12,
                              screen13,
                              screen14))

############################################
######### Relevel some factors
############################################
# screen$Grade <- factor(screen$Grade, levels=c("PK",
#                                               "KG",
#                                               "1",
#                                               "2",
#                                               "3",
#                                               "4",
#                                               "5",
#                                               "6",
#                                               "7",
#                                               "8",
#                                               "9",
#                                               "10",
#                                               "11",
#                                               "12"))
screen$Race <- factor(screen$Race, levels=c("W", "B", "M", "H", "A", "I"))


############################################
######### Fix missing decimals in screen$Growth_Height
############################################
screen$Growth_Height <- as.numeric(screen$Growth_Height)
screen$Growth_Height <- ifelse(screen$Growth_Height > 100,
                               screen$Growth_Height/10,
                               screen$Growth_Height)

############################################
######### Fix BMI
############################################
screen$Growth_Weight <- as.numeric(screen$Growth_Weight)
screen$bmi <- screen$Growth_Weight / ((screen$Growth_Height)^2) * 703


############################################
######### Read in data to calculate percentile values
############################################
lms <- read.csv("lms.csv")
lms <- lms[-c(1, 220),]
lms$Agemos <- as.numeric(as.character(lms$Agemos)) - 0.5
#convert all columns to numeric data
for (i in colnames(lms)){
  lms[,which(colnames(lms) == i)] <-
    as.numeric(as.character(lms[,which(colnames(lms) == i)]))}
#These data were downloaded on 6 February 2014 from 
#http://www.cdc.gov/growthcharts/percentile_data_files.htm
#good article for calculating this: http://www.cdc.gov/nchs/data/nhsr/nhsr063.pdf 
# i think the pdf has an error in the placement of the -1
#m = median 
#s = generalized coefficient of variation
#l = power in the Box-Cox transformation
#z = z-score
#x = percentile

############################################
######### Calculate percentile for age values
############################################

#boys
lms1 <- lms[which(lms$Sex == 1),]
#girls
lms2 <- lms[which(lms$Sex == 2),]

#eliminate those older than 240 months
screen <- screen[which(screen$Age_Months <= 240),]

screen$X <- screen$bmi
screen$id <- 1:nrow(screen)
screen$Z <- NA

#boys
for (i in screen$id[which(screen$Gender == "M")]){
  screen$Z[which(screen$id == i &
                   screen$Gender == "M")] <-
    (  (    screen$X[which(screen$id == i)] /
              lms1$M[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])]) ^ (
                lms1$L[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])]  )-1) /
    (lms1$L[which(lms1$Agemos==screen$Age_Months[which(screen$id == i)])]*
       lms1$S[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])])}

#girls
for (i in screen$id[which(screen$Gender == "F")]){
  screen$Z[which(screen$id == i &
                   screen$Gender == "F")] <-
    (  (    screen$X[which(screen$id == i)] /
              lms2$M[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])]) ^ (
                lms2$L[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])]  )-1) /
    (lms2$L[which(lms2$Agemos==screen$Age_Months[which(screen$id == i)])]*
       lms2$S[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])])}


#Remove extreme values using CDC's guidelines
#http://www.cdc.gov/pcd/issues/2009/jan/08_0007.htm
screen <- screen[which(is.infinite(screen$Z)== FALSE &
                         screen$Z >= -4 &
                         screen$Z <= 5 ),]


##############################
# Make a normal/oveweight/obese column
#############################
screen$cat <- factor(ifelse(screen$Z < 1.036436, 
                            "normal",
                            ifelse(screen$Z >=1.036436 & screen$Z <1.64485,
                                   "overweight",
                                   ifelse(screen$Z >=1.64485,
                                          "obese",
                                          NA))), levels=c("normal", "overweight", "obese"))
##############################
# Make a normal/oveweight (binary) column
#############################
screen$catbi <- factor(ifelse(screen$Z < 1.036436, 
                              "normal",
                              ifelse(screen$Z >=1.036436,
                                     "overweight",
                                     NA)))
###############################################################################
boxplot

hist(screen$Z[which(screen$Gender == "M")], breaks=40, col="grey", border=FALSE)
hist(screen$Z[which(screen$Gender == "F")], breaks=40, 
     col=adjustcolor("red", alpha.f=0.2), add=TRUE, border=FALSE)
barplot(c(
  nrow(screen[which(screen$Z > 1.036436 &
                      screen$Race == "W"),]) / nrow(screen[which(screen$Race == "W"),]),
  nrow(screen[which(screen$Z > 1.036436 &
                      screen$Race == "A"),]) / nrow(screen[which(screen$Race == "A"),])),
  names.arg=c("White", "Asian"), col=c("purple", "red"), border=FALSE, ylim=c(0,0.4))
abline(h=seq(0.1,1,0.1))
####
#Plot the total number of students we have in each grade
barplot(summary(screen$Grade), cex.axis=0.8, cex.names=0.9, 
        border="darkgrey", horiz=TRUE, las=1, 
        main="Cumulative students in school screening data: 2007-13",
        density=30, angle=45, col="red", cex.main=0.7)
abline(v=c(seq(1000,10000,1000)), lty=6, 
       col=adjustcolor("black", alpha.f=0.2))

#Plot summary of race
screen$Race <- factor(screen$Race, levels=c("W", "B", "M", "H", "A", "I"))
barplot(summary(screen$Race[order(screen$Race)]), border="darkgrey",
        col="blue", angle=45, density=30, main="Observations by race")

screen$fat <- factor(ifelse(screen$bmi > 20, "overweight", "normal"))
screen <- screen[which(screen$bmi < 35 & screen$bmi >14),]
boxplot(screen$bmi~screen$Race)
#save.image("J:/Shared/progEval/obesity/screen.RData")
#load("J:/Shared/progEval/obesity/screen.RData")
#save.image("E:/workingdirectory/obesity/screen.RData")
write.csv(screen, "J:/Shared/progEval/obesity/screenClean.csv")