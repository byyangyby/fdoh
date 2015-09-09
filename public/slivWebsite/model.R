#TWO STEPS TO MODELLING
#1. CREATE THE GENERIC MATHEMATICAL/ALGEBRAIC MODEL 
#2. APPLY IT TO THE STATE (AND TO ALACHUA) AT DIFFERING LEVELS
##############################
##############################
##############################
##############################


#
myLink <- "https://docs.google.com/spreadsheets/d/1-o_HtdEK9ppCT5eU0YteKW2D7imtNgJY601y8LcozUc/export?gid=1752249144&format=csv"

library(classInt)
library(RCurl)
library(maps)
library(xtable)
library(maps)
library(RColorBrewer)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


myCsv <- getURL(myLink)
frLunch <- read.csv(textConnection(myCsv), skip=0)

##############################
##############################
##############################
##############################
##############################
##############################
##1. write the model (with personnel costs)
##############################




#COSTS ONLY
CostFun <- function(totMem, 
                    vfcPer,
                    immRate,
                    privateVacCost,
                    delivCost,
                    billCost,
                    printCost,
                    storageCost,
                    nursePerHour,
                    randp){
  
  kidsVaccinated <-totMem*immRate/100
  vfcVaccines <- kidsVaccinated*vfcPer/100
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  otherCost <- (delivCost + billCost + printCost + storageCost) * kidsVaccinated 
  privateCost <- privateVacCost *privateVaccines
  
  nurseCost <- 0.95*kidsVaccinated
  
  #nurseCost <- (exp(3.26427 - (0.0038299*kidsVaccinated))) * 
  # kidsVaccinated / 60 * nursePerHour
  
  randpCost <- randp*totMem
  
  nurseCost + otherCost + randpCost + privateCost 
}

# GROSS REVENUE ONLY
RevFun <- function(totMem,
                   vfcPer,
                   immRate,
                   privDenRate,
                   privUnbillableRate,
                   vfcDenRate,
                   privAvgRe,
                   vfcAvgRe){
  vfcVaccines <- totMem*vfcPer/100*immRate/100
  
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  
  vfcRev <- vfcVaccines * vfcAvgRe * (1-(vfcDenRate/100))
  
  privateRev <- privateVaccines * privAvgRe * 
    (1-(privDenRate/100)) * (1-(privUnbillableRate/100))
  
  vfcRev + privateRev
}

# VFC REVENUE ONLY
vfcRevFun <- function(totMem,
                      vfcPer,
                      immRate,
                      privDenRate,
                      privUnbillableRate,
                      vfcDenRate,
                      privAvgRe,
                      vfcAvgRe){
  vfcVaccines <- totMem*vfcPer/100*immRate/100
  
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  
  vfcRev <- vfcVaccines * vfcAvgRe * (1-(vfcDenRate/100))
  
  privateRev <- privateVaccines * privAvgRe * 
    (1-(privDenRate/100)) * (1-(privUnbillableRate/100))
  
  vfcRev 
}

# PRIVATE REVENUE ONLY
privateRevFun <- function(totMem,
                          vfcPer,
                          immRate,
                          privDenRate,
                          privUnbillableRate,
                          vfcDenRate,
                          privAvgRe,
                          vfcAvgRe){
  vfcVaccines <- totMem*vfcPer/100*immRate/100
  
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  
  vfcRev <- vfcVaccines * vfcAvgRe * (1-(vfcDenRate/100))
  
  privateRev <- privateVaccines * privAvgRe * 
    (1-(privDenRate/100)) * (1-(privUnbillableRate/100))
  
  privateRev 
}



#VAC PURCHASE
VacFun <- function(totMem,
                   vfcPer,
                   immRate,
                   privateVacCost){
  
  kidsVaccinated <-totMem*immRate/100
  
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  
  privateCost <- privateVacCost *privateVaccines
  
  privateCost 
  
}


#COSTS NOT COUNTING VACCINE PURCHASE
OtherCostFun <- function(totMem, 
                         vfcPer,
                         immRate,
                         delivCost,
                         billCost,
                         printCost,
                         storageCost,
                         nursePerHour,
                         randp){
  
  kidsVaccinated <-totMem*immRate/100
  
  otherCost <- (delivCost + billCost + printCost + storageCost) * kidsVaccinated 
  
  nurseCost <- 0.95*kidsVaccinated
  
  
  #nurseCost <- (exp(3.26427 - (0.0038299*kidsVaccinated))) * 
  # kidsVaccinated / 60 * nursePerHour
  
  randpCost <- randp*totMem
  
  nurseCost + otherCost + randpCost 
}


#%%%%%%%%%%%%%%%%%% SANDBOX
CostFun(totMem = 1000, 
        vfcPer = 75,
        immRate = 60,
        privateVacCost = 17.5,
        delivCost = .05,
        billCost = 5.23,
        printCost = .1,
        storageCost = .05,
        nursePerHour = 25, 
        randp = 1
) 

VacFun(totMem = 1000, 
       vfcPer = 75,
       immRate = 60,
       privateVacCost = 17.5)

OtherCostFun(totMem = 1000, 
             vfcPer = 75,
             immRate = 60,
             delivCost = .05,
             billCost = 5.23,
             printCost = .1,
             storageCost = .05,
             nursePerHour = 25,
             randp =1)

RevFun(totMem = 1000, 
       vfcPer = 75,
       immRate = 60,
       privDenRate = 12.5,
       privUnbillableRate = 5.23,
       vfcDenRate = 20,
       privAvgRe = 39.3,
       vfcAvgRe = 5)

vfcRevFun(totMem = 1000, 
          vfcPer = 75,
          immRate = 60,
          privDenRate = 12.5,
          privUnbillableRate = 5.23,
          vfcDenRate = 20,
          privAvgRe = 39.3,
          vfcAvgRe = 5)

privateRevFun(totMem = 1000, 
              vfcPer = 75,
              immRate = 60,
              privDenRate = 12.5,
              privUnbillableRate = 5.23,
              vfcDenRate = 20,
              privAvgRe = 39.3,
              vfcAvgRe = 5)



#%%%%%%%%%%%%%%%% END SANDBOX





##############################
### Read in 2013/14 free lunch data
##############################
#frLunch <- read.csv("frLunch13.csv")
#Make numeric columns into numeric R objects
frLunch$free <- as.numeric(gsub(",|#" ,"", frLunch$free))
frLunch$totMem <- as.numeric(gsub(",|#" ,"", frLunch$totMem))
frLunch$reduced <- as.numeric(gsub(",|#" ,"", frLunch$reduced))

##############################
### makes dfSchool and ADD APPROPRIATE COLUMNS
##############################
dfSchool <- frLunch[,c("district", "id", "School", "totMem", "free",
                       "reduced", "frper")]
dfSchool$frper <- as.numeric(as.character(dfSchool$frper))

dfSchool$district <- as.character(dfSchool$district)
#We want to use frPer as a PROXY indicator for the percentage of 
#students who are going to receive the VFC vaccine for each county
#frPer for Alachua = 49.026091
#vfcPer for Alachua = 41.78791  5773 / (5773+8042) * 100
# 41.78791 / 49.026091  = 0.8523606
#Therefore, dfCounty$vfcPer (our estimate) = 0.8523606*dfCounty$frPer
##############################
### Make vfcPer
##############################
dfSchool$vfcPer <- 0.8523606*dfSchool$frper


##############################
### Make dfSchool2
##############################
#dfSchool2 <- dfSchool[,c("district",
#                         "id", 
#                         "School",
#                         "totMem",
#                         "vfcPer")]

#dfSchool2$priv <- 100 - dfSchool2$vfcPer

#colnames(dfSchool2) <-
#  c("District", "ID", "School", "Students", "Percent VFC", "Percent Private")

#dfSchool2 <- dfSchool2[order(dfSchool2$School),]



#################MAP TIME

myMap <- map("county", "florida")
myMap$county <- toupper(gsub("florida,|:main|:spit", "",myMap$names))
myMap$county <- gsub("ST", "ST.", myMap$county)
myMap$county <- gsub("DE SOTO", "DESOTO", myMap$county)
myMap$county <- gsub("GILCHRIST.", "GILCHRIST", myMap$county)

myMap$color <- ifelse(myMap$county == "ALACHUA",
                      "blue",
                      "grey")

map("county", "florida", border="darkgrey", fill=TRUE, col=myMap$color)


####################################
# WEYCKER REDUCTIONS
####################################

#### READ IN WEYCKER DATA
myLink2 <- "https://docs.google.com/spreadsheets/d/1Hun4-6-5oKeWcfFjEMkueb75_AWc3lcV9DBIX60K3wM/export?gid=0&format=csv"
myLink3 <- "https://docs.google.com/spreadsheets/d/12Iv72owJCLEb3bH2djOaQDMXmVZSOIWURLAIZOcbzN4/export?gid=0&format=csv"

#WEYCKER DATA
myCsv <- getURL(myLink2)
weycker <- read.csv(textConnection(myCsv), skip=0)

#FLORIDA POPULATION DATA
myCsv <- getURL(myLink3)
fl <- read.csv(textConnection(myCsv), skip=0)

#CLEAN UP fl
fl$pop <- as.numeric(as.character(gsub(",","", fl$pop)))
fl$county <- toupper(fl$county)

#CALCULATE PER COLUMNS FOR REDUCTIONS
weycker$casesPer <- weycker$cases/ weycker$cases[1]
weycker$hospitalizationsPer <- weycker$hospitalizations/ weycker$hospitalizations[1]
weycker$deathsPer <- weycker$deaths/ weycker$deaths[1]
weycker$directCostsPer <- weycker$directCosts/ weycker$directCosts[1]
weycker$indirectCostsPer <- weycker$indirectCosts/ weycker$indirectCosts[1]

weycker$unImmRate <- 100-weycker$immRate

# CALCULATE THE RELATIONSHIP BETWEEN IMMRATE AND REDUCTIONS
immTable <- as.data.frame(1:105)
colnames(immTable) <- "immRate"

immTable$immRateSliv <- c(0,0,0,0,0,1:100)

#cases
casesTemp <- approx(x = weycker$immRate, y = weycker$casesPer, n=105)
immTable$cases <- casesTemp$y

#hospitalizations
hospitalizationsTemp <- approx(x = weycker$immRate, y = weycker$hospitalizationsPer, n=105)
immTable$hospitalizations <- hospitalizationsTemp$y

#deaths
deathsTemp <- approx(x = weycker$immRate, y = weycker$deathsPer, n=105)
immTable$deaths <- deathsTemp$y

#directCosts
directCostsTemp <- approx(x = weycker$immRate, y = weycker$directCostsPer, n=105)
immTable$directCosts <- directCostsTemp$y

#indirectCosts
indirectCostsTemp <- approx(x = weycker$immRate, y = weycker$indirectCostsPer, n=105)
immTable$indirectCosts <- indirectCostsTemp$y


#CALCULATE CURRENT ESTIMATED BURDEN OF FLU

#florida's population as % of us
flPerPop <- 19.32 / 313.9
#population increase since Weycker artilce
popIncrease <- 317/295
#inflation since Weycker article
inflation <- 1.36

#add a per column to fl
fl$per <- fl$pop / sum(fl$pop)

#CASES
fl$cases <- fl$per * flPerPop*popIncrease*weycker$cases[which(weycker$immRate == 0)] * 1000 #weycker = cases in thousands

#hospitalizations
fl$hospitalizations <- fl$per * flPerPop * popIncrease * weycker$hospitalizations[which(weycker$immRate == 0)]

#deaths
fl$deaths <- fl$per * flPerPop * popIncrease * weycker$deaths[which(weycker$immRate == 0)]

#indirectCosts
fl$indirectCosts <- fl$per * flPerPop * popIncrease * inflation * 
  weycker$indirectCosts[which(weycker$immRate == 0)] * 1000000

#directCosts
fl$directCosts <- fl$per * flPerPop * popIncrease * inflation * 
  weycker$directCosts[which(weycker$immRate == 0)] * 1000000


########### WRITE FUNCTIONS TO CALCLUATE VARIOUS SAVINGS AT VARIOUS IMMUNIZATION RATES

#CASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CasesFun <- function(county, immRate){
  
  #a - b = cases averted
  
  #a. cases at 5% immunization level
  fl$cases[which(fl$county == county)] - 
    
    #b. cases at new immunization level
    (fl$cases[which(fl$county == county)] * 
       immTable$cases[which(immTable$immRateSliv == immRate)][1])
  
  
}

#CasesFun("ALACHUA", 100)

#Hospitalizations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
HospitalizationsFun <- function(county, immRate){
  
  #a - b = cases averted
  
  #a. cases at 5% immunization level
  fl$hospitalizations[which(fl$county == county)] - 
    
    #b. cases at new immunization level
    (fl$hospitalizations[which(fl$county == county)] * 
       immTable$hospitalizations[which(immTable$immRateSliv == immRate)][1])
  
  
}

#HospitalizationsFun("ALACHUA", 100)



#DEATHS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DeathsFun <- function(county, immRate){
  
  #a - b = cases averted
  
  #a. cases at 5% immunization level
  fl$deaths[which(fl$county == county)] - 
    
    #b. cases at new immunization level
    (fl$deaths[which(fl$county == county)] * 
       immTable$deaths[which(immTable$immRateSliv == immRate)][1])
  
  
}

#DeathsFun("ALACHUA", 100)


#indirectCosts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
IndirectCostsFun <- function(county, immRate){
  
  #a - b = cases averted
  
  #a. cases at 5% immunization level
  fl$indirectCosts[which(fl$county == county)] - 
    
    #b. cases at new immunization level
    (fl$indirectCosts[which(fl$county == county)] * 
       immTable$indirectCosts[which(immTable$immRateSliv == immRate)][1])
  
  
}

#IndirectCostsFun("ALACHUA", 60)


#directCosts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DirectCostsFun <- function(county, immRate){
  
  #a - b = cases averted
  
  
  #a. cases at 5% immunization level
  fl$directCosts[which(fl$county == county)] - 
    
    #b. cases at new immunization level
    (fl$directCosts[which(fl$county == county)] * 
       immTable$directCosts[which(immTable$immRateSliv == immRate)][1])
  
  
}

#DirectCostsFun("ALACHUA", 60)