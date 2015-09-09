###################
# PACKAGES
###################
library(dplyr)
library(rgdal)

###################
#SET DATE / TIME PARAMETERS
###################
today <- Sys.Date() 
yesterday <- today - 1

###################
#DEFINE AND SET WD
###################
if ( Sys.info()["sysname"] == "Linux" ){
  # Joe's linux
  if(Sys.info()["user"] == "joebrew"){
    private <- "/media/joebrew/JB/fdoh/private/emergency_room"
    private_historical <- "/media/joebrew/JB/fdoh/private/surv/historical"
    public <- "/home/joebrew/Documents/fdoh/public/emergency_room"
    public_gis <- "/media/joebrew/JB/fdoh/private/surv/gis"
  }
  # Joe's Windows computers:
} else {
  private <- "E:/fdoh/private/emergency_room"
  private_historical <- "E:/fdoh/private/surv/historical"
  public <- "C:/Users/BrewJR/Documents/fdoh/public/emergency_room"
  public_gis <- "E:/fdoh/private/surv/gis"
}

# 
# #####
# # DEFINE FUNCTION FOR GETTING LINK TO CORRECT DATA
# #####
# setwd(public)
# source("get_link.R")
# 
# #####
# # GET LINK FOR DATA GOING BACK A LONNNNGGG TIME
# #####
# link <- get_link(syndrome = NULL,
#                  patient_location = "alachua",
#                  hospital_location = "alachua",
#                  start_date = "2009-01-01",
#                  end_date = "2015-01-21",
#                  text = TRUE)
# browseURL(link)
# good_link <- "https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&startDate=1Jan2009&medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&endDate=21Jan2015&dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&geography=alachua&patientLoc=alachua&age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&medicalGrouping=all&timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all"


#########
# READ IN ALACHUA COUNTY RESIDENTS ALL SYNDROMES DATA
#########
# SET WD
setwd(private_historical) 
# READ DATA
dat <- read.csv("alless_old_updated.csv",
                stringsAsFactors = FALSE)

# Remove non-Alachua county visits
dat <- dat[which(dat$Region.of.the.Hospital == "Alachua"),]

#####
# CLEAN UP SOME STRINGS
#####
dat$Date <- as.Date(dat$Date,
                    format = "%Y-%m-%d")

#####
# READ IN AND CLEAN UP SHAPEFILES
#####
# Setwd to where I have shapefiles
setwd(public_gis)

# Read in Alachua shapefiles
zip <- readOGR("alachuazipcodes", "ACDPS_zipcode")

# Divide Alachua County into EAST/WEST
cut_off <- 2655000
plot(zip)
abline(v = cut_off, 
       col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)

# Get a df version of zip
zip_df <- data.frame(zip)
zip_df$ZIP <- as.numeric(as.character(zip_df$ZIP))
zip_df$Zipcode <- zip_df$ZIP

# flag and color eastside counties
zip_df$x <- coordinates(zip)[,1]
zip_df$y <- coordinates(zip)[,2]
zip_df$east <- ifelse(zip_df$x > cut_off, TRUE, FALSE)
zip_df$color <- ifelse(zip_df$east, "red", "grey")

# Confirm good assignment through choropleth
plot(zip, col = zip_df$color)

#####
# MERGE zip_df INTO dat
#####
dat <- left_join(x = dat,
                 y = zip_df,
                 by = "Zipcode")

#####
# DEFINE SOME CCDD-RELATED VARS
#####

# First, make all CCDD capitalized
dat$CCDD <- toupper(dat$CCDD)

# DEFINE DIABETES
diab <- "DIAB|INSULI|SUGAR|MELLITU|FASTI|GLYCEMI"
dat$diab <- grepl(diab, dat$CCDD)


# GET DIABETES VISITS BY DAY
x <- dat %>% group_by(Date, diab) %>% summarise(diabetes = n())
x <- x[which(x$diab),]

# GET EASTSIDE VISITS BY DAY
y <- dat %>% group_by(Date, east) %>% summarise(eastside = n())
y <- y[which(y$east),]

# GET BLACK VISITS BY DAY
z <- dat %>% group_by(Date, race) %>% summarise(black = n())
z <- z[which(z$race == "black"),]

# GET TOTAL VISITS BY DATA
tot <- dat %>% group_by(Date) %>% summarise(total = n())

# MERGE TOGETHER
ts <- left_join(x = x,
                y = y)
ts <- left_join(x = ts,
                y = z)
ts <- left_join(x = ts,
                y = tot)
ts <- ts[,c("Date", "diabetes", "eastside", "black")]


