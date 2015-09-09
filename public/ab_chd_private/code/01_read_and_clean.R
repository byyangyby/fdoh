########################################################################
# READ IN THE FOLLOWING, CLEAN AND MERGE:
###########
# THE FLSHOTS DATASET WAS SENT FROM PAUL MYERS (FDOH)
# ON 16 NOVEMBER 2014
# THE FOLLOWING ACPS DATASETS WERE SENT FROM STEVEN STARK
# (ACPS) ON 15 APRIL 2014:
#ab: absenteeism data from 11-12 to 12-13                              
#link: a document for linking student identity with the personID        
#sd: sociodemgoraphic information on each student     
########################################################################

#####
# SET WORKING DIRECTORY CONDITIONAL TO SYSTEM
#####
if ( Sys.info()["sysname"] == "Linux" ){
  private <- "/media/joebrew/JB/fdoh/private/ab_chd_private"
  public <- "/home/joebrew/Documents/fdoh/public/ab_chd_private"
} else {
  private <- "E:/fdoh/private/ab_chd_private"
  public <- "C:/Users/BrewJR/Documents/fdoh/public/ab_chd_private"
}
setwd(private) 

#####
#LOAD NECESSARY PACKAGES
#####
library(car) 
library(dplyr)
library(RCurl)

#####
#READ IN AND CLEAN UP ABSENTEEISM DATA
#####
#Read text file
ab <- read.table(paste0(private, "/data/acps/absenteeism.txt"), 
                 sep="|", header=TRUE, fill=TRUE,
                 as.is=c(TRUE, FALSE, TRUE))

#Change column names
colnames(ab) <- c("schoolYear", "personID", "date")

#Remove the summary line at end
ab <- ab[which(grepl("affected", ab$schoolYear)==FALSE),]

#Add a year column (first year of school year)
ab$year <- as.numeric(paste0("20",substr(ab$schoolYear,1,2)))

#make date an R date object
ab$date <- as.Date(ab$date, format="%Y-%m-%d")

#add week column
ab$week <- as.numeric(format(ab$date, format="%U"))

#make corrected week column (getting rid of zeroes, etc.)
ab$weekRec <- ifelse(ab$week == 0 & ab$year == 2013,
                     52,
                     ifelse(ab$week == 0 & ab$year == 2012,
                            53,
                            ab$week))
ab$week <- ifelse(is.na(ab$weekRec) == TRUE,
                  ab$week,
                  ab$weekRec)

ab$weekday <- factor(format(ab$date, "%A"),
                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))


#####
#READ IN AND CLEAN UP LINKAGE DATA
#####
#Read text file
link <- read.delim(paste0(private, "/data/acps/linkage.txt"),
                   sep="|", header=TRUE, fill=TRUE, quote="",
                   as.is=TRUE)

#Change column names
colnames(link) <- c("personID", "firstName", "middleName", "lastName", "dob")

#Remove the summary line at end
link <- link[which(grepl("affected", link$personID)==FALSE),]

#make personId numberic
link$personID <- as.numeric(link$personID)

#make dob an R date object
link$dob <- as.Date(link$dob, format="%Y-%m-%d")

# make names upper case
link$first <- toupper(link$firstName)
link$middle <- toupper(link$middleName)
link$last <- toupper(link$lastName)
link <- link[,c("personID", "first", "middle", "last", "dob")]

#####
#READ IN AND CLEAN UP SOCIODEMOGRAPHIC DATA
#####
#Read text file
sd <- read.delim(paste0(private, "/data/acps/socioDemo.txt"),
                 sep="|", header=TRUE, fill=TRUE, quote="",
                 as.is=TRUE)

#Change column names
colnames(sd) <- c("schoolYear", "personID", "lunchStatus",
                  "raceEthnicity", "schoolName", 
                  "teacherLastName", "grade")

#Remove the summary line at end
sd <- sd[which(grepl("affected", sd$schoolYear)==FALSE),]

#Add a year column (first year of school year)
sd$year <- as.numeric(paste0("20",substr(sd$schoolYear,1,2)))

#make personId numberic
sd$personID <- as.numeric(sd$personID)

#add a joeID column to account for school name spellings
mylink <- "https://docs.google.com/spreadsheets/d/1SKKU3SFEnd7CajSxLPzgHAKmR_SeuGiJx2qz_McHGlw/export?gid=0&format=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

#1314
myCsv <- getURL(mylink)
idLink <- read.csv(textConnection(myCsv), skip=0)
rm(mylink, myCsv)

#don't make grade numberic (includes PK and KG)
#PER HIDAHIS FIGUEROA MESA'S FEB 7 2014 EMAIL

#lunchStatus Key: %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#1  Applied Not Eligible
#0  Did not apply
#2	Eligible for free lunch
#6	Eligible for free lunch/Direct Certified/Decline
#9	Eligible for free lunch/Direct Certified
#3	Eligible Reduced
#4	Enrolled USDA approved Prov 2 school
#Z	Unknown

#race key %%%%%%%%%%%%%%%%%%%%%%
#W White, Non Hispanic
#B Black, Non Hispanic
#H Hispanic
#A Asian or Pacific Islander
#I American Indian or Alaskan Native
#M Multiracial

##########
#####
#NOW READ IN FL SHOTS DATA
#####
##########

shots <- read.csv(paste0(private, "/data/fl_shots_identified_full",
                         "/alachua.csv"),
                  skip = 3,
                  stringsAsFactors = FALSE)

#####
# MAKE DATE OBJECTS
#####
shots$dob <- as.Date(shots$dob, format="%m/%d/%Y")
shots$date <- as.Date(shots$date, format="%m/%d/%Y")

####
# CREATE first AND last NAME COLUMNS
####

# first name
shots$first <- gsub(".*,", "", shots$name) # Take only post-comma characters
shots$first <- gsub("^\\s+|\\s+$", "", shots$first) # Remove trail/leading spaces

# last name
shots$last <- gsub(",.*", "", shots$name) # Take only post-comma characters
shots$last <- gsub("^\\s+|\\s+$", "", shots$last) # Remove trail/leading spaces

# Add (school) year column
shots$school_year <- ifelse(shots$date >= "2010-06-01" &
                       shots$date < "2011-06-01",
                     "2010-2011",
                     ifelse(shots$date >= "2011-06-01" &
                              shots$date < "2012-06-01",
                            "2011-2012",
                            ifelse(shots$date >= "2012-06-01" &
                                     shots$date < "2013-06-01",
                                   "2012-2013",
                                   ifelse(shots$date >= "2013-06-01" &
                                            shots$date < "2014-06-01",
                                          "2013-2014",
                                          ifelse(shots$date >= "2014-06-01" &
                                                   shots$date < "2015-06-01",
                                                 "2014-2015",
                                                 NA)))))

# Add a simplified (numeric) school year
# This is the first year of the respective school year
shots$year <- as.numeric(substr(shots$school_year, 1, 4))

#####
# ASSIGN WHETHER FLU SEASON OR NOT FOR EACH ABSENCE
#####

# First, add a week variable to ab
ab$week <- as.numeric(format(ab$date, format = "%U"))

# Now, use Essence to define ranges for each flu season
ts <- read.csv(paste0(private, "/data/essence/tsWeekRegion3.csv"))

# figure out first and last week for which 
#% of emergency visits for ILI was > 1.9% of all visits
#2011-12 - weeks 49-52 || 2011-12-04: 2011-12-31
# ts$week[which(ts$season2011.2012per >= 0.019)]
# #2012-13 - weeks 47 through 9 || 2012-11-18 : 2013-03-09
# ts$week[which(ts$season2012.2013per >= 0.019)]
# #2013-14 - weeks 46 through 6 || 2014-11-17 : 2014-02-15
# ts$week[which(ts$season2013.2014per >= 0.019)] 

# Define exactly which absences are flu season or not
ab$flu_season <- ifelse(ab$date >= "2011-12-04" &
                          ab$date <= "2011-12-31", TRUE,
                        ifelse(ab$date >= "2012-11-18" &
                                 ab$date <= "2013-03-09",
                               TRUE,
                               ifelse(ab$date >= "2013-11-17" &
                                        ab$date <= "2014-02-15",
                                      TRUE,
                                      FALSE)))

###########################
# ALL DATA HAS BEEN CLEANED. NOW MERGE:
###########################

#####
# MERGE LINK TO SD
#####
dat <- left_join(x = sd,
                 y = link,
                 by = "personID")
dat$id <- dat$personID

#####
# MERGE idLink TO DAT
#####
dat <- left_join(x = dat,
                 y = idLink,
                 by = "schoolName")
#####
# ADD ABSENTEEISM INFORMATION
#####

# Create temp dataframe of total absences and merge to dat
temp <- ab %>%
  group_by(year, personID) %>%
  summarise(total_absences = n())

dat <- left_join(x = dat,
                 y = temp)
rm(temp)

# Create a temp dataframe of only flu season absences and merge to dat
temp <- ab %>%
  filter(flu_season) %>%
  group_by(year, personID) %>%
  summarise(flu_season_absences = n())

dat <- left_join(x = dat,
                 y = temp)

# NA's in absences should be 0 
dat$total_absences[is.na(dat$total_absences)] <- 0
dat$flu_season_absences[is.na(dat$flu_season_absences)] <- 0

#####
# ADD IMMUNIZATION DATA
#####

# First, clean dat so that names are standard with shots 
# (no dashes, no spaces)
dat$last <- gsub(" |-|'", "", dat$last)
shots$last <- gsub(" |-|'", "", shots$last)
dat$first <- gsub(" |-|'", "", dat$first)
shots$first <- gsub(" |-|'", "", shots$first)

# Create a full name column
# dat$full_name <- paste0(substr(dat$first,1,4), " ", 
#                         dat$last, " ", dat$year, " ",
#                         dat$dob)
# shots$full_name <- paste0(substr(shots$first, 1, 4), " ", 
#                           shots$last, " ", shots$year, " ",
#                           shots$dob)

# Create a match_name (first and last name only)
dat$match_name <- paste(dat$first, dat$last)
shots$match_name <- paste(shots$first, shots$last)

# Assign all imms to TRUE in shots
shots$imm <- TRUE

# Merge directly
# shots_small <- shots[,c("type", "admin", "full_name", "imm", "year", "dob")]
# dat <- left_join(x = dat,
#                  y = shots_small,
#                  by = c("full_name", "year", "dob"))

# Conditional to decide if running fuzzy is needed or not
setwd(paste0(private, "/data/matching"))

if("fuzzy_matched_names.csv" %in% dir()){
  fuzzy_matched_names <- read.csv("fuzzy_matched_names.csv",
                                  stringsAsFactors = FALSE)
} else{
  # Since names aren't perfect, write a function allowing
  # for somewhat fuzzy matching
  
  Fuzzy <- function(row_of_shots,
                    fuzzy_threshold = 4){ # 3 is good
    
    # Get name from shots
    shots_name <- shots$match_name[row_of_shots]
    
    # Subset dat only to those from that year with same dob
    dat_names <- dat$match_name[which(dat$year == shots$year[row_of_shots] &
                                        dat$dob == shots$dob[row_of_shots])]
    
    # If there's nobody with right dob and year, return NA
    if(length(dat_names) == 0){
      best_name <- NA
    } else{
      
      # Condtional to take perfect name
      if(shots_name %in% dat_names){
        
        best_ind <- which(dat_names  == shots_name)[1]
        mat <- 0
        
      } else{
        
        # Create matrix of match scores
        mat <- adist(x = shots_name,
                     y = dat_names,
                     ignore.case = TRUE)
        
        # What is index of best match in dat_small
        best_ind <- which.min(mat)[1]
      }
      
      # Return NA if fuzzy_threshold is met
      if(min(mat) > fuzzy_threshold){
        best_name <- NA
      } else{
        # What is the best name from dat
        best_name <- dat_names[best_ind][1] # only first
      }
    }  
    
    return(best_name)
  }
  
  
  # Create fuzzy_matched_names
  fuzzy_matched_names <- data.frame("shots_name" = shots$match_name,
                                    "dat_name" = NA)
  # Get best dat name for each name in shots
  for (i in 1:nrow(fuzzy_matched_names)){
    fuzzy_matched_names$dat_name[i] <-
      Fuzzy(i)
  }
  # Write a csv of fuzzy_matched_names
  write.csv(fuzzy_matched_names, "fuzzy_matched_names.csv")
}

# MERGE fuzzy_matched_names to shots
shots <- cbind(shots, fuzzy_matched_names)

# Create a small shots df just for merging
shots_small <- shots[,c("type", "admin", "dat_name", "imm", "year", "dob")]

#############
#Merge shots data into dat
#############
dat$dat_name <- dat$match_name
dat <- left_join(x = dat,
                 y = shots_small,
                 by = c("dat_name", "year", "dob"))

# If imm == NA, FALSE
dat$imm[which(is.na(dat$imm))] <- FALSE

# Remove duplicate rows
dat$name_year_dob <- paste(dat$dat_name, dat$year, dat$dob)
#dat$name_year_dob <- paste(dat$full_name, dat$year, dat$dob)

dat <- dat[which(!duplicated(dat$name_year_dob)),] 
# results in some loss of unclear data points ^^

# Clean up columns
dat <- dat[,c("first", "middle", "last", "year", "dob", "id", "schoolName",
              "teacherLastName", "grade", "idJoe", "total_absences",
              "flu_season_absences", "type", "admin", "imm",
              "raceEthnicity", "lunchStatus", "dat_name")]

#####
# CREATE age_group
# This will be prek, elem, mid, or high
# depending on grade level
#####
dat$age_group <- factor(ifelse(grepl("KG|01|02|03|04|05", dat$grade) == TRUE,
                          "elem",
                          ifelse(grepl("06|07|08", dat$grade) == TRUE,
                                 "mid",
                                 ifelse(grepl("09|10|11|12", dat$grade) == TRUE,
                                        "high",
                                        ifelse(dat$grade == "PK",
                                               "PK",
                                               NA)))))


#####
# RECODE THE lunch VARIABLE
# (for all this recoding, refer to the pdf I sent you
#  for explanation of the different variables)
#####
dat$lunch <- factor(Recode(dat$lunchStatus,
                              "'1' = 'not_free';
                              '0' = 'not_free';
                              '2' = 'free';
                              '6' = 'free';
                              '9' = 'free';
                              '3' = 'free'"))
dat$lunchStatus <- NULL

#####
# 6. RECODE THE RACE VARIABLE INTO A BINARY
#    WHITE / NON-WHITE VARIABLE
#####
dat$race_bi <- factor(ifelse(dat$raceEthnicity == "W",
                             "white",
                             "non_white"))
dat$race <- Recode(dat$raceEthnicity,
                   "'W' = 'White';
                   'B' =  'Black';
                   'H' = 'Hispanic';
                   'A' = 'Asian';
                   'I' = 'Native Amer.'; 
                   'M' =  'Multiracial'")

dat$raceEthnicity <- NULL

#####
# CREATE THE FOLLOWING COLUMNS
#    a. PERCENT ABSENT DURING FLU SEASON (ab_flu_per)
#    b. PERCENT ABSENT DURING NON-FLU SEASON
#####

#IN ORDER TO DO THIS, WE'LL FIRST NEED TO CALCULATE THE NUMBER 
#OF SCHOOL DAYS IN EACH YEAR'S FLU SEASON (AND NON-FLU SEASON).
##########

#2011 || WEEKS 49-52 || Dec 04, 2011 - Dec 31, 2011
daysFluSeason11 <- 10 
daysNonFluSeason11<- 180- daysFluSeason11

#2012 || WEEKS 47-9 || Nov 18, 2012 - Mar 10, 2013
daysFluSeason12 <- 63 
daysNonFluSeason12 <-180- daysFluSeason12

#2013 || WEEKS 46-6 || Nov 17, 2013 - Feb 15, 2013
#http://goo.gl/iHH1mO
daysFluSeason13 <- 48 
daysNonFluSeason13<-180-36- daysFluSeason13

#####
# CREATE COLUMNS FOR NUMBER OF FLU, NON-FLU, 
# TOTAL DAYS FOR EACH  SCHOOL YEAR
#####

# flu_days
dat$flu_days <- NA
dat$flu_days[which(dat$year == 2011)] <- daysFluSeason11
dat$flu_days[which(dat$year == 2012)] <- daysFluSeason12
dat$flu_days[which(dat$year == 2013)] <- daysFluSeason13

#non_flu_days
dat$non_flu_days <- NA
dat$non_flu_days[which(dat$year == 2011)] <- daysNonFluSeason11
dat$non_flu_days[which(dat$year == 2012)] <- daysNonFluSeason12
dat$non_flu_days[which(dat$year == 2013)] <- daysNonFluSeason13

#total_days
dat$total_days <- dat$flu_days + dat$non_flu_days

# Remove extra objects
rm(daysFluSeason11,
   daysFluSeason12,
   daysFluSeason13,
   daysNonFluSeason11,
   daysNonFluSeason12,
   daysNonFluSeason13)

#####
# POPULATE ab_flu_per WITH THE PERCENTAGE OF DAYS
# THAT A STUDENT WAS ABSENT FOR THE TIME PERIOD IN QUESTION
#####
dat$ab_flu_per <- dat$flu_season_absences / dat$flu_days

#####
# POPULATE ab_non_flu_per 
#####
dat$non_flu_season_absences <- dat$total_absences - dat$flu_season_absences
dat$ab_non_flu_per <- dat$non_flu_season_absences / dat$non_flu_days  

#####
# CREATE A COLUMN FOR NUMBER OF FLU SEASON PRESENCES
#####

# absolute
dat$flu_season_presences <- dat$flu_days - dat$flu_season_absences

# proportion
dat$pres_flu_per <- dat$flu_season_presences / dat$flu_days

#####
# REMOVE OUTLIERS, PREK STUDENTS, AND FAULTY DATA POINTS
#####
dat <- dat[which(dat$ab_non_flu_per < sd(dat$ab_non_flu_per)*2 + 
                   mean(dat$ab_non_flu_per) &
                   dat$grade != "PK" &
                   dat$ab_flu_per <= 1),] 

#####
# FILL IN SOME NA'S
#####
dat$type[is.na(dat$type)] <- "NONE"

#####
# RELEVEL SOME FACTORS FOR APPROPRIATE BASELINES
#####
dat$type <- factor(dat$type, 
                   levels = c("NONE", "LAIV", "TIV"))

#####
# DEFINE AGE GROUPS
#####

# Create an age on Dec 31 variable
dat$age <- NA
# Function for getting age on dec 31 of that school year
AgeFun <- function(year){
  as.Date(paste0(year, "-12-31"), format = "%Y-%m-%d") -
    dat$dob[which(dat$year == year)]
}
# Assign age to each year
for (i in 2011:2013){
  dat$age[which(dat$year == i)] <- 
    AgeFun(i) / 365.25
}
# # Assign age groups
# dat$age_group <- factor(ifelse(dat$age >= 2 & 
#                                  dat$age < 9,
#                                "young",
#                                ifelse(dat$age >=9 &
#                                         dat$age < 19,
#                                       "old",
#                                       "other")))

#####
# REMOVE THOSE FROM "OTHER" AGE GROUP
#####
dat <- dat[which(dat$age_group != "other"),]

# CALCULATE OVERALL IMMUNIZATION RATES
# FOR PARKER
#source(paste0(public, "/code/01b_overall_immunization_rate.R"))


# Remove junk
rm(sd, link, AgeFun, idLink, temp, ts, ab, shots_small, fuzzy_matched_names)

#####
# CLEAN UP SHOTS FOR COUNTY-WIDE ANALYSIS
#####
# 
# # Merge dat into shots so that I have all immunizations in one
# # (including relevant school information)
# 
# 
# dat_small <- dat[,c("year", "dob", "id", "schoolName", "teacherLastName", 
#                     "grade", "idJoe", "total_absences", "flu_season_absences",
#                     "type", "admin", "imm", "dat_name", "age_group",
#                     "lunch", "race_bi", "race", "flu_days", "non_flu_days",
#                     "total_days", "ab_flu_per", "non_flu_season_absences",
#                     "ab_non_flu_per", "flu_season_presences", "pres_flu_per")]
# shots_small <- shots[,c("dob", "date", "age", "type", "admin", "school_year",
#                         "year", "shots_name", "imm", "dat_name")] 
# shots <- left_join(x = shots_small,
#                  y = dat_small,
#                  by = c("dat_name", "year", "dob", "type", "admin", "imm"))
# rm(dat_small, shots_small)
# 
# # Mark shots for in school roster or not
# shots$student <- !is.na(shots$id)

# Function for getting age on dec 31 of that school year
AgeFun <- function(year){
  as.Date(paste0(year, "-12-31"), format = "%Y-%m-%d") -
    shots$dob[which(shots$year == year)]
}

# Assign age to each year
for (i in 2010:2014){
  shots$age[which(shots$year == i)] <- 
    AgeFun(i) / 365.25
}

# CREATE age_group
# This will be prek, elem, mid, or high
# depending on grade level
shots$age_group <- factor(ifelse(shots$age <= 5.5, "PK",
                                 ifelse(shots$age <= 11.5, "elem",
                                        ifelse(shots$age <= 14.5, "mid",
                                               ifelse(shots$age <=18.5, "high",
                                                      NA)))))

# READ IN CENSUS AGE DATA
setwd(paste0(public, "/data"))
pop <- read.csv("florida_details.csv", skip = 1)
names(pop) <- gsub("[.]", "", names(pop))
pop <- tbl_df(pop)

# Get the age distribution for Alachua County
age_dist <- pop %>% 
  filter(RaceEthnicity == "Total" &
           AreaName == "Alachua County") %>%
  select(TotalUnder1Year:Total110YearsandOlder)

names(age_dist)[c(1:2, 101:103)] <- 
  c("Total0Years", "Total1Years",
    "Total100-104Years", "Total105-109Years", "Total110+Years")

# GET NUMBERS OF EACH AGE
ages <- data.frame(t(age_dist))
names(ages) <- c("n")
ages$group <- row.names(ages)
ages$group <- as.numeric(substr(gsub("Total|Years|-|[+]", "", ages$group), 1,3))

rm(age_dist, pop)

# REMOVE DUPLICATES FROM SHOTS
shots$name_dob_year <- paste0(shots$match_name,
                              shots$dob,
                              shots$year)

shots <- shots[!duplicated(shots$name_dob_year),]

#####
# GET NUMBER IMMUNIZED FOR 2014/15 BY EACH AGE GROUP
#####
county_wide <- shots %>%
  group_by(age_group, year, type, admin) %>%
  summarise(LAIV = sum(type == "LAIV"),
            TIV = sum(type == "TIV"))

# Get 

# Add denominator
county_wide$denom <- NA
county_wide$denom[which(county_wide$age_group == "PK")] <- sum(ages$n[which(ages$group <=5)])
county_wide$denom[which(county_wide$age_group == "elem")] <- sum(ages$n[which(ages$group >5 &
                                                            ages$group <=11)])
county_wide$denom[which(county_wide$age_group == "mid")] <- sum(ages$n[which(ages$group >11 &
                                                           ages$group <=14)])
county_wide$denom[which(county_wide$age_group == "high")] <- sum(ages$n[which(ages$group >14 &
                                                            ages$group <=18)])

# Get TOTAL IMM and PERCENTAGE values
#total imm
county_wide$imm <- county_wide$LAIV + county_wide$TIV

#p
county_wide$LAIVp <- county_wide$LAIV / county_wide$denom * 100
county_wide$TIVp <- county_wide$TIV / county_wide$denom * 100
county_wide$immp <- county_wide$imm / county_wide$denom * 100

rm(ages, i, AgeFun)

#####
# READ IN INTERN-ENTERED DATA
#####

# Also read in private intern data from cuc
cuc <- read.csv(paste0(private, "/data/cucs_interns/cucs_interns_data.csv"))

#####
# Clean up both dat and cuc a bit
#####
dat$personID <- dat$id
dat$id <- NULL
cuc$X.1 <- NULL

#####
# MERGE CUC'S CFRR DATA TO ALACHUA DATA
#####
dat <- left_join(x = dat,
                 y = cuc,
                 by = c("personID", "year"))

######
# CLEAN UP SOME FACTORS
######

# Relevel grade
dat$grade <- factor(dat$grade,
                    levels = c("KG", "01", "02", "03", "04",
                               "05", "06", "07", "08", "09",
                               "10", "11", "12"))

# Relevel race
dat$race <- factor(dat$race,
                   levels = c("White", "Asian", "Hispanic",
                              "Black", "Multiracial", "Native Amer."))

# Relevel lunch
dat$lunch <- factor(dat$lunch,
                    levels = c("not_free", "free"))

#####
# SAVE IMAGE FOR NEXT PHASE
#####
save.image(paste0(private, "/data/images/01_read_and_clean.RData"))

