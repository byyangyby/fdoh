setwd("/media/joebrew/JB/fdoh/private/vital_stats")
temp <- list.files(pattern="*.csv") # get all csvs
temp <- temp[which(grepl("births", temp))] # only births
temp <- temp[21:35] # get only years since 2000
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #read in all csvs
all_births <- list(
  births2000.csv,
  births2001.csv,
  births2002.csv,
  births2003.csv,
  births2004.csv,
  births2005.csv,
  births2006.csv,
  births2007.csv,
  births2008.csv,
  births2009.csv,
  births2010.csv,
  births2011.csv,
  births2012.csv,
  births2013.csv,
  births2014.csv)
births <- data.frame(do.call(rbind, all_births))

rm(temp,
   births2000.csv,
   births2001.csv,
   births2002.csv,
   births2003.csv,
   births2004.csv,
   births2005.csv,
   births2006.csv,
   births2007.csv,
   births2008.csv,
   births2009.csv,
   births2010.csv,
   births2011.csv,
   births2012.csv,
   births2013.csv,
   births2014.csv,
   all_births,
   i,
   public,
   private)
births <- births[,c("EVENT_YEAR",
                    "DATE_OF_BIRTH",
                    "MOTHER_RES_ZIP",
                    "MOTHER_AGE",
                    "MOTHER_RES_COUNTY",
                    "MOTHER_EDCODE",
                    "MOTHER_MARRIED",
                    "BIRTH_WEIGHT_GRAMS",
                    "MOTHER_WIC_YESNO",
                    "PRINCIPAL_SRCPAY_CODE",
                    "MOTHER_RACE_BLACK",
                    "MOTHER_RACE_WHITE",
                    "TOBACCO_USE_YESNO",
                    "MOTHER_RES_ADDR1")]
names(births) <- c("year",
                   "dob",
                   "zip",
                   "age",
                   "county",
                   "ed",
                   "married",
                   "weight",
                   "wic",
                   "pay",
                   "black",
                   "white",
                   "tobacco",
                   "add")

# Create cleaned address
births$address <- paste0(births$add,
                         ", ",
                         births$zip,
                         ", ",
                         births$county,
                         " county, Florida")
births$address[which(is.na(births$add))] <- NA
births$address[which(births$add == "UNKNOWN")] <- NA
births$address[which(births$zip == 99999)] <- NA
births$add <- NULL

births$dob <- as.Date(births$dob, format = "%Y-%m-%d")
births$zip[which(births$zip == 99999)] <- NA
births$age[which(births$age >= 70)] <- NA
births$ed[which(births$ed == 9)] <- NA
births$weight[which(births$weight > 6000)] <- NA
births$married[which(births$married %in% c("U", "W"))] <- NA
births$married <- births$married == "Y"
births$wic[which(births$wic == "U")] <- NA
births$wic <- births$wic == "Y"
births$pay <- ifelse(births$pay == 1, "Medicaid",
                     ifelse(births$pay == 2, "Private",
                            ifelse(births$pay == 3, "Self-pay",
                                   NA)))
births$medicaid <- births$pay == "Medicaid"
births$pay <- NULL
births$black <- births$black == "Y"
births$white <- births$white == "Y"
births$tobacco <- births$tobacco == "Y"
save.image("births_2000-2014.RData")
