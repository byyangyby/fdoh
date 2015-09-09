###################
#SET DATE / TIME PARAMETERS
###################
today <- Sys.Date() 
yesterday <- today - 1

###################
#DEFINE AND SET WD
###################
private <- "/home/joebrew/Documents/private/sugimoto"
public <- "/home/joebrew/Documents/fdoh/public/sugimoto"
# SET WD
setwd(private) 

get_link <- function(syndrome = NULL,
                     start_date = NULL,
                     end_date = NULL){
  # Syndrome
  if(!is.null(syndrome)){
    syndrome <- paste0("medicalGrouping=", syndrome, "&")
  } else {
    syndrome <- ""
  }
  
  # Start date
  if(is.null(start_date)){
    start_date <- paste0("startDate=", format(Sys.Date() - 7, format = "%d%b%Y"), "&")
  } else {
    start_date <- paste0("startDate=", format(as.Date(start_date, format = "%Y-%m-%d"), format = "%d%b%Y"), "&")
  }
  
  # End date
  if(is.null(end_date)){
    end_date <- paste0("endDate=", format(Sys.Date() - 1, format = "%d%b%Y"), "&")
  } else {
    end_date <- paste0("endDate=", format(as.Date(end_date, format = "%Y-%m-%d"),  format = "%d%b%Y"), "&")
  }
  
   # Make link 
  if(is.null(syndrome)){
    link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&", 
                   start_date, 
                   "medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&", 
                   end_date, 
                   "dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&geography=all&patientLoc=all&age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&medicalGrouping=all&timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all")
  } else {
    link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&", 
                   start_date,
                   "medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&", 
                   end_date,
                   "dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&geography=all&patientLoc=all&age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&", 
                   syndrome,
                   "timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all")
  }
  return(link)  
}

# Create a dataframe of links
df <- data.frame(date = seq(as.Date("2005-01-01", format = "%Y-%m-%d"), today, 1),
                 gi = NA,
                 ili = NA,
                 injury = NA)

# Make date character (for looping)
df$date <- as.character(df$date)

# Create vector of symptoms to be downloaded
symptoms <- c("gi", "ili", "injury")

# Loop through each day and each symptom
for (i in df$date){
  for (j in symptoms){
    
    the_date <- i
    df[which(df$date == i), j] <- 
      get_link(syndrome = j,
               start_date = the_date,
               end_date = the_date)
  }
}

# Add a column of alless
df$alless <- NA
for (i in df$date){
  
  the_date <- i
  
  df$alless[which(df$date == i)] <- 
    get_link(syndrome = NULL,
            start_date = the_date,
             end_date = the_date)
}

# WRITE A CSV WITH ALL THE LINKS FOR DOWNLOAD
setwd(public)
write.csv(df, 'links_to_download.csv')

# # EXAMPLE:
get_link(syndrome = "injury",
         start_date = df$date[1],
         end_date = df$date[2])
