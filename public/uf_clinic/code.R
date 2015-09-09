library(dplyr)
library(ggplot2)
library(readxl)

#####
# DIRECTORY INFO
#####
if(Sys.info()['sysname'] == 'Windows'){
  code_dir <- 'C:/Users/BrewJR/Documents/fdoh/public/uf_clinic/'
  data_dir <- 'E:/fdoh/private/uf_clinic/'
} else {
  code_dir <- '/home/joebrew/Documents/fdoh/public/uf_clinic/'
  data_dir <- '/media/joebrew/JB/fdoh/private/uf_clinic/'
}

#####
# READ IN FDOH DATA
#####
fdoh <- read_excel(paste0(data_dir, '/ufSTI-data.xlsx'), 
                 skip = 3)

#####
# CLEAN UP THE TABULATION AT END
#####
fdoh <- fdoh[1:(nrow(fdoh)-16),]

#####
# GET RID OF THE MID-FILE HEADER ROWS AND THE RESULT DATE ROWS
#####
fdoh <- fdoh[which(!is.na(fdoh$Service) &
                 fdoh$Client != 'Client'),]

#####
# MANUALLY CLEAN OUT THE JAIL AND ACHD TESTS
#####
fdoh <- fdoh[9:nrow(fdoh),]

#####
# MAKE NORMAL DATAFRAME OBJECT
#####
fdoh <- data.frame(fdoh)

#####
# DEAL WITH THE TERRIBLY FORMATTED DATES
#####
fix_date <- function(dates){
  dates <- as.numeric(dates)
  year <- substr(dates, nchar(dates)-1 ,nchar(dates))
  day <- substr(dates, nchar(dates)-3 ,nchar(dates) -2)
  month <- ifelse(nchar(dates) == 5,
                  substr(dates,1,1),
                  substr(dates,1,2))
  new_dates <- paste0(year, '-', month, '-', day)
  new_dates <- as.Date(new_dates, format = '%y-%m-%d')
  return(new_dates)
}
fdoh$Client.DOB <- fix_date(fdoh$Client.DOB)
fdoh$Coll <- fix_date(fdoh$Coll)

fdoh$date <- fdoh$Coll
fdoh$month <- as.numeric(format(fdoh$date, format = '%m'))
fdoh$month_char <- format(fdoh$date, format = '%B')
fdoh$year <- as.numeric(format(fdoh$date, format = '%Y'))

#####
# GET COUNTS BY MONTH
#####
ts <- fdoh %>%
  group_by(year, month) %>%
  summarise(n = n())
ts$first_day_of_month <- as.Date(paste0(ts$year, '-', ts$month, '-', '01'))
ts$date_char <- paste0(ts$year, '-', ts$month, '-')

gg <- ggplot(data = ts, aes(x = first_day_of_month,
                            y = n))
gg + 
  theme_bw() +
  geom_area(fill = 'blue', alpha = 0.5) +
  xlab('Date') +
  ylab('Tests administered')

#####
# READ IN fdoh DATA
#####
setwd(data_dir)
uf <- read.csv('for_joe.csv')

#####
# CLEAN UP COLUMN NAMES AND SUCH
#####
uf$year <- paste0(ifelse(nchar(uf$visit.year) == 1,
                         '200', '20'),
                  uf$visit.year)
months <- c('January', 'February', 'March', 'April', 'May', 'June',
            'July', 'August', 'September', 'October', 'November', 'December')
uf$month <- NA
for (i in 1:nrow(uf)){
  uf$month[i] <-
    which(months == uf$visit.month[i])
}
uf <- uf[,c('age', 'sex', 'diag', 'cases', 'year', 'month')]

#####
# GET ONLY THE VARIABLES WE'RE INTERESTED IN
#####

# Create vector of stis
stis <- c('chlam|clam', 
          'gono|ghon|gonh',
          'syphi|sypi',
          'hpv|papillo|papilo',
          'herpes',
          'hiv|aids|immunodeficiency')


names(stis) <- c('chlamydia', 'gonorrhea', 'syphilis', 'hpv', 'herpes', 'hiv')

# Make diag lowercase
uf$diag <- tolower(uf$diag)

# Create a new variable for each of the STI's in question
for (j in 1:length(stis)){
  uf[,names(stis)[j]] <- NA
}
# Create a new variable for each of the STI's in question
for (j in 1:length(stis)){
  # Extract the sti in question
  sti <- stis[j]
  # Get whether each row has that STI
  new_var <- grepl(sti, uf$diag)
  # Put the results into a new variable
  uf[,names(stis)[j]] <- new_var
}

#####
# GET UF TIME SERIES
#####
ts_uf <- uf %>%
  filter(age >= 18 & age <= 22) %>%
  group_by(year, month) %>%
  summarise(chlamydia = sum(cases[chlamydia]),
            gonorrhea = sum(cases[gonorrhea]),
            syphilis = sum(cases[syphilis]),
            hpv = sum(cases[hpv]),
            herpes = sum(cases[herpes]),
            hiv = sum(cases[hiv]),
            sti = sum(cases[chlamydia|gonorrhea|syphilis|hpv|herpes|hiv]))

barplot(ts_uf$sti, names.arg = paste0(ts_uf$year, '-', ts_uf$month), las = 3, cex.names = 0.3)

##############################################
##############################################
##############################################
##############################################
##############################################

#####
# JOIN UF / FDOH DATA
#####
ts_uf$year <- as.numeric(ts_uf$year)
ts <- left_join(x = ts_uf, y = ts)

# Fix a column name
ts$n_tests <- ts$n
ts <- ts[,which(colnames(ts) != 'n')]

# Save an image
setwd(data_dir)
save.image('checkpoint.RData')
