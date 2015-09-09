#####
# READ IN HISTORICAL IMMUNIZATION RATE DATA
#####
library(RCurl)
my_link <- "https://docs.google.com/spreadsheets/d/1icEDpqkJVNuvGLV6GcULuvfVK0healPyPep3enHkceE/export?gid=0&format=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
my_csv <- getURL(my_link)
dat <- read.csv(textConnection(my_csv))

#####
# CALCULATE SCHOOL TYPE BY PAUL'S PREFERENCES
#####
dat$paul_type <- dat$type
dat$paul_type[which(dat$id == -13)] <- "elem" # Queen of peace
dat$paul_type[which(dat$id == -9)] <- "elem" # Jordan Glenn 

#####
# CALCULATE IR BY TYPE BY YEAR
#####
library(dplyr)
ts <- dat %>% 
  group_by(year, paul_type) %>%
  summarise(ir = weighted.mean(x = immRate, w = totMem))
  
ts[which(ts$paul_type == "elem"),]
ts[which(ts$paul_type == "mid"),]
ts[which(ts$paul_type == "high"),]
ts[which(ts$paul_type == "multi"),]

#####
# GET ONLY SCHOOLS WE STARTED WITH IN 2009
#####
dat$original <- NA
for (i in unique(sort(dat$id))){
  if(nrow(dat[which(dat$id == i &
                      dat$year == 2009),]) > 0){
    dat$original[which(dat$id == i)] <- TRUE
  } else{
    dat$original[which(dat$id == i)] <- FALSE
  }
}

# GET ONLY ORIGINAL SCHOOLS
tony <- dat %>%  group_by(year) %>%
  filter(original, paul_type == "elem") %>%
  summarise(ir = weighted.mean(x = immRate, w = totMem) * 100)

# Get rid of 2006
tony <- tony[which(tony$year != 2006),]

# Get growth rate for each year
tony$growth <- NA
for (i in 2:nrow(tony)){
  tony$growth[i] <-
    ((tony$ir[i] - tony$ir[i-1]) / tony$ir[i-1])* 100
}

bp <- barplot(tony$ir , ylim = c(0, 60),
              names.arg = tony$year,
              ylab = "Immunization rate")
text(x = bp[,1],
     y = tony$ir,
     pos = 3,
     labels = round(tony$ir, digits = 2))
# LOOP THROUGH TO GET GROWTH BY YEAR
type <- "elem"
df <- ts[which(ts$paul_type == type),]
