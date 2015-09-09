library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(dplyr)

#################
## Setwd
if ( Sys.info()["sysname"] == "Linux" ){
  setwd("/home/joebrew/")
} else {
  setwd("C:/Users/BrewJR/")
}

mywd <- getwd()

#################

# Read in bac STD's among 15-19 year-olds
bac <- read.csv("Documents/fdoh/public/std/teen_std/bac_std_1519.csv")

plot(bac$year, bac$alachua_rate, 
     col = adjustcolor("darkred", alpha.f = 0.6),
     type = "l",
     lwd = 2,
     ylim = c(0,3500),
     xlab = "Year",
     ylab = "Rate per 100,000")
points(bac$year, bac$alachua_rate, 
       pch = 1,
       col = adjustcolor("darkred", alpha.f = 0.6))

lines(bac$year, bac$florida_rate, 
     col = adjustcolor("darkblue", alpha.f = 0.6),
     lwd = 2)
points(bac$year, bac$florida_rate, 
       pch = 1,
       col = adjustcolor("darkblue", alpha.f = 0.6))
   
abline(h = seq(0,5000, 500),
       col = adjustcolor("black", alpha.f = 0.2))
abline(v = seq(1990, 2020, 5),
       col = adjustcolor("black", alpha.f = 0.2))

legend(x = "bottomright",
       lwd = 2,
       pch = 1,
       col = adjustcolor(c("darkred", "darkblue"), alpha.f = 0.6),
       legend = c("Alachua", "Florida"))
##################

# Read in birth rate by country
country <- read.csv("Documents/fdoh/public/std/teen_std/birth_rate_by_country.csv",
                    skip = 1)


# Make names more legible
country$country <- gsub(" ", "\n", country$country)

# Barplot
country <- country[order(country$birth_rate),]
barplot(country$birth_rate)

BarFun <- function(number){
  x <- country$birth_rate
  x[(length(x)-number):length(x)] <- NA
  barplot(x, ylim = c(0,60),
          names.arg = country$country,
          las = 3, 
          cex.names = 0.7,
          col = "grey")
}
for (i in 0:nrow(country)){
  BarFun(i)
}
barplot(country$birth_rate,
        ylim = c(0,60),
        names.arg = country$country,
        las = 3,
        cex.names = 0.7,
        col = c(rep("grey", 22), "red"))




plot(country$birth_rate, country$abortion_rate, pch = 16,
     xlab = "Teen birth rate", ylab = "Teen abortion rate",
     col = adjustcolor("black", alpha.f = 0.6),
     cex = 2)
text(country$birth_rate, country$abortion_rate,
     labels = country$country,
     cex = 0.6)

# #ADD LOESS LINE
# lox <- country$birth_rate
# loy <- country$abortion_rate
# lw1 <- loess(loy ~ lox, span=1)
# j <- order(lox)
# lines(lox[j],lw1$fitted[j],col=adjustcolor("red", alpha.f = 0.4),
#       lty=1,
#       lwd = 3)

# Add regression line
x <- country$birth_rate
y <- country$abortion_rate
fit <- lm(y~x)
abline(fit, col=adjustcolor("red", alpha.f = 0.4),
       lty=1,
       lwd = 3)









#################

#################
## Read in STD data (not stored in this directory)
std <- read.csv("Desktop/std_private/alachua_2009-2014.csv")

# Convert to local dataframe
std <- tbl_df(std)

# Format dates
std <-
  std %>%
  mutate(date = as.Date(date_added, 
                        format = "%Y-%m-%d"))

# Subset to keep only relevant columns
std <- std %>%
  select(zip, disease, disease_name, age_group, sex, race, ethnicity, pregnant, msm, date)

# Create a month / year column
std <- std %>%
  mutate(month = format(date, "%m"),
         year = as.numeric(format(date, "%Y")))

# Randomly assign NA's to some zip codes based on what George said about JobCorps
FixZip <- function(year, disease_code, number, df = std){
  std <- df  
  std$zip[which(std$zip == 32609 &
                  std$disease == disease_code &
                  std$year == year)][sample(1:nrow(std[which(std$zip == 32609 &
                                                                 std$disease == disease_code &
                                                               std$year == year),]), number)] <- NA
  return(std)
}


# Disease code 200
std <- FixZip(year = 2009, disease_code = 200, number = 50)
std <- FixZip(year = 2010, disease_code = 200, number = 18)
std <- FixZip(year = 2011, disease_code = 200, number = 16)
std <- FixZip(year = 2012, disease_code = 200, number = 37)
std <- FixZip(year = 2013, disease_code = 200, number = 15)
std <- FixZip(year = 2014, disease_code = 200, number = 17)

# Disease code 300
std <- FixZip(year = 2009, disease_code = 300, number = 4)
std <- FixZip(year = 2010, disease_code = 300, number = 2)
std <- FixZip(year = 2011, disease_code = 300, number = 3)
std <- FixZip(year = 2012, disease_code = 300, number = 6)
std <- FixZip(year = 2013, disease_code = 300, number = 6)
std <- FixZip(year = 2014, disease_code = 300, number = 1)

# Disease code 720
std <- FixZip(year = 2010, disease_code = 720, number = 1)

# Disease code 730
std <- FixZip(year = 2010, disease_code = 730, number = 1)
std <- FixZip(year = 2014, disease_code = 730, number = 1)





# number of cases by month by race
ts <- 
  std %>% 
  group_by(month, year, race, age_group) %>%
  summarise(cases = n()) %>%
  arrange(year, month,race)

# Define function for overlapping plots
BarFun <- function(df = ts){
  df <- df[which(df$age_group == "15 - 19"),] 
  # asian
  a <-
    df %>%
    filter(race == "Asian")
  # white
  w <-
    df %>%
    filter(race == "White")
  # black
  b <-
    df %>%
    filter(race == "Black/African American")
  
  mymax <- max(df$cases, na.rm = T)
  
  barplot(a$cases, ylim=c(0,mymax))
  Sys.sleep(3)
  barplot(w$cases, add = T, col = adjustcolor("blue", alpha.f=0.5))
  Sys.sleep(3)
  barplot(b$cases, add = T, col = adjustcolor("red", alpha.f=0.5))
}

BarFun(ts)
legend(x= "topright",
       fill = c("grey",
               adjustcolor("blue", alpha.f = 0.5),
               adjustcolor("red", alpha.f = 0.5)),
       legend = c("Asian", "White", "Black"))

# Define linefun to do the same
LineFun <- function(df = ts){

  df <- df[which(df$age_group == "15 - 19"),]
  
  # asian
  a <-
    df %>%
    filter(race == "Asian")
  a <- a[-nrow(a),]
  
  # white
  w <-
    df %>%
    filter(race == "White")
  w <- w[-nrow(w),]
  
  # black
  b <-
    df %>%
    filter(race == "Black/African American")
  b <- b[-nrow(b),]
  
  mymax <- max(df$cases, na.rm = T)
  
  plot(1:nrow(b),b$cases,
       xaxt = "n",
       xlab = "Date",
       ylab = "Cases",
       ylim = c(0,mymax),
       type = "n")
  axis(side = 1, at = 1:nrow(b),
       labels = paste(b$month,"-", b$year),
       cex.axis = 0.4, las = 3)
  lines(1:nrow(w), w$cases, col = adjustcolor("darkgreen", alpha.f =0.6))
  lines(1:nrow(b), b$cases, col = adjustcolor("darkblue", alpha.f =0.6))
  points(1:nrow(w), w$cases, col = adjustcolor("darkgreen", alpha.f =0.6))
  points(1:nrow(b), b$cases, col = adjustcolor("darkblue", alpha.f =0.6))
  
  abline(v=which(b$month == "01"),
         col = adjustcolor("black", alpha.f = 0.3))
  text(x = which(b$month == "01"),
       y = mymax,
       pos = 4,
       labels = b$year[which(b$month == "01")])
}
LineFun(ts)
legend(x = 60,
       y = 55,
       #bty = "n",
       pch = 1,
       lty = 1,
       col = adjustcolor(c("darkblue", "darkgreen"), alpha.f = 0.6),
       legend = c("Black", "White"))


####### RATES BY YEAR
ts <- std %>%
  group_by(year, race, age_group) %>%
  summarise(cases = n()) %>%
  arrange(year,race)

# BASELINE POP from Fl Charts: Add in number of 15-19 year old girls by race
#n white 15-19 year olds
ts$n_white_teens <- 7500*2
ts$n_white_teens[which(ts$year %in% 2009)] <- 7648*2
ts$n_white_teens[which(ts$year %in% 2010)] <- 8518*2
ts$n_white_teens[which(ts$year %in% 2011)] <- 8454*2
ts$n_white_teens[which(ts$year %in% 2012)] <- 8522 *2
ts$n_white_teens[which(ts$year %in% 2013)] <- 8446*2
ts$n_white_teens[which(ts$year %in% 2014)] <- 8446*2

#n black 15-19 year olds
ts$n_black_teens <- 4000*2
ts$n_black_teens[which(ts$year %in% 2009)] <- 4604*2
ts$n_black_teens[which(ts$year %in% 2010)] <- 4138*2
ts$n_black_teens[which(ts$year %in% 2011)] <- 4075*2
ts$n_black_teens[which(ts$year %in% 2012)] <- 4162 *2
ts$n_black_teens[which(ts$year %in% 2013)] <- 4132*2
ts$n_black_teens[which(ts$year %in% 2014)] <- 4132*2

#n 15-19 year olds
ts$n_teens <- 11500*2
ts$n_teens[which(ts$year %in% 2009)] <- 11712*2
ts$n_teens[which(ts$year %in% 2010)] <- 12656*2
ts$n_teens[which(ts$year %in% 2011)] <- 12529*2
ts$n_teens[which(ts$year %in% 2012)] <- 12684 *2
ts$n_teens[which(ts$year %in% 2013)] <- 12578*2
ts$n_teens[which(ts$year %in% 2014)] <- 12578*2


# Define linefunprop for rates
LineFunProp <- function(df = ts){
  
  df <- df[which(df$age_group == "15 - 19"),]

  # white
  w <- df[which(df$race == "White"),]
  w <- w[-nrow(w),]
  w$cases <- w$cases / w$n_white_teens * 100000
  
  # black
  b <- df[which(df$race == "Black/African American"),]
  b <- b[-nrow(b),]
  b$cases <- b$cases / b$n_black_teens * 100000
    
  mymax <- max(b$cases, na.rm = T)
  
  plot(1:nrow(b),b$cases,
       xaxt = "n",
       xlab = "Date",
       ylab = "Yearly incidence per 100,000",
       ylim = c(0,mymax),
       type = "n")
  axis(side = 1, at = 1:nrow(b),
       labels = paste(b$year),
       cex.axis = 1)
  lines(1:nrow(w), w$cases, col = adjustcolor("darkgreen", alpha.f =0.6))
  lines(1:nrow(b), b$cases, col = adjustcolor("darkblue", alpha.f =0.6))
  points(1:nrow(w), w$cases, col = adjustcolor("darkgreen", alpha.f =0.6))
  points(1:nrow(b), b$cases, col = adjustcolor("darkblue", alpha.f =0.6))
  

}
LineFunProp(ts)
legend(x = "topright",
       #bty = "n",
       pch = 1,
       lty = 1,
       col = adjustcolor(c("darkblue", "darkgreen"), alpha.f = 0.6),
       legend = c("Black", "White"))

# ALL RACES
####### RATES BY YEAR
ts <- std %>%
  group_by(year, age_group) %>%
  summarise(cases = n()) %>%
  arrange(year, age_group)
ts <- ts[which(ts$age_group == "15 - 19"),]
ts$n_teens <- 11500*2
ts$n_teens[which(ts$year %in% 2009)] <- 11712*2
ts$n_teens[which(ts$year %in% 2010)] <- 12656*2
ts$n_teens[which(ts$year %in% 2011)] <- 12529*2
ts$n_teens[which(ts$year %in% 2012)] <- 12684 *2
ts$n_teens[which(ts$year %in% 2013)] <- 12578*2
ts$n_teens[which(ts$year %in% 2014)] <- 12578*2
ts$per <- ts$cases / ts$n_teens * 100000

library(RColorBrewer)

# CASES PER YEAR
my_cols <- colorRampPalette(c("blue", "red"))(max(ts$cases))
barplot(ts$cases,
        names.arg = ts$year,
        col = my_cols[round(ts$cases)],
        ylab = "Cases")

# INCIDENCE PER YEAR
my_cols <- colorRampPalette(c("blue", "red"))(max(ts$per))
barplot(ts$per,
        names.arg = ts$year,
        col = my_cols[round(ts$per)],
        ylab = "Rate per 100,000")

## Read in Alachua zip code shapefiles
setwd("Documents/fdoh/public/gis/")
zip <- readOGR("alachuazipcodes", layer="ACDPS_zipcode")
fl <- readOGR("zip", layer = "zipbnd_2012") # this has 2010 pop

plot(fl[which(fl$PO_NAME %in% zip$PO_NAME),])

# Transform the projections of both
zip2 <- spTransform(zip, CRS("+init=epsg:4326"))
fl2 <- spTransform(fl, CRS("+init=epsg:4326"))

# Subset fl to keep only those zips in Alachua
fl2 <- fl2[which(fl2$PO_NAME %in% zip$PO_NAME),]

# Visualize to confirm that they're good
plot(zip2, col = "white", border = "green")
plot(fl2, col = adjustcolor("red", alpha.f=0.2), border = "red", add = TRUE)

# Rename
zip <- zip2
fl <- fl2
rm(zip2)
rm(fl2)

# Merge together
zip@data <- merge(zip,
           fl,
           by = "ZIP",
           all.x = TRUE,
           all.y = FALSE)

# Make zip$zip numeric
zip$zip <- as.numeric(as.character(zip$ZIP))

## Get cases by zip code
# Chlamydia
zip_df <- 
  std %>% 
  filter(disease_name == "Chlamydia") %>%
  group_by(zip) %>%
  summarise(chlamydia = n()) %>%
  arrange(desc(zip))

# Gonorrhea
x <- 
  std %>% 
  filter(disease_name == "Gonorrhea") %>%
  group_by(zip) %>%
  summarise(gonorrhea = n()) %>%
  arrange(desc(zip))
zip_df <- merge(zip_df, x, all.x = TRUE, all.y = FALSE)

# Syphilis
x <- 
  std %>% 
  filter(disease_name == "Syphilis") %>%
  group_by(zip) %>%
  summarise(syphilis = n()) %>%
  arrange(desc(zip)) 
zip_df <- merge(zip_df, x, all.x = TRUE, all.y = FALSE)

# Rejoin the data from zip_df to the spatial zip
zip@data <- merge(zip@data,
                  zip_df,
                  by = "zip",
                  all.x = TRUE,
                  all.y = FALSE)

## Read in population data for florida zip codes
setwd(paste0(mywd,"/Documents/fdoh/public/census"))
pop <- read.csv("florida_details.csv", skip = 1)
names(pop) <- gsub("[.]", "", names(pop))
pop <- tbl_df(pop)

# Get the age distribution for Alachua County
x <- pop %>% 
  filter(RaceEthnicity == "Total" &
           AreaName == "Alachua County") %>%
  select(TotalUnder1Year:Total110YearsandOlder)
names(x)[c(1:2, 
           101:103)] <- c("Total0Years", "Total1Years",
                         "Total100-104Years", "Total105-109Years", "Total110+Years")


# Fake Alachua data
y <- as.numeric(x[1,])
y[19:31] <- jitter(rep(2700,13), factor = 3)
bp <- barplot(y,
              border = adjustcolor("black", alpha.f = 0.3),
              las = 3,
              cex.names = 0.5,
              xlab = NA,
              ylab = NA,
              main = "Age distribution of normal counties",
              ylim = c(-103.880, 10388.000))
mtext("Age", side=1, line=0,
      col = adjustcolor("darkred", alpha.f = 0.6))
text(x = bp[,1][seq(1,101,10)],
     y = 500,
     labels = seq(0,100,10),
     cex = 0.8,
     col = adjustcolor("darkred", alpha.f = 0.6))

# Real Alachua data
bp <- barplot(as.numeric(x[1,]),
        border = adjustcolor("black", alpha.f = 0.3),
        names.arg = NA, #gsub("Total|Years|Year|Under|Over", "", names(x)), 
        las = 3,
        cex.names = 0.5,
        xlab = NA,
        ylab = NA,
        main = "Age distribution of Alachua residents")
mtext("Age", side=1, line=0,
      col = adjustcolor("darkred", alpha.f = 0.6))
text(x = bp[,1][seq(1,101,10)],
     y = 500,
     labels = seq(0,100,10),
     cex = 0.8,
     col = adjustcolor("darkred", alpha.f = 0.6))

# Get the numbers of each age
ages <- data.frame(t(x))
names(ages) <- c("n")
ages$group <- row.names(ages)
ages$group <- as.numeric(substr(gsub("Total|Years|-|[+]", "", ages$group), 1,3))

# What percentage of Alachuans are 15-19
n1519 <- sum(ages$n[which(ages$group >=15 & ages$group <= 19)])
p1519 <- n1519 / sum(ages$n)

# What percentage of Alachuans are 20-24
n2024 <- sum(ages$n[which(ages$group >=20 & ages$group <= 24)])
p2024 <- n2024 / sum(ages$n)

# What percentage of Alachuans are 15-24
n1524 <- sum(ages$n[which(ages$group >=15 & ages$group <= 24)])
p1524 <- n1524 / sum(ages$n)


# Make lots of relevant columns in zip

# (first, populate columns with NA's)
# for (j in unique(sort(std$year))){
#   for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
#     zip@data[,paste0(k, j)] <- NA
#   }
# }

# then populate with actual numbers (all age groups)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, j, "1524")] <- 
        (nrow(std[which(std$zip == i &
                         std$year %in% j &
                         std$disease_name == k),]) /
        zip$POP2010[which(zip$zip == i)] *
        p1524) * 100000
    }
  }
}

# And add columns for age groups (1519)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, j, "1519")] <- 
        (nrow(std[which(std$zip == i &
                         std$year %in% j &
                         std$disease_name == k),]) /
        zip$POP2010[which(zip$zip == i)] *
        p1519) * 100000
        
    }
  }
}

# And add columns for age groups (2024)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, j, "2024")] <- 
        (nrow(std[which(std$zip == i &
                          std$year %in% j &
                          std$disease_name == k),]) /
           zip$POP2010[which(zip$zip == i)] *
           p2024) * 100000
      
    }
  }
}

# And add columns for all STDS combined (all age groups)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
      zip@data[which(zip$zip == i),paste0("std", j, "1524")] <- 
        (nrow(std[which(std$zip == i &
                          std$year %in% j ),]) /
           zip$POP2010[which(zip$zip == i)] *
           p1524) * 100000
    }
  }



# Write function to plot choropleth map
MapFun <- function(disease = "std",
                   year = "all", 
                   min_age = 15, 
                   max_age = 24, 
                   color = "Blues"){
  plotvar <- zip@data[,paste0(disease, year, min_age, max_age)]
  nclr <- 8
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
  plot(zip, border="darkgrey", col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}

MapFun(disease = "std", 
       year = 2013,
       min_age = 15, 
       max_age = 24, 
       color = "Blues")





#######################
# SATURDAY PLOTS

# And add columns for age groups (1519)
for (i in zip$zip){
  for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
    zip@data[which(zip$zip == i),paste0(k, "1519")] <- 
     NA   
  }
}

for (i in zip$zip){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, "1519")] <- 
        (nrow(std[which(std$zip == i &
                          std$disease_name == k),]) /
           (zip$POP2010[which(zip$zip == i)] *
           p1519)/ 5.75  ) * 100000     
    }
  }


# Write function to plot choropleth map
MapFun <- function(var = "Chlamydia1519", 
                   color = "Blues",
                   breaks = 8){
  plotvar <- var
  nclr <- breaks
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
  plot(zip, border="darkgrey", col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.8, 
         border=NA,
         bty = "n",
         title = "Per 100,000")
}

MapFun(zip$Syphilis1519, color = "Greens", breaks = 4)
title(main = "Syphilis incidence (15-19 year-olds)")
MapFun(zip$Chlamydia1519, color = "Reds")
title(main = "Chlamydia incidence (15-19 year-olds)")
MapFun(zip$Gonorrhea1519, color = "Purples")
title(main = "Gonorrhea incidence (15-19 year-olds)")













#######################
# LBW AND TEEN BIRTH STUFF
#######################

########## 
# STD ANALYSIS USING VITAL STATS DATA (CANNOT BE RUN WITHOUT PRIVATE ACCESS)
###########
library(dplyr)

# Alachua births only
al <- ab %>%
  filter(MOTHER_RES_COUNTY == "ALACHUA",
         EVENT_YEAR > 1990,
         EVENT_YEAR < 2014)

# Get number of teen births each year
ts <- al %>%
  group_by(EVENT_YEAR) %>%
  filter(MOTHER_AGE < 20) %>%
  summarise(teen_births = n()) 

# Number of black teen births each year
x <- 
  al %>%
  group_by(EVENT_YEAR) %>%
  filter(MOTHER_AGE < 20,
         MOTHER_RACE_BLACK == "Y") %>%
  summarise(count = n()) %>%
  select(count)
ts$black_teens <- x$count

# Number of white teen births each year
x <- 
  al %>%
  group_by(EVENT_YEAR) %>%
  filter(MOTHER_AGE < 20,
         MOTHER_RACE_WHITE == "Y") %>%
  summarise(count = n()) %>%
  select(count)
ts$white_teens <- x$count

# BASELINE POP from Fl Charts: Add in number of 15-19 year old girls by race
ts$n_white_teens <- 7500
ts$n_white_teens[which(ts$EVENT_YEAR %in% 2008:2013)] <- c(7793, 7648, 8518, 8454, 8522, 8446)

ts$n_black_teens <- 4000
ts$n_black_teens[which(ts$EVENT_YEAR %in% 2008:2013)] <- c(4018, 4064, 4138, 4075, 4162, 4132)

ts$n_teens <- 11500
ts$n_teens[which(ts$EVENT_YEAR %in% 2008:2013)] <- c(11811, 11712, 12656, 12529, 12684, 12578)

##### PLOT TIME
library(RColorBrewer)

# PLOT TOTAL NUMBER OF TEEN BIRTHS BY YEAR
my_cols <- colorRampPalette(c("blue", "red"))(max(ts$teen_births))
bp <- barplot(ts$teen_births, col = my_cols[ts$teen_births], names.arg = ts$EVENT_YEAR)

# BIRTH RATE
my_cols <- colorRampPalette(c("blue", "red"))(max(round(ts$teen_births / ts$n_teens * 1000)))
bp <- barplot(ts$teen_births / ts$n_teens * 1000,
              names.arg = ts$EVENT_YEAR,
              col = my_cols[round(ts$teen_births / ts$n_teens * 1000)])



# barplot(ts$white_teens, ylim = c(0,200))
# barplot(ts$black_teens, col = adjustcolor("darkred", alpha.f = 0.2), add = T)

# NUMBER OF TEEN BIRTHS PER YEAR BY RACE
z <- matrix(cbind(ts$black_teens, ts$white_teens), ncol = 2)
barplot(t(z), beside = T, 
        col = adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
        names.arg = ts$EVENT_YEAR)
legend(x = "topright",
       fill =  adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
       legend = c("Black", "White"))

# TEEN BIRTH RATE PER YEAR BY RACE
z <- matrix(cbind(ts$black_teens / ts$n_black_teens * 1000, ts$white_teens / ts$n_white_teens * 1000), ncol = 2)
barplot(t(z), beside = T, 
        col = adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
        names.arg = ts$EVENT_YEAR)
legend(x = "topright",
       fill =  adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
       legend = c("Black", "White"))

# LAST TEN YEARS TOTAL
last10 <- ts[which(ts$EVENT_YEAR > 2003),]
black <- sum(last10$black_teens) / sum(last10$n_black_teens) * 1000
white <- sum(last10$white_teens) / sum(last10$n_white_teens) * 1000
bp <- barplot(c(black, white),
              col = adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
              names.arg = c("Black", "White"))
legend(x = "topright",
       fill =  adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
       legend = c("Black", "White"))
text(x = bp[,1],
     y = c(black, white),
     pos = 1,
     labels = round(c(black, white), digits = 2))

z <- matrix(cbind(sum(ts$black_teens / ts$n_black_teens * 1000),
                  ts$white_teens / ts$n_white_teens * 1000), ncol = 2)

# LBW RATE BY AGE
teen <- nrow(al[which(al$BIRTH_WEIGHT_GRAMS < 2500 & al$MOTHER_AGE < 20),]) / nrow(al[which(al$MOTHER_AGE < 20),])
adult <- nrow(al[which(al$BIRTH_WEIGHT_GRAMS < 2500 & al$MOTHER_AGE > 20),]) / nrow(al[which(al$MOTHER_AGE > 20),])
black_teen <- nrow(al[which(al$BIRTH_WEIGHT_GRAMS < 2500 & al$MOTHER_AGE < 20 & al$MOTHER_RACE_BLACK == "Y"),]) / 
  nrow(al[which(al$MOTHER_AGE < 20 & al$MOTHER_RACE_BLACK == "Y"),])
white_teen <- nrow(al[which(al$BIRTH_WEIGHT_GRAMS < 2500 & al$MOTHER_AGE < 20 & al$MOTHER_RACE_WHITE == "Y"),]) / 
  nrow(al[which(al$MOTHER_AGE < 20 & al$MOTHER_RACE_WHITE == "Y"),])
black_adult <- nrow(al[which(al$BIRTH_WEIGHT_GRAMS < 2500 & al$MOTHER_AGE > 20 & al$MOTHER_RACE_BLACK == "Y"),]) / 
  nrow(al[which(al$MOTHER_AGE > 20 & al$MOTHER_RACE_BLACK == "Y"),])
white_adult <- nrow(al[which(al$BIRTH_WEIGHT_GRAMS < 2500 & al$MOTHER_AGE > 20 & al$MOTHER_RACE_WHITE == "Y"),]) / 
  nrow(al[which(al$MOTHER_AGE > 20 & al$MOTHER_RACE_WHITE == "Y"),])



bp <- barplot(c(teen, adult)* 100,
              col = adjustcolor(c("darkred", "darkgrey"), alpha.f = 0.6),
              names.arg = c("Teen", "Adult (20+)"))

text(x = bp[,1],
     y = c(teen, adult)*100,
     pos = 1,
     labels = paste0(round(c(teen, adult)*100, digits = 2), "%"))

bp <- barplot(c(black_teen, white_teen, adult)* 100,
              col = adjustcolor(c("darkred", "darkgrey", "darkblue"), alpha.f = 0.6),
              names.arg = c("Black teen", "White teen", "Adult"))

text(x = bp[,1],
     y = c(black_teen, white_teen, adult)* 100,
     pos = 1,
     labels = paste0(round(c(black_teen, white_teen, adult)* 100, digits = 2), "%"))




x <- ab %>%
  group_by(EVENT_YEAR) %>%
  filter(MOTHER_RES_COUNTY == "ALACHUA") %>%
  summarise(count = n())

y <- ab %>%
  group_by(EVENT_YEAR) %>%
  filter(MOTHER_RES_COUNTY == "ALACHUA",
         MOTHER_AGE < 20) %>%
  summarise(teen = n())

u <- ab %>%
  group_by(EVENT_YEAR) %>%
  filter(MOTHER_RES_COUNTY == "ALACHUA",
         BIRTH_WEIGHT_GRAMS < 2500) %>%
  summarise(lbw = n())


z <- merge(x,y, by = "EVENT_YEAR", all.x = T, all.y = F)
z <- merge(z,u, by = "EVENT_YEAR", all.x = T, all.y = F)

head(z)
plot(z$EVENT_YEAR, z$lbw / z$count * 100, type = "l")
