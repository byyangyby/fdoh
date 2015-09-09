###
# SET WDS
###
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/fdoh/public/lbw"
  private <- "/media/joebrew/JB/fdoh/private/vital_stats"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/fdoh/public/lbw"
  private <- "E:/fdoh/private/vital_stats"
} else {
   stop("Don't work on this computer!")
}

########
# Read in private data
########
setwd(private)
#source("lbw_read_and_clean.R) #only necessary once

###
# LOAD 2000-2014 DATA
###
load("births_2000-2014.RData")

##########
##################################
# ADD THOMPSON / CLARK CATEGORIES
##################################

births$cat <- ifelse(births$black==FALSE &
                       births$married  &
                       births$ed >= 3,
                     1,
                     ifelse(births$black==FALSE &
                              births$married  &
                              births$ed < 3,
                            2,
                            ifelse(births$black==FALSE &
                                     births$married ==FALSE &
                                     births$ed >= 3,
                                   3,
                                   ifelse(births$black==FALSE &
                                            births$married ==FALSE &
                                            births$ed < 3,
                                          4,
                                          ifelse(births$black &
                                                   births$married  &
                                                   births$ed >= 3,
                                                 5,
                                                 ifelse(births$black &
                                                          births$married  &
                                                          births$ed < 3,
                                                        6,
                                                        ifelse(births$black &
                                                                 births$married ==FALSE &
                                                                 births$ed >= 3,
                                                               7,
                                                               ifelse(births$black &
                                                                        births$married ==FALSE &
                                                                        births$ed < 3,
                                                                      8,
                                                                      9))))))))

# ADD LBW
births$lbw <- births$weight <= 2500

# Get rid of NA and * counties
births <- births[which(births$county != "*" &
                         !is.na(births$county)),]

# Fix St Johns / St Lucie
births$county[which(births$county == "ST. JOHNS")] <- "ST JOHNS"
births$county[which(births$county == "SAINT JOHNS")] <- "ST JOHNS"
births$county[which(births$county == "SAINT LUCIE")] <- "ST LUCIE"

# Fix Dade
births$county[which(births$county == "DADE")] <- "MIAMI-DADE"

# Fix unknown
births$county[which(births$county == "UNKNOWN")] <- NA

###
# LOAD FLORIDA ZIP CODE SHAPEFILE
###
setwd(public)
library(rgdal)
fl <- readOGR("florida_zip", "zip")
fl$name <- toupper(fl$NAME)

# since merging messes with order, assign and id and keep in this order
fl$id <- 1:nrow(fl)

# test some stuff with the map
fl$color <- "blue"
fl$color[which(fl$name == "ALACHUA")] <- "red"
fl$color[which(fl$name == "MARION")] <- "green"
fl$color[which(fl$name == "MIAMI-DADE")] <- "orange"
plot(fl, col = fl$color)

###
# READ IN INCOME AND STD RATES
###
# read in the std rate data
std <- read.csv("std.csv", stringsAsFactors = FALSE)

# compare to see if names are perfect matches
table(fl@data$name == std$NAME) # they're not all correct

# get the closest match for each county

std$name <- NA 
for (i in 1:nrow(std)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(std$NAME[i],
             fl$name)
  # get the one with the least differences
  ind <- which.min(m)
  # get the name from std
  best <- fl$name[ind][1]
  # assign to fl
  std$name[i] <- best
}

# fix the duplicated lake issue
std$name[which(std$NAME == "DADE")] <- "MIAMI-DADE" 

# Merge the two datasets together
fl@data <- merge(x = fl@data,
                 y = std,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)

# test out to make sure there was no reordering
fl$color <- "blue"
fl$color[which(fl$name == "ALACHUA")] <- "red"
fl$color[which(fl$name == "MARION")] <- "green"
fl$color[which(fl$name == "MIAMI-DADE")] <- "orange"
plot(fl, col = fl$color)


##########
# READ IN FLORIDA DATA ON LOW BIRTH WEIGHT, POPULATION CHARACTERISTICS
########## 
setwd(public)
dat <- read.csv("lbw.csv")
dat$lbw_births <- NULL
dat$live_births <- NULL
# Make a column called percent_black - this should be the percentage of each county's residents which are black
dat$percent_black <- (dat$black/dat$total_pop)*100

# Make a column called percent_white - this should be the percentage of each county's residents which are white
dat$percent_white<- (dat$white/dat$total_pop)*100

# Make a column called percent_other - this should be the percentage of each county's residents which are white
dat$percent_other <- (dat$other/dat$total_pop)*100

# get the closest match for each county
dat$county <- as.character(toupper(dat$county))
dat$name <- NA # create empty vector where we'll put our names
for (i in 1:nrow(dat)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(dat$county[i],
             fl$name)
  # get the one with the least differences
  ind <- which.min(m)
  # get the name from std
  best <- fl$name[ind][1]
  # assign to fl
  dat$name[i] <- best
}

# check matches and then merge
cbind(dat$name, dat$county)
fl@data <- merge(x = fl@data,
                 y = dat,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)


# Get lbw stats for each county
library(dplyr)

# Total births
x <- births %>%
  group_by(county) %>%
  summarise(total_births = n())
total_births <- x$total_births

#(create a dataframe to stick all my aggregations into)
counties <- data.frame(x)

# Category 8 births
x <- births %>%
  group_by(county) %>%
  filter(cat == 8) %>%
  summarise(cat8_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Category 8 births enrolled in wic
x <- births %>%
  group_by(county) %>%
  filter(cat == 8, wic) %>%
  summarise(cat8_wic_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Category 8 births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i, cat == 8) %>%
    summarise(cat8_births = n())
  x[,paste0("cat8_births", i)] <- x$cat8_births
  x$cat8_births <- NULL
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
  
}

# Births on medicaid enrolled in wic by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           cat == 8, 
           wic) %>%
    summarise(cat8_wic_births = n())
  
  x[,paste0("cat8_wic_births", i)] <- x$cat8_wic_births
  x$cat8_wic_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# Cat 8 wic enrollment rate by year
for (i in 2000:2014){
  counties[,paste0("cat8_wic_rate", i)] <-
    counties[,paste0("cat8_wic_births", i)] /
    counties[,paste0("cat8_births", i)]
}


# Cat 8 lbw births
x <- births %>%
  group_by(county) %>%
  filter(cat == 8, weight < 2500) %>%
  summarise(cat8_lbw_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Category 8 lbw births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i, cat == 8, weight < 2500) %>%
    summarise(cat8_lbw_births = n())
  x[,paste0("cat8_lbw_births", i)] <- x$cat8_lbw_births
  x$cat8_lbw_births <- NULL
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
  
}

# Category 8 lbw enrolled in wic births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           cat == 8, 
           wic,
           weight < 2500) %>%
    summarise(cat8_lbw_wic_births = n())
  
  x[,paste0("cat8_lbw_wic_births", i)] <- x$cat8_lbw_wic_births
  x$cat8_lbw_wic_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}


# Category 8 lbw NOT enrolled in wic births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           cat == 8, 
           !wic,
           weight < 2500) %>%
    summarise(cat8_lbw_nowic_births = n())
  
  x[,paste0("cat8_lbw_nowic_births", i)] <- x$cat8_lbw_nowic_births
  x$cat8_lbw_nowic_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# Category 8 NOTenrolled in wic births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           cat == 8, 
           !wic) %>%
    summarise(cat8_nowic_births = n())
  
  x[,paste0("cat8_nowic_births", i)] <- x$cat8_nowic_births
  x$cat8_nowic_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# Cat 8 wic enrollment rate by year
for (i in 2000:2014){
  counties[,paste0("cat8_lbw_wic_rate", i)] <-
    counties[,paste0("cat8_lbw_wic_births", i)] /
    counties[,paste0("cat8_wic_births", i)]
  
  counties[,paste0("cat8_lbw_nowic_rate", i)] <-
    counties[,paste0("cat8_lbw_nowic_births", i)] /
    counties[,paste0("cat8_nowic_births", i)]
  
  counties[,paste0("cat8_lbw_rate", i)] <-
    counties[,paste0("cat8_lbw_births", i)] /
    counties[,paste0("cat8_births", i)]
}



###### !!!!!!!!!!!!!!!!!!!!!!!!!!

# Cat 8 lbw wic lbw rate
x <- births %>%
  group_by(county) %>%
  filter(cat == 8, weight < 2500, wic) %>%
  summarise(cat8_wic_lbw_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Cat 8 lbw non wic lbw rate
x <- births %>%
  group_by(county) %>%
  filter(cat == 8, weight < 2500, !wic) %>%
  summarise(cat8_nowic_lbw_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)





###############################################


# Births on medicaid
x <- births %>%
  group_by(county) %>%
  filter(medicaid) %>%
  summarise(medicaid_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# LBW Births on medicaid
x <- births %>%
  group_by(county) %>%
  filter(medicaid, weight < 2500) %>%
  summarise(medicaid_lbw_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Births on medicaid enrolled in wic
x <- births %>%
  group_by(county) %>%
  filter(medicaid, wic) %>%
  summarise(medicaid_wic_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# LBW on medicaid enrolled in wic
x <- births %>%
  group_by(county) %>%
  filter(medicaid, wic, weight < 2500) %>%
  summarise(medicaid_lbw_wic_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Births on medicaid by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i, medicaid) %>%
    summarise(medicaid_births = n())
  x[,paste0("medicaid_births", i)] <- x$medicaid_births
  x$medicaid_births <- NULL
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
  
}

# LBW Births on medicaid by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i, medicaid, weight < 2500) %>%
    summarise(medicaid_lbw_births = n())
  x[,paste0("medicaid_lbw_births", i)] <- x$medicaid_lbw_births
  x$medicaid_lbw_births <- NULL
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
  
}

# Births on medicaid enrolled in wic by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           medicaid, 
           wic) %>%
    summarise(medicaid_wic_births = n())
  
  x[,paste0("medicaid_wic_births", i)] <- x$medicaid_wic_births
  x$medicaid_wic_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# LBW Births on medicaid enrolled in wic by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           medicaid, 
           wic,
           weight < 2500) %>%
    summarise(medicaid_lbw_wic_births = n())
  
  x[,paste0("medicaid_lbw_wic_births", i)] <- x$medicaid_lbw_wic_births
  x$medicaid_lbw_wic_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# Medicaid wic enrollment rate by year
for (i in 2000:2014){
  
  counties[,paste0("med_wic_rate", i)] <-
    counties[,paste0("medicaid_wic_births", i)] /
    counties[,paste0("medicaid_births", i)]
  
  counties[,paste0("med_lbw_wic_rate", i)] <-
    counties[,paste0("medicaid_lbw_wic_births", i)] /
    counties[,paste0("medicaid_wic_births", i)]
  
  counties[,paste0("med_lbw_rate", i)] <-
    counties[,paste0("medicaid_lbw_births", i)] /
    counties[,paste0("medicaid_births", i)]
}


# Total births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i) %>%
    summarise(total_births = n())
  x[,paste0("total_births", i)] <- x$total_births
  x$total_births <- NULL
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
  
}

# Low weight births
x <- births %>%
  group_by(county) %>%
  filter(weight < 2500) %>%
  summarise(lbw_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# LBW births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           weight < 2500) %>%
    summarise(lbw_births = n())
  
  x[,paste0("lbw_births", i)] <- x$lbw_births
  x$lbw_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# LBW rate by year
for (i in 2000:2014){
  counties[,paste0("lbw_rate", i)] <-
    counties[,paste0("lbw_births", i)] /
    counties[,paste0("total_births", i)]
}

### 
# Bring counties data into fl
###
counties$county <- as.character(counties$county)
counties$name <- NA
for (i in 1:nrow(counties)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(counties$county[i],
             fl$name)
  # get the one with the least differences
  ind <- which.min(m)[1]
  # get the name from std
  best <- fl$name[ind]
  # assign to county
  counties$name[i] <- best
}

# Check the names match
cbind(as.character(counties$name), as.character(counties$county))
# Remove the uknown row
counties <- counties[which(!is.na(counties$county)),]
# Check on reps
length(unique(counties$name)) == length(counties$name)

# Merge the two datasets together
fl@data <- merge(x = fl@data,
                 y = counties,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)

rm(counties, x, std, m, best, total_births, ind)


#####
# CALCULATE TRAVEL DISTANCE
#####

# Function for calculating travel distance
library(ggmap)
TravelTime <- function(from, to){
  mapdist(from, to, mode = "driving")
}

# Subset just Alachua births
alachua <- births[which(births$county == "ALACHUA"),]

# Just recent years
alachua <- alachua[which(alachua$year >= 2010),]

# Just category 8
alachua <- alachua[which(alachua$cat == 8),]

# Get dataframe of just addresses 
address_df <- data.frame("address" = unique(alachua$address))

# Geocode addresses in addres_df
# x <- geocode(as.character(address_df$address))
# address_df <- cbind(address_df, x)

# 

# Geocode place of enrollment
wic_locations <- data.frame(
  "location" = c("Park Avenue", "Family Services Center", "Wiles Elementary"),
  "address" = c("910 NW 57th Street Gainesville, FL 32605",
                "3600 NE 15th Street, Gainesville, FL 32609-2484",
                "4555 SW 75th St., Gainesville, FL 32607") )
# x <- geocode(as.character(wic_locations$address))
# wic_locations <- cbind(wic_locations, x)

# Get nearest location's travel time
address_df$seconds <- NA
for (i in address_df$address){
  
  # Geocode travel time
  x <- TravelTime(from = as.character(i), 
             to = as.character(wic_locations$address))
  
  # Get minimum travel time
  address_df$seconds[which(address_df$address == i)] <- 
    x$seconds[which.min(x$seconds)][1]  
}

# Merge back into alachua
alachua <- left_join(x = alachua,
                     y = address_df,
                     by = "address")

# # Save temporary
# save.image(paste0(private, "/temp.RData"))

# Visualize
x <- alachua$seconds
y <- alachua$weight
my_colors <- colorRampPalette(c("lightblue", "darkorange"))(max(round(y)))
plot(x,y,
     xlab = "Seconds from nearest WIC enrollment site (jittered)",
     ylab = "Birth weight (grams)",
     col = my_colors[round(y)],
     pch = 16)
points(x,y,
       col = adjustcolor("black", alpha.f = 0.6))

abline(lm(y~x),
       col = adjustcolor("darkred", alpha.f = 0.4),
       lwd = 3)
legend(x = "bottomright",
       pch = NA,
       legend = c(paste("Correlation:", round(cor(x,y), digits = 2)),
                  paste("R-squared:", round(summary(lm(y~x))$adj.r.squared, digits = 2))),
       bty = "n")


# CONSTRUCT MODEL
fit <- glm(wic ~ seconds + age + tobacco + medicaid,
           data = alachua,
           family = binomial("logit"))
summary(fit)
exp(coef(fit))


# Make predictions
fake <- data.frame("seconds" = rep(0:1, each = 2),
                   "wic" = c(TRUE, FALSE))
fake$lbw <- predict(fit,
                    newdata = fake)
fake$lbw <- exp(fake$lbw)

# Barplot predictions
x <- rbind(fake$lbw[which(!fake$wic)],
           fake$lbw[which(fake$wic)])
barplot(x, beside = T,
        col = adjustcolor(c("darkorange", "lightblue"),
                          alpha.f = 0.6),
        names.arg = 1:8,
        xlab = "Category",
        ylab = "Likelihood of LBW",
        border = NA)
legend(x = "topleft",
       bty = "n",
       fill = adjustcolor(c("darkorange", "lightblue"),
                          alpha.f = 0.6),
       legend = c("Non-WIC", "WIC"),
       border = NA)


# odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit), confint(fit)))
x
x <- data.frame(x)
x <- x[-1,]
my_barplot <- barplot(x$OR,
                      names.arg = row.names(x),
                      col = "darkorange",
                      ylim = c(0,max(x$OR)*1.2),
                      border = NA,
                      cex.names = 0.4,
                      las = 3)

########
# SURFACE FUNCTION
########
###################
#DEFINE AND SET WD
###################

if ( Sys.info()["sysname"] == "Linux" ){
  
  originalwd <- "/media/joebrew/JB/fdoh/private/mom_care/"
  vitalwd <- "/media/joebrew/JB/fdoh/private/vital_stats/"
  
} else {
  
  originalwd <- "E:/fdoh/private/mom_care/"
  vitalwd <- "E:/fdoh/private/vital_stats/"
  
}

setwd(originalwd) 

# Define x (category 8 births since 2008)
x <- births[which(births$cat == 8 &
                    births$year >= 2008),]

#################
# READ IN ZIP CODE
#################
setwd(originalwd)
library(rgdal)
zip <- readOGR("zcta", "tl_2010_12_zcta510")
zip$zip <- as.numeric(as.character(zip$ZCTA5CE10))

#

# Get enrolled in wic by zip
zip$enrolled <- NA
for (i in zip$zip){
  zip$enrolled[which(zip$zip == i)] <-
    nrow(x[which(x$wic &
                   x$zip == i),])
}

# Get not total known enrollment status (Y or N) by zip
zip$denom <- NA
for (i in zip$zip){
  zip$denom[which(zip$zip == i)] <-
    nrow(x[which(x$zip == i),])
}

# Get rate per 100,000
zip$rate <- zip$enrolled / zip$denom * 100

# Read in state and collapse into just boundary
library(maptools)
state <- readOGR("FCTY2", "FCTY2")
boundary <- unionSpatialPolygons(state, rep(1, length(state@polygons)))

# Define color vector
my_colors <- colorRampPalette(c("darkorange", "lightblue"))(100)

# getting coordinates of alachua boundary
boundary_points <- boundary@polygons[[1]]@Polygons
boundary_points <- boundary_points[[14]]@coords
#x <- unlist(boundary_points)


# Get trap locations and data values
a <- data.frame("x" = coordinates(zip)[,1],
                "y" = coordinates(zip)[,2],
                "z" = zip$rate)

# Make into a geodata object
library(gstat)
library(geoR)
library(rgdal)
b <- as.geodata(a)

# Predict multiple points in Florida's boundary
x <- seq(min(boundary_points[,1]), max(boundary_points[,1]), length = 500)
y <- seq(min(boundary_points[,2]), max(boundary_points[,2]), length = 500)

# Make a grid of those points
pred.grid <- expand.grid(x,y)


# kriging calculations
kc <- krige.conv(geodata = b, coords = b$coords, data = b$data,
                 locations = pred.grid,
                 borders = boundary_points,
                 #borders = boundary@polygons,
                 # borders = ALACHUA BORDERS!,
                 krige = krige.control(type.krige = "ok",
                                       cov.pars = c(10,3.33))) #10, 3.33 # what is this?



# Plot!
# displaying predicted values
image(kc, loc = pred.grid, 
      col = my_colors,
      xlab=NA, ylab=NA,
      xaxt = "n",
      yaxt = "n",
      xpd = NA,
      bty = "n")

# Define percentiles for legend
legtemp <-  round(quantile(kc$predict, probs = seq(0,1,, length = 10)))

legend(x="topright",
       fill = my_colors[c(1,11,22,33,44,55,66,77,88,100)],
       legend = c(legtemp[1], NA, NA, legtemp[4], NA, NA, legtemp[7], NA, NA, legtemp[10]),
       border = FALSE,
       bty = "n",
       ncol = 1,
       y.intersp = 0.5,
       title = "Interpolation",
       cex = 0.75)

plot(fl, add = T)



#####
# ASSOCIATION OF MEDICAID MOTHERS ENROLLED INTO WIC AND LBW AMONG THEM
#####
x <- fl@data$med_wic_rate2013
y <- fl@data$med_lbw_rate2013
size <- (fl@data$medicaid_births2013)^(1/5) 
my_colors <- colorRampPalette(c("lightblue", "darkorange"))(max(round(y*100)))
my_colors <- adjustcolor(my_colors, alpha.f = 0.7)

plot(x,y, cex = size,
     xlab = "Percent of Medicaid mothers enrolled in WIC",
     ylab = "Percent LBW among Medicaid mothers",
     pch = 16,
     col = my_colors[round(y*100)] )
points(x,y, cex = size)     
abline(lm(y~x, weights = fl@data$medicaid_births2013),
       col = adjustcolor("darkred", alpha.f = 0.4),
       lwd = 3)
legend(x = "topleft",
       pch = NA,
       legend = c(paste("Correlation:", round(cor(x,y), digits = 2)),
                  paste("R-squared:", round(summary(lm(y~x))$adj.r.squared, digits = 2))),
       bty = "n")

county_fit <- lm(med_lbw_wic_rate2012 ~ med_wic_rate2012, 
                  data = fl@data,
                 weights = medicaid_births2012)
summary(county_fit)


#####
# ASSOCIATION OF CAT 8 MOTHERS ENROLLED INTO WIC AND LBW AMONG THEM
#####
x <- fl@data$cat8_wic_rate2013
y <- fl@data$cat8_lbw_rate2013

plot(x,y, cex = (fl@data$cat8_births2013)^ (1/5))
text(x, y, labels = fl@data$name)


fit <- glm(lbw ~ wic*factor(cat),
           data = births[which(births$year == 2013),],
           family = binomial("logit"))

summary(fit)

# Make predictions
fake <- data.frame("cat" = rep(1:8, each = 2),
                   "wic" = c(TRUE, FALSE))
fake$lbw <- predict(fit,
                    newdata = fake)
fake$lbw <- exp(fake$lbw)

y <- data.frame("category" = 1:8,
                "Non-WIC" =  fake$lbw[which(!fake$wic)],
                "WIC" =  fake$lbw[which(fake$wic)])

# Barplot predictions
x <- rbind(fake$lbw[which(!fake$wic)],
           fake$lbw[which(fake$wic)])
barplot(x, beside = T,
        col = adjustcolor(c("darkorange", "lightblue"),
                          alpha.f = 0.6),
        names.arg = 1:8,
        xlab = "Category",
        ylab = "Likelihood of LBW",
        border = NA)
legend(x = "topleft",
       bty = "n",
       fill = adjustcolor(c("darkorange", "lightblue"),
                          alpha.f = 0.6),
       legend = c("Non-WIC", "WIC"),
       border = NA)



#############
# MAPS
#############
# Source function from github (dot map, choropleth map, etc.)
library(devtools)
source_url("https://raw.githubusercontent.com/joebrew/misc/master/dot_map/dot_map.R")
source_url("https://raw.githubusercontent.com/joebrew/misc/master/functions/functions.R")
fl_boundary <- collapse_map(fl)

#####
# PLOT INCIDENCE OF LBW
#####
choro(shape = fl,
      boundary = fl_boundary,
      var = fl$lbw_rate2014 * 100,
      color1 = "darkorange",
      color2 = "lightblue",
      legend_round = 0,
      legend_pos = "bottom",
      long_legend = TRUE,
      fixed_scale = NULL,
      lwd = 0.2,
      border = TRUE)

#####
# PLOT CHOROPLETH OF 2013 WIC ENROLLMENTS
#####
par(mfrow = c(1,2))

choro(shape = fl,
      boundary = fl_boundary,
      var = fl$cat8_wic_rate2013 * 100,
      color1 = "darkorange",
      color2 = "lightblue",
      legend_round = 0,
      legend_pos = "bottom",
      long_legend = TRUE,
      fixed_scale = NULL,
      lwd = 0.2,
      border = TRUE)
title(main = "Percent of Medicaid mothers enrolled in WIC",
      cex.main = 0.8)

choro(shape = fl,
      boundary = fl_boundary,
      var = fl$cat8_lbw_wic_rate2013 * 100,
      color1 = "lightblue",
      color2 = "darkorange",
      legend_round = 0,
      legend_pos = "bottom",
      long_legend = TRUE,
      fixed_scale = NULL,
      lwd = 0.2,
      border = TRUE)
title(main = "Percent LBW among Medicaid mothers",
      cex.main = 0.8)

par(mfrow = c(1,1))

# Cat 8 enrollment into WIC
choro(shape = fl,
      boundary = fl_boundary,
      var = fl$cat8_wic_rate2013 * 100,
      color1 = "darkorange",
      color2 = "lightblue",
      legend_round = 0,
      legend_pos = "bottom",
      long_legend = TRUE,
      fixed_scale = NULL,
      lwd = 0.2,
      border = TRUE)
title(main = "Percentage of category 8 mothers enrolled in WIC",
      cex.main = 0.8)

# ADD AN OUTLINE FOR NORTH FLORIDA WIC
nf <- fl[which(fl$name %in% 
                 c("ALACHUA", "BRADFORD", "COLUMBIA",
                   "DIXIE", "GILCHRIST", "HAMILTON",
                   "LAFAYETTE", "LEVY", "SUWANNEE",
                   "UNION")),]
nf_boundary <- collapse_map(nf)

plot(nf_boundary, add = TRUE, lwd = 3)

library(maps)
map.text("county", "fl", add = T,
         col = adjustcolor("black", alpha.f = 0.5),
         cex = 0.3)

#####
# PLOT DOTS FOR EACH LIVE BIRTH
#####


# Map WIC enrollment of medicaid mothers and WIC medicaid lbw rate

# Plot dots for live 2013 births
DotFun(main_shape = fl,
       points_var = fl$total_births2013,
       points_col = "black",
       cex = 0.01,
       border = "grey",
       fill = "white",
       alpha = 0.1)

# Plot dots for LBW
DotFun(main_shape = fl,
       points_var = fl$lbw_births2013,
       points_col = "darkred",
       cex = 0.01,
       border = NA,
       fill = "white",
       alpha = 0.2,
       add = T)


#####
# Get 2013 Alachua births by zip code
#####
alachua <- births[which(births$county == "ALACHUA"),]

# sort and tabulate by zip code
x <- alachua %>%
  filter(year == 2013) %>%
  group_by(zip) %>%
  summarise(births = n())

#####
# READ IN ALACHUA COUNTY SHAPEFILE WITH POP NUMBERS
#####
ct <- readOGR(paste0(public, "/Alachua_CT_POP"), "Alachua_CT_POP")

#####
# COLLAPSE MAP INTO ONLY OUTER BOUNDARY
#####
collapse_map <- function(x){
  require(maptools)
  boundary <- unionSpatialPolygons(x, rep(1, length(x@polygons)))
}

ct_boundary <- collapse_map(ct)

#####
# BRING ALACHUA DATA INTO CT
#####
# ct@data <- left_join(x = ct@data,
#                      y = x,
#                      )

##########################################
# PREPARATION FINISHED
##########################################

#####
# LBW RATE BY CAT
dat <- data.frame("cat" = 1:8)

# total births
x <- births %>%
  group_by(cat) %>%
  summarise(total_births = n())
x <- x[which(!is.na(x$cat)),]
dat$total_births <- x$total_births

# lbw_births
x <- births %>%
  filter(weight < 2500) %>%
  group_by(cat) %>%
  summarise(lbw_births = n())
x <- x[which(!is.na(x$cat)),]
dat$lbw_births <- x$lbw_births

# lbw_rate
dat$lbw_rate <- dat$lbw_births / dat$total_births

### PLOTS
my_colors <- colorRampPalette(c("lightblue", "darkorange"))(8)
barplot(dat$total_births/sum(dat$total_births)*100, 
        names.arg = dat$cat,
        col = my_colors,
        ylab = "Percent",
        border = FALSE)
title(main = "Births by category")

my_colors <- colorRampPalette(c("lightblue", "darkorange"))(8)
barplot(dat$lbw_rate*100, 
        names.arg = dat$cat,
        col = my_colors,
        ylab = "%",
        border = FALSE)
title(main = "LBW rate by category")



###
# Time series chart
###
ts <- data.frame("year" = 2000:2014)
for (i in ts$year){
  ts$lbw_rate[which(ts$year == i)] <-
    sum(counties[,paste0("lbw_births",i)], na.rm = TRUE) / 
  sum(counties[,paste0("total_births",i)], na.rm = TRUE)
}

# plot state
plot(ts$year,
     ts$lbw_rate,
     ylim = c(0.06,0.14),
     xlab = "Year",
     ylab = "LBW rate",
     type = "l",
     col = adjustcolor("black", alpha.f = 0.7),
     lwd = 3)
points(ts$year,
     ts$lbw_rate,
     pch = 16)

# add lines for counties
library(splines)
#my_colors <- adjustcolor(rainbow(nrow(counties)), alpha.f = 0.6)
my_colors <- colorRampPalette(c("grey", "black"))(nrow(counties))
my_colors <- adjustcolor(my_colors, alpha.f = 0.6)
for (i in 1:nrow(counties)){
  xspline(x = 2000:2014,
        y = counties[i,
                     names(counties[grepl("lbw_rate", names(counties))])],
        border = my_colors[i], 
        shape = 1)
}

# thick line for alachua
xspline(x = 2000:2014,
        y = counties[which(counties$county == "ALACHUA"),
                     names(counties[grepl("lbw_rate", names(counties))])],
        border = "red", 
        shape = 1, lwd = 2)

##################
# MAPPING
##################
# Create boundary shapefile of just florida
library(maptools)
library(RColorBrewer)
library(classInt)
boundary <- unionSpatialPolygons(fl, rep(1, length(fl@polygons)))


################
# MAP OF ALACHUA
################
setwd(public)
zip <- readOGR("alachuazipcodes", "ACDPS_zipcode")

#####################33
# GOOGLE VIS
###########################
# suppressPackageStartupMessages(library(googleVis))
# x <- gvisBubbleChart(data = fl@data,
#                      idvar = "name",
#                      xvar = "percent_black",
#                      yvar = "lbw_rate2013")
# plot(x)
###
# EXPLORE A BIT
###

### barplot lbw 2013 by county
my_colors <- colorRampPalette(c("lightblue", "darkorange"))(nrow(counties))
x <- counties[order(counties$lbw_rate2013),]
mybp <- barplot(x$lbw_rate2013,
        names.arg = x$county,
        las = 3,
        cex.names = 0.3,
        col = my_colors)
text(x = mybp[which(x$county == "ALACHUA"),1],
     y = .09,
     labels = "Alachua")
title(main = "2013 LBW rate by county")

###
# scatterplot

# Income
x <- fl@data[order(fl@data$lbw_rate2013),]
plot(x = x$INCOME,
     y = x$lbw_rate2013,
     xlab = "Income",
     ylab = "LBW rate",
     col = adjustcolor(my_colors, alpha.f = 0.4),
     pch = 16,
     cex = ((x$total_births)^(1/4))/3)
points(x = x$INCOME,
     y = x$lbw_rate2013,
    col = adjustcolor(my_colors, alpha.f = 1),
    cex = ((x$total_births)^(1/4))/3)

lox <- x$INCOME
loy <- x$lbw_rate2013
lw1 <- loess(loy ~ lox, span=0.5,
             weights = x$total_births)
j <- order(lox)
lines(lox[j],lw1$fitted[j],col="red", lty=1)

# Race
x <- fl@data[order(fl@data$lbw_rate2013),]
plot(x = x$percent_black,
     y = x$lbw_rate2013,
     xlab = "Percent African American",
     ylab = "LBW rate",
     col = adjustcolor(my_colors, alpha.f = 0.4),
     pch = 16,
     cex = ((x$total_births)^(1/4))/3)
points(x = x$percent_black,
       y = x$lbw_rate2013,
       col = adjustcolor(my_colors, alpha.f = 1),
       cex = ((x$total_births)^(1/4))/3)

lox <- x$percent_black
loy <- x$lbw_rate2013
lw1 <- loess(loy ~ lox, span=1,
             weights = x$total_births)
j <- order(lox)
lines(lox[j],lw1$fitted[j],col="red", lty=1)



library(dplyr)
x <- births %>%
  group_by(age) %>%
  summarise(n = median(weight, na.rm = TRUE))
x <- x[which(x$age > 12 & x$age <=50),]
plot(x$age, x$n)
boxplot(births$weight ~ births$age)

library(hexbin)
HexFun <- function(x, y, xlab = NA, ylab = NA, main = NA){
  bin<-hexbin(x, y, xbins=50) 
  plot(bin, main=main,
       xlab = xlab,
       ylab = ylab)
}
HexFun(x = births$age,
       y = births$weight,
       xlab = "Mother's age",
       ylab = "Birth weight (grams)",
       main = "Age and weight")


# Histogram of LBW
hist(births$weight / 453.6,
     breaks = 30,
     xlab = "Weight (pounds)",
     freq = FALSE,
     las = 1,
     main = "Distribution of weight at birth",
     col = "grey",
     border = "darkgrey")
abline(v = 2500 / 453.6,
       col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)
abline(v = 1500 / 453.6,
       col = adjustcolor("purple", alpha.f = 0.6),
       lwd = 2)
legend("topright",
       lty = 1,
       lwd = 2,
       col = adjustcolor(c("darkred", "purple"), alpha.f = 0.6),
       legend = c("low birth weight", "very low birth weight"),
       cex = 0.7,
       bty= "n")

hist(births$weight[which(births$white)] / 453.6,
     breaks = 30,
     xlab = "Weight (pounds)",
     freq = FALSE,
     las = 1,
     main = "Distribution of weight at birth",
     col = "grey",
     border = "darkgrey")
abline(v = 2500 / 453.6,
       col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)
abline(v = 1500 / 453.6,
       col = adjustcolor("purple", alpha.f = 0.6),
       lwd = 2)
legend("topright",
       lty = 1,
       lwd = 2,
       col = adjustcolor(c("darkred", "purple"), alpha.f = 0.6),
       legend = c("low birth weight", "very low birth weight"),
       cex = 0.7,
       bty= "n")

x <- births[which(births$age > 12 &
                    births$age <= 40),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

plot(x$age, x$weight, type = "l",
     xlab = "Age",
     ylab = "Weight (grams)",
     main = "Relationship between mother's age and birth weight",
     lty = 1,
     lwd = 2,
     col = adjustcolor("black", alpha.f = 0.6),
     las = 3,
     ylim = c(3000, 3400))
legend("topleft",
       lty = 1,
       col = adjustcolor(c(
         "black",
         "darkred",
         "purple",
         "darkgreen",
         "darkorange"),
         alpha.f = 0.6),
       legend = c("All",
                  "black",
                  "white",
                  "'high-risk' white",
                  "'high-risk' black",
                  "tobacco use during pregnancy"),
       bty = "n",
       border = NA,
       cex = 0.8)


x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$black),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("darkred", alpha.f = 0.6))

x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$white),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("purple", alpha.f = 0.6))

x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$cat == 4),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("darkgreen", alpha.f = 0.6))

x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$cat == 8),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("darkorange", alpha.f = 0.6))



x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$tobacco),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("grey", alpha.f = 0.6))


