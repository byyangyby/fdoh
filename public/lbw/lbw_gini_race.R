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
# LOAD 2010-2014 DATA
###
 load("births_2000-2014.RData")
# births <- births[which(births$year >= 2010),]
# save.image("births_2010-2014.RData")
# load("births_2010-2014.RData")

#####
# CLEAN UP ZIP CODE
#####
births$zip <- as.numeric(substr(births$zip, 1, 5))

#####
# ADD THOMPSON / CLARK CATEGORIES
#####

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

### ZIP CODE
# Zip code shape file
zip <- readOGR("zip", "zipbnd_2012")

# Get centroids for each zip code
zip_df <- data.frame(zip@data)
zip_df$x <- coordinates(zip)[,1]
zip_df$y <- coordinates(zip)[,2]

# Make zip_df spatial
coordinates(zip_df) <- ~x+y
proj4string(zip_df) <- proj4string(zip)

# Convert to lat lon
zip_df <- spTransform(zip_df, 
                      CRS("+init=epsg:4326"))

# Clean up a bit
zip_df$zip <- as.numeric(as.character(zip_df$ZIP))
zip_df <- data.frame(zip =  zip_df$zip, 
                     lat = coordinates(zip_df)[,2],
                     lon = coordinates(zip_df)[,1])

# Merge lat lon coordinates into births
library(dplyr)
births <- left_join(x = births,
                    y = zip_df,
                    by = "zip")

### COUNTY
# County shape file
fl <- readOGR("florida_counties", "counties")
fl$name <- toupper(fl$NAME)

# since merging messes with order, assign and id and keep in this order
fl$id <- 1:nrow(fl)

# test some stuff with the map
fl$color <- "blue"
fl$color[which(fl$name == "ALACHUA")] <- "red"
fl$color[which(fl$name == "MARION")] <- "green"
fl$color[which(fl$name == "MIAMI-DADE")] <- "orange"
plot(fl, col = fl$color)

#####
# READ IN GINI COEFFICIENT DATA FOR EACH USA COUNTY
#####
gini <- read.csv(paste0(public, "/gini_all_counties.csv"), skip = 3)

# Clean up column names
names(gini) <- c("county", "gini", "moe")

# Subset to only include florida
gini <- gini[which(grepl(", Florida", gini$county)),]

# Clean up county names
gini$county <- as.character(gini$county)
gini$county <- toupper(gsub(" County, Florida", "", gini$county))

# Compare county names in gini to fl
table(unique(sort(fl@data$name)) == unique(sort(gini$county)))

# Make a name column in gini
gini$name <- gini$county

# Join gini to fl
fl@data <- merge(x = fl@data,
                 y = gini,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)
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

################
# GET SUMMARY BIRTH STATISTICS BY COUNTY
################

# Get lbw stats for each county
library(dplyr)

x <- births %>%
  group_by(year, county) %>%
  summarise(total_births = n(),
            black_births = length(black[which(black)]),
            lbw_births = length(lbw[which(lbw)]),
            teenage_births = length(age[which(age < 20)]),
            married_births = length(married[which(married)]),
            tobacco_births = length(tobacco[which(tobacco)]),
            medicaid_births = length(medicaid[which(medicaid)]),
            mean_age = mean(age, na.rm = TRUE))

# Add rates
x <- x %>%
  mutate(black_rate = black_births / total_births,
         lbw_rate = lbw_births / total_births,
         teenage_rate = teenage_births / total_births,
         married_rate = married_births / total_births,
         tobacco_rate = tobacco_births / total_births,
         medicaid_rate = medicaid_births / total_births)

#(create a dataframe to stick all my aggregations into)
counties <- data.frame(x)


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

# Merge gini into counties
table(unique(sort(counties$name)) == unique(sort(gini$county)))

counties <- left_join(x = counties,
                      y = gini,
                      by = "name")

# Merge just 2013 data into map
counties13 <- counties[which(counties$year == 2013),]

# Merge the two datasets together
fl@data <- merge(x = fl@data,
                 y = counties13,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)

rm(x, std, m, best, total_births, ind)

#########################################
# IDENTIFY CLUSTERS
##########################################

######
# Attach packages
#######
library(gdata)
library(dplyr)
library(maptools)
library(rgdal)
library(spatstat)
library(maps)
library(SpatialEpi)

#####
# MODEL BASED ONLY ON CATEGORIES
#####
fit <- glm(lbw ~ factor(cat),
           data = births,
           family = binomial("logit"))

# Add a predicted column to each birth row
births <- births[!is.na(births$cat) & !is.na(births$lbw),]
births$predicted <-fitted.values(fit)

#####
# GET DATA BY ZIP CODE
#####
zip_data <- births %>%
  group_by(zip) %>%
  summarise(total_births = n(),
            black_births = length(black[which(black)]),
            lbw_births = length(lbw[which(lbw)]),
            teenage_births = length(age[which(age < 20)]),
            married_births = length(married[which(married)]),
            tobacco_births = length(tobacco[which(tobacco)]),
            medicaid_births = length(medicaid[which(medicaid)]),
            mean_age = mean(age, na.rm = TRUE),
            cat1 = length(cat[which(cat == 1)]),
            cat2 = length(cat[which(cat == 2)]),
            cat3 = length(cat[which(cat == 3)]),
            cat4 = length(cat[which(cat == 4)]),
            cat5 = length(cat[which(cat == 5)]),
            cat6 = length(cat[which(cat == 6)]),
            cat7 = length(cat[which(cat == 7)]),
            cat8 = length(cat[which(cat == 8)]),
            predicted_lbw_births = sum(predicted))

# Add rates
zip_data <- zip_data %>%
  mutate(black_rate = black_births / total_births,
         lbw_rate = lbw_births / total_births,
         teenage_rate = teenage_births / total_births,
         married_rate = married_births / total_births,
         tobacco_rate = tobacco_births / total_births,
         medicaid_rate = medicaid_births / total_births,
         cat1_rate = cat1 / total_births, 
         cat2_rate = cat2 / total_births, 
         cat3_rate = cat3 / total_births, 
         cat4_rate = cat4 / total_births, 
         cat5_rate = cat5 / total_births, 
         cat6_rate = cat6 / total_births, 
         cat7_rate = cat7 / total_births, 
         cat8_rate = cat8 / total_births,
         predicted_lbw_rate = predicted_lbw_births / total_births)


######
# Use SpatialEpi's kulldorff function for clustering detection
# (spatial scan)
######

# Make zip_data geographical

zip_data <- left_join(zip_data, 
                      zip_df,
                      by = "zip")
zip_data <- data.frame(zip_data)
zip_data <- zip_data[which(!is.na(zip_data$lon) & !is.na(zip_data$lat)),]
coordinates(zip_data) <- ~lon+lat

my_geo <- latlong2grid(coordinates(zip_data))
my_cases <- zip_data$lbw_births
my_population <- zip_data$total_births
n_strata <- nrow(zip_data)
expected_cases <- zip_data$predicted_lbw_births

# LOOP TO GET CLUSTERS AT DIFFERENT POP UPPER BOUND LEVELS
for (i in c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5)){
  ## Kulldorff using Poisson likelihoods
  k_poisson <- kulldorff(geo = my_geo,
                         cases = my_cases,
                         population = my_population,
                         expected.cases = expected_cases,
                         pop.upper.bound = i,
                         n.simulations = 999,
                         alpha.level = 0.05,
                         plot = TRUE)
  assign(paste0("cluster", i), k_poisson$most.likely.cluster$location.IDs.included
  )
}

## plot
plot(zip, col = "grey", border = NA)
plot(zip[cluster0.5,],add=TRUE,col=adjustcolor("yellow", alpha.f = 0.9), border = "darkgrey")
plot(zip[cluster0.25,],add=TRUE,col=adjustcolor("orange", alpha.f = 0.9), border = "darkgrey")
plot(zip[cluster0.1,],add=TRUE,col=adjustcolor("darkorange", alpha.f = 0.9), border = "darkgrey")
plot(zip[cluster0.05,],add=TRUE,col=adjustcolor("red", alpha.f = 0.9), border = "darkgrey")
plot(zip[cluster0.01,],add=TRUE,col=adjustcolor("darkred", alpha.f = 0.9), border = "darkgrey")
plot(zip[cluster0.001,],add=TRUE,col=adjustcolor("purple", alpha.f = 0.9), border = "darkgrey")

legend("bottom",
       fill = c("purple", "darkred","red",  "darkorange", "orange", "yellow"),
       legend = c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5),
       title = "PIL",
       bty = "n",
       border = "darkgrey")

## Kulldorff using Poisson likelihoods
k_poisson <- kulldorff(geo = my_geo,
                       cases = my_cases,
                       population = my_population,
                       expected.cases = expected_cases,
                       pop.upper.bound = 0.1,
                       n.simulations = 999,
                       alpha.level = 0.05,
                       plot = TRUE)

cluster <- k_poisson$most.likely.cluster$location.IDs.included
secondary_cluster <- as.numeric(
  unlist(
    k_poisson$secondary.clusters)[which(
      grepl(
        "location.IDs.included", 
        names(unlist(k_poisson$secondary.clusters))))])

## plot
plot(zip, col = "grey", border = "black")
plot(zip[cluster,],add=TRUE,col=adjustcolor("darkred", alpha.f = 0.9))
plot(zip[secondary_cluster,],add=TRUE,col=adjustcolor("darkorange", alpha.f = 0.9))

title(main = "Most Likely Cluster after 999 Monte-Carlo simulations")
legend("bottom",
       fill = c("darkred", "darkorange"),
       legend = c("Primary cluster", "Secondary cluster"),
       bty = "n")


######
# PRINT INFORMATION
######
summary(k_poisson)
k_poisson$most.likely.cluster
#Cluster:
as.character(stl$NAME[cluster])

##########################################
##########################################

##########################################
##########################################

#####
# PLOT CORRELATIONS
#####

lbw_scatter <- function(var,
                        color1 = "lightblue",
                        color2 = "darkred",
                        color_var = counties$gini,
                        ylab = "Incidence of LBW",
                        xlab = NA,
                        size_var = counties$total_births){
  
  my_colors <- colorRampPalette(c(color1, "grey", color2))(10)
  
  my_quantiles <- quantile(color_var, na.rm = TRUE, probs = seq(0,1, length = 10))
  
  my_values <- vector(mode = "numeric", length = length(color_var))
  for (i in 1:length(color_var)){
    diffs <- (color_var[i] - as.numeric(my_quantiles))^2
    best <- which.min(diffs)[1]
    my_values[i] <- best
  }
  plot_colors <- my_colors[my_values]
  
  plot(x = var,
       y = counties$lbw_rate,
       cex = (size_var)^(1/5)/3,
       col = plot_colors,
       pch = 16,
       ylab = ylab,
       xlab = xlab,
       ylim = c(0.03, max(counties$lbw_rate)*1.05))
  
  points(x = var,
         y = counties$lbw_rate,
         col = adjustcolor("black", alpha.f = 0.6),
         cex = (size_var)^(1/5)/3)
  
  abline(lm(counties$lbw_rate~var),
         col = adjustcolor("grey", alpha.f = 0.4),
         lwd = 3)
  
  abline(h = seq(0, 1, 0.02),
         col = adjustcolor("black", alpha.f = 0.2))
  abline(v = seq(0, 1, 0.1),
         col = adjustcolor("black", alpha.f = 0.2))
  
  legend(x = "bottomright",
         pch = NA,
         legend = c(paste("Correlation:", round(cor(var,counties$lbw_rate), digits = 2)),
                    paste("R-squared:", round(summary(lm(counties$lbw_rate~var))$r.squared, digits = 2))),
         #bty = "n",
         cex = 0.7)
  
  legend(x = "bottom",
         pch = 1,
         pt.cex = quantile(size_var,  probs = c(0.25, 0.5, 1))^(1/5)/3,
         legend = 0.2*round(quantile(size_var, probs = c(0.25, 0.5, 1)), digits = -2),
         #bty = "n",
         ncol = 3,
         title = "Annual births",
         cex = 0.7)
  
  legend(x = "bottomleft",
         pch = 16,
         col = my_colors[c(1,5,10)],
         legend = round(my_quantiles[c(1,5,10)], digits = 2),
         #bty = "n",
         title = "Gini coefficient",
         cex = 0.7,
         ncol = 3,
         pt.cex = 2)

  
}

# teenage_rate
lbw_scatter(var = counties$teenage_rate * 100,
            xlab = "% teenage births")

# tobacco_rate
lbw_scatter(var = counties$tobacco_rate,
            xlab = "% tobacco use during pregnancy")

# medicaid_rate
lbw_scatter(var = counties$medicaid_rate,
            xlab = "% births on medicaid")

# married_rate
lbw_scatter(var = counties$married_rate * 100,
            xlab = "% married at birth")

# black_rate
lbw_scatter(var = counties$black_rate,
            xlab = "% black")

par(mfrow = c(1,1))


#####
# GOOGLE VIS
#####

counties_small <- counties[,c(
  "name",
  "year",
  "total_births",
  "black_rate",
  "lbw_rate",
  "teenage_rate",
  "married_rate",
  "tobacco_rate",
  "medicaid_rate",
  "mean_age",
  "gini")]
suppressPackageStartupMessages(library(googleVis))

x <- gvisMotionChart(counties_small, 
                     idvar="name", 
                     timevar="year",
                     xvar = "married_rate",
                     yvar = "lbw_rate",
                     colorvar = "gini",
                     sizevar = "total_births")
plot(x)


#############
# MAPS
#############
# Source function from github (dot map, choropleth map, etc.)
library(devtools)
# For creating dot map
source_url("https://raw.githubusercontent.com/joebrew/misc/master/dot_map/dot_map.R")
# For creating choropleth maps
source_url("https://raw.githubusercontent.com/joebrew/misc/master/functions/functions.R")

#####
# MAP LOW BIRTH WEIGHT RATE
#####

# Collapse florida boundary
fl_boundary <- collapse_map(fl)

# Choropleth of LBW
choro(shape = fl,
      boundary = fl_boundary,
      var = fl$lbw_rate * 100,
      color1 = "lightblue",
      color2 = "darkorange",
      legend_round = 0,
      legend_pos = "bottom",
      long_legend = FALSE,
      fixed_scale = NULL,
      lwd = 0.2,
      border = FALSE,
      legend_text_col = "black")
title(main = "LBW Rate",
      cex.main = 0.8)

##########################################
# MODELING
##########################################
alachua_births <- births[which(births$county == "ALACHUA" &
                                 births$year > 2010),]
# LOGISTIC REGRESSION
fit <- glm(lbw ~ black + tobacco + age + wic + medicaid,
           data = alachua_births,
           family = binomial("logit"))
summary(fit)
data.frame(exp(coef(fit)))

# RANDOM FOREST
library(randomForest)


rf <- randomForest(lbw ~ black + tobacco + age + wic + medicaid + lat + lon,
                   data = alachua_births,
                   na.action = na.omit,
                   importance = TRUE)

# CLASSIFICATION TREE
library(party)
my_tree <- ctree(lbw ~ black + tobacco + age + wic + medicaid + lat + lon,
                   data = alachua_births)

plot(my_tree)

# GAM including location #############
library(mgcv)
source("myvisgam.R")
small_births <- sample_n(births, 1000000)
system.time( # 520 seconds
  fit <- bam(lbw ~ 
                 s(age) + as.numeric(black) + as.numeric(medicaid) +
               as.numeric(wic) +
               as.numeric(tobacco) +
                 s(lon, lat, bs="ad"),               
               data = small_births,
               family=binomial("logit"))
)

# RANDOM FOREST
library(randomForest)
#small_births <- births[,c("year", "age", "married", "wic", "black", "tobacco", "medicaid", "lat", "lon")]
rf1 <- randomForest(lbw ~ 
                      age + black + medicaid +
                      wic + tobacco,
                    data = small_births,
                    na.action = na.omit)

plot(fit)
myvis.gam(fit,
          type = "response",
          color = "jet")
myvis.gam(fit,
          type = "response",
          color = "jet",
          view = c("tobacco", "age"))
myvis.gam(fit,
          view = c("lon", "lat"),
          color = "jet",
          n.grid = 500,
          plot.type = "contour",
          type = "response",
          too.far = 1)
plot(fl_boundary, add = T, lwd = 2)
plot(fl, add = T)

mylm <- lm(lbw_rate*100 ~ 
             teenage_rate +
             married_rate +
             tobacco_rate + 
             black_rate + 
             medicaid_rate +
             gini +
             black_rate,
           data = counties)
summary(mylm)

# BARPLOT OF INCIDENCE BY YEAR
x <- births %>% 
  group_by(year) %>% 
  summarise(lbw = length(lbw[which(lbw)]),
            total = n())
x2 <- births %>% 
  filter(county == "ALACHUA") %>%
  group_by(year) %>% 
  summarise(lbw = length(lbw[which(lbw)]),
            total = n())

barplot(x$lbw / x$total * 100,
        names.arg = x$year,
        density = 20, angle = 45,
        ylim = c(0,10),
        ylab = "LBW rate",
        xlab = "Year")
abline(h = c(0,2,4,6,8,10),
       col = adjustcolor("black", alpha.f = 0.2))
box("plot")
barplot(x2$lbw / x2$total * 100,
        col = adjustcolor("darkred", alpha.f = 0.6),
        add = T)
