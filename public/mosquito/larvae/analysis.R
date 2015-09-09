setwd("E:/fdoh/public/mosquito/larvae")

lar <- read.csv("july2014.csv", skip=1)

#############
# READ IN LAT LONG SCRIPT FROM GOOGLE
#############
#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}



# Use plyr to getgeocoding for a vector
address <- paste0(lar$site, ", Gainesville, FL")
locations <- ldply(address, function(x) geoCode(x))
names(locations) <- c("lat","lon","location_type", "forAddress")

# CBIND IT ALL TOGETHER
lar <- cbind(lar, locations)
lar$lat <- as.numeric(lar$lat)
lar$lon <- as.numeric(lar$lon)
###################
#LOAD  PACKAGES
###################
library(RColorBrewer)
library(xtable)
library(gdata)
library(plyr)
library(maps)
library(mapdata)
library(maptools)
library(RColorBrewer)
library(rJava)
library(OpenStreetMap)
library(rgdal)
library(shapefiles)
library(SemiPar)
library(wordcloud)
library(RCurl)
library(classInt)

# MAP OF ALACHUA COUNTY
###################
#MAP PARAMETERS
###################
joewatercolor <- openmap(c(29.96, -82.7), c(29.35,-81.9),
                        type="stamen-watercolor")
joemapwatercolor <- openproj(joewatercolor, projection = "+proj=longlat")
plot(joemapwatercolor)

alachuacol <- NA

countycols <- rep("white", 67)

map("county", "florida", 
    fill=TRUE, 
    col=c(alachuacol,countycols),
    xlim=c(-82.75,-82), 
    ylim=c(29.3,30.1),
    border="white", 
    add=T)

points(x = jitter(lar$lon[which(is.na(lar$larvae_per_dip))], factor=5),
       y = jitter(lar$lat[which(is.na(lar$larvae_per_dip))], factor=5),
       col = adjustcolor("black", alpha.f=0.6),
       cex = 0.6,
       pch=16)

points(x = jitter(lar$lon[which(!is.na(lar$larvae_per_dip))], factor=1),
       y = jitter(lar$lat[which(!is.na(lar$larvae_per_dip))], factor=1),
       col = adjustcolor("darkred", alpha.f=0.6),
       cex = lar$larvae_per_dip[which(!is.na(lar$larvae_per_dip))] / 2,
       pch=16)

legend(x="bottom",
       pch=16,
       col=adjustcolor("black", alpha.f=0.6),
       legend = "Trap site",
       bty="n",
       pt.cex=0.6)

legend(x="bottomleft",
       pch=16,
       col=adjustcolor("darkred", alpha.f=0.6),
       legend = c(2,4,6,8),
       bty="n",
       pt.cex=c(2,4,6,8)/2,
       title="Larvae per dip", 
       ncol=2)
title(main="July 2014: Inspections and Larvaciding")

