library(gdata)
library(maps)
library(mapdata)
library(maptools)
library(RColorBrewer)
library(rJava)
library(OpenStreetMap)
library(rgdal)
library(shapefiles)

#READ IN MAP
setwd("E:/workingdirectory/gis/elem")
elementary <- readShapePoly("SBAC_school_zones_elementary.shp")

#READ IN DATA
setwd("E:/workingdirectory/alexMaps")
dental <- read.xls("dental.xls")
dental$Untreated.cavities <- as.numeric(as.character(gsub("%", "",dental$Untreated.cavities)))
dental$Caries.experience <- as.numeric(as.character(gsub("%", "",dental$Caries.experience)))
dental$Urgent.need <- as.numeric(as.character(gsub("%", "",dental$Urgent.need)))
dental$Sealants <- as.numeric(as.character(gsub("%", "",dental$Sealants)))


dental <- dental[order(dental$Facility),]
#CREATE PALETTE


library(RColorBrewer)
library(classInt)

#MAKE A DENTAL MAP DATABASE
dentalMap <- as.data.frame(elementary$Facility)
colnames(dentalMap) <- "Facility"

par(mfrow=c(1,1))
par(mfrow=c(2,2))
par(oma=c(1,1,1,1))
par(mar=c(1,2,2,3))
######################
#Untreated.cavities
######################
dentalMap$Untreated.cavities <- NA
for (i in dentalMap$Facility){
  dentalMap$Untreated.cavities[which(dentalMap$Facility==i)] <-
    dental$Untreated.cavities[which(dental$Facility==i)]}

#Untreated.cavities
nclr <- 5 # number of bins
min <- floor(min(dentalMap$Untreated.cavities))# theoretical minimum
max <- ceiling(max(dentalMap$Untreated.cavities)) # theoretical maximum
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "Purples")
plotvar <- dentalMap$Untreated.cavities
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

plot(elementary, col="grey",  lty=0)
plot(elementary, col=colcode, lty=0, add=TRUE)
plot(elementary, border="grey", lty=1, lwd=1.4, add=TRUE)

colcode2 <- gsub("[[]|[)]|[]]","",names(attr(colcode, "table")))
colcode2 <- gsub(","," - ", colcode2)
legend("topright", # position
       legend = colcode2, 
       title = "Untreated cavities (%)",
       fill = attr(colcode, "palette"),
       cex = 0.56,
       bty = "n") # border



######################
#Caries.experience
######################
dentalMap$Caries.experience <- NA
for (i in dentalMap$Facility){
  dentalMap$Caries.experience[which(dentalMap$Facility==i)] <-
    dental$Caries.experience[which(dental$Facility==i)]}

#Caries.experience
nclr <- 5 # number of bins
min <- floor(min(dentalMap$Caries.experience))# theoretical minimum
max <- ceiling(max(dentalMap$Caries.experience)) # theoretical maximum
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "Greens")
plotvar <- dentalMap$Caries.experience
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

plot(elementary, col="grey",  lty=0)
plot(elementary, col=colcode, lty=0, add=TRUE)
plot(elementary, border="grey", lty=1, lwd=1.4, add=TRUE)

colcode2 <- gsub("[[]|[)]|[]]","",names(attr(colcode, "table")))
colcode2 <- gsub(","," - ", colcode2)
legend("topright", # position
       legend = colcode2, 
       title = "Caries experience (%)",
       fill = attr(colcode, "palette"),
       cex = 0.56,
       bty = "n") # border



######################
#Urgent.need
######################
dentalMap$Urgent.need <- NA
for (i in dentalMap$Facility){
  dentalMap$Urgent.need[which(dentalMap$Facility==i)] <-
    dental$Urgent.need[which(dental$Facility==i)]}

#Urgent.need
nclr <- 5 # number of bins
min <- floor(min(dentalMap$Urgent.need))# theoretical minimum
max <- ceiling(max(dentalMap$Urgent.need)) # theoretical maximum
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "Reds")
plotvar <- dentalMap$Urgent.need
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

plot(elementary, col="grey",  lty=0)
plot(elementary, col=colcode, lty=0, add=TRUE)
plot(elementary, border="grey", lty=1, lwd=1.4, add=TRUE)

colcode2 <- gsub("[[]|[)]|[]]","",names(attr(colcode, "table")))
colcode2 <- gsub(","," - ", colcode2)
legend("topright", # position
       legend = colcode2, 
       title = "Urgent need (%)",
       fill = attr(colcode, "palette"),
       cex = 0.56,
       bty = "n") # border



######################
#Sealants
######################
dentalMap$Sealants <- NA
for (i in dentalMap$Facility){
  dentalMap$Sealants[which(dentalMap$Facility==i)] <-
    dental$Sealants[which(dental$Facility==i)]}

#Sealants
nclr <- 5 # number of bins
min <- floor(min(dentalMap$Sealants))# theoretical minimum
max <- ceiling(max(dentalMap$Sealants)) # theoretical maximum
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "Blues")
plotvar <- dentalMap$Sealants
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

plot(elementary, col="grey",  lty=0)
plot(elementary, col=colcode, lty=0, add=TRUE)
plot(elementary, border="grey", lty=1, lwd=1.4, add=TRUE)

colcode2 <- gsub("[[]|[)]|[]]","",names(attr(colcode, "table")))
colcode2 <- gsub(","," - ", colcode2)
legend("topright", # position
       legend = colcode2, 
       title = "Sealants (%)",
       fill = attr(colcode, "palette"),
       cex = 0.56,
       bty = "n") # border

title(main="Oral Health Indicators in Alachua County Elementary Schools", outer=TRUE, line=-1)






#











dental$Untreated.cavitiesCat <- cut(dental$Untreated.cavities, breaks=5)
dental$Caries.experienceCat <- cut(dental$Caries.experience, breaks=5)
dental$Urgent.needCat <- cut(dental$Urgent.need, breaks=5)
dental$SealantsCat <- cut(dental$Sealants, breaks=5)

dental$Untreated.cavitiesCol <- 


cols <- colorRampPalette(brewer.pal(5, "Purples"))(5)



par(mfrow=c(2,2))
par(oma=c(1,1,1,1))
par(mar=c(5,4,2,1))
#PLOT Untreated.cavities
dental$Untreated.cavitiesround <- round(dental$Untreated.cavities)
alexColsb <- colorRampPalette(brewer.pal(9, "Purples"))(max(dental$Untreated.cavitiesround) -
                                                       min(dental$Untreated.cavitiesround))
plot(elementary, col=alexColsb[(dental$Untreated.cavitiesround-min(dental$Untreated.cavities)+1)], border="grey")
legend(x="topright", title="Untreated cavities (%)", fill=c(alexColsb[1], 
                                                        alexColsb[median(dental$Untreated.cavitiesround)-min(dental$Untreated.cavitiesround)],
                                                        alexColsb[max(dental$Untreated.cavitiesround)- min(dental$Untreated.cavitiesround)]),
       legend=c(min(dental$Untreated.cavitiesround), median(dental$Untreated.cavitiesround), max(dental$Untreated.cavitiesround)),
       border="grey", bty="n", cex=0.7, y.intersp=0.8)

#PLOT Caries.experience
dental$Caries.experienceround <- round(dental$Caries.experience)
alexColsb <- colorRampPalette(brewer.pal(9, "Greens"))(max(dental$Caries.experienceround) -
                                                          min(dental$Caries.experienceround))
plot(elementary, col=alexColsb[(dental$Caries.experienceround-min(dental$Caries.experience)+1)], border="grey")
legend(x="topright", title="Caries experience (%)", fill=c(alexColsb[1], 
                                                            alexColsb[median(dental$Caries.experienceround)-min(dental$Caries.experienceround)],
                                                            alexColsb[max(dental$Caries.experienceround)- min(dental$Caries.experienceround)]),
       legend=c(min(dental$Caries.experienceround), median(dental$Caries.experienceround), max(dental$Caries.experienceround)),
       border="grey", bty="n", cex=0.7, y.intersp=0.8)

#PLOT Urgent.need
dental$Urgent.needround <- round(dental$Urgent.need)
alexColsb <- colorRampPalette(brewer.pal(9, "Reds"))(max(dental$Urgent.needround) -
                                                          min(dental$Urgent.needround))
plot(elementary, col=alexColsb[(dental$Urgent.needround-min(dental$Urgent.need)+1)], border="grey")
legend(x="topright", title="Urgent need (%)", fill=c(alexColsb[1], 
                                                            alexColsb[median(dental$Urgent.needround)-min(dental$Urgent.needround)],
                                                            alexColsb[max(dental$Urgent.needround)- min(dental$Urgent.needround)]),
       legend=c(min(dental$Urgent.needround), median(dental$Urgent.needround), max(dental$Urgent.needround)),
       border="grey", bty="n", cex=0.7, y.intersp=0.8)

#PLOT Sealants
dental$Sealantsround <- round(dental$Sealants)
alexColsb <- colorRampPalette(brewer.pal(9, "Blues"))(max(dental$Sealantsround) -
                                                          min(dental$Sealantsround))
plot(elementary, col=alexColsb[(dental$Sealantsround-min(dental$Sealants)+1)], border="grey")
legend(x="topright", title="Sealants (%)", fill=c(alexColsb[1], 
                                                            alexColsb[median(dental$Sealantsround)-min(dental$Sealantsround)],
                                                            alexColsb[max(dental$Sealantsround)- min(dental$Sealantsround)]),
       legend=c(min(dental$Sealantsround), median(dental$Sealantsround), max(dental$Sealantsround)),
       border="grey", bty="n", cex=0.7, y.intersp=0.8)
title(main="Dental Health Indicators in Alachua Elementary Schools", outer=TRUE, line=-1)


#################
# NOW WITH QUARTILES
##################


par(mfrow=c(1,1))
par(oma=c(1,1,1,1))
par(mar=c(3,3,6,1))
#PLOT Untreated.cavities
dental$Untreated.cavitiesround <- round(dental$Untreated.cavities)
alexColsb <- colorRampPalette(brewer.pal(9, "Purples"))(max(dental$Untreated.cavitiesround) -
                                                          min(dental$Untreated.cavitiesround)+1)
plot(elementary, col=alexColsb[(dental$Untreated.cavitiesround-min(dental$Untreated.cavities)+1)], border="grey")
legend(x="topright", title="Untreated cavities (%)", fill=alexColsb[as.numeric(quantile(dental$Untreated.cavitiesround)) - 
                                                                      min(dental$Untreated.cavitiesround) +1],
       legend=min(dental$Untreated.cavitiesround) + as.numeric(quantile(dental$Untreated.cavitiesround)) - min(dental$Untreated.cavitiesround),
       border="grey", bty="n", cex=0.6, y.intersp=0.7)
title(main="Oral Health Indicators in Alachua County Elementary Schools", outer=TRUE, line=-1)
#PLOT Caries.experience
dental$Caries.experienceround <- round(dental$Caries.experience)
alexColsb <- colorRampPalette(brewer.pal(9, "Greens"))(max(dental$Caries.experienceround) -
                                                          min(dental$Caries.experienceround)+1)
plot(elementary, col=alexColsb[(dental$Caries.experienceround-min(dental$Caries.experience)+1)], border="grey")
legend(x="topright", title="Caries experience (%)", fill=alexColsb[as.numeric(quantile(dental$Caries.experienceround)) - 
                                                                      min(dental$Caries.experienceround) +1],
       legend=min(dental$Caries.experienceround) + as.numeric(quantile(dental$Caries.experienceround)) - min(dental$Caries.experienceround),
       border="grey", bty="n", cex=0.6, y.intersp=0.7)
title(main="Oral Health Indicators in Alachua County Elementary Schools", outer=TRUE, line=-1)
#PLOT Urgent.need
dental$Urgent.needround <- round(dental$Urgent.need)
alexColsb <- colorRampPalette(brewer.pal(9, "Reds"))(max(dental$Urgent.needround) -
                                                          min(dental$Urgent.needround)+1)
plot(elementary, col=alexColsb[(dental$Urgent.needround-min(dental$Urgent.need)+1)], border="grey")
legend(x="topright", title="Urgent need (%)", fill=alexColsb[as.numeric(quantile(dental$Urgent.needround)) - 
                                                                      min(dental$Urgent.needround) +1],
       legend=min(dental$Urgent.needround) + as.numeric(quantile(dental$Urgent.needround)) - min(dental$Urgent.needround),
       border="grey", bty="n", cex=0.6, y.intersp=0.7)
title(main="Oral Health Indicators in Alachua County Elementary Schools", outer=TRUE, line=-1)
#PLOT Sealants
dental$Sealantsround <- round(dental$Sealants)
alexColsb <- colorRampPalette(brewer.pal(9, "Blues"))(max(dental$Sealantsround) -
                                                          min(dental$Sealantsround)+1)
plot(elementary, col=alexColsb[(dental$Sealantsround-min(dental$Sealants)+1)], border="grey")
legend(x="topright", title="Sealants (%)", fill=alexColsb[as.numeric(quantile(dental$Sealantsround)) - 
                                                                      min(dental$Sealantsround) +1],
       legend=min(dental$Sealantsround) + as.numeric(quantile(dental$Sealantsround)) - min(dental$Sealantsround),
       border="grey", bty="n", cex=0.6, y.intersp=0.7)


title(main="Oral Health Indicators in Alachua County Elementary Schools", outer=TRUE, line=-1)