library(Hmisc)
library(RColorBrewer)
library(car)

# Set wd
setwd("C:/Users/BrewJR/Documents/fdoh/public/tobacco")

# Read in data
#comb <- read.csv("comb1.csv", stringsAsFactors = FALSE)
comb <- read.csv("combined_corrected.csv", stringsAsFactors = FALSE)
comb[,1] <- NULL
# Recode all 77, 88, 99
for (i in 1:ncol(comb)){
  var <-  comb[,i]
  var[which(var %in% c(77,88,99))] <- NA
  var[which(var %in% c("77", "88", "99"))] <- NA
  comb[,i] <- var
}

# Selectively recode
yesnos <- c(7,8, 12:31, 33:35)
for (i in yesnos){
  comb[,i] <- Recode(comb[,i],
                     "1 = 'Yes';
                      2 = 'No'")
}

# Size category
comb[,5] <- Recode(comb[,5],
                   "1 = 'Small'; 
                   2 = 'Large'")

# Type of insurance coverage
comb[,9] <- Recode(comb[,9],
                   "1 = 'Self'; 
                   2 = 'Fully';
                   0 = NA;
                  ' ' = NA; 
                  '' = NA; 
                  '1- ' = NA;  
                   '1-' = 'Self';
                   '2l' = 'Fully'")

# Employer sector
comb[,11] <- Recode(comb[,11],
                    "1 = 'Government';
                    2 = 'Healthcare';
                    3 = 'Business';
                    4 = 'County School District';
                    5 = 'City Municipality';
                    6 = 'County Municipality'")

# Extent of TFG policy coverage
comb[,32] <- Recode(comb[,32], "
                    1 = '100% SFG';
                    2 = '100% TFG';
                    3 = 'SFIW';
                    4 = 'SFIWO'")

# Insurance carrier
comb[,10] <- Recode(comb[,10], 
                   "'1' = 'UHC';
                    '2' = 'Aetna';
                    '3' = 'Avmed';
                    '4' = 'BCBS';
                    '5' = 'Capital';
                    '6' = 'Florida Blue';
                    '7' = 'Cigna';
                    '8' = 'Health First';
                    '9' = 'Humana';
                    '10' = 'Self-insured';
                    '11' = 'Other';
                    '12' = 'Combination'")

# FIX THE SECTOR ISSUE
# library(gdata)
# school_districts <- read.xls("tobacco.xlsm", sheet = 4)
# city <- read.xls("tobacco.xlsm", sheet = 5)
# county <- read.xls("tobacco.xlsm", sheet = 6)
# 
# # school districts
# comb[,11][which(comb[,3] %in% as.character(school_districts[,3]))] <-
#   "County School District"
# 
# # city municipalities
# comb[,11][which(comb[,3] %in% as.character(city[,3]))] <-
#   "City Municipality" 
# 
# # county municipalities
# comb[,11][which(comb[,3] %in% as.character(county[,3]))] <-
#   "County Municipality" 
# 
# # visualize
# bp <- barplot(table(comb[,11]), cex.names = 0.55, las = 1, border = "darkgrey")
# abline(h=seq(0,1000,100),
#        col = adjustcolor("darkgreen", alpha.f = 0.2))
# text(x = bp[,1],
#      y = 150,
#      pos = 1,
#      labels = paste0(100*round(prop.table(table(comb[,11])), digits = 4), "%"),
#      cex = 0.5)
# 
# #write.csv(comb, "combined_corrected.csv")

# Read in names of questions
comb_names <- read.csv("comb1.csv", header = FALSE)
comb_names <- comb_names[1,]

###################
# SIMPASYM FUNCTION FOR CALCULATIN C.I. OF PROPORTIONS
###################
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

###################
# WRITE FUNCTION FOR PLOTTING BARS
###################
BarFun <- function(var, by_var = NULL, recode_var = NULL, ref = NULL,
                   cex.names = 1, las = 1, legend = FALSE, rain = FALSE,
                   border = "black", percent = TRUE,
                   legend.cex = 0.8, legend.title = NULL,
                   err.cex = 0.8){
  
#   var <- comb[,"Insurance.Carrier"]
#   by_var = comb[,5]
#   ref <- NULL
#   recode_var <- NULL
#   cex.names = 1
#   las = 1
#   percent = TRUE
#   border = "black"
#   rain = TRUE
#   legend = TRUE
  # Ensure it's treated as a character
  var <- as.character(var)
  
  # Make var a factor and relevel so "unknown" comes first
  var <- factor(var)
  if(!is.null(by_var)){by_var <- factor(by_var)}
  if(is.null(ref)){
    var <- var
  } else {
    var <- relevel(var, ref = ref)
  }
    
  # Make a table
  if(is.null(by_var)){
    var_table <- table(var)
    var_names <- names(var_table)
    var_vals <- as.numeric(var_table)
    var_prop <- prop.table(var_table) * 100
    
    # Calculate confidence intervals
    ci <- simpasym(n=sum(var_table), 
                   p=var_prop / 100, 
                   z=1.96, cc=TRUE) 
    lb <- ci$lb * sum(var_table)
    ub <- ci$ub * sum(var_table)
    
  } else {
    #var_table <- table(var, by_var)
    var_table <- table(by_var, var)
    var_names <- levels(var)
    var_vals <- matrix(var_table, ncol = length(levels(by_var)))
    var_prop <- prop.table(var_table, 2) * 100
    
    # Calculate confidence intervals
    ci <- simpasym(n= rep(apply(var_table, 1, sum),2), #sum(var_table), 
                   p=var_prop / 100, 
                   z=1.96, cc=TRUE) 
    lb <- ci$lb * rep(apply(var_table, 1, sum),2) #var_table#sum(var_table)
    ub <- ci$ub * rep(apply(var_table, 1, sum),2)#var_table#sum(var_table)
    
  }

  
  # Assing positions based on relative value compared to others
  var_pos <- ifelse(var_prop < 0.5 * max(var_prop),
                    3, 1)
  

  
  # Create color vector (first is always red if there are unknowns)
#   if( var_table["Unknown"] > 0 ){
#     my_colors <- c("Red", colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)) -1))
#   } else {
#   }

if(is.null(by_var)){
  my_colors <- colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)))
  
  if(rain){
    my_colors <- rainbow(length(levels(var)))
  }
  
}else{
  my_colors <- colorRampPalette(c("darkblue", "darkgreen"))(length(levels(by_var)))
  if(rain){
    my_colors <- rainbow(length(levels(by_var)))
  }
}

  my_colors <- adjustcolor(my_colors, alpha.f = 0.6)


  # Make barplot
  if(is.null(by_var)){
    
    if(percent){
      
      bp <- barplot(var_prop,
                    ylim = c(0, max(ci$ub)*120),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las, 
                    beside = TRUE)
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(var),
               cex = legend.cex,
               title = legend.title)
      }
      
      # Add point estimate text
      #   text(x = bp[,1],
      #        y = var_vals,
      #        labels = paste0(round(var_prop, digits = 1), "%"),
      #        pos = var_pos)
      
      # Add error bars
      errbar(x=bp[,1],
             y=var_prop,
             yplus=ci$ub*100,
             yminus=ci$lb*100,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp[,1], 
           y= ifelse(var_pos == 1, 
                     var_prop - (.1*max(var_prop)),
                     var_prop + (.1*max(var_prop))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
      
    }else {
      
      bp <- barplot(var_table,
                    ylim = c(0, max(ub)*1.2),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las, 
                    beside = TRUE)
      
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(var),
               cex = legend.cex,
               title = legend.title)
      }
      # Add error bars
      errbar(x=bp[,1],
             y=var_table,
             yplus=ub,
             yminus=lb,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp[,1], 
           y= ifelse(var_pos == 1, 
                     var_table - (.1*max(var_table)),
                     var_table + (.1*max(var_table))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
      
    }
   
    # begin by_var
  
  } else{
    
    if(percent){
      bp <- barplot(var_prop,
                    ylim = c(0, max(ci$ub)*120),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las,
                    beside = TRUE)
      
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(by_var),
               cex = legend.cex,
               title = legend.title)
      }
      
      # Add error bars
      errbar(x=bp,
             y=var_prop,
             yplus=ci$ub*100,
             yminus=ci$lb*100,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp, 
           y= ifelse(var_pos == 1, 
                     var_prop - (.1*max(var_prop)),
                     var_prop + (.1*max(var_prop))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
      
    } else{
      
      bp <- barplot(var_table,
                    ylim = c(0, max(ub)*1.2),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las,
                    beside = TRUE)
      
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(by_var),
               cex = legend.cex,
               title = legend.title)
      }
      
      # Add error bars
      errbar(x=bp,
             y=var_table,
             yplus=ub,
             yminus=lb,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp, 
           y= ifelse(var_pos == 1, 
                     var_table - (.1*max(var_table)),
                     var_table + (.1*max(var_table))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
    }
  } 
}

BarFun(comb[,"Insurance.Carrier"],
       comb[,5],
       ref = NULL,
       recode_var = NULL,
       cex.names = 0.6,
       las = 3,
       percent = TRUE,
       border = "black",
       #rain = TRUE,
       legend = TRUE,
       err.cex = 0.4,
       legend.title = "Employer size")

BarFun(comb[,5],
       comb[,"Insurance.Carrier"],
       ref = NULL,
       recode_var = NULL,
       cex.names = 0.6,
       las = 3,
       percent = TRUE,
       border = "black",
       #rain = TRUE,
       legend = TRUE,
       err.cex = 0.4,
       legend.title = "Employer size")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
par(mfrow=c(1,1))






#1 . Size of employer and does employer provide health insurance
BarFun(comb[,5], by_var = comb[,8], legend = TRUE, rain = TRUE)
par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == "Small"),8])
title(main = "Small", cex.main = 0.8)
BarFun(comb[which(comb[,5] == "Large"),8])
title(main = "Large", cex.main = 0.8)
title(main = "Does your employeer provide health insurance?",
      outer = TRUE,
      line = -1)
par(mfrow = c(1,1))

# 2. Insurance carrier x size of employer
BarFun(comb[,"Insurance.Carrier"], by_var = comb[,8])

par(mfrow=c(1,2))

BarFun(var = comb[which(comb[,5] == "Small"),"Insurance.Carrier"],
       cex.names = 0.6,
       las = 3)
title(main = "Small", cex.main = 0.8)

BarFun(var = comb[which(comb[,5] == "Large"),"Insurance.Carrier"],
       cex.names = 0.6,
       las = 3)
title(main = "Large", cex.main = 0.8)
title(main = "Insurers by business size",
      outer = TRUE,
      line = -1)

#3 . Size of employeer by OTC only
par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == 1),16])
title(main = "Small", cex.main = 0.8)
BarFun(comb[which(comb[,5] == 2),16])
title(main = "Large", cex.main = 0.8)
title(main = "Covers OTC only",
      outer = TRUE,
      line = -1)

#4. Size of employer x Rx only
par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == 1),21])
title(main = "Small", cex.main = 0.8)
BarFun(comb[which(comb[,5] == 2),21])
title(main = "Large", cex.main = 0.8)
title(main = "Covers Rx only",
      outer = TRUE,
      line = -1)

# 5. Size of employer and combination OTC and RX
par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == 1),22])
title(main = "Small", cex.main = 0.8)
BarFun(comb[which(comb[,5] == 2),22])
title(main = "Large", cex.main = 0.8)
title(main = "Covers OTC and Rx",
      outer = TRUE,
      line = -1)

# 6. Size of employer and gold criteria met
par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == 1),35])
title(main = "Small", cex.main = 0.8)
BarFun(comb[which(comb[,5] == 2),35])
title(main = "Large", cex.main = 0.8)
title(main = "Gold criteria met",
      outer = TRUE,
      line = -1)

# 7. 

#8. 

# 9. Insurance carrier by OTC nly
par(mfrow=c(1,2))
BarFun(comb[which(comb[,10] == 1),16])
title(main = "Small", cex.main = 0.8)
BarFun(comb[which(comb[,10] == 2),16])
title(main = "Large", cex.main = 0.8)
title(main = "Covers OTC only",
      outer = TRUE,
      line = -1)

# 10. 

for (i in 7:57){
  BarFun(comb[,i])
}

###############
# GEOCODING
###############

# Having already geocoded, just read in locations
locations <- read.csv("locations.csv")

# merge with comb
comb <- cbind(comb, locations)

# THE BELOW ONLY NEEDS TO BE RUN ONCE:
# places <- paste0(comb$Name.of.Employer,
#                 ", ", 
#                 comb$County.Reporting,
#                 " ", "county",
#                 ", ",
#                 "Florida")
# 
# library(RCurl)
# library(RJSONIO)
# library(plyr)
# 
# url <- function(address, return.call = "json", sensor = "false") {
#   root <- "http://maps.google.com/maps/api/geocode/"
#   u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
#   return(URLencode(u))
# }
# 
# geoCode <- function(address,verbose=FALSE) {
#   if(verbose) cat(address,"\n")
#   u <- url(address)
#   doc <- getURL(u)
#   x <- fromJSON(doc,simplify = FALSE)
#   if(x$status=="OK") {
#     lat <- x$results[[1]]$geometry$location$lat
#     lng <- x$results[[1]]$geometry$location$lng
#     location_type <- x$results[[1]]$geometry$location_type
#     formatted_address <- x$results[[1]]$formatted_address
#     return(c(lat, lng, location_type, formatted_address))
#   } else {
#     return(c(NA,NA,NA, NA))
#   }
# }
# 
# 
# locations <- ldply(places, function(x) geoCode(x))
# names(locations) <- c("lat","lon","location_type", "forAddress")

#write.csv(locations, "locations.csv")

###############
# MAP FUNCTION
###############
par(mfrow = c(1,1))
par(mar=c(1,1,1,1))
library(maps)
mymap <- map("county", "fl",
             fill = TRUE,
             col = "grey",
             border = "darkgrey")

size_col <- ifelse(comb[,5] == 1, "blue", 
                   ifelse(comb[,5] == 2, "red",
                          "darkgreen"))
size_pch <- ifelse(comb[,5] == 1, 16,
                   ifelse(comb[,5] == 2, 15,
                          17))

# points(comb$lon, comb$lat, col = adjustcolor("black", alpha.f = 0.6),
#        cex = 0.5)
points(comb$lon, comb$lat, 
       col = adjustcolor(size_col, alpha.f = 0.2),
       pch = size_pch,
       cex = 0.5)

legend(x = "left",
       pch = c(16,15,17),
       col = adjustcolor(c("blue", "red", "darkgreen"), alpha.f = 0.4),
       legend = c("Small (<50)", "Large (50+)", "Unknown"),
       bty = "n",
       cex = 0.75,
       title = "Business size")




##########
# MAP FUNCTION
##########
TractFun <- function(var, color){
  plotvar <- var
  nclr <- 5
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "%")
  plot(tract2, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}