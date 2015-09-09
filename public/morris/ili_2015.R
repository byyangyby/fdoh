############### Originally from fdoh/public/ili2015

library(gdata)
library(dplyr)

### SETWD
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/fdoh/public/ili_2015"
  private <- "/media/joebrew/JB/fdoh/private/ili/florida/"
} else {
  public <- "C:/Users/BrewJR/Documents/fdoh/public/ili_2015"
  private <- "E:/fdoh/private/ili/florida/"
}

### Read in private ESSENCE data (small)
setwd(private)
# ili <- read.table("ili_florida_2010-2015.txt",
#                   sep= ",", header=TRUE)
# 
# # Subset to only a few columns
# small_ili <- ili[,c("Date", "Age", "Zipcode", "Sex", "CCDD", "Region")]
# 
# # Write a much smaller, more workable csv
# write.csv(small_ili, "ili_florida_2010-2015_small.csv")
ili <- read.csv("ili_florida_2010-2015_small.csv")

### Clean into form of only 4 columns: Date, cases, region, age_group

# Fix county/region issue
ili$county <- toupper(ili$Region)
ili$Region <- NULL

# Establish age_group
ili$age_group <- ifelse(ili$Age <= 4, "00-04",
                        ifelse(ili$Age <= 17, "05-17",
                               ifelse(ili$Age <= 44, "18-44",
                                      ifelse(ili$Age <= 64, "45-64",
                                             ifelse(ili$Age >= 65, "65+", NA)))))

# Establish region
ili$region <- NA
# Assign vector of region 3, florida, etc.
region3 <- toupper(c("Alachua",
                     "Baker",
                     "Bradford",
                     "Clay",
                     "Duval",
                     "Flagler",
                     "Gilchrist",
                     "Levy",
                     "Marion",
                     "Nassau",
                     "Putnam",
                     "St. Johns",
                     "Union"))

# Assign a vector of region
# As before, each region is EXCLUSIVE (ie, Alachua is NOT part of region 3 is NOT part of florida)
ili$region <- ifelse(ili$county == "ALACHUA", "ALACHUA",
                     ifelse(ili$county %in% region3, "REGION3",
                            "FLORIDA"))
rm(region3)

#####
# GLENN'S QUESTION - WHICH AGE GROUPS CHANGED IN WHICH WAYS
#####


# DEFINE FLU SEASONS
ili$date <- as.Date(ili$Date, format = '%m/%d/%Y')
ili$flu <- 
  ili$date >= '2010-10-03' & ili$date <= '2011-05-21' |
  ili$date >= '2011-10-02' & ili$date <= '2012-05-19' |
  ili$date >= '2012-09-30' & ili$date <= '2013-05-18' |
  ili$date >= '2013-09-29' & ili$date <= '2014-05-17' |
  ili$date >= '2014-09-28' & ili$date <= '2015-05-23'

ili$flu_season <- ifelse(
  ili$date >= '2010-10-03' & ili$date <= '2011-05-21',
  '2010-2011',
  ifelse(ili$date >= '2011-10-02' & ili$date <= '2012-05-19',
         '2011-2012',
         ifelse(ili$date >= '2012-09-30' & ili$date <= '2013-05-18',
                '2012-2013',
                ifelse(ili$date >= '2013-09-29' & ili$date <= '2014-05-17',
                       '2013-2014',
                       ifelse(ili$date >= '2014-09-28' & ili$date <= '2015-05-23',
                              '2014-2015', 
                              NA)))))

# Get just alachua
alachua <- ili[which(ili$region == 'ALACHUA'),]

# Plot
library(ggplot2)

pdf('/home/joebrew/Documents/fdoh/public/morris/alachua_age_distribution.pdf',
    height = 6, width = 9)
g <- ggplot()
g + 
  xlim(0, 100) + 
  geom_density(data = alachua[which(alachua$flu_season %in% c('2012-2013', '2013-2014', '2014-2015')),],
               aes(x = Age, y = ..density.., group = flu_season, color = flu_season, fill = flu_season, lty = flu_season), 
               alpha = 0.1) +
  theme_bw() +
  ggtitle('Age distribution of flu season Alachua resident ILI ED patients')
dev.off()

# Group together counts
ili <- ili %>%
  group_by(Date, age_group, region) %>%
  summarise(cases = n())

# Fix date
ili$Date <- as.Date(ili$Date, format = "%m/%d/%Y")

# Since this is time series, expand the data and merge with ili
# in order to get zeroes, when appropriate

# expand data so that we have a combination of every possible date, age_group and region
expanded_data <- expand.grid(Date = unique(ili$Date), 
                         age_group = unique(ili$age_group),
                         region = unique(ili$region))
#join the expanded data to the actual data
library(data.table)
expanded_data <- as.data.frame(expanded_data)
ili <- as.data.frame(ili)

expanded_data <- left_join(x = tbl_dt(expanded_data), 
               y = ili, 
               by = c("Date", "age_group", "region"),
               copy = TRUE)


# fill in NA's with 0's (since an NA means there were no cases for that date, region, age_group comb)
expanded_data$cases[is.na(expanded_data$cases)] <- 0    # fill with zeroes for summarise below.
# assign expanded_data to ili
ili <- expanded_data
# remove expanded_data
rm(expanded_data)

# ### Read in ili data by region
# setwd(public)
# alachua <- read.xls("alachua.xls", skip = 1)
# region3 <- read.xls("region3.xls", skip = 1)
# florida <- read.xls("florida.xls", skip = 1)
# 
# # Rename columns
# alachua$cases <- alachua$Data
# region3$cases <- region3$Data
# florida$cases <- florida$Data
# 
# # Add region columns
# alachua$region <- "ALACHUA"
# florida$region <- "FLORIDA"
# region3$region <- "REGION3"
# 
# # Get rid of a few excess columns
# alachua <- alachua[,c("Date", "cases", "region")]
# region3 <- region3[,c("Date", "cases", "region")]
# florida <- florida[,c("Date", "cases", "region")]
# 
# # Rbind all together
# ili <- rbind(alachua, region3, florida)

# # Merge all three sets together
# ili <- left_join(florida, left_join(alachua, region3))
# rm(alachua, florida, region3)

# # Remove region3 numbers from Florida
# # # Remove Alachua from region3 and from florida
# for (i in 1:nrow(ili)){
#   
#   # Extract date for that row
#   date <- ili$Date[i]
#   
#   # Extract number of cases for region3 and alachua
#   # (recalling that alachua is PART of region 3)
#   alachua_cases <- sum(ili$cases[which(ili$Date == date & 
#                                          ili$region == "ALACHUA")], na.rm = TRUE)
#   region3_cases <- sum(ili$cases[which(ili$Date == date &
#                                          ili$region == "REGION3")], na.rm = TRUE)
#   
#   # Subtract alachua from region 3, so that the aggregations are mutually exclusive
#   region3_cases <- region3_cases - alachua_cases
#   
#   if(ili$region[i] == "FLORIDA"){
#     ili$cases[i] <- ili$cases[i] - region3_cases - alachua_cases
#   } else if(ili$region[i] == "REGION3"){
#     ili$cases[i] <- region3_cases
#   } else {
#     ili$cases[i] <- alachua_cases
#   }
#   
# }

# ### Add a formatted date
# ili$Date <- as.Date(ili$Date, format = "%d%b%y")

##############################
# Census data for county-specific population
##############################
setwd(public)
pop <- read.csv("florida_details.csv", skip = 1)
names(pop) <- gsub("[.]", "", names(pop))
pop <- tbl_df(pop)

pop$AreaName <- as.character(toupper(sub(" County", "", pop$AreaName)))
state <- pop %>% 
  filter(RaceEthnicity == "Total" ) %>%
  select(AreaName, TotalUnder1Year:Total110YearsandOlder)
names(state)[c(2:3, 
               102:104)] <- c("Total0Years", "Total1Years",
                              "Total100-104Years", "Total105-109Years", "Total110+Years")
rm(pop)

# Assign vector of region 3, florida, etc.
region3 <- toupper(c("Alachua",
                     "Baker",
                     "Bradford",
                     "Clay",
                     "Duval",
                     "Flagler",
                     "Gilchrist",
                     "Levy",
                     "Marion",
                     "Nassau",
                     "Putnam",
                     "St. Johns",
                     "Union"))

# In state, assign a vector of region
# As before, each region is EXCLUSIVE (ie, Alachua is NOT part of region 3 is NOT part of florida)
state$region <- NA
state$region <- ifelse(state$AreaName == "ALACHUA", "ALACHUA",
                       ifelse(state$AreaName %in% region3, "REGION3",
                              "FLORIDA"))
rm(region3)

# Remove the aggregate florida county row
state <- state[which(state$AreaName != "FLORIDA"),]

# Give denoms
denoms <- data.frame(region = c("ALACHUA", "REGION3", "FLORIDA"))
denoms$pop <- NA
for (i in denoms$region){
  
  # Get just the appropriate area
  df <- state[which(state$region == i),]
  
  # Get sum of the population for that area
  pop <- sum(df[,names(state) != "region" & names(state) != "AreaName"])
  
  denoms$pop[which(denoms$region == i)] <- 
    pop
}

# Get age specific denoms in state
state$Total0004 <- NA
state$Total0517 <- NA
state$Total1844 <- NA
state$Total4564 <- NA
state$Total65plus <- NA

for (i in 1:nrow(state)){
  state$Total0004[i] <- sum(state[i,2:6])
  state$Total0517[i] <- sum(state[i,7:19])
  state$Total1844[i] <- sum(state[i,20:46])
  state$Total4564[i] <- sum(state[i,47:66])
  state$Total65plus[i] <- sum(state[i,67:104])
}

# Get those age specific denoms into the denoms dataframe
denoms$pop0004 <- NA
denoms$pop0517<- NA
denoms$pop1844 <- NA
denoms$pop4564 <- NA
denoms$pop65plus <- NA
ili$pop <- NA
for (i in denoms$region){
  for (j in c("0004", "0517", "1844", "4564", "65plus")){
    
    # Get just the appropriate area
    df <- state[which(state$region == i),]
    
    # Get sum of the population for that area
    pop <- sum(df[,paste0("Total", j)])
    
    # Apply that sum to denoms
    denoms[which(denoms$region == i),paste0("pop", j)] <- pop
    
    # Apply a denom column into ILI
    abbreviated_age_group <- substr(j, 1, 2)
    ili$pop[which(ili$region == i &
                    substr(ili$age_group, 1, 2) == abbreviated_age_group)] <-
      pop
  }
}

# Remove those with no age_group
ili <- ili[which(!is.na(ili$age_group)),]


# ADD AN "all" CATEGORY ROW TO AGE_GROUP
# first make a dataframe of all age groups
all <- ili %>%
  group_by(Date, region) %>%
  summarise(cases = sum(cases))

# then, get the pop (baseline) for that age group
all <- left_join(x = all,
                 y = denoms[,c("region", "pop")],
                 by = "region",
                 copy = TRUE)

# next, add an age_group column to the all dataframe
all$age_group <- "all"

# Make normal dataframe
all <- data.frame(all)

# finally, add the all dataframe to ili
all <- all[,c("Date", "age_group", "region", "cases", "pop")]
ili <- rbind(all, ili)
ili <- ili[order(ili$Date),]

# ADD A PER 1000 COLUMN
ili$k <- ili$cases / ili$pop * 1000

# Convert back to regular dataframe
ili <- data.frame(ili)

# Plot # add a non-smoothed
plot_ts <- function(region = "FLORIDA", 
                    per1000 = TRUE,
                    age_group = "all",
                    add = FALSE,
                    col = "blue",
                    alpha = 0.7,
                    moving_average = 14,
                    actual_line = TRUE,
                    points = FALSE,
                    ylim = NULL){
  
  # Define color vector
  if(!is.null(alpha)){
    col <- adjustcolor(col, alpha.f = alpha)
  }
  
  # Subset data #####
  df <- ili[which(ili$region == region &
                    ili$age_group == age_group),]  
  
  
  # Cases or per 1000  
  if(per1000){
    y <- df$k
  } else{
    y <- cases
  }
  
  # plot
  
  # if points are true, type is p, otherwise n
  type <- ifelse(points, "p", "n")
  pch <- ifelse(points, 16, NA)
  
  if(!add){
    if(is.null(ylim)){
      plot(x = df$Date,
           y = y,
           col = adjustcolor(col, alpha.f = 0.3), 
           type = type,
           xlab = "Date",
           ylab = ifelse(per1000, "Cases per 1,000 residents", "Cases"),
           pch = 16,
           cex = 0.5)
    } else{
      plot(x = df$Date,
           y = y,
           col = adjustcolor(col, alpha.f = 0.3), 
           type = type,
           xlab = "Date",
           ylab = ifelse(per1000, "Cases per 1,000 residents", "Cases"),
           pch = 16,
           cex = 0.5,
           ylim = ylim)
    }

  } else{
    points(x = df$Date,
           y = y,
           col = adjustcolor(col, alpha.f = 0.3), 
           pch = pch,
           cex = 0.5)
    
  }
  
  # Get moving average
  if(!is.null(moving_average)){
    ma <- vector(mode = "numeric", length = nrow(df))
    for (i in 1:nrow(df)){
      
      # define dates
      end <- df$Date[i]
      start <- end - (moving_average-1) # e.g. to get a 2 day date period, you only need to minus 1
      
      # get number of cases (or per k) for given date range
      ma[i] <- 
        sum(y[which(df$Date >= start &
                      df$Date <= end)], na.rm = TRUE)
    }
    # divide by number of days
    ma <- ma / moving_average
    
    # Add moving average line
    lines(x = df$Date,
          y = ma,
          col = col,
          lwd = 2)
    
  }
  
  # Add actual line
  lines(x = df$Date,
        y = y,
        col = adjustcolor(col, alpha.f = 0.1),
        lwd = 1)
}

# Define a function to do the fancy plot for each age group
plot_fancy <- function(age_group,
                       ylim = NULL){
  
  if(is.null(ylim)){
    # Lay the base
    plot(ili$Date[which(ili$region == "FLORIDA"
                        & ili$age_group == age_group)],
         y = ili$k[which(ili$region == "FLORIDA"
                         & ili$age_group == age_group)],
         xlab = "Date",
         ylab = "Cases per 1,000 residents",
         type = "n")
  } else {
    # Lay the base
    plot(ili$Date[which(ili$region == "FLORIDA"
                        & ili$age_group == age_group)],
         y = ili$k[which(ili$region == "FLORIDA"
                         & ili$age_group == age_group)],
         xlab = "Date",
         ylab = "Cases per 1,000 residents",
         type = "n",
         ylim = ylim)
  }
  

  
  # Add grey background
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = adjustcolor("gray",alpha.f = 0.2))
  
  
  # Add gridlines
  abline(h = seq(0, 1, 0.01),
         col = adjustcolor("white", alpha.f = 0.9))
  abline(v = as.Date(paste0(seq(2010, 2015, 1), "-01-01"), format = "%Y-%m-%d"),
         col = adjustcolor("white", alpha.f = 0.9))
  
  # Florida
  plot_ts(points = TRUE, add = T, age_group = age_group)
  
  # Region 3
  plot_ts(region = "REGION3",
          col = "darkgreen",
          alpha = 0.5,
          add = TRUE,
          points = TRUE,
          age_group = age_group)
  
  # Alachua
  plot_ts(region = "ALACHUA",
          col = "red",
          alpha = 0.5,
          add = TRUE,
          points = TRUE,
          age_group = age_group)
  
  # ADD LEGEND
  legend(x = "topleft",
         pch = 16,
         lty = 1,
         col = adjustcolor(c("blue", "darkgreen", "red"), alpha.f = 0.5),
         legend = c("Florida", "Region 3", "Alachua"),
         bty = "n")
  
  # Add explanation
  legend(x = "top",
         pch = c(16, NA),
         lty = c(1, 1),
         lwd = c(1, 2),
         col = adjustcolor("black", alpha.f = 0.5),
         legend = c("Daily observation", 
                    "14 day rolling average"),
         bty = "n",
         pt.cex = 0.5) 
}

# ALL age groups
pdf(file = paste0('/home/joebrew/Documents/fdoh/public/morris/by_age_group.pdf'),
                  width = 11,
                  height = 8.5)
plot_fancy("all") ; title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.3)) ; title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17") ; title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44") ; title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64") ; title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+") ; title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()


###########################
# Originally from fdoh/public/ili_replicate

#####
# BARPLOTS
#####


# DEFINE FLU SEASONS
ili$date <- ili$Date

#####
# DEFINE FLU SEASONS
#####
ili$flu <- 
  ili$date >= '2010-10-03' & ili$date <= '2011-05-21' |
  ili$date >= '2011-10-02' & ili$date <= '2012-05-19' |
  ili$date >= '2012-09-30' & ili$date <= '2013-05-18' |
  ili$date >= '2013-09-29' & ili$date <= '2014-05-17' |
  ili$date >= '2014-09-28' & ili$date <= '2015-05-23'

ili$flu_season <- ifelse(
  ili$date >= '2010-10-03' & ili$date <= '2011-05-21',
  '2010-2011',
  ifelse(ili$date >= '2011-10-02' & ili$date <= '2012-05-19',
         '2011-2012',
         ifelse(ili$date >= '2012-09-30' & ili$date <= '2013-05-18',
                '2012-2013',
                ifelse(ili$date >= '2013-09-29' & ili$date <= '2014-05-17',
                       '2013-2014',
                       ifelse(ili$date >= '2014-09-28' & ili$date <= '2015-05-23',
                              '2014-2015', 
                              NA)))))

temp <- ili %>%
  group_by(flu_season, age_group, region) %>%
  summarise(cases = sum(cases),
            pop = mean(pop))

# Get cases per 100
temp$pop100 <- temp$cases / temp$pop * 100

# Get p (proportion)
temp$p <- temp$cases / temp$pop

# Get confidence interval on proportion
library(Hmisc)
# https://aghaynes.wordpress.com/2014/04/09/calculating-confidence-intervals-for-proportions/
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- data.frame(lb = NA, ub = NA)
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

temp$lb <- NA
temp$ub <- NA
for (i in 1:nrow(temp)){
  conf <- simpasym(n = temp$pop[i],
                   p = temp$p[i])
  temp$lb[i] <- conf$lb * 100
  temp$ub[i] <- conf$ub * 100
}

# Get colors
cols <- adjustcolor(c('darkred', 'darkgreen', 'darkblue'), alpha.f = 0.6)
temp$col <- ifelse(temp$region == 'ALACHUA', cols[1],
                   ifelse(temp$region == 'REGION3', cols[2],
                          ifelse(temp$region == 'FLORIDA', cols[3],
                                 NA)))

# Define function for pretty barplots
plot_bar <- function(flu_season = '2010-2011'){
  
  # subset to only include the flu season of interest
  sub_temp <- temp[which(temp$flu_season == flu_season),]
  
  #
  bp <- barplot(sub_temp$pop100, 
                space = rep(c(1,0,0), nrow(sub_temp)/3),
                xaxt = 'n',
                ylab = 'Flu season cases per 100 residents',
                col = 'white', border = 'white',
                ylim = c(0, 4.5))  #c(0, max(sub_temp$pop100) * 1.2))
  # Add grey background
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = adjustcolor("gray",alpha.f = 0.2))
  barplot(sub_temp$pop100, 
          space = rep(c(1,0,0), nrow(sub_temp)/3),
          xaxt = 'n',
          yaxt = 'n',
          ylab = NA,
          col = sub_temp$col, add = TRUE, border = NA)
  
  # Add axis
  axis(side = 1,
       at = bp[,1][seq(2, nrow(sub_temp), 3)],
       labels = sub_temp$age_group[seq(2, nrow(sub_temp), 3)],
       tick = FALSE, cex = 2)
  
  # Add gridlines
  abline(h = seq(0, 10, 0.5),
         col = adjustcolor("black", alpha.f = 0.6))
  
  # Add confidence intervals
  errbar(x = bp[,1],
         y = sub_temp$pop100,
         yplus = sub_temp$ub,
         yminus = sub_temp$lb,
         pch = NA, 
         add = TRUE,
         errbar.col = adjustcolor('darkred', alpha.f = 0.8))
  
  # Add legend
  legend('topright',
         fill = cols,
         legend = c('Alachua', 'Region 3', 'Florida'),
         border = NA, cex = 2, bty = 'n')
  
  # Add title
  title(main = paste(flu_season, 'flu season cumulative incidence rate'))
}

## plot
pdf(file = paste0("/home/joebrew/Documents/fdoh/public/morris/protection_barplot_1_page.pdf"),
    height = 8, width = 12)
par(mfrow = c(2,2))

#plot_bar(flu_season = '2010-2011')
plot_bar(flu_season = '2011-2012')
plot_bar(flu_season = '2012-2013')
plot_bar(flu_season = '2013-2014')
plot_bar(flu_season = '2014-2015')
dev.off()

# Real numbers
temp$cases[which(temp$region == 'ALACHUA' & 
                   temp$flu_season == '2014-2015' &
                   temp$age_group == 'all')] 

temp$pop[which(temp$region == 'ALACHUA' & 
                 temp$flu_season == '2014-2015' &
                 temp$age_group == 'all')] *
  
  
  temp$p[which(temp$region == 'FLORIDA' & 
                 temp$flu_season == '2014-2015' &
                 temp$age_group == 'all')]

# #####################
# # BREAK DOWN BY MORE THAN JUST FLU SEASON FOR MORRIS
# #####################
# 
# ili$month <- as.numeric(format(ili$date, '%m'))
# ili$year <- as.numeric(format(ili$date, '%Y'))
# 
# temp <- ili %>%
#   group_by(year, month, age_group, region) %>%
#   summarise(cases = sum(cases),
#             pop = mean(pop))
# 
# # Get cases per 100
# temp$pop100 <- temp$cases / temp$pop * 100
# 
# # Get p (proportion)
# temp$p <- temp$cases / temp$pop
# 
# # Get confidence interval on proportion
# temp$lb <- NA
# temp$ub <- NA
# for (i in 1:nrow(temp)){
#   conf <- simpasym(n = temp$pop[i],
#                    p = temp$p[i])
#   temp$lb[i] <- conf$lb * 100
#   temp$ub[i] <- conf$ub * 100
# }
# 
# # Get colors
# temp$col <- ifelse(temp$region == 'ALACHUA', cols[1],
#                    ifelse(temp$region == 'REGION3', cols[2],
#                           ifelse(temp$region == 'FLORIDA', cols[3],
#                                  NA)))
# 
# # Define function for pretty barplots
# plot_glenn <- function(year = 2010, month = 1){
#   
#   # subset to only include the flu season of interest
#   sub_temp <- temp[which(temp$year == year &
#                            temp$month == month),]
#   
#   #
#   if(nrow(sub_temp) > 0){
#     bp <- barplot(sub_temp$pop100, 
#                   space = rep(c(1,0,0), nrow(sub_temp)/3),
#                   xaxt = 'n',
#                   ylab = 'Flu season cases per 100 residents',
#                   col = 'white', border = 'white',
#                   ylim = c(0, 0.7))  #c(0, max(sub_temp$pop100) * 1.2))
#     # Add grey background
#     rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = adjustcolor("gray",alpha.f = 0.2))
#     barplot(sub_temp$pop100, 
#             space = rep(c(1,0,0), nrow(sub_temp)/3),
#             xaxt = 'n',
#             yaxt = 'n',
#             ylab = NA,
#             col = sub_temp$col, add = TRUE, border = NA)
#     
#     # Add axis
#     axis(side = 1,
#          at = bp[,1][seq(2, nrow(sub_temp), 3)],
#          labels = sub_temp$age_group[seq(2, nrow(sub_temp), 3)],
#          tick = FALSE, cex = 2)
#     
#     # Add gridlines
#     abline(h = seq(0, 10, 0.1),
#            col = adjustcolor("black", alpha.f = 0.6))
#     
#     # Add confidence intervals
#     errbar(x = bp[,1],
#            y = sub_temp$pop100,
#            yplus = sub_temp$ub,
#            yminus = sub_temp$lb,
#            pch = NA, 
#            add = TRUE,
#            errbar.col = adjustcolor('darkred', alpha.f = 0.8))
#     
#     # Add legend
#     legend('topright',
#            fill = cols,
#            legend = c('Alachua', 'Region 3', 'Florida'),
#            border = NA, cex = 2, bty = 'n')
#     
#     # Add title
#     title(main = paste(year, 'month', month, 'flu season cumulative incidence rate'),
#           cex.main = 1.8)
#   }
# }
# 
# ## plot
# plot_glenn(year = 2016, month = 2)
# 
# years <- 2010:2015
# months <- 1:12
# for (i in years){
#   for (j in months){
#     month_name <- j
#     while(nchar(as.character(month_name)) < 3){
#       month_name <- paste0(0, month_name)
#     }
#     png(file = paste0(public, "/visuals/gif/protection", i, month_name, ".png"),
#         width = 650)
#     plot_glenn(year = i, month = j)
#     dev.off()
#     
#   }
# }
# 

####################################################################
#####
# JONATHAN'S POPULATION DENOMINATORS
#####

# Make a new plot fancy

# Define a function to do the fancy plot for each age group
plot_fancy <- function(age_group,
                       ylim = NULL,
                       moving_average = 7, 
                       data = df,
                       ...){
  
  if(is.null(ylim)){
    # Lay the base
    plot(data$Date[which(data$region == "FLORIDA"
                         & data$age_group == age_group)],
         y = data$k[which(data$region == "FLORIDA"
                          & data$age_group == age_group)],
         xlab = "Date",
         ylab = "Cases per 1,000 residents",
         type = "n",
         ...)
  } else {
    # Lay the base
    plot(data$Date[which(data$region == "FLORIDA"
                         & data$age_group == age_group)],
         y = data$k[which(data$region == "FLORIDA"
                          & data$age_group == age_group)],
         xlab = "Date",
         ylab = "Cases per 1,000 residents",
         type = "n",
         ylim = ylim, 
         ...)
  }
  
  
  
  # Add grey background
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = adjustcolor("gray",alpha.f = 0.2))
  
  
  # Add gridlines
  abline(h = seq(0, 1, 0.01),
         col = adjustcolor("white", alpha.f = 0.9))
  abline(v = as.Date(paste0(seq(2010, 2015, 1), "-01-01"), format = "%Y-%m-%d"),
         col = adjustcolor("white", alpha.f = 0.9))
  
  # Florida
  plot_ts(points = TRUE, add = T, age_group = age_group,
          moving_average = moving_average, data = data)
  
  # Region 3
  plot_ts(region = "REGION3",
          col = "darkgreen",
          alpha = 0.5,
          add = TRUE,
          points = TRUE,
          age_group = age_group,
          moving_average = moving_average, data = data)
  
  # Alachua
  plot_ts(region = "ALACHUA",
          col = "red",
          alpha = 0.5,
          add = TRUE,
          points = TRUE,
          age_group = age_group,
          moving_average = moving_average, data = data)
  
  # ADD LEGEND
  legend(x = "topleft",
         pch = 16,
         lty = 1,
         col = adjustcolor(c("blue", "darkgreen", "red"), alpha.f = 0.5),
         legend = c("Florida", "Region 3", "Alachua"),
         bty = "n")
  
  # Add explanation
  legend(x = "top",
         pch = c(16, NA),
         lty = c(1, 1),
         lwd = c(1, 2),
         col = adjustcolor("black", alpha.f = 0.5),
         legend = c("Daily observation", 
                    paste0(moving_average, " day rolling average")),
         bty = "n",
         pt.cex = 0.5) 
}

# Make a new plot_ts

# Plot # add a non-smoothed
plot_ts <- function(region = "FLORIDA", 
                    per1000 = TRUE,
                    age_group = "all",
                    add = FALSE,
                    col = "blue",
                    alpha = 0.7,
                    moving_average = 14,
                    actual_line = TRUE,
                    points = FALSE,
                    ylim = NULL,
                    data = jon_df){
  
  # Define color vector
  if(!is.null(alpha)){
    col <- adjustcolor(col, alpha.f = alpha)
  }
  
  # Subset data #####
  data <- data[which(data$region == region &
                       data$age_group == age_group),]  
  
  
  # Cases or per 1000  
  if(per1000){
    y <- data$k
  } else{
    y <- cases
  }
  
  # plot
  
  # if points are true, type is p, otherwise n
  type <- ifelse(points, "p", "n")
  pch <- ifelse(points, 16, NA)
  
  if(!add){
    if(is.null(ylim)){
      plot(x = data$Date,
           y = y,
           col = adjustcolor(col, alpha.f = 0.3), 
           type = type,
           xlab = "Date",
           ylab = ifelse(per1000, "Cases per 1,000 residents", "Cases"),
           pch = 16,
           cex = 0.5)
    } else{
      plot(x = data$Date,
           y = y,
           col = adjustcolor(col, alpha.f = 0.3), 
           type = type,
           xlab = "Date",
           ylab = ifelse(per1000, "Cases per 1,000 residents", "Cases"),
           pch = 16,
           cex = 0.5,
           ylim = ylim)
    }
    
  } else{
    points(x = data$Date,
           y = y,
           col = adjustcolor(col, alpha.f = 0.3), 
           pch = pch,
           cex = 0.5)
    
  }
  
  # Get moving average
  if(!is.null(moving_average)){
    ma <- vector(mode = "numeric", length = nrow(data))
    for (i in 1:nrow(data)){
      
      # define dates
      end <- data$Date[i]
      start <- end - (moving_average-1) # e.g. to get a 2 day date period, you only need to minus 1
      
      # get number of cases (or per k) for given date range
      ma[i] <- 
        sum(y[which(data$Date >= start &
                      data$Date <= end)], na.rm = TRUE)
    }
    # divide by number of days
    ma <- ma / moving_average
    
    # Add moving average line
    lines(x = data$Date,
          y = ma,
          col = col,
          lwd = 2)
    
  }
  
  # Add actual line
  lines(x = data$Date,
        y = y,
        col = adjustcolor(col, alpha.f = 0.1),
        lwd = 1)
}



library(readstata13)
setwd('/home/joebrew/Documents/fdoh/public/morris')
census <- read.dta13("popprojections.dta")

# Assign vector of region 3, florida, etc.
region3 <- toupper(c("Alachua",
                     "Baker",
                     "Brailiord",
                     "Clay",
                     "Duval",
                     "Flagler",
                     "Gilchrist",
                     "Levy",
                     "Marion",
                     "Nassau",
                     "Putnam",
                     "St. Johns",
                     "Union"))

census$place <- ifelse(census$region == 'Alachua',
                       'ALACHUA',
                       ifelse(toupper(census$region) %in% region3,
                              'REGION3',
                              'FLORIDA'))

census <- census %>%
  group_by(place, year, agegrp) %>%
  summarise(pop = sum(popestimate))

census$age_group <- 
  ifelse(census$agegrp == 1, '00-04',
         ifelse(census$agegrp %in% c(2, 3),
                '05-17',
                ifelse(census$agegrp == 4, '15-19',
                       ifelse(census$agegrp %in% c(5, 6, 7, 8, 9),
                              '18-44',
                              ifelse(census$agegrp %in% c(10, 11, 12, 13),
                                     '45-64',
                                     ifelse(census$agegrp %in% c(14, 15, 16, 17, 18),
                                            '65+', NA))))))


# Re-aggregate
census <- census %>%
  group_by(place, year, age_group) %>%
  summarise(pop = sum(pop))

# Fix the 05-17 and 18-44 age group
for (i in 1:nrow(census)){
  
  #05-17
  if(census$age_group[i] == '05-17'){
    census$pop[i] <- census$pop[i] + (0.6 * census$pop[(i+1)])
  }
  
  #18-44
  if(census$age_group[i] == '18-44'){
    census$pop[i] <- census$pop[i] + (0.4 * census$pop[(i-1)])
  }
}
census <- census[which(census$age_group != '15-19'),]

# ADD AN "all" CATEGORY ROW TO census
# first make a dataframe of all age groups
all <- census %>%
  group_by(year, place) %>%
  summarise(pop = sum(pop))

all$age_group <- 'all'
all <- all[,c('place', 'year', 'age_group', 'pop')]


# Add the all age group to census
all <- all[,c('place', 'year', 'age_group', 'pop')]
census <- rbind(census, all)

# rename census
names(census) <- c('region', 'year', 'age_group', 'pop')

# Model to get 2015 census data
census2015 <- census[which(census$year == 2000),]
census2015$year <- 2015

fit <- lm(pop ~
            region*age_group*year,
          data = census)

census2015$pop <- NA
census2015$pop <- predict(fit, census2015)

#bind census 2015
census <- rbind(census, census2015)

# Get a smaller ili and join with census
ili$month <- as.numeric(format(ili$date, '%m'))
ili$year <- as.numeric(format(ili$date, '%Y'))
small_ili <- ili[,c('Date', 'age_group', 'region', 'cases',
                  'date', 'flu_season', 'month', 'year')]

census <- data.frame(census)
jon_ili <- left_join(small_ili, census)

# Create the k variable
jon_ili$k <- jon_ili$cases / jon_ili$pop * 1000
jon_ili$k[which(is.na(jon_ili$k))] <- 0

#####
# PLOT USING JONATHAN'S DENOMINATORS
#####
public <- '/home/joebrew/Documents/fdoh/public/morris'

# 2014-15 zoom-in
pdf(file = paste0(public, "/", "201415_zoomin_jonathans_denoms.pdf"),
    width = 11,
    height = 7)
plot_fancy("all", moving_average = 30,
           xlim = c(max(jon_ili$Date) - 365, max(jon_ili$Date)),
           ylim = c(0, 0.1),
           data = jon_ili)
title(main = '2015-15 flu season: all age groups')
dev.off()

# Was the onset earlier in Alachua than in previous years


pdf(file = paste0(public, "/", "onset.pdf"),
    width = 11,
    height = 7)
temp <- jon_ili
temp$day_number <- as.numeric(format(temp$date, '%j'))

g <- ggplot(data = temp[which(temp$region == 'ALACHUA' & 
                                temp$year != 2015 &
                                temp$age_group == 'all'),], 
            aes(x = day_number, y = cases))
g + geom_point(alpha = 0.5) + 
  geom_smooth() +
  xlim(250, 290) +
  geom_vline(aes(xintercept = 271)) +
  xlab('Day of year') + 
  ylab('Cases') +
  ylim(0, 20) +
  ggtitle('Alachua resident cases by day of year (vertical line at September 28)') +
  facet_grid(year ~ .) 
dev.off()

# ALL age groups
pdf(file = paste0(public, "/", "by_age_group_jonathans_denoms.pdf"),
    width = 11,
    height = 7)
plot_fancy("all", moving_average = 30,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           ylim = c(0, 0.1),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.4), moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17", moving_average = 60,
           ylim = c(0, 0.2),
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44", moving_average = 60,
           ylim = c(0, 0.1),
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64", moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           ylim = c(0, 0.1),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+", moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           ylim = c(0, 0.1),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()

# SAME SCALE
# ALL age groups
pdf(file = paste0(public, "/", "by_age_group_same_scale_jonathans_denoms.pdf"),
    width = 12,
    height = 7)
par(mfrow = c(2,3))
plot_fancy("all", moving_average = 30,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)), ylim = c(0, 0.3),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.3), moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17", moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)), ylim = c(0, 0.3),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44", moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)), ylim = c(0, 0.3),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64", moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)), ylim = c(0, 0.3),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+", moving_average = 60,
           xlim = c(min(jon_ili$Date) + 180, max(jon_ili$Date)), ylim = c(0, 0.3),
           data = jon_ili)
title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()



#####
# BARPLOTS
#####


# DEFINE FLU SEASONS
#####
# DEFINE FLU SEASONS
#####
jon_ili$flu <- 
  jon_ili$date >= '2010-10-03' & jon_ili$date <= '2011-05-21' |
  jon_ili$date >= '2011-10-02' & jon_ili$date <= '2012-05-19' |
  jon_ili$date >= '2012-09-30' & jon_ili$date <= '2013-05-18' |
  jon_ili$date >= '2013-09-29' & jon_ili$date <= '2014-05-17' |
  jon_ili$date >= '2014-09-28' & jon_ili$date <= '2015-05-23'

jon_ili$flu_season <- ifelse(
  jon_ili$date >= '2010-10-03' & jon_ili$date <= '2011-05-21',
  '2010-2011',
  ifelse(jon_ili$date >= '2011-10-02' & jon_ili$date <= '2012-05-19',
         '2011-2012',
         ifelse(jon_ili$date >= '2012-09-30' & jon_ili$date <= '2013-05-18',
                '2012-2013',
                ifelse(jon_ili$date >= '2013-09-29' & jon_ili$date <= '2014-05-17',
                       '2013-2014',
                       ifelse(jon_ili$date >= '2014-09-28' & jon_ili$date <= '2015-05-23',
                              '2014-2015', 
                              NA)))))


temp <- jon_ili %>%
  group_by(flu_season, age_group, region) %>%
  summarise(cases = sum(cases),
            pop = mean(pop))

# Get cases per 100
temp$pop100 <- temp$cases / temp$pop * 100

# Get p (proportion)
temp$p <- temp$cases / temp$pop

# Get confidence interval on proportion
library(Hmisc)
# https://aghaynes.wordpress.com/2014/04/09/calculating-confidence-intervals-for-proportions/
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- data.frame(lb = NA, ub = NA)
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

temp$lb <- NA
temp$ub <- NA
for (i in 1:nrow(temp)){
  conf <- simpasym(n = temp$pop[i],
                   p = temp$p[i])
  temp$lb[i] <- conf$lb * 100
  temp$ub[i] <- conf$ub * 100
}

# Get colors
cols <- adjustcolor(c('darkred', 'darkgreen', 'darkblue'), alpha.f = 0.6)
temp$col <- ifelse(temp$region == 'ALACHUA', cols[1],
                   ifelse(temp$region == 'REGION3', cols[2],
                          ifelse(temp$region == 'FLORIDA', cols[3],
                                 NA)))

# Is the case right in Alachua in 2014/15 different from 2013/14 or 2012/13
prop.test(x = c(temp$cases[which(temp$region == 'ALACHUA' &
                                   temp$flu_season == '2014-2015' &
                                   temp$age_group == 'all')],
                temp$cases[which(temp$region == 'ALACHUA' &
                                   temp$flu_season == '2013-2014' &
                                   temp$age_group == 'all')]),
          n = c(temp$pop[which(temp$region == 'ALACHUA' &
                                   temp$flu_season == '2014-2015' &
                                   temp$age_group == 'all')],
                temp$pop[which(temp$region == 'ALACHUA' &
                                   temp$flu_season == '2013-2014' &
                                   temp$age_group == 'all')]))
#
prop.test(x = c(temp$cases[which(temp$region == 'ALACHUA' &
                                   temp$flu_season == '2014-2015' &
                                   temp$age_group == 'all')],
                temp$cases[which(temp$region == 'ALACHUA' &
                                   temp$flu_season == '2012-2013' &
                                   temp$age_group == 'all')]),
          n = c(temp$pop[which(temp$region == 'ALACHUA' &
                                 temp$flu_season == '2014-2015' &
                                 temp$age_group == 'all')],
                temp$pop[which(temp$region == 'ALACHUA' &
                                 temp$flu_season == '2012-2013' &
                                 temp$age_group == 'all')]))

# Define function for pretty barplots
plot_bar <- function(flu_season = '2010-2011',
                     add_text = TRUE){
  
  # subset to only include the flu season of interest
  sub_temp <- temp[which(temp$flu_season == flu_season),]
  
  #
  bp <- barplot(sub_temp$pop100, 
                space = rep(c(1,0,0), nrow(sub_temp)/3),
                xaxt = 'n',
                ylab = 'Flu season cases per 100 residents',
                col = 'white', border = 'white',
                ylim = c(0, 4.5))  #c(0, max(sub_temp$pop100) * 1.2))
  # Add grey background
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = adjustcolor("gray",alpha.f = 0.2))
  barplot(sub_temp$pop100, 
          space = rep(c(1,0,0), nrow(sub_temp)/3),
          xaxt = 'n',
          yaxt = 'n',
          ylab = NA,
          col = sub_temp$col, add = TRUE, border = NA)
  
  # Add axis
  axis(side = 1,
       at = bp[,1][seq(2, nrow(sub_temp), 3)],
       labels = sub_temp$age_group[seq(2, nrow(sub_temp), 3)],
       tick = FALSE, cex = 2)
  
  # Add gridlines
  abline(h = seq(0, 10, 0.5),
         col = adjustcolor("black", alpha.f = 0.3))
  
  # Add confidence intervals
  errbar(x = bp[,1],
         y = sub_temp$pop100,
         yplus = sub_temp$ub,
         yminus = sub_temp$lb,
         pch = NA, 
         add = TRUE,
         errbar.col = adjustcolor('darkred', alpha.f = 0.8))
  
  # Add text
  if(add_text){
    text(x = bp[,1],
         y = sub_temp$ub,
         labels = paste0(round(sub_temp$lb, digits = 3),'-\n',
                         round(sub_temp$ub, digits = 3), ' '),
         col = adjustcolor('black', alpha.f = 0.6),
         cex = 0.4,
         pos = 3)
    
    axis(side = 1,
         at = bp[,1],
         line = -1.35,
         labels = round(sub_temp$pop100, digits = 2),
         col.axis = adjustcolor('black', alpha.f = 0.6),
         cex.axis = 0.5,
         tick = FALSE)
  }
  
  # Add legend
  legend('topright',
         fill = cols,
         legend = c('Alachua', 'Region 3', 'Florida'),
         border = NA, cex = 2, bty = 'n')
  
  # Add title
  title(main = paste(flu_season, 'flu season cumulative incidence rate'))
}

## plot
pdf(file = paste0(public, "/protection_barplot_1_page_jonathans_denoms.pdf"),
    height = 8, width = 12)
par(mfrow = c(2,2))

#plot_bar(flu_season = '2010-2011')
plot_bar(flu_season = '2011-2012')
plot_bar(flu_season = '2012-2013')
plot_bar(flu_season = '2013-2014')
plot_bar(flu_season = '2014-2015')
dev.off()




#####
# VACCINE EFFECTIVENESS
#####
library(tidyr)

# Comparing Alachua to both florida and region3
temp <- jon_ili %>%
  group_by(flu_season, age_group, region) %>%
  summarise(cases = sum(cases),
            pop = mean(pop))

# Cumulative incidence
temp$cumulative_incidence <- temp$cases / temp$pop

#ve <- spread(temp, key = 'region', value = cases)

# Subset into 3 dataframes
alachua <- temp[which(temp$region == 'ALACHUA'),]
florida <- temp[which(temp$region == 'FLORIDA'),]
region3 <- temp[which(temp$region == 'REGION3'),]

# ve
ve <- temp[,c('flu_season', 'age_group')]
ve <- ve[!duplicated(ve),]


# Vaccine effectiveness against florida
ve$florida <- (1 - (alachua$cumulative_incidence / florida$cumulative_incidence)) * 100

# Vaccine effectiveness against region3
ve$region3 <- (1 - (alachua$cumulative_incidence / region3$cumulative_incidence)) *100

# Get rid of NA rows
ve <- ve[which(!is.na(ve$flu_season)),]


# Confidence intervals
#ve$florida_lb <- ve$florida_ub <- ve$region3_lb <- ve$region3_ub<-  NA
ve$florida_p_value <- ve$region3_p_value <- NA
ve$rr_florida <- ve$rr_region3 <- NA
ve$rr_95_florida <- ve$rr_95_region3 <- NA
ve$ve_95_florida <- ve$ve_95_region3 <- NA

for (i in 1:nrow(ve)){
  
  # florida cases
  florida_cases <- temp$cases[which(temp$flu_season == ve$flu_season[i] &
                                      temp$age_group == ve$age_group[i] &
                                      temp$region == 'FLORIDA')]
  
  # region3 cases
  region3_cases <- temp$cases[which(temp$flu_season == ve$flu_season[i] &
                                      temp$age_group == ve$age_group[i] &
                                      temp$region == 'REGION3')]
  
  # alachua cases
  alachua_cases <- temp$cases[which(temp$flu_season == ve$flu_season[i] &
                                      temp$age_group == ve$age_group[i] &
                                      temp$region == 'ALACHUA')]
  
  # florida pop
  florida_pop <- temp$pop[which(temp$flu_season == ve$flu_season[i] &
                                  temp$age_group == ve$age_group[i] &
                                  temp$region == 'FLORIDA')]
  
  # region3 pop
  region3_pop <- temp$pop[which(temp$flu_season == ve$flu_season[i] &
                                  temp$age_group == ve$age_group[i] &
                                  temp$region == 'REGION3')]
  
  # alachua pop
  alachua_pop <- temp$pop[which(temp$flu_season == ve$flu_season[i] &
                                  temp$age_group == ve$age_group[i] &
                                  temp$region == 'ALACHUA')]
  
  try({
    # proportion test = florida
    florida_obj <- prop.test(x = c(alachua_cases, florida_cases),
                             n = c(alachua_pop, florida_pop))
    
    ve$florida_p_value[i] <- florida_obj$p.value
    
    ve$rr_florida[i] <- (alachua_cases / alachua_pop) / 
      (florida_cases / florida_pop)
    
    # confidence intervals on vaccine effectiveness for jonathan
    florida_ci <- simpasym(n = florida_pop, p = florida_cases / florida_pop)
    alachua_ci <- simpasym(n = alachua_pop, p = alachua_cases / alachua_pop)
    # 95% confidence intervals on true relative risk
    ci_95 <- alachua_ci / florida_ci
    ve$rr_95_florida[i] <- paste0(round(ci_95[1], digits = 4), ' - ', round(ci_95[2], digits = 4))
    # 95% confidence intervals on true vaccine effectiveness
    ve$ve_95_florida[i] <- paste0(round(1 - ci_95[1], digits = 4), ' - ', round(1 - ci_95[2], digits = 4))
    
    
    
    # proportion test = region3
    region3_obj <- prop.test(x = c(alachua_cases, region3_cases),
                             n = c(alachua_pop, region3_pop))
    
    ve$region3_p_value[i] <- region3_obj$p.value
    
    ve$rr_region3[i] <- (alachua_cases / alachua_pop) / 
      (region3_cases / region3_pop)
    
    # confidence intervals on vaccine effectiveness for jonathan
    region3_ci <- simpasym(n = region3_pop, p = region3_cases / region3_pop)
    alachua_ci <- simpasym(n = alachua_pop, p = alachua_cases / alachua_pop)
    # 95% confidence intervals on true relative risk
    ci_95 <- alachua_ci / region3_ci
    ve$rr_95_region3[i] <- paste0(round(ci_95[1], digits = 4), ' - ', round(ci_95[2], digits = 4))
    # 95% confidence intervals on true vaccine effectiveness
    ve$ve_95_region3[i] <- paste0(round(1 - ci_95[1], digits = 4), ' - ', round(1 - ci_95[2], digits = 4))
    
    
  })
  
}

write.csv(ve, paste0(public, '/vaccine_effectiveness.csv'), row.names = FALSE)

#####################################################
# GLENN'S AUGUST 5 2015 REQUESTS - line 1051

# Can’t tell from the graph, but it appears that the onset of cases was earlier in Alachua 
# County in 2014/15 than in previous years.  Just wanted to make sure that by using the 
# September 28 date we’re not losing the initial upstroke on that curve.


#I’m assuming that the case rate in Alachua County in 2015/15 was not significantly 
# different from either 2013/14 or 2012/13, based on the graphs – is this correct?


# You indicate that the percent of 2014/15 cases in persons 20 years of age or younger 
# in alachua county was significantly higher than in 2013/14.  Given that our other 
# numbers are for kids 17 and younger, can you recalculate this relationship for 17 and younger?  
# Were there significant differences in percent of kids in 2012/13 vs. 2014/15, or 2012/13 vs. 2013/14?




temp <- jon_ili

temp$young <- temp$age_group %in% c('00-04', '05-17')
temp <- temp %>%
  filter(young) %>%
  group_by(flu_season, region) %>%
  #summarise(young_pop = mean(pop), # !!! is this right?  is it accidentally taking the mean of 0-4 and 5-17, and should instead be summing those?
  summarise(babies_pop = mean(pop[age_group == '00-04']),
            kids_pop = mean(pop[age_group == '05-17']),
            young_cases = sum(cases))
temp$young_pop <- temp$babies_pop + temp$kids_pop

old <- jon_ili
old <- old %>%
  filter(age_group == 'all') %>%
  group_by(flu_season, region) %>%
  summarise(all_pop = mean(pop), 
            all_cases = sum(cases))

temp <- left_join(temp, old)
temp <- temp[!is.na(temp$flu_season),]

# Are other regions / years different from 2014-15 in regards
# to proportion of young people (00-17?)

temp$p_young <- temp$young_cases / temp$all_cases
temp$p_young_relative_to_1415 <- temp$p <- temp$significant <- NA

# table
for (i in 1:nrow(temp)){
  this_row <- temp[i,]
  this_region <- this_row$region
  
  # Define the 2014-15 case we're comparing to
  this_region_1415 <- temp[which(temp$region == this_region &
                                   temp$flu_season == '2014-2015'),]
  
  pt <- prop.test(x = c(this_region_1415$young_cases, this_row$young_cases),
                  n = c(this_region_1415$all_cases, this_row$all_cases))
  
  val <- ifelse(pt$estimate[1] > pt$estimate[2],
                'less',
                ifelse(pt$estimate[1] < pt$estimate[2],
                       'greater', 'same'))
  temp$p_young_relative_to_1415[i] <- val
  temp$p[i] <- pt$p.value
  temp$significant[i] <- pt$p.value <= 0.05
  
}
setwd(public)
write.csv(temp, 'percentage_young_table.csv', row.names = FALSE)

# Did the same relationships hold in Region 3?  i.e., was the proportion of cases 
# in kids 17 and younger significantly higher in 2014/15 than in 2013/14?  
# What about 2012/13 vs. 2014/15 or 2012/13 vs. 2013/14?


# And for Florida as a whole for all of the above combinations?
# And were the vaccine effectiveness calculations done for the CDC time periods for each of the years?


# was the overall rate of flu cases reported through ESSENCE significantly increased for 
#2014/15 as compared with either 2012/13 or 2013/14 for Region 3?  For Florida?

temp <- jon_ili
temp <- temp %>%
  filter(age_group == 'all') %>%
  group_by(flu_season, region) %>% 
  summarise(pop = mean(pop),
            cases = sum(cases))
temp <- temp[which(!is.na(temp$flu_season)),]


temp$p_relative_to_1415 <- temp$p <- temp$significant <- NA
temp$rate <- temp$cases / temp$pop 
# table
for (i in 1:nrow(temp)){
  this_row <- temp[i,]
  this_region <- this_row$region
  
  # Define the 2014-15 case we're comparing to
  this_region_1415 <- temp[which(temp$region == this_region &
                                   temp$flu_season == '2014-2015'),]
  
  pt <- prop.test(x = c(this_region_1415$cases, this_row$cases),
                  n = c(this_region_1415$pop, this_row$pop))
  
  val <- ifelse(pt$estimate[1] > pt$estimate[2],
                'less',
                ifelse(pt$estimate[1] < pt$estimate[2],
                       'greater', 'same'))
  temp$p_relative_to_1415[i] <- val
  temp$p[i] <- pt$p.value
  temp$significant[i] <- pt$p.value <= 0.05
}

setwd(public)
write.csv(temp, 'percentage_overall_table.csv', row.names = FALSE)


