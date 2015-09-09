library(gdata)
library(dplyr)

### SETWD
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/fdoh/public/ili_replicate"
  private <- "/media/joebrew/JB/fdoh/private/ili_replicate"
} else {
  public <- "C:/Users/BrewJR/Documents/fdoh/public/ili_replicate"
  private <- "E:/fdoh/private/ili_replicate"
}

### Read in private ESSENCE data (small)
setwd(private)
if(!'cleaned.RData' %in% dir()){
  files <- dir()[grepl('.txt', dir())]
  for (i in 1:length(files)){
    d <- read.table(files[i],
                    sep= ",", header=TRUE)
    assign(paste0('ili', gsub('.txt', '', files[i])),
           d)
    rm(d)
    print(paste(i, 'of', length(files)))
  }
  
  # Join together
  dfs <- names(which(sapply(.GlobalEnv, is.data.frame))) 
  for (i in 1:length(dfs)){
    
    temp <- get(dfs[i])
    temp$datasetame <- dfs[i]
    
    if(i == 1){
      df <- temp
    } else {
      df <- rbind(df, temp)
    }
    # remove individuals
    rm( list = c(dfs[i]), temp)
  }
  
  # # Subset to only a few columns
  df <- df[,c("Date", "Age", "Zipcode", "Sex", "CCDD", "Region")]
  
  # Save an image
  save('df', file = 'cleaned.RData')
} else{
  load('cleaned.RData')
}

# Fix county/region issue
df$county <- toupper(df$Region)
df$Region <- NULL

# Establish age_group
df$age_group <- ifelse(df$Age <= 4, "00-04",
                       ifelse(df$Age <= 17, "05-17",
                              ifelse(df$Age <= 44, "18-44",
                                     ifelse(df$Age <= 64, "45-64",
                                            ifelse(df$Age >= 65, "65+", NA)))))

# Establish region
df$region <- NA
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
df$region <- ifelse(df$county == "ALACHUA", "ALACHUA",
                    ifelse(df$county %in% region3, "REGION3",
                           "FLORIDA"))
rm(region3)

# Group together counts
df <- df %>%
  group_by(Date, age_group, region) %>%
  summarise(cases = n())

# Fix date
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Since this is time series, expand the data and merge with df
# in order to get zeroes, when appropriate

# expand data so that we have a combination of every possible date, age_group and region
expanded_data <- expand.grid(Date = unique(df$Date), 
                             age_group = unique(df$age_group),
                             region = unique(df$region))
#join the expanded data to the actual data
#library(data.table)
# expanded_data <- as.data.frame(expanded_data)
# df <- as.data.frame(df)

expanded_data <- left_join(x = expanded_data,# tbl_dt(expanded_data), 
                           y = df, 
                           by = c("Date", "age_group", "region"),
                           copy = TRUE)


# fill in NA's with 0's (since an NA means there were no cases for that date, region, age_group comb)
expanded_data$cases[is.na(expanded_data$cases)] <- 0    # fill with zeroes for summarise below.
# assign expanded_data to df
df <- expanded_data
# remove expanded_data
rm(expanded_data)

# Remove the NA age_group
df <- df[which(!is.na(df$age_group)),]


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
  temp <- state[which(state$region == i),]
  
  # Get sum of the population for that area
  pop <- sum(temp[,names(state) != "region" & names(state) != "AreaName"])
  
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
df$pop <- NA
for (i in denoms$region){
  for (j in c("0004", "0517", "1844", "4564", "65plus")){
    
    # Get just the appropriate area
    temp <- state[which(state$region == i),]
    
    # Get sum of the population for that area
    pop <- sum(temp[,paste0("Total", j)])
    
    # Apply that sum to denoms
    denoms[which(denoms$region == i),paste0("pop", j)] <- pop
    
    # Apply a denom column into df
    abbreviated_age_group <- substr(j, 1, 2)
    df$pop[which(df$region == i &
                   substr(df$age_group, 1, 2) == abbreviated_age_group)] <-
      pop
  }
}

# ADJUST FOR POPULATION GROWTH (census data from 2010)
growth <- data.frame(year = 2009:2014,
                     pop = c(18.65, 18.84, 19.11, 19.36, 19.6, 19.89))
daily_growth_rate <- 
  ((max(growth$pop) - min(growth$pop)) * 1000000) /
  ((max(growth$year) - min(growth$year)) * 365)

p_daily_growth_rate <- daily_growth_rate / (growth$pop[growth$year == 2010]*1000000)

nrow_df <- nrow(df)
for (i in 1:nrow_df){
  days_different <- as.numeric(df$Date[i] - as.Date('2010-01-01'))
  change_rate <- days_different * p_daily_growth_rate
  change <- change_rate * df$pop[i]
  new_pop <- df$pop[i] + change
  df$pop[i] <- new_pop
  print(paste(i, 'of', nrow_df))
}


# ADD AN "all" CATEGORY ROW TO AGE_GROUP
# first make a dataframe of all age groups
all <- df %>%
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

# finally, add the all dataframe to df
all <- all[,c("Date", "age_group", "region", "cases", "pop")]
df <- rbind(all, df)
df <- df[order(df$Date),]

# ADD A PER 1000 COLUMN
df$k <- df$cases / df$pop * 1000

# Convert back to regular dataframe
df <- data.frame(df)

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

# ALL age groups
pdf(file = paste0(public, "/visuals/", "by_age_group.pdf"),
    width = 11,
    height = 7)
plot_fancy("all", moving_average = 30,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.3), moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()

# SAME SCALE
# ALL age groups
pdf(file = paste0(public, "/visuals/", "by_age_group_same_scale.pdf"),
    width = 12,
    height = 7)
par(mfrow = c(2,3))
plot_fancy("all", moving_average = 30,
           xlim = c(min(df$Date) + 180, max(df$Date)), ylim = c(0, 0.3))
title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.3), moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)))
title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)), ylim = c(0, 0.3))
title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)), ylim = c(0, 0.3))
title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)), ylim = c(0, 0.3))
title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+", moving_average = 60,
           xlim = c(min(df$Date) + 180, max(df$Date)), ylim = c(0, 0.3))
title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()


#####
# BARPLOTS
#####


# DEFINE FLU SEASONS
df$date <- df$Date

# df$flu <- 
#   df$date >= '2010-10-03' & df$date <= '2011-05-21' |
#   df$date >= '2011-10-02' & df$date <= '2012-05-19' |
#   df$date >= '2012-09-30' & df$date <= '2013-05-18' |
#   df$date >= '2013-09-29' & df$date <= '2014-05-17' |
#   df$date >= '2014-09-29' & df$date <= '2014-05-20'

df$flu_season <- ifelse(
  df$date >= '2010-10-01' & df$date <= '2011-04-13',
  '2010-2011',
  ifelse(df$date >= '2011-10-01' & df$date <= '2012-04-13',
         '2011-2012',
         ifelse(df$date >= '2012-10-01' & df$date <= '2013-04-13',
                '2012-2013',
                ifelse(df$date >= '2013-10-01' & df$date <= '2014-04-13',
                       '2013-2014',
                       ifelse(df$date >= '2014-10-01' & df$date <= '2015-04-13',
                              '2014-2015', 
                              NA)))))

temp <- df %>%
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
                ylim = c(0, 3.5))  #c(0, max(sub_temp$pop100) * 1.2))
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
pdf(file = paste0(public, "/visuals/protection_barplot_1_page.pdf"),
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

#####################
# BREAK DOWN BY MORE THAN JUST FLU SEASON FOR MORRIS
#####################

df$month <- as.numeric(format(df$date, '%m'))
df$year <- as.numeric(format(df$date, '%Y'))

temp <- df %>%
  group_by(year, month, age_group, region) %>%
  summarise(cases = sum(cases),
            pop = mean(pop))

# Get cases per 100
temp$pop100 <- temp$cases / temp$pop * 100

# Get p (proportion)
temp$p <- temp$cases / temp$pop

# Get confidence interval on proportion
temp$lb <- NA
temp$ub <- NA
for (i in 1:nrow(temp)){
  conf <- simpasym(n = temp$pop[i],
                   p = temp$p[i])
  temp$lb[i] <- conf$lb * 100
  temp$ub[i] <- conf$ub * 100
}

# Get colors
temp$col <- ifelse(temp$region == 'ALACHUA', cols[1],
                   ifelse(temp$region == 'REGION3', cols[2],
                          ifelse(temp$region == 'FLORIDA', cols[3],
                                 NA)))

# Define function for pretty barplots
plot_glenn <- function(year = 2010, month = 1){
  
  # subset to only include the flu season of interest
  sub_temp <- temp[which(temp$year == year &
                           temp$month == month),]
  
  #
  if(nrow(sub_temp) > 0){
    bp <- barplot(sub_temp$pop100, 
                  space = rep(c(1,0,0), nrow(sub_temp)/3),
                  xaxt = 'n',
                  ylab = 'Flu season cases per 100 residents',
                  col = 'white', border = 'white',
                  ylim = c(0, 0.7))  #c(0, max(sub_temp$pop100) * 1.2))
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
    abline(h = seq(0, 10, 0.1),
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
    title(main = paste(year, 'month', month, 'flu season cumulative incidence rate'),
          cex.main = 1.8)
  }
}

## plot
plot_glenn(year = 2016, month = 2)

years <- 2010:2015
months <- 1:12
for (i in years){
  for (j in months){
    month_name <- j
    while(nchar(as.character(month_name)) < 3){
      month_name <- paste0(0, month_name)
    }
    png(file = paste0(public, "/visuals/gif/protection", i, month_name, ".png"),
        width = 650)
    plot_glenn(year = i, month = j)
    dev.off()
    
  }
}

plot_glenn(flu_season = '2011-2012')
plot_glenn(flu_season = '2012-2013')
plot_glenn(flu_season = '2013-2014')
plot_glenn(flu_season = '2014-2015')

####################################################################
#####
# JONATHAN'S POPULATION DENOMINATORS
#####
install.packages("readstata13")
library(readstata13)
census <- read.dta13("popprojections.dta")

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

# Get a smaller df and join with census
small_df <- df[,c('Date', 'age_group', 'region', 'cases',
                  'date', 'flu_season', 'month', 'year')]
jon_df <- left_join(small_df, census)

# Create the k variable
jon_df$k <- jon_df$cases / jon_df$pop * 1000
jon_df$k[which(is.na(jon_df$k))] <- 0

#####
# PLOT USING JONATHAN'S DENOMINATORS
#####


# ALL age groups
pdf(file = paste0(public, "/visuals/", "by_age_group_jonathans_denoms.pdf"),
    width = 11,
    height = 7)
plot_fancy("all", moving_average = 30,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           ylim = c(0, 0.1),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.4), moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17", moving_average = 60,
           ylim = c(0, 0.2),
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44", moving_average = 60,
           ylim = c(0, 0.1),
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64", moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           ylim = c(0, 0.1),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+", moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           ylim = c(0, 0.1),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()

# SAME SCALE
# ALL age groups
pdf(file = paste0(public, "/visuals/", "by_age_group_same_scale_jonathans_denoms.pdf"),
    width = 12,
    height = 7)
par(mfrow = c(2,3))
plot_fancy("all", moving_average = 30,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)), ylim = c(0, 0.3),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: all ages")
plot_fancy("00-04", ylim = c(0, 0.3), moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 0-4")
plot_fancy("05-17", moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)), ylim = c(0, 0.3),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 5-17")
plot_fancy("18-44", moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)), ylim = c(0, 0.3),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 18-44")
plot_fancy("45-64", moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)), ylim = c(0, 0.3),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 45-64")
plot_fancy("65+", moving_average = 60,
           xlim = c(min(jon_df$Date) + 180, max(jon_df$Date)), ylim = c(0, 0.3),
           data = jon_df)
title(main = "ILI ED rate per 1,000 residents: ages 65 +")
dev.off()



#####
# BARPLOTS
#####


# DEFINE FLU SEASONS
jon_df$date <- jon_df$Date

# jon_df$flu <- 
#   jon_df$date >= '2010-10-03' & jon_df$date <= '2011-05-21' |
#   jon_df$date >= '2011-10-02' & jon_df$date <= '2012-05-19' |
#   jon_df$date >= '2012-09-30' & jon_df$date <= '2013-05-18' |
#   jon_df$date >= '2013-09-29' & jon_df$date <= '2014-05-17' |
#   jon_df$date >= '2014-09-29' & jon_df$date <= '2014-05-20'

jon_df$flu_season <- ifelse(
  jon_df$date >= '2010-10-01' & jon_df$date <= '2011-04-13',
  '2010-2011',
  ifelse(jon_df$date >= '2011-10-01' & jon_df$date <= '2012-04-13',
         '2011-2012',
         ifelse(jon_df$date >= '2012-10-01' & jon_df$date <= '2013-04-13',
                '2012-2013',
                ifelse(jon_df$date >= '2013-10-01' & jon_df$date <= '2014-04-13',
                       '2013-2014',
                       ifelse(jon_df$date >= '2014-10-01' & jon_df$date <= '2015-04-13',
                              '2014-2015', 
                              NA)))))

temp <- jon_df %>%
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
                ylim = c(0, 4))  #c(0, max(sub_temp$pop100) * 1.2))
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
pdf(file = paste0(public, "/visuals/protection_barplot_1_page_jonathans_denoms.pdf"),
    height = 8, width = 12)
par(mfrow = c(2,2))

#plot_bar(flu_season = '2010-2011')
plot_bar(flu_season = '2011-2012')
plot_bar(flu_season = '2012-2013')
plot_bar(flu_season = '2013-2014')
plot_bar(flu_season = '2014-2015')
dev.off()


#####################
# BREAK DOWN BY MORE THAN JUST FLU SEASON FOR MORRIS
#####################

jon_df$month <- as.numeric(format(jon_df$date, '%m'))
jon_df$year <- as.numeric(format(jon_df$date, '%Y'))

temp <- jon_df %>%
  group_by(year, month, age_group, region) %>%
  summarise(cases = sum(cases),
            pop = mean(pop))

# Get cases per 100
temp$pop100 <- temp$cases / temp$pop * 100

# Get p (proportion)
temp$p <- temp$cases / temp$pop

# Get confidence interval on proportion
temp$lb <- NA
temp$ub <- NA
for (i in 1:nrow(temp)){
  conf <- simpasym(n = temp$pop[i],
                   p = temp$p[i])
  temp$lb[i] <- conf$lb * 100
  temp$ub[i] <- conf$ub * 100
}

# Get colors
temp$col <- ifelse(temp$region == 'ALACHUA', cols[1],
                   ifelse(temp$region == 'REGION3', cols[2],
                          ifelse(temp$region == 'FLORIDA', cols[3],
                                 NA)))

# Define function for pretty barplots
plot_glenn <- function(year = 2010, month = 1){
  
  # subset to only include the flu season of interest
  sub_temp <- temp[which(temp$year == year &
                           temp$month == month),]
  
  #
  if(nrow(sub_temp) > 0){
    bp <- barplot(sub_temp$pop100, 
                  space = rep(c(1,0,0), nrow(sub_temp)/3),
                  xaxt = 'n',
                  ylab = 'Flu season cases per 100 residents',
                  col = 'white', border = 'white',
                  ylim = c(0, 0.7))  #c(0, max(sub_temp$pop100) * 1.2))
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
    abline(h = seq(0, 10, 0.1),
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
    title(main = paste(year, 'month', month, 'flu season cumulative incidence rate'),
          cex.main = 1.8)
  }
}

## plot
plot_glenn(year = 2016, month = 2)

years <- 2010:2015
months <- 1:12
for (i in years){
  for (j in months){
    month_name <- j
    while(nchar(as.character(month_name)) < 3){
      month_name <- paste0(0, month_name)
    }
    png(file = paste0(public, "/visuals/gif/protection_jonathans_denoms", i, month_name, ".png"),
        width = 650)
    plot_glenn(year = i, month = j)
    dev.off()
    
  }
}


#####
# VACCINE EFFECTIVENESS
#####
library(tidyr)

# Comparing Alachua to both florida and region3
temp <- jon_df %>%
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
    
    ve$rr_florida <- (alachua_cases / alachua_pop) / 
      (florida_cases / florida_pop)
      
    
    # proportion test = region3
    region3_obj <- prop.test(x = c(alachua_cases, region3_cases),
                             n = c(alachua_pop, region3_pop))
    
    ve$region3_p_value[i] <- region3_obj$p.value
    
    ve$rr_region3 <- (alachua_cases / alachua_pop) / 
      (region3_cases / region3_pop)

  })
  
}

# Write some csvs
write.csv(ve, 'tables/vaccine_effectiveness.csv', row.names = FALSE)
write.csv(temp, 'tables/cumulative_incidence.csv', row.names = FALSE)
