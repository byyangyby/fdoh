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

# # ALL age groups
# pdf(file = paste0(public, "/visuals/", "by_age_group.pdf"),
#                   width = 11,
#                   height = 8.5)
# plot_fancy("all") ; title(main = "ILI ED rate per 1,000 residents: all ages")
# plot_fancy("00-04", ylim = c(0, 0.3)) ; title(main = "ILI ED rate per 1,000 residents: ages 0-4")
# plot_fancy("05-17") ; title(main = "ILI ED rate per 1,000 residents: ages 5-17")
# plot_fancy("18-44") ; title(main = "ILI ED rate per 1,000 residents: ages 18-44")
# plot_fancy("45-64") ; title(main = "ILI ED rate per 1,000 residents: ages 45-64")
# plot_fancy("65+") ; title(main = "ILI ED rate per 1,000 residents: ages 65 +")
# dev.off()

# METRICS GRAPHICS
# library(devtools)
# install_github("metricsgraphics/hrbrmstr")
library(metricsgraphics)

df <- ili[which(ili$region == "FLORIDA" &
                       ili$age_group == "all"),]
region3 <- ili[which(ili$region == "REGION3" &
                       ili$age_group == "all"),]
alachua <- ili[which(ili$region == "ALACHUA" &
                       ili$age_group == "all"),]
df$florida <- df$k
df$region3 <- region3$k
df$alachua <- alachua$k
mjs_plot(data = df,
         x = Date, 
         y = k) %>%
  mjs_line() %>%
  mjs_add_line(region3) %>%
  mjs_add_line(alachua) %>%
  mjs_axis_x(xax_format = "date") %>%
  mjs_add_legend(c("Florida", "Region 3", "Alachua"))
