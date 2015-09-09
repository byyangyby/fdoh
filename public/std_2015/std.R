library(dplyr)
library(RColorBrewer)

wd <- 'C:/Users/BrewJR/Documents/fdoh/public/std_2015'
setwd(wd)

#####
# Census data for county-specific population
#####
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

# SUM OF ALL
names(state)[17:26]
sum(state[1,17:26]) / sum(state[1,-1])
sum(state[2:68,17:26]) / sum(state[2:68,-1])



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

# Alacua denom girls 13 to 17 years old
denom <- sum(state[which(state$AreaName == 'ALACHUA'),paste0('Total', 13:17, 'Years')]) / 2

#####
# CDC DATA HERE: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm6329a3.htm
#####
cdc <- data.frame(year = 2007:2013,
                  girls1 = c(25.1, 37.2, 44.3, 48.7, 53.0, 53.8, 57.3),
                  girls1lwr = c(22.3, 35.2, 42.4, 46.9, 51.4, 52, 55.4),
                  girls1upr = c(28.1, 39.3, 46.1, 50.5, 54.7, 55.7, 59.2),
                  girls2 = c(16.9, 28.3, 35.8, 40.7, 43.9, 43.4, 47.7),
                  girls2lwr = c(14.6, 26.4, 34.1, 38.9, 42.3, 41.5, 45.7),
                  girls2upr = c(19.6, 30.3, 37.6, 42.5, 45.6, 45.2, 49.6),
                  girls3 = c(5.9, 17.9, 26.7, 32.0, 34.8, 33.4, 37.6),
                  girls3lwr = c(4.4, 16.3, 25.2, 30.3, 33.2, 31.7, 35.7),
                  girls3upr = c(7.8, 19.6, 28.3, 33.6, 36.4, 35.2, 39.6),
                  boys1 = c(0, 0, 0, 0, 8.3, 20.8, 34.6),
                  boys1lwr = c(0, 0, 0, 0, 7.4, 19.4, 32.7),
                  boys1upr = c(0, 0, 0, 0, 9.3, 22.4, 36.5),
                  boys2 = c(0, 0, 0, 0, 3.8, 12.7, 23.5),
                  boys2lwr = c(0, 0, 0, 0, 3.2, 11.5, 21.8),
                  boys2upr = c(0, 0, 0, 0, 4.5, 14, 25.3),
                  boys3 = c(0, 0, 0, 0, 1.3, 6.8, 13.9),
                  boys3lwr = c(0, 0, 0, 0, 1, 5.9, 12.5),
                  boys3upr = c(0, 0, 0, 0, 1.7, 7.8, 15.3))

# MAKE AN ALACHUA df INTERPOLATING FROM CDC
alachua <- cdc
for (j in 2:ncol(cdc)){
  column <- cdc[,j]
  column <- column * denom / 100
  alachua[,j] <- round(column)
}

# Make charts
library(Hmisc)

# ALACHUA GIRLS
cols <- brewer.pal(5, 'Blues')[3:5]
columns <- c('girls1', 'girls2', 'girls3')
sub_data <- alachua[,columns]
obj <- t(as.matrix(sub_data))
girls_max <- max(obj)

lwrs <- paste0(columns, 'lwr')
uprs <- paste0(columns, 'upr')
lwrs <- t(as.matrix(alachua[,lwrs]))
uprs <- t(as.matrix(alachua[,uprs]))

bp <- barplot(obj,
              beside = TRUE,
              ylim = c(0, girls_max * 1.2),
              col = cols,
              border = NA,
              las = 1,
              names.arg = alachua$year,
              xlab = 'Year', 
              ylab = 'Residents',
              main = 'HPV vaccine coverage among 13-17 Alachua resident females (estimated)',
              cex.main = 0.85)
legend('topleft',
       fill = cols,
       border = NA,
       legend = paste(1:3, 'dose'))
legend('topright',
       lty = 1,
       col = adjustcolor('darkred', alpha.f = 0.6),
       legend = '95% confidence interval',
       bty = 'n',
       cex = 0.8)

errbar(x = as.numeric(bp),
       y = as.numeric(obj),
       yplus = as.numeric(uprs),
       yminus = as.numeric(lwrs),
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor('darkred', alpha.f = 0.6))


# ALACHUA BOYS
cols <- brewer.pal(5, 'Greens')[3:5]
columns <- c('boys1', 'boys2', 'boys3')
sub_data <- alachua[,columns]
obj <- t(as.matrix(sub_data))

lwrs <- paste0(columns, 'lwr')
uprs <- paste0(columns, 'upr')
lwrs <- t(as.matrix(alachua[,lwrs]))
uprs <- t(as.matrix(alachua[,uprs]))


bp <- barplot(obj,
              beside = TRUE,
              ylim = c(0, girls_max * 1.2),
              col = cols,
              border = NA,
              las = 1,
              names.arg = alachua$year,
              xlab = 'Year', 
              ylab = 'Residents',
              main = 'HPV vaccine coverage among 13-17 Alachua resident males (estimated)',
              cex.main = 0.85)
legend('topleft',
       fill = cols,
       border = NA,
       legend = paste(1:3, 'dose'))

legend('topright',
       lty = 1,
       col = adjustcolor('darkred', alpha.f = 0.6),
       legend = '95% confidence interval',
       bty = 'n',
       cex = 0.8)

errbar(x = as.numeric(bp),
       y = as.numeric(obj),
       yplus = as.numeric(uprs),
       yminus = as.numeric(lwrs),
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor('darkred', alpha.f = 0.6))

# PROPORTIONAL GIRLS
cols <- brewer.pal(5, 'Blues')[3:5]
columns <- c('girls1', 'girls2', 'girls3')
sub_data <- cdc[,columns]
obj <- t(as.matrix(sub_data))
girls_max <- max(obj)

lwrs <- paste0(columns, 'lwr')
uprs <- paste0(columns, 'upr')
lwrs <- t(as.matrix(cdc[,lwrs]))
uprs <- t(as.matrix(cdc[,uprs]))


bp <- barplot(obj,
              beside = TRUE,
              ylim = c(0, girls_max * 1.2),
              col = cols,
              border = NA,
              las = 1,
              names.arg = cdc$year,
              xlab = 'Year', 
              ylab = 'Percent',
              main = 'HPV vaccine coverage among 13-17 cdc resident females (estimated)',
              cex.main = 0.85)
legend('topleft',
       fill = cols,
       border = NA,
       legend = paste(1:3, 'dose'))

legend('topright',
       lty = 1,
       col = adjustcolor('darkred', alpha.f = 0.6),
       legend = '95% confidence interval',
       bty = 'n',
       cex = 0.8)

errbar(x = as.numeric(bp),
       y = as.numeric(obj),
       yplus = as.numeric(uprs),
       yminus = as.numeric(lwrs),
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor('darkred', alpha.f = 0.6))


# PROPORTIONAL BOYS
cols <- brewer.pal(5, 'Greens')[3:5]
columns <- c('boys1', 'boys2', 'boys3')
sub_data <- cdc[,columns]
obj <- t(as.matrix(sub_data))


# CURRENT

lwrs <- paste0(columns, 'lwr')
uprs <- paste0(columns, 'upr')
lwrs <- t(as.matrix(cdc[,lwrs]))
uprs <- t(as.matrix(cdc[,uprs]))

bp <- barplot(obj,
              beside = TRUE,
              ylim = c(0, girls_max * 1.2),
              col = cols,
              border = NA,
              las = 1,
              names.arg = cdc$year,
              xlab = 'Year', 
              ylab = 'Percent',
              main = 'HPV vaccine coverage among 13-17 cdc resident males (estimated)',
              cex.main = 0.85)
legend('topleft',
       fill = cols,
       border = NA,
       legend = paste(1:3, 'dose'))

legend('topright',
       lty = 1,
       col = adjustcolor('darkred', alpha.f = 0.6),
       legend = '95% confidence interval',
       bty = 'n',
       cex = 0.8)

errbar(x = as.numeric(bp),
       y = as.numeric(obj),
       yplus = as.numeric(uprs),
       yminus = as.numeric(lwrs),
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor('darkred', alpha.f = 0.6))


