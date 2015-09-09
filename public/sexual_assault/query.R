# Packages
library(dplyr)
library(rgdal)
library(readr)

# Locations
if(Sys.info()['sysname'] == 'Linux'){
  public <- '/home/joebrew/Documents/fdoh/public/sexual_assault'
  dt <- '/home/joebrew/Desktop'
} else {
  public <- 'C:/Users/BrewJR/Documents/fdoh/public/sexual_assault'
  dt <- 'C:/Users/BrewJR/Desktop'
}

# Source helpers
source(paste0(public, '/helpers.R'))

# Read in data
# df <- read.table(paste0(dt, '/alachua_hospitals_2009_onwards.txt'), sep = ',', header = TRUE)
# save(df, file = paste0(dt, '/alachua_hospitals_2009_onwards.RData'))
load(paste0(dt, '/alachua_hospitals_2009_onwards.RData'))

# Lowercase the ccdd
df$ccdd <- tolower(df$CCDD)

# Format date
df$Date <- as.Date(df$Date, format = '%m/%d/%Y')

# Get year
df$year <- as.numeric(format(df$Date, '%Y'))


# 
# Search for term
raw_data <- data.frame(df[which(grepl('rape | rape', df$ccdd) |
                                  (grepl('sexual', df$ccdd) & grepl('assault|asault|assalt|attack|abuse', df$ccdd)) ),], 
                       stringsAsFactors = FALSE)
# AGGREGATE BY YEAR
agg_data <- raw_data %>%
  group_by(year) %>%
  summarise(n = n())

# Plot
# remove 2015
agg_data <- agg_data[agg_data$year != '2015',]
bp <- barplot(agg_data$n,
        names.arg = agg_data$year,
        border = NA,
        xlab = 'Year',
        ylab = 'Cases',
        main = 'Sexual assault cases in Alachua emergency facilities',
        ylim = c(0, max(agg_data$n) * 1.1))
text(x = bp[,1],
     y = agg_data$n,
     pos = 3,
     cex = 2,
     labels = agg_data$n,
     col = adjustcolor('darkred', alpha.f = 0.6))

# #####
# # FUNCTIONS
# #####
# query <- function(term = 'sa', ts = TRUE, return_object = NULL){
#   raw_data <- data.frame(df[which(grepl(term, df$ccdd)),], stringsAsFactors = FALSE)
#   agg_data <- raw_data %>%
#     group_by(year) %>%
#     summarise(n = n())
#   if(is.null(return_object)){
#     return(list("raw_data" = raw_data,
#                 "agg_data" = agg_data))
#   } else{
#     return(return_object)
#   }
# }
