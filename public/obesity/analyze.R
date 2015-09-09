library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(RColorBrewer)

#####
# DIRECTORIES
#####
set_directories <- function(){
  if(Sys.info()['sysname'] == 'Windows'){
    assign('private', 'E:/fdoh/private/obesity/data201415/', envir = .GlobalEnv)
    assign('public', 'C:/Users/BrewJR/Documents/fdoh/public/obesity/', envir = .GlobalEnv)
  } else {
    assign('private', '/media/joebrew/JB/fdoh/private/obesity/data201415/', envir = .GlobalEnv)
    assign('public', '/home/joebrew/Documents/fdoh/public/obesity/', envir = .GlobalEnv)
  }
}
set_directories()

#####
# READ IN DATA
#####
setwd(private)
if('temp.RData' %in% dir()){
  load(paste0(private, 'temp.RData'))
} else {
  
  
  # Read in 2014 data
  linkage <- read.csv(paste0(private, 'FDOH_Obesity_Linkage.csv'))
  df <- read.csv(paste0(private, 'BMI.csv'))
  names(df)[2] <- 'PersonID'
  
  # join together linkage with data
  screen14 <- left_join(x = df,
                        y = linkage[,c('PersonID', 'Homeroom_Teacher')])
  
  # Clean up column names
  names(screen14) <- c('School_Name', 'PersonID', 'Grade',
                       'Lunch_Status', 'Gender', 'Race', 'DOB',
                       'Health_Screening_Date', 'Age_Months', 'Growth_Height', 'Growth_Weight',
                       'BMI', 'BMI_Percentile', 'Teacher')
  
  screen14$DOB <- screen14$Teacher <- NULL
  
  ############################################
  ######### Read in the old data
  ############################################
  library(gdata)
  setwd(paste0(private, 'screen'))
  screen13 <- read.xls("screen2.xlsx", sheet=7)
  #Fix the column names of 2013 in order to match the previous years
  colnames(screen13)[13] <- "Lunch_Status"
  screen12 <- read.xls("screen2.xlsx", sheet=6)
  screen11 <- read.xls("screen2.xlsx", sheet=5)
  screen10 <- read.xls("screen2.xlsx", sheet=4)
  screen09 <- read.xls("screen2.xlsx", sheet=3)
  screen08 <- read.xls("screen2.xlsx", sheet=2)
  screen07 <- read.xls("screen2.xlsx", sheet=1)
  
  
  
  ############################################
  ######### Delete any student with multiple entries
  ############################################
  clean_up <- function(x){
    x <- x[!duplicated(x),]
    x <- x[,names(screen14)]
  }
  
  screen14 <- clean_up(screen14)
  screen13 <- clean_up(screen13)
  screen12 <- clean_up(screen12)
  screen11 <- clean_up(screen11)
  screen10 <- clean_up(screen10)
  screen09 <- clean_up(screen09)
  screen08 <- clean_up(screen08)
  screen07 <- clean_up(screen07)
  
  ############################################
  ######### Make one master df
  ############################################
  screen <- as.data.frame(rbind(screen07,
                                screen08,
                                screen09,
                                screen10,
                                screen11,
                                screen12,
                                screen13,
                                screen14))
  
  
  screen$Race <- factor(screen$Race, levels=c("W", "B", "M", "H", "A", "I"))
  
  
  ############################################
  ######### Fix missing decimals in screen$Growth_Height
  ############################################
  screen$Growth_Height <- as.numeric(screen$Growth_Height)
  screen$Growth_Height <- ifelse(screen$Growth_Height > 100,
                                 screen$Growth_Height/10,
                                 screen$Growth_Height)
  
  ############################################
  ######### Fix BMI
  ############################################
  screen$Growth_Weight <- as.numeric(screen$Growth_Weight)
  screen$bmi <- screen$Growth_Weight / ((screen$Growth_Height)^2) * 703
  
  
  ############################################
  ######### Read in data to calculate percentile values
  ############################################
  setwd(public)
  lms <- read.csv("lms.csv")
  lms <- lms[-c(1, 220),]
  lms$Agemos <- as.numeric(as.character(lms$Agemos)) - 0.5
  #convert all columns to numeric data
  for (i in colnames(lms)){
    lms[,which(colnames(lms) == i)] <-
      as.numeric(as.character(lms[,which(colnames(lms) == i)]))}
  #These data were downloaded on 6 February 2014 from 
  #http://www.cdc.gov/growthcharts/percentile_data_files.htm
  #good article for calculating this: http://www.cdc.gov/nchs/data/nhsr/nhsr063.pdf 
  # i think the pdf has an error in the placement of the -1
  #m = median 
  #s = generalized coefficient of variation
  #l = power in the Box-Cox transformation
  #z = z-score
  #x = percentile
  
  ############################################
  ######### Calculate percentile for age values
  ############################################
  
  #boys
  lms1 <- lms[which(lms$Sex == 1),]
  #girls
  lms2 <- lms[which(lms$Sex == 2),]
  
  #eliminate those older than 240 months
  screen <- screen[which(screen$Age_Months <= 240),]
  
  screen$X <- screen$bmi
  screen$id <- 1:nrow(screen)
  screen$Z <- NA
  
  #boys
  for (i in screen$id[which(screen$Gender == "M")]){
    screen$Z[which(screen$id == i &
                     screen$Gender == "M")] <-
      (  (    screen$X[which(screen$id == i)] /
                lms1$M[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])]) ^ (
                  lms1$L[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])]  )-1) /
      (lms1$L[which(lms1$Agemos==screen$Age_Months[which(screen$id == i)])]*
         lms1$S[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])])}
  
  #girls
  for (i in screen$id[which(screen$Gender == "F")]){
    screen$Z[which(screen$id == i &
                     screen$Gender == "F")] <-
      (  (    screen$X[which(screen$id == i)] /
                lms2$M[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])]) ^ (
                  lms2$L[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])]  )-1) /
      (lms2$L[which(lms2$Agemos==screen$Age_Months[which(screen$id == i)])]*
         lms2$S[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])])}
  
  
  #Remove extreme values using CDC's guidelines
  #http://www.cdc.gov/pcd/issues/2009/jan/08_0007.htm
  screen <- screen[which(is.infinite(screen$Z)== FALSE &
                           screen$Z >= -4 &
                           screen$Z <= 5 ),]
  
  
  ##############################
  # Make a normal/oveweight/obese column
  #############################
  screen$cat <- factor(ifelse(screen$Z < 1.036436, 
                              "normal",
                              ifelse(screen$Z >=1.036436 & screen$Z <1.64485,
                                     "overweight",
                                     ifelse(screen$Z >=1.64485,
                                            "obese",
                                            NA))), levels=c("normal", "overweight", "obese"))
  ##############################
  # Make a normal/oveweight (binary) column
  #############################
  screen$catbi <- factor(ifelse(screen$Z < 1.036436, 
                                "normal",
                                ifelse(screen$Z >=1.036436,
                                       "overweight",
                                       NA)))
  
  # Get year
  screen$Health_Screening_Date <- as.Date(screen$Health_Screening_Date)
  screen$year <- as.numeric(format(screen$Health_Screening_Date, '%Y'))
  screen$day_number <- as.numeric(format(screen$Health_Screening_Date, '%j'))
  screen$year <- ifelse(screen$day_number <= 150, screen$year - 1, screen$year)
  
  # Fix grade
  screen$Grade <- ifelse(screen$Grade %in% c('PK', 'KG'),
                         as.character(screen$Grade),
                         as.character(as.numeric(as.character(screen$Grade))))
  
  
  
  #####
  # MANUALLY CLEAN SCHOOL NAMES
  #####
  screen$School_Name <- as.character(screen$School_Name)
  screen$school <- ifelse(screen$School_Name %in%
                            c('ALACHUA LEARNING CENTER ',
                              'ALACHUA LEARNING CENTER ELEMENTARY',
                              'ALACHUA LEARNING CENTER MIDDLE',
                              'ALACHUA LEARNING CNR ELE'),
                          'ALACHUA LEARNING CENTER',
                          ifelse(screen$School_Name == 'CARING & SHARING LEARNIN',
                                 'CARING & SHARING LEARNING',
                                 ifelse(screen$School_Name == 'DUVAL, CHARLES W. ELEMEN',
                                        'DUVAL, CHARLES W. ELEMENTARY',
                                        ifelse(screen$School_Name == 'FORT CLARKE MIDDLE SCHOO',
                                               'FORT CLARKE MIDDLE SCHOOL',
                                               ifelse(screen$School_Name == 'FOSTER, STEPHEN ELEMENTA',
                                                      'FOSTER, STEPHEN ELEMENTARY',
                                                      ifelse(screen$School_Name == 'HIGH SPRINGS COMMUNITY S',
                                                             'HIGH SPRINGS COMMUNITY SCHOOL',
                                                             ifelse(screen$School_Name == 'LINCOLN, ABRAHAM MIDDLE ',
                                                                    'LINCOLN, ABRAHAM MIDDLE SCHOOL',
                                                                    ifelse(screen$School_Name == 'MEADOWBROOK ELEMENTARY S',
                                                                           'MEADOWBROOK ELEMENTARY SCHOOL',
                                                                           ifelse(screen$School_Name == 'MEBANE, A.L. MIDDLE SCHO',
                                                                                  'MEBANE, A.L. MIDDLE SCHOOL',
                                                                                  ifelse(screen$School_Name == 'METCALFE, W.A. ELEMENTAR',
                                                                                         'METCALFE, W.A. ELEMENTARY',
                                                                                         ifelse(screen$School_Name == 'MICANOPY AREA COOP SCHOO',
                                                                                                'MICANOPY AREA COOP SCHOOL',
                                                                                                ifelse(screen$School_Name == 'RAWLINGS, MARJORIE K. EL',
                                                                                                       'RAWLINGS, MARJORIE K. ELEMENTARY',
                                                                                                       ifelse(screen$School_Name == 'SHELL, CHESTER ELEMENTAR',
                                                                                                              'SHELL, CHESTER ELEMENTARY',
                                                                                                              ifelse(screen$School_Name == 'SWEETWATER BRANCH ELEMENTARY',
                                                                                                                     'SWEETWATER BRANCH ACADEMY',
                                                                                                                     ifelse(screen$School_Name == 'TALBOT, WILLIAM S. ELEME',
                                                                                                                            'TALBOT, WILLIAM S. ELEMENTARY',
                                                                                                                            ifelse(screen$School_Name == 'TERWILLIGER, MYRA ELEMEN',
                                                                                                                                   'TERWILLIGER, MYRA ELEMENTARY',
                                                                                                                                   ifelse(screen$School_Name == 'WILES, KIMBALL ELEMENTAR',
                                                                                                                                          'WILES, KIMBALL ELEMENTARY',
                                                                                                                                          ifelse(screen$School_Name == 'WILLIAMS, JOSEPH ELEMENT',
                                                                                                                                                 'WILLIAMS, JOSEPH ELEMENTARY',
                                                                                                                                                 screen$School_Name))))))))))))))))))
  screen$School_Name <- NULL
  
  
  #PER HIDAHIS FIGUEROA MESA'S FEB 7 2014 EMAIL
  
  #race key %%%%%%%%%%%%%%%%%%%%%%
  #W White, Non Hispanic
  #B Black, Non Hispanic
  #H Hispanic
  #A Asian or Pacific Islander
  #I American Indian or Alaskan Native
  #M Multiracial
  screen$Race <- factor(screen$Race,
                        levels=c("W","I","A",  "B", "H", "M"),
                        labels=c("White",
                                 "Indian",
                                 "Asian",
                                 "Black",
                                 "Hispanic",
                                 "Multiracial"))
  
  
  #lunchStatus Key: %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #1  Applied Not Eligible
  #0  Did not apply
  #2  Eligible for free lunch
  #6	Eligible for free lunch/Direct Certified/Decline
  #9	Eligible for free lunch/Direct Certified
  #3	Eligible Reduced
  #4	Enrolled USDA approved Prov 2 school
  #Z	Unknown
  
  screen$lunch <- factor(Recode(screen$Lunch_Status,
                                "'1' = 'not_free';
                                '0' = 'not_free';
                                '2' = 'free';
                                '6' = 'free';
                                '9' = 'free';
                                '3' = 'free'"))
  screen$Lunch_Status <- NULL
  screen$lunch[which(screen$lunch == 'Z')] <- NA
  screen$lunch <- factor(screen$lunch)
  
  #####
  # 6. RECODE THE RACE VARIABLE INTO A BINARY
  #    WHITE / NON-WHITE VARIABLE
  #####
  screen$race <- Recode(screen$Race,
                        "'W' = 'White';
                        'B' =  'Black';
                        'H' = 'Hispanic';
                        'A' = 'Asian';
                        'I' = 'Native Amer.'; 
                        'M' =  'Multiracial'")
  screen$race_bi <- factor(ifelse(screen$race == "White",
                                  "white",
                                  "non_white"))
  screen$Race <- NULL
  
  
  
  #####
  # SAVE A CHECKPOINT
  #####
  save.image(paste0(private, 'temp.RData'))
}

# Get percentile
screen$bmi_percentile <- pnorm(screen$Z) * 100

# Get rid of 2015 data
screen <- screen[screen$year != 2015,]

#####
# RESET DIRECTORIES (in case image was saved on other system)
#####
set_directories()

#####
# SOURCE HELPERS
#####
setwd(public)
source('helpers.R')

###############################################################################

#####
# DEFINE KOURTNEY's 9 SCHOOLS OF INTEREST
##### 
k9 <- c('ALACHUA ELEMENTARY',
        'CARING & SHARING LEARNING',
        'DUVAL, CHARLES W. ELEMENTARY',
        'IDYLWILD ELEMENTARY',
        'LAKE FOREST ELEMENTARY',
        'METCALFE, W.A. ELEMENTARY',
        'RAWLINGS, MARJORIE K. ELEMENTARY',
        'TERWILLIGER, MYRA ELEMENTARY',
        'WILLIAMS, JOSEPH ELEMENTARY')

#####
# GET DISTRIBUTIONS
#####

# ALL SCHOOLS
temp <- get_ts(dist = TRUE, race_bi ='non_white')

# SPECIFIC SCHOOLS
temp <- get_ts(dist = TRUE, school = k9[3], grade = 3)

years <- as.numeric(gsub('year_', '', names(temp)))
colors <- colorRampPalette(c('darkblue', 'darkred'))(length(years))

for (i in 1:length(years)){
  hist(temp[[paste0('year_', years[i])]], freq = FALSE, 
       border = 'white',
       ylim = c(0, 0.04),
       xlim = c(0, 100),
       main = NA,
       xlab = 'Percentile BMI for age')
  
  for (j in which(years <= years[i])){
    lines(density(temp[[paste0('year_', years[j])]]),
          col = adjustcolor('black', alpha.f = 0.6))
  }
  
  lines(density(temp[[paste0('year_', years[i])]]),
        col = 'red',
        lwd = 2)
  title(main = years[i])
  Sys.sleep(1)
}

ggplot() + 
  ylim(0, 0.03) +
  geom_density(aes(temp[[paste0('year_', years[1])]]), 
               fill = colors[1],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[2])]]), 
               fill = colors[2],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[3])]]), 
               fill = colors[3],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[4])]]), 
               fill = colors[4],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[5])]]), 
               fill = colors[5],
               alpha = 0.5)

#####
# FREE REDUCED LUNCH
#####
schools <- screen %>%
  group_by(school) %>%
  summarise(Z = mean(Z),
            bmi_percentile = mean(bmi_percentile),
            n = n(),
            free_reduced = length(which(lunch == 'free')),
            non_white = length(which(race_bi == 'non_white')))

schools$p_free <- schools$free_reduced / schools$n * 100
schools$p_non_white <- schools$non_white / schools$n * 100

plot(schools$p_free, schools$bmi_percentile)
g <- ggplot(data = schools[which(schools$bmi_percentile >= 40),], aes(x = p_non_white, 
                                y = bmi_percentile))

g + 
  geom_jitter(alpha = 0.6, color = 'darkgreen') +
  #geom_smooth() + 
  geom_smooth(color = 'red')


#####
# PLOT EACH OF 9 SCHOOLS
#####
for (i in 1:length(k9)){
  
  # Everyone
  all_schools <- get_ts()
  all_schools$school <- 'all'
  
  this_school <- get_ts(school = k9[i])
  this_school$school <- k9[i]
  
  temp <- rbind(all_schools, this_school)
  
  g <- ggplot(data = temp, aes(x = year, y = bmi_percentile, group = school, color = school))
  g1 <- g + geom_line() +
    # geom_smooth() +
    ylim(50, 100) +
    ggtitle('All students')
  
  # Free/reduced lunch only
  all_schools <- get_ts(lunch = 'free')
  all_schools$school <- 'all'
  
  this_school <- get_ts(school = k9[i], lunch = 'free')
  this_school$school <- k9[i]
  
  temp <- rbind(all_schools, this_school)
  
  g <- ggplot(data = temp, aes(x = year, y = bmi_percentile, group = school, color = school))
  g2 <- g + geom_line() +
    # geom_smooth() +
    ylim(50, 100) +
    ggtitle('Only free/reduced lunch students')
  
  # Nonwhite only
  all_schools <- get_ts(race_bi = 'non_white')
  all_schools$school <- 'all'
  
  this_school <- get_ts(school = k9[i], race_bi = 'non_white')
  this_school$school <- k9[i]
  
  temp <- rbind(all_schools, this_school)
  
  g <- ggplot(data = temp, aes(x = year, y = bmi_percentile, group = school, color = school))
  g3 <- g + geom_line() +
    # geom_smooth() +
    ylim(50, 100) +
    ggtitle('Only non-white students')
  
  multiplot(g1, g2, g3)
}


g <- ggplot(data = screen[which(!is.na(screen$lunch)),],
            aes(x = factor(year), y = Z))

g + geom_jitter(alpha = 0.2) +
  geom_violin(fill = 'orange', alpha = 0.3, color = NA) +
  facet_grid(lunch ~ race_bi)

#### 
g <- ggplot(data = screen[which(screen$year != 2015 & !is.na(screen$lunch)),])
g + geom_density(aes(x = Z, y = ..density.., group = lunch, fill = lunch),
                 alpha = 0.3) +
  facet_grid( year ~ .)

# Get by year, school, grade, lunch
temp <- screen %>%
  filter(year != 2015) %>%
  group_by(year, lunch) %>%
  summarise(n = n(),
            normal = length(which(cat == 'normal')),
            overweight = length(which(cat == 'overweight')),
            obese = length(which(cat == 'obese')),
            Z = mean(Z))

# Get percentages
columns <- c('normal', 'overweight', 'obese')
for (j in 1:length(columns)){
  temp[,paste0(columns[j], '_p')] <- 
    temp[,columns[j]] / temp$n * 100
}


# Reshape
temp_gathered <- gather(temp, key = key, value = value, normal_p:obese_p)

g <- ggplot(data = temp_gathered,
            aes(x = year, y = Z, group = lunch, color = lunch))
g + geom_line()


# Get by year
temp <- screen %>%
  filter(year != 2015) %>%
  group_by(year, Grade) %>%
  summarise(n = n(),
            normal = length(which(cat == 'normal')),
            overweight = length(which(cat == 'overweight')),
            obese = length(which(cat == 'obese')))

# Get percentages
columns <- c('normal', 'overweight', 'obese')
for (j in 1:length(columns)){
  temp[,paste0(columns[j], '_p')] <- 
    temp[,columns[j]] / temp$n * 100
}

# Keep only 6th graders
temp6 <- temp[which(temp$Grade == 6),]

# Reshape
temp6_gathered <- gather(temp6, key = key, value = value, normal_p:obese_p)

# Relevel key so that obese is on the bottom
temp6_gathered$key <- factor(temp6_gathered$key,
                             levels = c('obese_p', 'overweight_p', 'normal_p'))

temp6_gathered <- arrange(temp6_gathered, (key))

# Get a version of temp6_gathered using n()
temp6_gathered_n <- gather(temp6, key = key, value = value, normal:obese)
temp6_gathered_n <- arrange(temp6_gathered_n, (key))

# Plot ###############################3


library(reshape2)

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

g <- ggplot(temp6_gathered)
g1 <- g +
  aes(x = year, y = value, group = key, colour = key) +
  #geom_line(lwd = 1, alpha = 0.5) +
  geom_area(aes(group = key, fill = key), color = NA, position = 'stack') +
  theme_bw() +
  xlab('Year') + ylab('Percentage') +
  ggtitle('6th grade adiposity over time (Relative)') +
  theme(axis.text.x = element_text(size = 7, angle = 90),
        #legend.position = 'none',
        axis.title = element_text(size = 8),
        plot.title = element_text(size = rel(0.8))) +
  scale_fill_manual(values = c('darkred', 'orange', 'beige'),
                    guide = guide_legend(reverse=TRUE))

g <- ggplot(temp6_gathered_n)
g2 <- g +
  aes(x = year, y = value, group = key, colour = key) +
  #geom_line(lwd = 1, alpha = 0.5) +
  geom_area(aes(group = key, fill = key), color = NA, position = 'stack') +
  theme_bw() +
  xlab('Year') + ylab('Students') +
  ggtitle('6th grade adiposity over time (Absolute)') +
  theme(axis.text.x = element_text(size = 7, angle = 90),
        #legend.position = 'none',
        axis.title = element_text(size = 8),
        plot.title = element_text(size = rel(0.8))) +
  scale_fill_manual(values = c('darkred', 'orange', 'beige'),
                    guide = guide_legend(reverse=TRUE))



screen <- screen[which(screen$year != 2015),]
g3 <- ggplot(data = screen, aes(x = factor(year), y = Z)) +
  geom_jitter(color = 'orange', alpha = 0.2) +
  geom_violin(fill = 'blue', alpha = 0.3) +
  xlab('Year') +
  ylab('Z-score (0 = average)') +
  ggtitle('Distribution by year')

g4 <- ggplot(data = screen) + 
  aes(x = Health_Screening_Date, y = Z) + 
  geom_point(color = 'orange', alpha = 0.2) + 
  geom_smooth() + 
  xlab('Date of screening') +
  ylab('Z-score') +
  ggtitle('Smoothed trend')

multiplot(g1, g2, g3, g4, cols = 2)
