#####
# LOAD PACKAGES
#####
library(xtable)
library(Hmisc)
library(gvlma)
library(car)
library(ResourceSelection)
library(Hmisc)
library(dplyr)
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


#####
# SET WORKING DIRECTORY CONDITIONAL TO SYSTEM
#####
if ( Sys.info()["sysname"] == "Linux" ){
  private <- "/media/joebrew/JB/fdoh/private/ab_chd_private"
  public <- "/home/joebrew/Documents/fdoh/public/ab_chd_private"
} else {
  private <- "E:/fdoh/private/ab_chd_private"
  public <- "C:/Users/BrewJR/Documents/fdoh/public/ab_chd_private"
}
setwd(private) 

#####
# READ IN THE DATA
#####
load(paste0(private, "/data/images/01_read_and_clean.RData"))

#####
# REESTABLISH WORKING DIRECTORY CONDITIONAL TO SYSTEM
#####
if ( Sys.info()["sysname"] == "Linux" ){
  private <- "/media/joebrew/JB/fdoh/private/ab_chd_private"
  public <- "/home/joebrew/Documents/fdoh/public/ab_chd_private"
} else {
  private <- "E:/fdoh/private/ab_chd_private"
  public <- "C:/Users/BrewJR/Documents/fdoh/public/ab_chd_private"
}
setwd(private) 

#####
### IMM RATE BY YEAR
#####
x <- shots %>%
  group_by(year, age_group, admin) %>%
  summarise(imm = sum(imm, na.rm = TRUE))

x[which(x$age_group == "mid"),]

###############
# ABSENTEEISM ANALYSIS
###############

# Calculate fsar and nfsar
dat$fsar <- dat$flu_season_absences / 
  dat$flu_days
dat$nfsar <- dat$non_flu_season_absences / 
  dat$non_flu_days

# Cut points for nfsar
cut_point <- mean(dat$nfsar) + (sd(dat$nfsar)*3)
hist(dat$nfsar)
abline(v = cut_point)


# Exclude those with greater than 3 sd nfsar and pre-k
#dat <- dat[which(dat$age_group != "PK" &
#                   dat$nfsar < cut_point),]



#####
# GET DATA BY AGE_GROUP
#####
ag_data <- dat %>% group_by(age_group, year, imm) %>%
  summarise(flu_season_absences= sum(flu_season_absences, na.rm = T),
            flu_days = sum(flu_days, na.rm = T),
            non_flu_season_absences= sum(non_flu_season_absences, na.rm = T),
            non_flu_days = sum(non_flu_days),
            students = n())

ag_data <- ag_data %>% group_by(age_group, imm) %>%
  summarise(fsar = sum(flu_season_absences, na.rm = TRUE) / sum(flu_days, na.rm = TRUE),
            nfsar = sum(non_flu_season_absences, na.rm = TRUE) / sum(non_flu_days, na.rm = TRUE))


#####
# GET DATA BY SCHOOL
#####

# Get data organized
school <- dat %>%
  group_by(year, idJoe) %>%
  summarise(flu_season_absences= sum(flu_season_absences, na.rm = T),
            flu_season_absences_imm = sum(flu_season_absences[which(imm)], na.rm = T),
            flu_season_absences_non_imm = sum(flu_season_absences[which(!imm)], na.rm = T),
            flu_days = sum(flu_days, na.rm = T),
            flu_days_imm = sum(flu_days[which(imm)], na.rm = T),
            flu_days_non_imm = sum(flu_days[which(!imm)], na.rm = T),
            non_flu_season_absences= sum(non_flu_season_absences, na.rm = T),
            non_flu_season_absences_imm= sum(non_flu_season_absences[which(imm)], na.rm = T),
            non_flu_season_absences_non_imm= sum(non_flu_season_absences[which(!imm)], na.rm = T),
            non_flu_days = sum(non_flu_days),
            non_flu_days_imm = sum(non_flu_days[which(imm)], na.rm = T),
            non_flu_days_non_imm = sum(non_flu_days[which(!imm)], na.rm = T),
            imm = sum(imm),
            non_imm = n() - sum(imm),
            students = n(),
            cfr = length(cf[cf == "Yes"]),
            cfn = length(cf[!is.na(cf)]))

# Add type
GooFun <- function(myLink){
  csvLink <- gsub("/pubhtml", "/export?&format=csv", myLink)
  myCsv <- getURL(csvLink)
  df <- read.csv(textConnection(myCsv))
  return(df)
}
link <- GooFun("https://docs.google.com/spreadsheets/d/1sDxHwtNCWSIUVCzb01vu7rmLMXq88diGIbt7PH4Ntrk/pubhtml")

# Merge school to linked info
school$id <- school$idJoe
school <- left_join(x = school,
                    y = link,
                    by = "id")

# Add a teacher/year/grade/school variables
school$merger <- paste0(school$year, school$school)

# Calculate fsar and nfsar
school$school_fsar <- school$flu_season_absences / school$flu_days
school$school_nfsar <- school$non_flu_season_absences / school$non_flu_days

school$school_fsar_imm <- school$flu_season_absences_imm / school$flu_days_imm
school$school_nfsar_imm <- school$non_flu_season_absences_imm / school$non_flu_days_imm

school$school_fsar_non_imm <- school$flu_season_absences_non_imm / school$flu_days_non_imm
school$school_nfsar_non_imm <- school$non_flu_season_absences_non_imm / school$non_flu_days_non_imm

# Calculate immunization rate
school$school_ir <- school$imm / school$students

# Merge school data into individual data
school$merger <- paste0(school$year, school$school)
dat$merger <- paste0(dat$year, dat$school)

# Get a smaller school for merging
school_small <- data.frame(school)
school_small <- school_small[,c("merger", "school_ir")]

dat <- left_join(x = dat,
                 y = school_small,
                 by = "merger")
dat$dummy <- 1

# Remove private schools
school <- school[which(school$pubPriv == "pub"),]

# Add age_group
school$age_group <- school$type

# Remove merger
dat$merger <- NULL
#########################################



#####
# GET IMMUNIZATION RATE BY CLASSROOM
#####

# Get data organized
classroom <- dat %>%
  group_by(year, teacherLastName, grade, schoolName) %>%
  summarise(flu_season_absences= sum(flu_season_absences, na.rm = T),
            flu_season_absences_imm = sum(flu_season_absences[which(imm)], na.rm = T),
            flu_season_absences_non_imm = sum(flu_season_absences[which(!imm)], na.rm = T),
            flu_days = sum(flu_days, na.rm = T),
            flu_days_imm = sum(flu_days[which(imm)], na.rm = T),
            flu_days_non_imm = sum(flu_days[which(!imm)], na.rm = T),
            non_flu_season_absences= sum(non_flu_season_absences, na.rm = T),
            non_flu_season_absences_imm= sum(non_flu_season_absences[which(imm)], na.rm = T),
            non_flu_season_absences_non_imm= sum(non_flu_season_absences[which(!imm)], na.rm = T),
            non_flu_days = sum(non_flu_days),
            non_flu_days_imm = sum(non_flu_days[which(imm)], na.rm = T),
            non_flu_days_non_imm = sum(non_flu_days[which(!imm)], na.rm = T),
            imm = sum(imm),
            non_imm = n() - sum(imm),
            students = n())


# Add a teacher/year/grade/school variables
classroom$merger <- paste0(classroom$year, classroom$teacherLastName,
                           classroom$grade, classroom$schoolName)

# Calculate fsar and nfsar
classroom$classroom_fsar <- classroom$flu_season_absences / classroom$flu_days
classroom$classroom_nfsar <- classroom$non_flu_season_absences / classroom$non_flu_days

classroom$classroom_fsar_imm <- classroom$flu_season_absences_imm / classroom$flu_days_imm
classroom$classroom_nfsar_imm <- classroom$non_flu_season_absences_imm / classroom$non_flu_days_imm

classroom$classroom_fsar_non_imm <- classroom$flu_season_absences_non_imm / classroom$flu_days_non_imm
classroom$classroom_nfsar_non_imm <- classroom$non_flu_season_absences_non_imm / classroom$non_flu_days_non_imm

# Calculate immunization rate
classroom$classroom_ir <- classroom$imm / classroom$students

# Add type
classroom$age_group <- factor(ifelse(grepl("KG|01|02|03|04|05", classroom$grade) == TRUE,
                                     "elem",
                                     ifelse(grepl("06|07|08", classroom$grade) == TRUE,
                                            "mid",
                                            ifelse(grepl("09|10|11|12", classroom$grade) == TRUE,
                                                   "high",
                                                   ifelse(classroom$grade == "PK",
                                                          "PK",
                                                          NA)))))

# Merge classroom data into individual data
dat$merger <- paste0(dat$year, dat$teacherLastName,
                     dat$grade, dat$schoolName)

# Get a smaller classroom for merging
classroom_small <- data.frame(classroom)
classroom_small <- classroom_small[,c("merger", "classroom_ir")]

dat <- left_join(x = dat,
                 y = classroom_small,
                 by = "merger")
dat$dummy <- 1



#####
# RAW MODEL (AS IF EVERYONE WAS UNIMMUNIZED, NO ADJUSTMENT)
#####

# ONLY 2012-13 and 13-14 #####################################
dat <- dat[which(dat$year > 2011),]
my_dat <- dat[which(!dat$imm & dat$year > 2011),]
raw_predicted <- sum(my_dat$flu_season_absences) / sum(my_dat$flu_days)
raw_nfsar <- sum(my_dat$non_flu_season_absences) / sum(my_dat$non_flu_days)
raw_nfsar
raw_predicted

# Add those predictions to all students
dat$raw_predicted_fsar <- raw_predicted

# Calculate predicted flu_season_absences
dat$raw_predicted_flu_season_absences <- dat$raw_predicted_fsar * dat$flu_days

# Expected vs. observed for NON-immunized
my_dat <- dat[which(!dat$imm),]
sum(my_dat$raw_predicted_flu_season_absences - my_dat$flu_season_absences)

# Expected vs. observed for immunized
my_dat <- dat[which(dat$imm),]
avoided <- sum(my_dat$raw_predicted_flu_season_absences - my_dat$flu_season_absences)
avoided

# Avoided absences per student-year
avoided / nrow(my_dat)

# How many immunizations to avoid an absence
1 / (avoided / nrow(my_dat)) 

#####
# BARPLOT
#####
bp_data <- data.frame(dat %>% group_by(imm) %>%
  summarise(fsar = sum(flu_season_absences) / sum(flu_days),
            nfsar = sum(non_flu_season_absences) / sum(non_flu_days)))

bp_data <- t(as.matrix(bp_data[,-1]))
my_bp <- barplot(bp_data, 
        beside = T,
        col = adjustcolor(c("blue", "darkred"), alpha.f = 0.6),
        names.arg = c("Non-immunized", "Immunized"),
        ylim = c(0, max(bp_data)*1.05),
        ylab = "Absenteeism rate")
legend(x = "topright",
       fill = adjustcolor(c("blue", "darkred"), alpha.f = 0.6),
       legend = c("NFSAR", "FSAR"))
abline(h = seq(0,1, 0.01),
       col = adjustcolor("black", alpha.f = 0.2))
box("plot")
text(x = matrix(my_bp), 
     y = matrix(bp_data), 
     pos = 1,
     labels = paste(round(100*matrix(bp_data), digits = 2), "%"))

#####
# FIT MODEL TO PREDICT FLU SEASON ABSENCES (ONLY AMONG NON IMMUNIZED)
#####
adj_fit <- glm(cbind(flu_season_absences, flu_season_presences) ~ 
      ab_non_flu_per, 
    data = dat[which(!dat$imm),],
    family=binomial("logit"))
summary(adj_fit)
exp(coef(adj_fit))

#####
# PREDICT ODDS OF FLU SEASON ABSENCE
#####
dat$predicted_fsar <- predict(adj_fit, 
                              newdata = dat, 
                              type = "response")
# Calculate predicted flu_season_absences
dat$predicted_flu_season_absences <- dat$predicted_fsar * dat$flu_days

# Expected vs. observed for non-immunized
my_dat <- dat[which(!dat$imm),]
sum(my_dat$predicted_flu_season_absences - my_dat$flu_season_absences)

# Expected vs. observed for immunized
my_dat <- dat[which(dat$imm),]
avoided <- sum(my_dat$predicted_flu_season_absences - my_dat$flu_season_absences)
avoided

# Avoided absences per student-year
avoided / nrow(my_dat)

# How many immunizations to avoid an absence
1 / (avoided / nrow(my_dat)) 

#####
# HERD IMMUNITY MODEL
#####
dat$school_ir <- dat$school_ir * 100
dat$classroom_ir <- dat$classroom_ir * 100

plot(x = jitter(dat$school_ir, 100),
     y = jitter(dat$classroom_ir, 100))

plot(x = jitter(dat$school_ir, 100),
     y = jitter(dat$fsar, 100))

plot(x = jitter(dat$classroom_ir, 100),
     y = jitter(dat$fsar, 100))


dat_small <- dat[which(!is.na(dat$school_ir)),]
herd_fit <-glm(cbind(flu_season_absences, flu_season_presences) ~ 
                     ab_non_flu_per +
                     poly(school_ir, 2) +imm, 
                 #I(ir^2) + I(ir^3) + imm,
               family=binomial("logit"),
                   data = dat_small)

summary(herd_fit)
exp(coef(herd_fit))

##
# Make some fake data to visualize herd_fit
##

# imm
fake1 <- data.frame(school_ir = (1:100) ,
                    classroom_ir = (1:100),
                   imm = TRUE,
                   ab_non_flu_per = mean(classroom$classroom_nfsar, na.rm = T))

# nonimm
fake2 <- data.frame(school_ir = (1:100) ,
                    classroom_ir = (1:100),
                    imm = FALSE,
                    ab_non_flu_per = mean(classroom$classroom_nfsar, na.rm = T))

# combine
fake <- rbind(fake1, fake2) ; rm(fake1, fake2)
#fake <- fake2

# predict
fake$predicted <- predict(herd_fit, newdata = fake,
                          type = "response")

# predict confidence


# Plot
plot(fake$school_ir, fake$predicted*100, col = c(rep("blue", 100), rep("red", 100)),
     type = "n",
     xlab = "Classroom immunization rate (%)",
    ylab = "Flu season absenteeism rate (%)")

my_vec <- c(TRUE, FALSE)
my_colors <- c("blue", "red")
for (i in 1:2){
  my_data <- fake[which(fake$imm == my_vec[i]),]
  lines(my_data$school_ir, my_data$predicted*100,
        col = my_colors[i])        
}
legend(x = "topright",
       col = rev(my_colors),
       legend = c("Non imm", "Imm"),
       lty = 1)

#####
# PLOTS FOR EACH HIGH SCHOOL
#####
par(mfrow = c(2,1))

high_schools <- c("BUCHHOLZ, F. W. HIGH SCHOOL",
                  "EASTSIDE HIGH SCHOOL", 
                  "GAINESVILLE HIGH SCHOOL",
                  "LOFTEN, W.T. HIGH SCHOOL",
                  "HAWTHORNE HIGH SCHOOL",
                  "SANTA FE HIGH SCHOOL")

ts_high <- dat %>%
  filter(schoolName %in% high_schools, grade %in% c("09", "10", "11", "12")) %>%
  group_by(year, grade, schoolName) %>%
  summarise(students = n(),
            imm = sum(imm))

# calculate imm rate
ts_high$imm_rate <- ts_high$imm / ts_high$students * 100

# make grade numeric
ts_high$grade <- as.numeric(as.character(ts_high$grade))

# PLOT
plot(x = 2011:2013,
     y = c(0, 0, 50),
     xlab = "Year",
     ylab = "Immunization rate",
     xaxt = "n",
     type = "n")
axis(side = 1, 
     at = 2011:2013,
     labels = paste0(2011:2013, "-", 2012:2014, ""),
     tick = FALSE)

# grids
abline(h = seq(0,100,10),
       col = adjustcolor("black", alpha.f = 0.2))

# lines
my_colors <- c("darkgreen", "darkblue", "darkred", "purple")
my_colors <- (adjustcolor(my_colors, alpha.f = 0.9))
my_lines <- c(2, 4:6)
my_points <- c(0:2,15:17)
# for (i in 1:6){
#   for (j in 1:4){
#     
#     my_data <- ts_high[which(ts_high$schoolName ==unique(sort(ts_high$schoolName))[i] &
#                                ts_high$grade == c(9:12)[j]),]
#     lines(my_data$year, my_data$imm_rate,
#           col = my_colors[j],
#           lty = my_lines[i],
#           lwd = 2)
#     points(my_data$year, my_data$imm_rate,
#            col = my_colors[j],
#            pch = my_points[i],
#            cex = 2)
#   }
# 
# }
# 
# # legend
# legend (x = "bottomright",
#         lty = my_lines,
#         pch = my_points,
#         col = my_colors,
#         legend = paste0(9:12, "th grade"))
# 
# # title
# title(main = "Immunization rates among students enrolled in public high schools")

# INDIVIDUAL PLOTS
pdf(file = paste0(public, "/reports/high_schools_by_grade.pdf"))
par(mfrow = c(1,1))
for (i in 1:6){
    
    school_name <- unique(sort(ts_high$schoolName))[i]
  
    my_data <- ts_high[which(ts_high$schoolName == school_name),]
    
    # PLOT
    plot(x = 2011:2013,
         y = c(0, 0, 50),
         xlab = "Year",
         ylab = "Immunization rate",
         xaxt = "n",
         type = "n")
    axis(side = 1, 
         at = 2011:2013,
         labels = paste0(2011:2013, "-", 2012:2014, ""),
         tick = FALSE)
    
    # grids
    abline(h = seq(0,100,10),
           col = adjustcolor("black", alpha.f = 0.2))
    
    # Line for mean school
    school_overall <- my_data %>% 
      group_by(year) %>%
      summarise(imm_rate = weighted.mean(imm_rate, students, na.rm = T))
    
    # Add line
    lines(school_overall$year, school_overall$imm_rate,
          col = adjustcolor("black", alpha.f = 0.6),
          lty = 1,
          lwd = 2)
    points(school_overall$year, school_overall$imm_rate,
           col = adjustcolor("black", alpha.f = 0.6),
           pch= 8)          
    legend(x = "topleft",
           lty = 1,
           pch = 8, 
           col = adjustcolor("black", alpha.f = 0.6),
           legend = "School-wide rate")
    
    for (j in 1:4){
      
      my_grade <- my_data[which(my_data$grade == c(9:12)[j]),]
      
    
    lines(my_grade$year, my_grade$imm_rate,
          col = my_colors[j],
          lty = my_lines[j],
          lwd = 2)
    points(my_grade$year, my_grade$imm_rate,
           col = my_colors[j],
           pch = my_points[j],
           cex = 1)
  }
  
  title(main = school_name)
  # legend
  legend (x = "bottomright",
          lty = my_lines,
          pch = my_points,
          col = my_colors,
          legend = paste0(9:12, "th grade"))
}
dev.off()


#####
# HIGH SCHOOL IMMUNIZATION BY GRADE AND YEAR
#####
par(mfrow = c(2,1))

ts_high <- dat %>%
  filter(grade %in% c("09", "10", "11", "12")) %>%
  group_by(year, grade) %>%
  summarise(students = n(),
            imm = sum(imm))

# calculate imm rate
ts_high$imm_rate <- ts_high$imm / ts_high$students * 100

# make grade numeric
ts_high$grade <- as.numeric(as.character(ts_high$grade))

# PLOT
plot(x = 2011:2013,
     y = c(0, 0, 40),
     xlab = "Year",
     ylab = "Immunization rate",
     xaxt = "n",
     type = "n")
axis(side = 1, 
     at = 2011:2013,
     labels = paste0(2011:2013, "-", 2012:2014, ""),
     tick = FALSE)

# grids
abline(h = seq(0,100,10),
       col = adjustcolor("black", alpha.f = 0.2))

# lines
my_colors <- c("darkgreen", "darkblue", "darkred", "black")
my_lines <- 2:5
my_points <- 15:18
for (i in 1:4){
  my_data <- ts_high[which(ts_high$grade == c(9:12)[i]),]
  lines(my_data$year, my_data$imm_rate,
        col = my_colors[i],
        lty = my_lines[i],
        lwd = 2)
  points(my_data$year, my_data$imm_rate,
        col = my_colors[i],
        pch = my_points[i],
        cex = 2)
}

# legend
legend (x = "bottomright",
        lty = my_lines,
        pch = my_points,
        col = my_colors,
        legend = paste0(9:12, "th grade"))

# title
title(main = "Immunization rates among students enrolled in public high schools")

#####
# ALL GRADES IMMUNIZATION 
#####
ts_all <- dat %>%
  group_by(year, age_group) %>%
  summarise(students = n(),
            imm = sum(imm))

# calculate imm rate
ts_all$imm_rate <- ts_all$imm / ts_all$students * 100

# PLOT
plot(x = 2011:2013,
     y = c(0, 0, 50),
     xlab = "Year",
     ylab = "Immunization rate",
     xaxt = "n",
     type = "n")
axis(side = 1, 
     at = 2011:2013,
     labels = paste0(2011:2013, "-", 2012:2014, ""),
     tick = FALSE)

# grids
abline(h = seq(0,100,10),
       col = adjustcolor("black", alpha.f = 0.2))

# lines
my_colors <- c("darkgreen", "darkblue", "darkred")
my_lines <- 2:4
my_points <- 15:17
for (i in 1:3){
  my_data <- ts_all[which(ts_all$age_group == c("elem", "mid", "high")[i]),]
  lines(my_data$year, my_data$imm_rate,
        col = my_colors[i],
        lty = my_lines[i],
        lwd = 2)
  points(my_data$year, my_data$imm_rate,
         col = my_colors[i],
         pch = my_points[i],
         cex = 2)
}

# legend
legend (x = "bottomright",
        lty = my_lines,
        pch = my_points,
        col = my_colors,
        legend = c("Elementary", "Middle", "High"))

# title
title(main = "Immunization rates among students enrolled in public schools")

# Draw secular trend line
for (i in 1:3){
  my_data <- ts_all[which(ts_all$age_group == c("elem", "mid", "high")[i] &
                            ts_all$year != 2013),]
  my_fit <- lm(my_data$imm_rate ~ my_data$year)
  abline(my_fit, col = adjustcolor(my_colors[i], alpha.f = 0.4))
}

lines(x = c(2011.3, 2011.5),
      y = c(4, 8),
      col = adjustcolor("black", alpha.f = 0.4))
text(x = 2011.5,
     y = 6,
     labels = "Semi-transparent lines:\n2011/12 - 2012/13 trend",
     pos = 4,
     cex = 0.6)
par(mfrow = c(1,1))


#####
# CFRR BY RACE
#####
# Quck model to see likelihood of cf by race
dat$race <- factor(dat$race,
                   levels = c("White", "Asian","Black", "Hispanic", "Multiracial", "Native Amer."))
x <- glm(cf == "Yes" ~ race,
         data = dat,
         family = binomial("logit"))
summary(x)
exp(coef(x))

race_table <- dat %>%
  filter(!is.na(intern)) %>%
  group_by(race) %>%
  summarise(cf_returned = sum(cf == "Yes", na.rm = TRUE),
            cf_not_returned = sum(cf == "No", na.rm = TRUE),
            cf_denom = sum(cf %in% c("Yes", "No")),
            imm = sum(v == "Yes", na.rm = TRUE),
            non_imm = sum(v == "No", na.rm = TRUE))
race_table$cf_returned_p <- race_table$cf_returned / race_table$cf_denom * 100
race_table <- race_table[order(race_table$cf_returned_p),]

barplot(race_table$cf_returned_p,
        names.arg = race_table$race,
        ylab = "Consent form return rate",
        xlab = "Race",
        density = 50,
        col = adjustcolor("lightblue", alpha.f = 0.6),
        border = NA,
        ylim = c(0, max(race_table$cf_returned_p) * 1.1))
abline(h = seq(0,100,10),
       col = adjustcolor("black", alpha.f = 0.2))
box("plot")

#####
# SET SEED
#####
set.seed(1)

# #####
# # RAW NUMBERS BY YEAR
# #####
# sum(dat$ab_flu_per[which(dat$year == 2012)] * 
#       dat$flu_days[which(dat$year == 2012)]) / 
#   sum(dat$flu_days[which(dat$year == 2012)])
# 
# sum(dat$ab_flu_per[which(dat$year == 2011 & !dat$imm)] * 
#       dat$flu_days[which(dat$year == 2011 & !dat$imm)]) / 
#   sum(dat$flu_days[which(dat$year == 2011 & !dat$imm)])
# 
# sum(dat$flu_season_absences[which(!dat$imm & dat$year == 2011)])
# sum(dat$flu_season_absences[which(!dat$imm & dat$year == 2012)])
# sum(dat$flu_season_absences[which(!dat$imm & dat$year == 2013)])
# 
# nrow(dat[which(dat$year == 2013 & dat$imm),]) * # number of students
# sum(dat$ab_flu_per[which(dat$year == 2013 & !dat$imm)] *  #fsar
#       dat$flu_days[which(dat$year == 2013 & !dat$imm)]) / #fsar
#   sum(dat$flu_days[which(dat$year == 2013 & !dat$imm)]) *  #fsar
#   mean(dat$flu_days[which(dat$year == 2013 & dat$imm)][1]) #flu days
# 
# # Remove 2010 and 2014 from county_wide
# county_wide <- county_wide[which(county_wide$year != 2010 &
#                                    county_wide$year != 2014),]
# 
# ts <- county_wide %>%
#   group_by(year, age_group, admin) %>%
#   summarise(imm = sum(imm),
#             denom = mean(denom))
#   
# ts_fun <- function( age_group = "elem",
#                      admin = c("PR", "PU"),
#                     data = ts){
#   data <- data[which(data$age_group == age_group &
#                      data$admin %in% admin),]
#   
#   
#   data <- data %>%
#     group_by(year) %>%
#     summarise(imm = sum(imm),
#               denom = mean(denom))
#   
#   data$p <- data$imm / data$denom * 100
#   return(data)
# }
# 
# plot_ts <- function(age_group = "elem",
#                     col1 = "lightblue",
#                     col2 = adjustcolor("darkorange", alpha.f = 0.6),
#                     border = adjustcolor("black", alpha.f = 0.7)){
#   
#   # Plot overall for that age group
#   df <- ts_fun(age_group)
#   vals <- df$p
#   bp <- barplot(vals,
#                 ylim = c(0, max(vals) * 1.1),
#                 names.arg = 2011:2013,
#                 ylab = "Immunization rate",
#                 col = col2,
#                 border = border)
#   
#   legend(x = "topleft",
#          legend = c("CHD", "Private"),
#          fill = c(col1, col2),
#          bty = "n",
#          border = border)
#   
#   text(bp[,1],
#        y = vals,
#        pos = 3,
#        labels = paste0(round(vals, digits = 2), "%  ",
#                        "(", df$imm, " immunizations)"),
#        cex = 0.7)
#   
#   # Add private only
#   df <- ts_fun(age_group, "PR")
#   priv <- df$p
#   text(bp[,1],
#        y = vals,
#        pos = 1,
#        labels = paste0(round(priv, digits = 2), "%  ",
#                        "(", df$imm, " immunizations)"),
#        cex = 0.7)
#   
#   # Add just public for that age group
#   df <- ts_fun(age_group, "PU")
#   vals <- df$p
#   bp <- barplot(vals, add = TRUE, col = col1,
#                 border = border)
#   text(bp[,1],
#        y = vals,
#        pos = 1,
#        labels = paste0(round(vals, digits = 2), "%  ",
#                        "(", df$imm, " immunizations)"),
#        cex = 0.7)
#   box("plot")
# }
# 
# plot_ts("PK")
# title(main = "Pre-K")
# plot_ts("elem")
# title(main = "Elementary")
# 
# plot_ts("mid")
# title(main = "Middle")
# 
# plot_ts("high")
# title(main = "High")
# 
# 
# 
# plot_ts <- function(age_group = "elem",
#                      admin = c("PR", "PU"),
#                      data = ts,
#                     col,
#                     pch, add = FALSE){
#   x <- ts_fun(age_group = age_group,
#               admin = admin,
#               data = data)
#   
#   if(!add){
#     plot(x$year)
#   }
#   
# }
# 
# #####
# # WRITE FUNCTION FOR PLOTTING COUNTY_WIDE DATA
# #####
# 
# # First, clean up county_wide a bit
# county_wide$LAIV <- NULL
# county_wide$TIV <- NULL
# county_wide$LAIVp <- NULL
# county_wide$TIVp <- NULL
# 
# CountyFun <- function(data = county_wide,
#                       age_group = c("elem", "mid", "high", "PK"),
#                       year = c(2010, 2011, 2012, 2013, 2014),
#                       type = c("LAIV", "TIV"),
#                       admin = c("PR", "PU")){
#   
#   # Subset to parameters
#   data <- data[which(data$age_group %in% age_group &
#                     data$year %in% year &
#                     data$type %in% type &
#                     data$admin %in% admin),]
#   
#   #
#   return(data)
# 
# }
# 
# x <- CountyFun(age_group = "elem",
#           type = "LAIV",
#           admin = "PU")
# plot(x$year, x$imm)
# CountyFun

#####
# GET UNIQUE CLASSROOMS
#####
dat$classroom <- paste(dat$teacherLastName,
                       dat$schoolName)
#####
# ANALYZE ABSENTEEISM BY CLASSROMM
#####

# Get data organized
classroom <- dat %>%
  group_by(year, teacherLastName, grade, schoolName) %>%
  summarise(flu_season_absences= sum(flu_season_absences, na.rm = T),
            flu_season_absences_imm = sum(flu_season_absences[which(imm)], na.rm = T),
            flu_season_absences_non_imm = sum(flu_season_absences[which(!imm)], na.rm = T),
            flu_days = sum(flu_days, na.rm = T),
            flu_days_imm = sum(flu_days[which(imm)], na.rm = T),
            flu_days_non_imm = sum(flu_days[which(!imm)], na.rm = T),
            non_flu_season_absences= sum(non_flu_season_absences, na.rm = T),
            non_flu_season_absences_imm= sum(non_flu_season_absences[which(imm)], na.rm = T),
            non_flu_season_absences_non_imm= sum(non_flu_season_absences[which(!imm)], na.rm = T),
            non_flu_days = sum(non_flu_days),
            non_flu_days_imm = sum(non_flu_days[which(imm)], na.rm = T),
            non_flu_days_non_imm = sum(non_flu_days[which(!imm)], na.rm = T),
            imm = sum(imm),
            non_imm = n() - sum(imm),
            students = n())


# Add a teacher/year/grade/school variables
classroom$merger <- paste0(classroom$year, classroom$teacherLastName,
                           classroom$grade, classroom$schoolName)

# Calculate fsar and nfsar
classroom$fsar <- classroom$flu_season_absences / classroom$flu_days
classroom$nfsar <- classroom$non_flu_season_absences / classroom$non_flu_days

classroom$fsar_imm <- classroom$flu_season_absences_imm / classroom$flu_days_imm
classroom$nfsar_imm <- classroom$non_flu_season_absences_imm / classroom$non_flu_days_imm

classroom$fsar_non_imm <- classroom$flu_season_absences_non_imm / classroom$flu_days_non_imm
classroom$nfsar_non_imm <- classroom$non_flu_season_absences_non_imm / classroom$non_flu_days_non_imm

# Calculate immunization rate
classroom$ir <- classroom$imm / classroom$students

# Add type
classroom$age_group <- factor(ifelse(grepl("KG|01|02|03|04|05", classroom$grade) == TRUE,
                                     "elem",
                                     ifelse(grepl("06|07|08", classroom$grade) == TRUE,
                                            "mid",
                                            ifelse(grepl("09|10|11|12", classroom$grade) == TRUE,
                                                   "high",
                                                   ifelse(classroom$grade == "PK",
                                                          "PK",
                                                          NA)))))



# Define plot function
PlotFun <- function(age_group = "elem", 
                    year = 2013,
                    size = 1,
                    model = TRUE,
                    too_high = 33,
                    too_low = 4,
                    include_legend = TRUE,
                    flu = TRUE, 
                    data = classroom,
                    legend_sizes = c(5,15,25),
                    add = FALSE,
                    col = "black",
                    line_col = "darkred",
                    imm = "both",
                    jitter_x = TRUE,
                    jitter_factor = 2){# or imm or non_imm
  
  
  # Subset according to conditions
  temp <- data.frame(data)
  sub_data <- temp[which(temp[,"age_group"] %in% age_group &
                           temp[,"year"] %in% year),]
  
  # Remove classes over designated limit
  if (!is.null(too_high)){
    sub_data <- sub_data[which(sub_data$students < too_high),]
  }
  
  # Remove classes under designated limit
  if (!is.null(too_low)){
    sub_data <- sub_data[which(sub_data$students > too_low),]
  }
  
  # Make sub_data a regular df
  sub_data <- data.frame(sub_data)
  
  # FSAR or NFSAR
  if(flu){
    if(imm == "both"){
      y = sub_data[,"fsar"]
    } else{
      y = sub_data[,paste0("fsar_", imm)]
    }
  } else {
    if(imm == "both"){
      y = sub_data[,"nfsar"]
    } else{
      y = sub_data[,paste0("nfsar_", imm)]
    }
  }
  
  # Jitter or not
  if(jitter_x){
    x <- jitter(sub_data$ir, jitter_factor)
  } else {
    x <- sub_data$ir
  }
  
  # Plot
  if (!add){
    plot(x = x,
         y = y,
         pch = 16,
         cex = size * sub_data$students/15,
         col = adjustcolor(col, alpha.f = 0.5),
         xlab = "Immunization rate",
         ylab = "Absenteeism rate",
         main = paste(age_group, "|", year, "|", 
                      ifelse(flu,
                             "flu season absenteeism rate",
                             "non-flu season absenteeism rate"),
                      "|", ifelse(imm == "imm", "immunized students", "non-immunized students")),
         ylim = c(0,0.2),
         cex.main = 0.6)
  } else{
    points(x = x,
         y = y,
         pch = 16,
         cex = size * sub_data$students/15,
         col = adjustcolor(col, alpha.f = 0.5))
  }

  
  # Model
  if(model){
    my_model <- lm(y ~ ir, 
                   data = sub_data, 
                   weights = students
    )
    abline(my_model, 
           col = adjustcolor(line_col, alpha.f = 0.6),
           lwd = 2.5)
  }
  
  # Legend
  if(include_legend){
    
    legend(x = "top",
           pch = 16,
           col = adjustcolor(col, alpha.f = 0.5),
           pt.cex = legend_sizes * size / 15,
           legend = legend_sizes,
           title = "Number of students",
           bty = "n",
           cex = 0.6,
           y.intersp = 1)
  }
  
  # Gridlines
  abline(h = seq(0,0.5, 0.05),
         col = adjustcolor("black", alpha.f = 0.3),
         lty = 3)
}

#####
# FSAR AND NFSAR - BY CLASSROOM AND IMM STATUS
#####
####################
par(mfrow = c(3,4))
for (i in c("elem", "mid", "high")){
  for (k in 2011:2013){
  for (j in c(TRUE, FALSE)){
      PlotFun(age_group = i,
              flu = j,
              year = k,
              imm = "imm",
              col = "blue",
              line_col = "blue")
      
      PlotFun(age_group = i,
              flu = j,
              year = k,
              imm = "non_imm",
              col = "red",
              line_col = "red")

    }
  }
}
####################
# Model 
non_imm_fsar <- classroom$fsar_non_imm*100
imm_rate <- classroom$ir*100
age_group <- classroom$age_group
non_imm_nfsar <- classroom$nfsar_non_imm * 100
fit <- lm(non_imm_fsar ~
            imm_rate + age_group + non_imm_nfsar)

summary(fit)

par(mfrow = c(2,3))
for (i in c("elem", "mid", "high")){
  for (j in c(TRUE, FALSE)){
    for (k in 2011:2013){
      
      PlotFun(age_group = i, 
              flu = j,
              year = k)
    }
  }
}

#####
# FSAR AND NFSAR - BY SCHOOL
#####

par(mfrow = c(2,3))
for (i in c("elem", "mid", "high")){
  for (j in c(TRUE, FALSE)){
    for (k in 2011:2013){
      
      PlotFun(age_group = i, 
              flu = j,
              year = k,
              data = school,
              too_high = NULL,
              too_low = NULL,
              size = 0.04,
              legend_sizes = c(50, 250, 1000))
    }
  }
}



#####
# FSAR ==== ELEM, MIDDLE, HIGH - BY CLASSROOM
#####
par(mfrow = c(3,3))
for (i in c("elem", "mid", "high")){
  for(j in 2011:2013){
    PlotFun(age_group = i, 
            year = j)
  }
}

#####
# NFSAR ==== ELEM, MIDDLE, HIGH - BY CLASSROOM
#####
par(mfrow = c(3,3))
for (i in c("elem", "mid", "high")){
  for(j in 2011:2013){
    PlotFun(age_group = i, 
            year = j,
            flu = FALSE)
  }
}

#####
# JUST 2013 ELEMENTARY
#####
par(mfrow = c(1,1))

PlotFun(age_group = "elem",
        year = 2013,
        flu = TRUE,
        col = "darkred")
PlotFun(age_group = "elem",
        year = 2013,
        flu = FALSE,
        add = TRUE,
        col = "darkblue",
        line_col = "darkblue")

legend(x = "topright",
       col = adjustcolor(c("darkred", "darkblue"), alpha.f = 0.5),
       pch = 16,
       lty = 1,
       legend = c("Flu season", "Non-flu season"))

#####
# REGRESSION ON NFSAR AND FSAR BY SCHOOL
#####
elem13 <- classroom[which(classroom$year == 2013 & classroom$age_group == "elem"),]

# separate out fsar and nfsar
elem13fsar <- elem13
elem13fsar$value <- elem13fsar$fsar
elem13fsar$season <- "flu"

elem13nfsar <- elem13
elem13nfsar$value <- elem13nfsar$nfsar
elem13nfsar$season <- "non-flu"

elem13 <- rbind(elem13fsar, elem13nfsar)

fit <- lm(value ~ season*ir, data = elem13)
summary(fit)

## odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit), confint(fit)))
x
x <- data.frame(x)
x <- x[-1,]
my_barplot <- barplot(x$OR,
                      names.arg = row.names(x),
                      col = "darkorange",
                      ylim = c(0,max(x$OR)*1.2),
                      border = NA,
                      cex.names = 0.4,
                      las = 3)
box("plot")
abline(h = 1, col = adjustcolor("black", alpha.f = 0.7))
errbar(x = my_barplot[,1],
       y = x$OR,
       yminus = x$X2.5..,
       yplus = x$X97.5..,
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor("darkblue", alpha.f = 0.8))

### PLOT FOR PARKER
elem13$col <- ifelse(elem13$season == "flu", "red", "blue")
plot(elem13$ir, elem13$value,
     col = elem13$col)

####################################



#####
# BASIC SUMMARY STATISTICS
#####
prop.table(table(dat$imm, dat$age_group), 2)

# NUMBER OF STUDENTS BY AGE_GROUP BY YEAR
table(dat$age_group, dat$year, dat$type)

# IMM RATE BY YEAR
table(dat$imm, dat$year)
x <- table(dat$imm, dat$year, dat$age_group)
x
prop.table(x, 2)

# FLU SEASON ABSENCES BY YEAR / AGE GROUP / VACCINE TYPE

# Define function for comparing two groups
PropFun <- function(year, type1, type2, age_group){
  i1j1 <- sum(dat$flu_season_absences[which(dat$year == year &
                                              dat$type == type1 &
                                              dat$age_group == age_group)])
  i2j1 <- sum(dat$flu_days[which(dat$year == year &
                                   dat$type == type1 &
                                   dat$age_group == age_group)])
  i1j2 <- sum(dat$flu_season_absences[which(dat$year == year &
                                              dat$type == type2 &
                                              dat$age_group == age_group)])
  i2j2 <- sum(dat$flu_days[which(dat$year == year &
                                   dat$type == type2 &
                                   dat$age_group == age_group)])
  
  
  mat <- matrix(c(i1j1, i1j2, i2j1 , i2j2 ), byrow = FALSE, ncol = 2)
  tab <- as.table(mat)
  
  prop.test(tab)
}

# 2011 young
PropFun(year = 2011,
        type1 = "TIV",
        type2 = "LAIV",
        age_group = "old")

i1j1 <- sum(dat$flu_season_absences[which(dat$year == 2013 &
                                            dat$type == "NONE" &
                                            dat$age_group == "young")])
i2j1 <- sum(dat$flu_days[which(dat$year == 2013 &
                                 dat$type == "NONE" &
                                 dat$age_group == "young")])
i1j2 <- sum(dat$flu_season_absences[which(dat$year == 2013 &
                                            dat$type == "LAIV" &
                                            dat$age_group == "young")])
i2j2 <- sum(dat$flu_days[which(dat$year == 2013 &
                                 dat$type == "LAIV" &
                                 dat$age_group == "young")])


mat <- matrix(c(i1j1, i1j2, i2j1 , i2j2 ), byrow = FALSE, ncol = 2)
tab <- as.table(mat)

prop.test(tab)

Xsq <- chisq.test(mat)
Xsq$observed
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
#


# year
summary_table <- data.frame("year" = 2011:2013)

# flu_season_absences
summary_table$flu_season_absences <- NA
for (i in 2011:2013){
  summary_table$flu_season_absences[which(summary_table$year == i)] <-
    sum(dat$flu_season_absences[which(dat$year == i)])
}

# flu_days
summary_table$flu_days <- NA
for (i in 2011:2013){
  summary_table$flu_days[which(summary_table$year == i)] <-
    sum(dat$flu_days[which(dat$year == i)])
}

# by vaccine type - flu season_absences and flu_days
for (i in levels(dat$type)){
  for (j in levels(dat$age_group)){
    summary_table[,paste0("flu_season_absences_",i, "_", j)] <- NA
    summary_table[,paste0("flu_days_",i, "_", j)] <- NA
  }
}

for (i in levels(dat$type)){
  for (j in 2011:2013){
    for (k in levels(dat$age_group)){
      
      summary_table[which(summary_table$year == j), 
                    paste0("flu_season_absences_", i, "_", k)] <-
        sum(dat[which(dat$year == j &
                        dat$type == i &
                        dat$age_group == k), "flu_season_absences"])
      
      summary_table[which(summary_table$year == j), 
                    paste0("flu_days_", i, "_", k)] <-
        sum(dat[which(dat$year == j &
                        dat$type == i &
                        dat$age_group == k), "flu_days"])
      
    }
  }
}

#####
# MAKING PROPORTIONAL COLUMNS
#####
summary_table <- summary_table %>%
  mutate(fsar = flu_season_absences / flu_days,
         fsar_NONE_old = flu_season_absences_NONE_old / flu_days_NONE_old,
         fsar_NONE_young = flu_season_absences_NONE_young / flu_days_NONE_young,         
         fsar_LAIV_old =  flu_season_absences_LAIV_old / flu_days_LAIV_old,
         fsar_LAIV_young =  flu_season_absences_LAIV_young / flu_days_LAIV_young,
         fsar_TIV_old =  flu_season_absences_TIV_old / flu_days_TIV_old,
         fsar_TIV_young =  flu_season_absences_TIV_young / flu_days_TIV_young)

#####
# TIME SERIES PLOT
#####

# Define some colors and shapes
TIV_color <- adjustcolor("darkorange", alpha.f = 0.6)
NONE_color <- adjustcolor("red", alpha.f = 0.6)
LAIV_color <- adjustcolor("blue", alpha.f = 0.6)
young_pch <- 16
old_pch <- 2
young_lty <- 1
old_lty <-2

# main plot outline
plot(summary_table$year,
     c(0,0.5,1),
     type = "n",
     xaxt = "n",
     xlab = "Year",
     ylab = "Flu season absenteeism rate",
     ylim = c(0.035,0.085))

# x axis
axis(side = 1,
     at = 2011:2013,
     labels = paste0(2011:2013,"-",12:14))

# LINES
LineFun <- function(age_group, type, col, pch, lty, size = 2){
  lines(summary_table$year,
        summary_table[,paste0("fsar", "_", type, "_", age_group)],
        col = col,
        lty = lty,
        lwd = size)
  points(summary_table$year,
         summary_table[,paste0("fsar", "_", type, "_", age_group)],
         col = col,
         pch = pch,
         cex = size)
}

# fsar_NONE_old
LineFun("old", "NONE", pch = old_pch, col = NONE_color, lty = old_lty)

# fsar_NONE_young
LineFun("young", "NONE", pch = young_pch, col = NONE_color, lty = young_lty)

# fsar_LAIV_old
LineFun("old", "LAIV", pch = old_pch, col = LAIV_color, lty = old_lty)

# fsar_LAIV_young
LineFun("young", "LAIV", pch = young_pch, col = LAIV_color, lty = young_lty)

# fsar_TIV_old
LineFun("old", "TIV", pch = old_pch, col = TIV_color, lty = old_lty)

# fsar_TIV_young
LineFun("young", "TIV", pch = young_pch, col = TIV_color, lty = young_lty)

abline(h = seq(0,0.1,0.01), col = adjustcolor("grey", alpha.f = 0.2))

legend(x = "topleft",
       #bty = "n",
       border = FALSE,
       lty = c(young_lty, young_lty, young_lty, old_lty, old_lty, old_lty),
       pch = c(young_pch, young_pch, young_pch, old_pch, old_pch, old_pch),
       lwd = 2, pt.cex = 2, 
       col = c(TIV_color, NONE_color, LAIV_color,  
               TIV_color, NONE_color, LAIV_color),
       legend = c("TIV 5-8",
                  "No vaccine 5-8",
                  "LAIV 5-8",
                  "TIV 9-18",
                  "No vaccine 9-18",
                  "LAIV 9-18"), ncol = 2)


########################
# MODELS
########################

# COMMENT ON MODEL CHOICE
#Given that our outcome is basically just a compilation of binary observations
#(absent or non-absent), we should use logistic regression, weighting our outcome
#by the number of observations.  In R, the way to do this is glm() using the binomial family
#in which our dependent outcome is a two column matrix in which the first
#column is the number of success and the second is the number of failures
#In other words, our outcome is the matrix of dat$abFlu, dat$presFlu
#see here: http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html

#LOGIT TRANSFORMATION OR MIXTURE MODEL
#http://www.esajournals.org/doi/full/10.1890/10-0340.1
#http://www.theanalysisfactor.com/when-dependent-variables-are-not-fit-for-glm-now-what/
#beta ? http://stats.stackexchange.com/questions/29038/regression-for-an-outcome-ratio-between-0-and-1
# http://www.theanalysisfactor.com/proportions-as-dependent-variable-in-regression-which-type-of-model/


#####
# LOGIT MODEL : immunization type
#####

# Merge classroom data into individual data
dat$merger <- paste0(dat$year, dat$teacherLastName,
                           dat$grade, dat$schoolName)

dat <- left_join(x = dat,
                 y = classroom,
                 by = "merger")

# Categorical imm rate of classroom
dat$class_imm <- cut(dat$ir, breaks = c(0, 0.25, 0.5, 0.75, 1))

indirect_protection <- 
  glm(cbind(flu_season_absences.x, flu_season_presences) ~ 
        ab_non_flu_per + class_imm, 
      data = dat[which(dat$age_group.y == "elem"  &!dat$imm.x),],
      family=binomial("logit"))
summary(indirect_protection)
exp(coef(indirect_protection))

# Plot for Parker
x = dat$ir[which(dat$age_group.x == "elem")] * 100
xj = jitter(x, factor = 200) 
y = dat$fsar[which(dat$age_group.x == "elem")] * 100
xy <- cbind(xj,y)
plot(xy,
     col = adjustcolor("darkgreen", alpha.f = 0.05),
     pch = 16,
     xlab = "Classroom immunization rate (jittered)",
     ylab = "Flu season absenteeism rate",
     cex = 0.75,
     main = "Non-immunized elementary students, all years")
fit <- lm(y~x)
abline(fit, col = "red")

abline(h = seq(0, 100, 5), col = adjustcolor("black", alpha.f = 0.1))
abline(v = seq(0, 100, 10), col = adjustcolor("black", alpha.f = 0.1))

#ADD LOESS LINE
lox <- x
loy <- y
lw1 <- loess(loy ~ lox, span=0.2)
j <- order(lox)
lines(lox[j],lw1$fitted[j],col="blue", lty=1)

# LEGEND
legend("top",
       lty = 1,
       col = c("blue", "red"),
       legend = c("Local regression", "Linear regression"),
       bty = "n")

# Break up by deciles
x = dat$ir[which(dat$age_group.x == "elem")] * 100
x <- cut(x, breaks = seq(0,100,10), include.lowest = TRUE, right = FALSE)
xy <- data.frame(x,y)

xyz <- xy %>%
  group_by(x) %>%
  summarise(fsar = mean(y))

head(xyz)
barplot(xyz$fsar,
        names.arg = xyz$x,
       # las = 3,
        xlab = "Classroom immunization rate decile",
        ylab = "Flu season absenteeism rate",
        main = "Non-immunized elementary students (all years)",
        border = NA,
        cex.names = 0.7,
        col = adjustcolor("darkred", alpha.f = 0.6))
box("plot")
abline(h = 0:10, col = adjustcolor("black", alpha.f = 0.3))

mod1 <- glm(cbind(flu_season_absences, flu_season_presences) ~ 
              type + ab_non_flu_per + lunch, 
            data = dat[which(dat$age_group == "old" & dat$year == 2013),],
            family=binomial("logit"))
summary(mod1)

## odds ratios and 95% CI
x <- exp(cbind(OR = coef(mod1), confint(mod1)))
x
x <- data.frame(x)
x <- x[-1,]
my_barplot <- barplot(x$OR,
                      names.arg = row.names(x),
                      col = "darkorange",
                      ylim = c(0,max(x$OR)*1.2),
                      border = NA,
                      cex.names = 0.4,
                      las = 3)
box("plot")
abline(h = 1, col = adjustcolor("black", alpha.f = 0.7))
errbar(x = my_barplot[,1],
       y = x$OR,
       yminus = x$X2.5..,
       yplus = x$X97.5..,
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor("darkblue", alpha.f = 0.8))
