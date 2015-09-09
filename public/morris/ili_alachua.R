setwd('/home/joebrew/Documents/fdoh/public/morris/')
df <- read.csv('ili_alachua.csv')

# format date
df$date <- as.Date(df$Date, '%d%b%y')

# plot
png('updated_figure.png', width = 650, height = 550)
plot(df$date, df$Data, 
     pch = 19,
     cex = 1,
     col = adjustcolor('blue', alpha.f = 0.4),
     xaxt = 'n',
     ylab = 'Cases',
     main = 'Cases of ED ILI among Alachua residents at any Florida ESSENCE facility',
     xlab = 'Date',
     xlim = c(as.Date('2011-01-01'), max(df$date)),
     type = 'n')
# Add grey background
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = adjustcolor("gray",alpha.f = 0.2))

# Add gridlines
abline(h = seq(0, 100, 5),
       col = adjustcolor("white", alpha.f = 0.9))
abline(v = as.Date(paste0(seq(2010, 2015, 1), "-01-01"), format = "%Y-%m-%d"),
       col = adjustcolor("white", alpha.f = 0.9), lwd = 3)

points(df$date, df$Data, 
     pch = 19,
     cex = 1,
     col = adjustcolor('blue', alpha.f = 0.4))

lines(df$date, df$Data,
      col = adjustcolor('blue', alpha.f = 0.3),
      lwd = 1)

#axis
axis(side = 1,
     at = as.Date(paste0(2010:2015, '-01-01')),
     labels = paste0('Jan 1,\n',2010:2015),
     tick = TRUE,
     cex.axis = 0.75)
     
date_vec <- as.Date(paste0(2010:2015, rep(c('-04-01', '-07-01', '-10-01'), each = length(2010:2015))))
axis(side = 1,
     at = date_vec,
     labels = gsub(' ', '\n', format(date_vec, format = '%b %d')),
     tick = FALSE,
     cex.axis = 0.5)

# Rolling average
df$rolling <- NA
for (i in 1:nrow(df)){
  date <- df$date[i]
  df$rolling[i] <- mean(df$Data[which(df$date >= (date-6) &
                                        df$date <= date)])
}
lines(df$date, df$rolling, col=adjustcolor("red", alpha.f=0.6),
      lwd=3)

#Legend
legend('topleft',
       pch = c(19, NA),
       lty = c(1, 1),
       lwd = c(1, 3),
       col = adjustcolor(c('blue', 'red'), alpha.f = 0.6),
       legend = c('Daily observation', 'Rolling 7 day average'))

# # loss line
# lw1 <- loess(df$Data ~ as.numeric(df$date), span = 0.1)
# ord <- order(df$date)
# lines(df$date[ord], lw1$fitted[ord], col=adjustcolor("red", alpha.f=0.8),
#       lwd=3)

dev.off()


#####
# DEFINE FLU SEASONS
#####
df$flu <- 
  df$date >= '2010-10-03' & df$date <= '2011-05-21' |
  df$date >= '2011-10-02' & df$date <= '2012-05-19' |
  df$date >= '2012-09-30' & df$date <= '2013-05-18' |
  df$date >= '2013-09-29' & df$date <= '2014-05-17' |
  df$date >= '2014-09-28' & df$date <= '2015-05-23'

df$flu_season <- ifelse(
  df$date >= '2010-10-03' & df$date <= '2011-05-21',
  '2010-2011',
  ifelse(df$date >= '2011-10-02' & df$date <= '2012-05-19',
         '2011-2012',
         ifelse(df$date >= '2012-09-30' & df$date <= '2013-05-18',
                '2012-2013',
                ifelse(df$date >= '2013-09-29' & df$date <= '2014-05-17',
                       '2013-2014',
                       ifelse(df$date >= '2014-09-28' & df$date <= '2015-05-23',
                              '2014-2015', 
                              NA)))))
library(dplyr)
agg <- df %>%
  group_by(flu_season) %>% 
  summarise(days = n(),
            cases = sum(Data))
agg <- agg[!is.na(agg$flu_season),]
agg$year <- as.numeric(substr(agg$flu_season, 1, 4))

# colors
cols <- rep('black', nrow(agg))
cols[length(cols)] <- 'darkred'
cols <- adjustcolor(cols, alpha.f = 0.7)

# Add population
#(from census, captured in google search for "alachua county population")
agg$pop <- c(243574,
             247680,
             249653,
             251801,
             253451)
# fit <- lm(pop ~ year, data = agg)
# agg$pop[5] <- predict(fit, agg[5,])

# Cases per 100,000
agg$cases_per <- agg$cases / agg$pop * 100000



# PLOT CASES
png('barplot.png', height = 800, width = 500)
par(mfrow = c(2,1))

bp <- barplot(agg$cases,
              border = NA,
              ylim = c(0, max(agg$cases) * 1.15),
              yaxt = 'n',
              col = cols,
              names.arg = paste0(gsub('-', '-\n',agg$flu_season), ' '),
              cex.names = 0.7,
              space = 0.2,
              main = 'Alachua resident ILI cases')
axis(side = 2, 
     at = seq(0, 2000, 200),
     labels = seq(0, 2000, 200),
     las = 1,
     cex.axis = 0.8)
text(x = bp[,1],
     y = agg$cases,
     col = adjustcolor(cols, alpha.f = 0.8),
     pos = 3,
     labels = round(agg$cases, digits = 1),
     cex = 1.4)
abline(h = seq(0, 2000, 200),
       col = adjustcolor('black', alpha.f = 0.2),
       lty = 3) 
box('plot')




# PLOT CASES PER 100,000
bp <- barplot(agg$cases_per,
              border = NA,
              ylim = c(0, max(agg$cases_per) * 1.15),
              yaxt = 'n',
              col = cols,
              names.arg = paste0(gsub('-', '-\n',agg$flu_season), ' '),
              cex.names = 0.7,
              space = 0.2,
              main = 'Alachua resident ILI cases per 100,000')
axis(side = 2, 
     at = seq(0, 2000, 100),
     labels = seq(0, 2000, 100),
     las = 1,
     cex.axis = 0.8)
text(x = bp[,1],
     y = agg$cases_per,
     col = adjustcolor(cols, alpha.f = 0.8),
     pos = 3,
     labels = round(agg$cases_per, digits = 1),
     cex = 1.4)
abline(h = seq(0, 2000, 100),
       col = adjustcolor('black', alpha.f = 0.2),
       lty = 3) 
box('plot')
dev.off()

# # SECULAR TREND
# fit <- lm(cases_per ~ year,
#           data = agg[which(agg$year != 2014),])
# predict(fit, agg[which(agg$year == 2014),], 
#         se.fit = TRUE, interval = 'confidence')


# STATISTICS
pt <- prop.test(x = c(sum(agg$cases[1:4]),agg$cases[5]),
          n = c(sum(agg$pop[1:4]),agg$pop[5]))
pt$conf.int * 100000

#####
# READ IN ALL RESIDENT DATA
#####
private_dir <- '/media/joebrew/JB/fdoh/private/surv/historical'
setwd(private_dir)
df <- read.csv('symOldUpdated.csv')

#####
# ASSIGN FLU SEASON
#####
df$date <- as.Date(df$Date)
df$flu_season <- ifelse(
  df$date >= '2010-10-03' & df$date <= '2011-05-21',
  '2010-2011',
  ifelse(df$date >= '2011-10-02' & df$date <= '2012-05-19',
         '2011-2012',
         ifelse(df$date >= '2012-09-30' & df$date <= '2013-05-18',
                '2012-2013',
                ifelse(df$date >= '2013-09-29' & df$date <= '2014-05-17',
                       '2013-2014',
                       ifelse(df$date >= '2014-09-28' & df$date <= '2015-05-23',
                              '2014-2015', 
                              NA)))))
#####
# SUBSET TO INCLUDE ONLY ILI
#####
ili <- df[which(df$cat == 'ili'),]

#####
# GET RESPECTIVE FLU SEASONS 
#####
ili13 <- ili[which(ili$flu_season == '2013-2014'),]
ili14 <- ili[which(ili$flu_season == '2014-2015'),]

#####
# VISUALIZATION
#####
setwd('/home/joebrew/Documents/fdoh/public/morris')
pdf('morris_plots.pdf', height = 11, width = 8.5)
par(mfrow = c(2,1))
hist(ili13$Age, 
     xlab = 'Age', 
     main = '2013-2014 flu season',
     xlim = c(0, 100),
     breaks = 20,
     col = adjustcolor('blue', alpha.f = 0.6),
     border = 'white',
     ylim = c(0,150))
hist(ili14$Age, 
     xlab = 'Age', 
     main = '2014-2015 flu season',
     xlim = c(0, 100),
     breaks = 20,
     col = adjustcolor('darkred', alpha.f = 0.6),# 'grey',
     border = 'white',
     ylim = c(0, 150))
par(mfrow = c(1,1))
dev.off()

#### PYRAMID
pdf('pyrmaid_plots.pdf', height = 8.5, width = 11)
h13 <- cut(ili13$Age, breaks = seq(0, 100, 5))
t13 <- table(h13)
names(t13) <- as.numeric(unique(sort(factor(h13)))) * 5

h14 <- cut(ili14$Age, breaks = seq(0, 100, 5))
t14 <- table(h14)
names(t14) <- as.numeric(unique(sort(factor(h14)))) * 5

bp <- barplot(t13 * -1,
              horiz = TRUE,
              las = 1,
              xlim = c(-170, 170),
              col = adjustcolor('blue', alpha.f = 0.6),
              border = 'white',
              space = 0,
              ylab = 'Age (rounded up to nearest 5)',
              xlab = 'Cases')
barplot(t14,
        horiz = TRUE,
        add = TRUE,
        col = adjustcolor('darkred', alpha.f = 0.6),
        border = 'white',
        space = 0,
        yaxt = 'n', xaxt = 'n')
abline(v = seq(-200, 200, 20),
       col = adjustcolor('black', alpha.f = 0.2))
abline(h = bp[,1],
       col = adjustcolor('black', alpha.f = 0.2),
       lty = 2)
legend('topright',
       fill = adjustcolor(c('blue', 'red'), alpha.f = 0.6),
       legend = c('2013-2014', '2014-2015'),
       border = 'white')
box('plot')
title(main = 'Age distribution of Alachua resident ILI ED patients, 2013-2014 vs. 2014-2015')
dev.off()

#####
# Polygon density
#####
poly_den <- function(x,
                     plot_hist = FALSE,
                     col = 'darkred',
                     add = FALSE,
                     ...){
  
  if(plot_hist){
    hist(x, freq = FALSE,
         main = NA, xlab = NA, ylab = NA)
  } else{
    if(!add){
      hist(x, border = 'white',freq = FALSE,
           ...)
    }
    
  }
  
  xl <- density(x)
  polygon(x = c(xl$x, rev(xl$x)), 
          y = c(xl$y, rep(0, length(xl$y))), 
          col = adjustcolor(col, alpha.f = 0.5)) 
}

# VIEW POLYGONS
pdf('overlaid_distribution.pdf', height = 8.5, width = 11)
par(mfrow = c(1,1))
hist(ili13$Age, 
     xlab = 'Age', 
     main = 'Age distribution of 2013-2014 vs. 2014-2015 flu seasons',
     xlim = c(0, 100),
     breaks = 20,
     col = 'white',
     border = 'white',
     freq = FALSE)
poly_den(ili13$Age, col = 'blue', add = TRUE)
poly_den(ili14$Age[!is.na(ili14$Age)], col = 'red', add = TRUE)
legend('topright',
       fill = adjustcolor(c('blue', 'red'), alpha.f = 0.6),
       legend = c('2013-2014', '2014-2015'))
dev.off()

#####
# KS TEST
#####
ks <- ks.test(ili13$Age, ili14$Age)
ks

#####
# T TEST
#####
tt <- t.test(ili13$Age, ili14$Age)
tt

#####
# MEANS
#####
mean(ili13$Age)
mean(ili14$Age, na.rm = TRUE)

prop.test(x = c(nrow(ili13[which(ili13$Age <= 17),]),
                nrow(ili14[which(ili14$Age <= 17),])),
          n = c(nrow(ili13),
                nrow(ili14)))

#####
# 95 % confidence intervals on years
#####
agg <- data.frame(agg)
agg$lwr <- agg$upr <- NA
for (i in 1:nrow(agg)){
  pt <- prop.test(agg[i, 'cases'], agg[i, 'pop'])
  agg$lwr[i] <- as.numeric(pt$conf.int * 100000)[1]
  agg$upr[i] <- as.numeric(pt$conf.int * 100000)[2]
}