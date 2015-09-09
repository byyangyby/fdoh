par(mfrow=c(1,1))
setwd("C:/Users/Joe Brew/Documents/workingdirectory")
trap <- read.csv("simple.csv", head=T, sep=",")

trap$daterec <- as.Date(trap$date, "%m/%d/%y")

trap2008 <- trap[which(trap$year==2008),] 
trap2009 <- trap[which(trap$year==2009),] 
trap2010 <- trap[which(trap$year==2010),] 
trap2011 <- trap[which(trap$year==2011),] 
trap2012 <- trap[which(trap$year==2012),] 
trap2013 <- trap[which(trap$year==2013),] 

#A label vector for plots
dates <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
dates2 <- c("Jun","Jul","Aug")

#Average vectors per trap for all entries
Avgvec <- sum(trap$vector)/138
Avgvec

#6 years on x axis
plot(trap$daterec, trap$vector, type="p")
lines(trap$daterec, trap$vector, lty=1, lwd=3)

#1 year on x axis
library(splines)
plot(trap$dayofyear, trap$vector, type="n",
     main="Vectors",
     cex.main=1.3,
     ylim=c(0,1050),
     xlab="Date",
     ylab="Average Vectors Per Trap",
     cex.lab=1,
     cex.axis=1,
     xaxt="n")
axis(1, c(105,135,165,195,225,255,285,315), labels=dates, las=1, cex.axis=1)
lines(loess.smooth(trap$dayofyear, trap$vector), col="black", lty=1, lwd=1)
legend(x="topleft",legend=c("Smoothed average","2008","2009","2010","2011","2012","2013",
                            "Joe큦 2013 estimate"),
       fill=c(
  "black","forestgreen","dodgerblue4","deeppink4","darkslategray4",
  "darkred","darkmagenta","orange"), bor=T, bty="n", cex=0.6)
xspline(trap2008$dayofyear, trap2008$vector, shape=1,
        border="forestgreen", lty=1, lwd=0.4)
xspline(trap2009$dayofyear, trap2009$vector, shape=1,
        border="dodgerblue4", lty=1, lwd=0.4)
xspline(trap2010$dayofyear, trap2010$vector, shape=1,
        border="deeppink4", lty=1, lwd=0.4)
xspline(trap2011$dayofyear, trap2011$vector, shape=1,
        border="darkslategray4", lty=1, lwd=0.4)
xspline(trap2012$dayofyear, trap2012$vector, shape=1,
        border="darkred", lty=1, lwd=0.4)
xspline(trap2013$dayofyear, trap2013$vectorjoe, shape=1,
        border="orange", lty=6, lwd=1)
xspline(trap2013$dayofyear, trap2013$vector, shape=1,
        border="darkmagenta", lty=1, lwd=1)

























#############################################


#Clarke큦 estimates
library(splines)
plot(trap2013$dayofyear, trap2013$totaljoe, type="n",
     main="2013 Mosquito Population",
     cex.main=1.3,
     xlab="Date",
     ylab="Average Mosquitoes Per Trap",
     ylim=c(0,500),
     cex.lab=1,
     cex.axis=1,
     xaxt="n")
axis(1, c(165,195,225), labels=dates2, las=1, cex.axis=1)
legend(x="topleft",legend=c("Clarke큦 total estimate",
                            "Clarke큦 vector estimate"),
       fill=c("dodgerblue3","darkmagenta"), bor=T, bty="n", cex=0.6)
xspline(trap2013$dayofyear, trap2013$vector, shape=0.3,
        border="darkmagenta", lty=1, lwd=1)
xspline(trap2013$dayofyear, trap2013$total, shape=0.3,
        border="dodgerblue3", lty=1, lwd=1)





#############################################

#Joe큦 estimates
library(splines)
plot(trap2013$dayofyear, trap2013$totaljoe, type="n",
     main="2013 Mosquito Population (alternative estimates)",
     cex.main=1.3,
     xlab="Date",
     ylab="Average Mosquitoes Per Trap",
     ylim=c(0,500),
     cex.lab=1,
     cex.axis=1,
     xaxt="n")
axis(1, c(165,195,225), labels=dates2, las=1, cex.axis=1)
legend(x="topleft",legend=c("Clarke큦 total estimate",
                            "Clarke큦 vector estimate",
                            "Joe큦 total estimate",
                            "Joe큦 vector estimate"),
       fill=c("dodgerblue3","darkmagenta","orange","dark red"), bor=T, bty="n", cex=0.6)
xspline(trap2013$dayofyear, trap2013$vector, shape=0.3,
        border="darkmagenta", lty=1, lwd=1)
xspline(trap2013$dayofyear, trap2013$vectorjoe, shape=0.3,
        border="dark red", lty=6, lwd=2)
xspline(trap2013$dayofyear, trap2013$total, shape=0.3,
        border="dodgerblue3", lty=1, lwd=1)
xspline(trap2013$dayofyear, trap2013$totaljoe, shape=0.3,
        border="orange", lty=6, lwd=2)









################





#xspline(rainfall2weeksbefore*50, shape=-0.3, open=T, 
# border="dark green", lwd=2, lty=10)
#axis(1, 1:7, labels=dates, las=1, cex.axis=0.7)
#axis(4, c(0,50,100,150,200), labels=rainlabels, las=2, cex.axis=0.8)


#1 year on x axis
#plot(trap$dayofyear, trap$vector, type="p",
 #    main="Mosquitoes",
  #   cex.main=1.3,
   #  xlab="Day of year",
    # ylab="Average Vectors per Trap",
     #cex.lab=0.8,
     #cex.axis=0.8)
#lines(stats::lowess(trap$dayofyear,trap$vector), lty=1, lwd=3)
#lines(trap2008$dayofyear, trap2008$vector, lty=1, lwd=2, col="forestgreen")
#lines(trap2009$dayofyear, trap2009$vector, lty=1, lwd=2, col="dodgerblue4")
#lines(trap2010$dayofyear, trap2010$vector, lty=1, lwd=2, col="deeppink4")
#lines(trap2011$dayofyear, trap2011$vector, lty=1, lwd=2, col="darkslategray4")
#lines(trap2012$dayofyear, trap2012$vector, lty=1, lwd=2, col="darkred")
#lines(trap2013$dayofyear, trap2013$vector, lty=1, lwd=4, col="darkmagenta")



