setwd("C:/Users/BrewJR/Documents/workingdirectory/census")
census <- read.csv("censusflorida2.csv")
leg <- read.csv("censuslegend.csv")
std <- read.csv("std3.csv")
std$County <- std$X
std$X <- NULL

std <- cbind(std[23], std[1:22])

fl <- census[c(4, 7, 11, 14, 18, 21, 25, 28, 32, 35, 39,
               42, 46, 49, 53, 56, 60, 63,
               67, 70, 74, 77, 81, 84, 88, 91, 95, 98, 
               102, 105, 109, 112, 116, 119, 123, 126, 130, 133, 137, 140)]

colnames(fl) <- leg$des[c(4, 7, 11, 14, 18, 21, 25, 28, 32, 35, 39,
                          42, 46, 49, 53, 56, 60, 63,
                          67, 70, 74, 77, 81, 84, 88, 91, 95, 98, 
                          102, 105, 109, 112, 116, 119, 123, 126, 130, 133, 137, 140)]

colnames(fl) <-c("totalpop","totalpopper","x0004","x0004per",
                 "x0509","x0509per","x1014","x1014per","x1519","x1519per",
                 "x2024","x2024per","x2529","x2529per","x3034","x3034per",
                 "x3539","x3539per","x4044","x4044per","x4549","x4549per",
                 "x5054","x5054per","x5559","x5559per","x6064","x6064per",
                 "x6569","x6569per","x7074","x7074per","x7579","x7579per",
                 "x8084","x8084per","x8589","x8589per","x90","x90per")

fl$county <- std$County
fl <- cbind(fl[41], fl[1:40])




setwd("C:/Users/BrewJR/Documents/workingdirectory/census")
std1519bac <- read.csv("bacstd1519.csv")
floridaRate <- 2251.2



library(SemiPar)
library(RColorBrewer)
library(splines)
library(xtable)
mycols <- colorRampPalette(brewer.pal(8, "Greens"))(68)
library(SemiPar)
library(RColorBrewer)
mycols <- colorRampPalette(brewer.pal(8, "Spectral"))(68)
mycols <- rev(mycols)
mycols2 <- colorRampPalette(brewer.pal(8, "Reds"))(50)
mycols2 <- mycols2[21:50]
mycols3 <- colorRampPalette(brewer.pal(8, "Blues"))(68)
mycols4 <- colorRampPalette(brewer.pal(8, "Greens"))(12)
mycolsz <- colorRampPalette(brewer.pal(8, "Greens"))(68)
stdcounties <- std[-1,]

hist(stdcounties$Rate, breaks=67, col=mycols)

test <- stdcounties[order(stdcounties$Rate),]
test <- std1519bac[order(std1519bac$Rate),]

mycolsz[38] <- adjustcolor("blue", alpha.f=0.5)
mycolsz[52] <- adjustcolor("red", alpha.f=0.5)

mybarplot <- barplot(test$Rate, col=mycols, main="Bacterial STD rates among 15-19 year olds",
                     xlab="County", ylab="Rate per 100,000", border=TRUE)
axis(side=1, at=mybarplot[,1], labels=test$County, las=3, cex.axis=0.5,
     tick=FALSE, line=-1)
legend(x="topleft", fill=adjustcolor(c("blue","red"), alpha.f=0.5), cex=0.7,
       legend=c("Florida (2251 per 100,000)", 
                "Alachua (3031 per 100,00, 51st place)"), 
       border=FALSE, bty="n")

#### TIME SERIES

myts <- read.csv("bacstd1519ts.csv")

plot(myts$Year, myts$alachuaRate, type="n", xlab="Year", 
     ylab="Rate per 100,000", main="Bacterial STD Rate Among 15-19 Year-Olds")
xspline(myts$Year, myts$alachuaRate, border=adjustcolor("red", alpha.f=0.5), 
        shape=0.6, lwd=3)
xspline(myts$Year, myts$floridaRate, border=adjustcolor("blue", alpha.f=0.5), 
        shape=0.6, lwd=3)
legend(x="topleft", lty=1, col=adjustcolor(c("red","blue"), alpha.f=0.5),
       legend=c("Alachua", "Florida"))

test2 <- test[-38,]
row.names(test2) <- NULL
print(xtable(test2), size=1)


#### MAKE A BARPLOT OF 15-19 year old population size
mycols3[67] <- adjustcolor("red", alpha.f=0.8)
fl2 <- fl[order(fl$x1519per),]
barplot(fl2$x1519per, names.arg=fl2$county, las=2, cex.names=0.4, col=mycols3,
        main="Percentage of population 15-19 years old", xlab="County", ylab="Percentage",
        border=TRUE)


######## NOW MAKE A REGRESSION PLOT 
bacstd <- read.csv("bacstd.csv")
bacstd$County <- fl$county
std1519bac$County <- fl$county
mycols <- colorRampPalette(brewer.pal(8, "Spectral"))(56)
mycols <- rev(mycols)

plot(fl$x1519per, bacstd$Rate, type="n",
     xlab="Percentage of residents aged 15-19",
     ylab="Total Bacterial STD Rate per 100,000",
     main="15-19 year-olds as % of pop and total STD rate")
abline(lm(bacstd$Rate ~ fl$x1519per), col=adjustcolor("black", alpha.f=0.2), lwd=3)
points(fl$x1519per, bacstd$Rate, pch=16, 
       col=adjustcolor(mycols[floor(std1519bac$Rate/100)], alpha.f=0.7), 
       cex=round(std1519bac$Rate/1000))

text(x=fl$x1519per[which(fl$county=="ALACHUA")],
     y=bacstd$Rate[which(fl$county=="ALACHUA")],
     labels=bacstd$County[which(fl$county=="ALACHUA")], cex=0.5, pos=2)
text(x=fl$x1519per[which(fl$county=="LEON")],
     y=bacstd$Rate[which(fl$county=="LEON")],
     labels=bacstd$County[which(fl$county=="LEON")], cex=0.5, pos=2)
text(x=fl$x1519per[which(fl$county=="GULF")],
     y=bacstd$Rate[which(fl$county=="GULF")],
     labels=bacstd$County[which(fl$county=="GULF")], cex=0.5, pos=2)
text(x=fl$x1519per[which(fl$county=="GADSDEN")],
     y=bacstd$Rate[which(fl$county=="GADSDEN")],
     labels=bacstd$County[which(fl$county=="GADSDEN")], cex=0.5, pos=2)
text(x=fl$x1519per[which(fl$county=="SUMTER")]+1,
     y=bacstd$Rate[which(fl$county=="SUMTER")],
     labels=bacstd$County[which(fl$county=="SUMTER")], cex=0.5, pos=2)


legend(x="topleft", pch=16, pt.cex=c(1,3,5), legend=c("1000", "3000", "5000"),
       title="Bacterial STD rate among 15-19 year olds", cex=0.5, 
       col=adjustcolor(mycols[c(10,30,50)], alpha.f=0.7), border=FALSE, bty="n",
       y.intersp=2)

mylm <- lm(bacstd$Rate ~ fl$x1519per)
summary(mylm)

save.image("~/workingdirectory/census/bacstd1519.RData")