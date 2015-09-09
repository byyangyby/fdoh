library(gdata)
setwd("E:/workingdirectory/obesity")
dir()
myred <- adjustcolor("red", alpha.f=0.6)
### 2010 obesity data
obesity <- read.csv("obesity2010.csv", skip=1)
colnames(obesity) <- c("county", "rate")
obesity <- obesity[order(obesity$rate),]

#Adult obesity hist
hist(obesity$rate, breaks=10, cex.main=0.7, main="Obesity rate by county", xlab="Percent", col="lightblue")
abline(v=obesity$rate[which(obesity$county=="Alachua")], col=myred, lwd=4)
text(x=obesity$rate[which(obesity$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult obesity barplot
obesity$col <- "grey"
obesity$col[which(obesity$county == "Alachua")] <- myred
barplot(obesity$rate, space=0, cex.main=0.7, main="Obesity rate by county", xlab="County", col=obesity$col,
        ylab="Percent of adults who are obese", names.arg=obesity$county, las=3, cex.names=0.3)

#DO WHITES ONLY
obesityWhite <- read.csv("obesityWhite2010.csv", skip=1)
colnames(obesityWhite) <- c("county", "rate")
obesityWhite <- obesityWhite[order(obesityWhite$rate),]
#Adult obesityWhite hist
hist(obesityWhite$rate, breaks=10, cex.main=0.7, main="Obesity rate by county (white adults)", xlab="Percent", col="lightblue")
abline(v=obesityWhite$rate[which(obesityWhite$county=="Alachua")], col=myred, lwd=4)
text(x=obesityWhite$rate[which(obesityWhite$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult obesityWhite barplot
obesityWhite$col <- "grey"
obesityWhite$col[which(obesityWhite$county == "Alachua")] <- myred
barplot(obesityWhite$rate, space=0, cex.main=0.7, main="Obesity rate by county (white adults)", xlab="County", col=obesityWhite$col,
        ylab="Percent obese", names.arg=obesityWhite$county, las=3, cex.names=0.3)



#DO BlackS ONLY
obesityBlack <- read.csv("obesityBlack2010.csv", skip=1)
colnames(obesityBlack) <- c("county", "rate")
obesityBlack <- obesityBlack[order(obesityBlack$rate),]
#Adult obesityBlack hist
hist(obesityBlack$rate, breaks=10, cex.main=0.7, main="Obesity rate by county (black adults)", xlab="Percent", col="lightblue")
abline(v=obesityBlack$rate[which(obesityBlack$county=="Alachua")], col=myred, lwd=4)
text(x=obesityBlack$rate[which(obesityBlack$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult obesityBlack barplot
obesityBlack$col <- "grey"
obesityBlack$col[which(obesityBlack$county == "Alachua")] <- myred
barplot(obesityBlack$rate, space=0, cex.main=0.7, main="Obesity rate by county (black adults)", xlab="County", col=obesityBlack$col,
        ylab="Percent obese", names.arg=obesityBlack$county, las=3, cex.names=0.3)

################# WHY?
#SEDENTARISM
sedentary <- read.csv("sedentary2007.csv", skip=1)
colnames(sedentary) <- c("county", "rate")
sedentary <- sedentary[order(sedentary$rate),]

#Adult sedentary hist
hist(sedentary$rate, breaks=10, cex.main=0.7, main="Sedentism rate by county", xlab="Percent", col="lightblue")
abline(v=sedentary$rate[which(sedentary$county=="Alachua")], col=myred, lwd=4)
text(x=sedentary$rate[which(sedentary$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult sedentary barplot
sedentary$col <- "grey"
sedentary$col[which(sedentary$county == "Alachua")] <- myred
barplot(sedentary$rate, space=0, cex.main=0.7, main="Sedentism rate by county", xlab="County", col=sedentary$col,
        ylab="Percent of adults who are sedentary", names.arg=sedentary$county, las=3, cex.names=0.3)

#whites only sedentarism
sedentaryWhite <- read.csv("sedentaryWhite2007.csv", skip=1)
colnames(sedentaryWhite) <- c("county", "rate")
sedentaryWhite <- sedentaryWhite[order(sedentaryWhite$rate),]
#Adult sedentaryWhite hist
hist(sedentaryWhite$rate, breaks=10, cex.main=0.7, main="Sedentism rate by county (white adults)", xlab="Percent", col="lightblue")
abline(v=sedentaryWhite$rate[which(sedentaryWhite$county=="Alachua")], col=myred, lwd=4)
text(x=sedentaryWhite$rate[which(sedentaryWhite$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult sedentaryWhite barplot
sedentaryWhite$col <- "grey"
sedentaryWhite$col[which(sedentaryWhite$county == "Alachua")] <- myred
barplot(sedentaryWhite$rate, space=0, cex.main=0.7, main="Sedentism rate by county (white adults)", xlab="County", col=sedentaryWhite$col,
        ylab="Percent sedentary", names.arg=sedentaryWhite$county, las=3, cex.names=0.3)

#### BLACKS SEDENTARISM

#whites only sedentarism
sedentaryBlack <- read.csv("sedentaryBlack2007.csv", skip=1)
colnames(sedentaryBlack) <- c("county", "rate")
sedentaryBlack <- sedentaryBlack[order(sedentaryBlack$rate),]
#Adult sedentaryBlack hist
hist(sedentaryBlack$rate, breaks=10, cex.main=0.7, main="Sedentism rate by county (black adults)", xlab="Percent", col="lightblue")
abline(v=sedentaryBlack$rate[which(sedentaryBlack$county=="Alachua")], col=myred, lwd=4)
text(x=sedentaryBlack$rate[which(sedentaryBlack$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult sedentaryBlack barplot
sedentaryBlack$col <- "grey"
sedentaryBlack$col[which(sedentaryBlack$county == "Alachua")] <- myred
barplot(sedentaryBlack$rate, space=0, cex.main=0.7, main="Sedentism rate by county (black adults)", xlab="County", col=sedentaryBlack$col,
        ylab="Percent sedentary", names.arg=sedentaryBlack$county, las=3, cex.names=0.3)



##### NOW TIME FOR CHILDREN

#middle school
mid <- read.csv("midbmi95.csv", skip=1)
colnames(mid) <- c("county", "rate")
mid <- mid[order(mid$rate),]

#Adult mid hist
hist(mid$rate, breaks=10, cex.main=0.7, main="BMI > 95% among middle schoolers", xlab="Percent", col="lightblue")
abline(v=mid$rate[which(mid$county=="Alachua")], col=myred, lwd=4)
text(x=mid$rate[which(mid$county=="Alachua")], y=16, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

#Adult mid barplot
mid$col <- "grey"
mid$col[which(mid$county == "Alachua")] <- myred
barplot(mid$rate, space=0, cex.main=0.7, main="BMI > 95% among middle schoolers", xlab="County", col=mid$col,
        ylab="Percent middle schoolers with BMI > 95%", names.arg=mid$county, las=3, cex.names=0.3)

#high school
high <- read.csv("highbmi95.csv", skip=1)
colnames(high) <- c("county", "rate")
high <- high[order(high$rate),]

# high hist
hist(high$rate, breaks=10, cex.main=0.7, main="BMI > 95% among high schoolers", xlab="Percent", col="lightblue")
abline(v=high$rate[which(high$county=="Alachua")], col=myred, lwd=4)
text(x=high$rate[which(high$county=="Alachua")], y=10, col=adjustcolor("black", alpha.f=0.6),
     cex=1, label="Alachua")

# high barplot
high$col <- "grey"
high$col[which(high$county == "Alachua")] <- myred
barplot(high$rate, space=0, cex.main=0.7, main="BMI > 95% among high schoolers", xlab="County", col=high$col,
        ylab="Percent high schoolers with BMI > 95%", names.arg=high$county, las=3, cex.names=0.3)

save.image("E:/workingdirectory/obesity/fourKourtney28jan.RData")
