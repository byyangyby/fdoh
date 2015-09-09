#####
# EVERYTHING HAS BEEN ADJUSTED FOR MASSACHUSETTS
#####

options(scipen = 999)

#setwd('fdoh/public/ili_2015')

x <- read.csv('parker_medicaid_savings.csv')

# ADJUST FOR MASSACHUSETTS
x$medicaid_costs <- x$medicaid_costs / 2.95

current <- x$medicaid_costs[which(x$imm == 20)]

x$savings1 <- NA
x$savings2 <- NA
x$savings3 <- NA

for (i in 1:nrow(x)){
  x$savings1[i] <-
    (current - x$medicaid_costs[i]) * (1/3)
  x$savings2[i] <-
    (current - x$medicaid_costs[i]) * (2/3)
  x$savings3[i] <-
    (current - x$medicaid_costs[i]) 
}


# MAKE EVERYTHING CUT IN HALF (FLORIDA'S SHARE OF MEDICAID SHOULD BE )
# BASED ON 15 MILLION, NOT 30
for (i in 4:6){
  x[,i] <- x[,i] / 2
}

bp <- barplot(x$savings3, border = FALSE, space = 0,
              cex.names = 0.5,
              xlab = "Pediatric immunization rate",
              ylab = "ROI (Medicaid dollars only)",
              cex.axis = 0.9,
              names.arg = x$imm,
              ylim = c(min(x$savings3, na.rm = TRUE) * 1.05, max(x$savings3, na.rm = TRUE) * 1.2),
              yaxt = "n",
              xaxt = "n")
axis_seq <- seq(-100, 100, 1)
axis(side = 2,
     at = axis_seq * 1000000, 
     labels = paste0(axis_seq, "M"),
     cex.axis = 1.1,
     las = 1)
x_seq <- seq(0, 100, 10)
axis(side = 1,
     at = x_seq,
     labels = x_seq,
     cex.axis = 1)

# abline(h = 2000000,
#        col = adjustcolor("darkblue", alpha.f = 0.5),
#        lwd = 2)
# text(x = bp[10,1],
#      y = 2000000,
#      col = adjustcolor("darkblue", alpha.f = 0.5),
#      labels = "$2 million (savings after 1 years) ",
#      cex = 0.9,
#      pos = 3)
# abline(h = 4000000,
#        col = adjustcolor("darkgreen", alpha.f = 0.5),
#        lwd = 2)
# text(x = bp[10,1],
#      y = 4000000,
#      col = adjustcolor("darkgreen", alpha.f = 0.5),
#      labels = "$4 million (savings after 2 years)",
#      cex = 0.9,
#      pos = 3)
# abline(h = 6000000,
#        col = adjustcolor("black", alpha.f = 0.5),
#        lwd = 2)
# text(x = bp[10,1],
#      y = 6000000,
#      col = adjustcolor("black", alpha.f = 0.5),
#      labels = "$6 million (savings after 3 years)",
#      cex = 0.9,
#      pos = 3)
abline(h = 0)

text(x = bp[,1][seq(0,95, 10)][-1],
     y = x$savings3[seq(0,95, 10)][-1],
     pos = 3,
     labels = paste0(x$imm[seq(0,95, 10)][-1], "%"),
     cex = 1,
     col = adjustcolor("darkgreen", alpha.f = 0.5))
bp <- barplot(x$savings2, border = FALSE, space = 0,
              col = adjustcolor("darkgreen", alpha.f = 0.3),
              add = T,
              xaxt = "n",
              yaxt = "n")

bp <- barplot(x$savings1, border = FALSE, space = 0,
              col = adjustcolor("darkblue", alpha.f = 0.3),
              add = T,
              xaxt = "n",
              yaxt = "n")


# text(x = bp[95,1],
#      y = 2000000,
#      col = adjustcolor("darkred", alpha.f = 0.5),
#      labels = "$2 million",
#      cex = 0.8,
#      pos = 3)
box("plot")
legend(x = "topleft",
       fill = adjustcolor(c("darkgrey", "darkgreen", "darkblue"), alpha.f = 0.5),
       border = NA,
       legend = c("3rd year", "2nd year", "1st year"),
       bty = "n")
title(main = "ROI in Medicaid dollars (Massachusetts-funded only)")

# ###################
vals <- as.numeric(x[29, 4:6])
bp <- barplot(vals,
        names.arg = paste0("Year ", 1:3),
        cex.names = 1,
        xlab = "Year",
        ylab = "Dollars",
        main = "Medicaid savings",
        border = FALSE,
        cex.axis = 0.6,
        ylim = c(0, max(vals) * 1.1))
text(x = bp[,1],
     y = vals,
     col = adjustcolor("darkgreen", alpha.f = 0.6),
     cex = 0.7,
     pos = 3,
     labels = paste0(round(vals, digits = -5)/1000000, " million"))
box("plot")
abline(h = 2000000,
       col = adjustcolor("darkred", alpha.f = 0.5),
       lwd = 2)
abline(h = 0)
text(x = bp[3.5,1],
     y = 2000000,
     col = adjustcolor("darkred", alpha.f = 0.5),
     labels = "$2 million",
     cex = 0.8,
     pos = 3)

# abline(h = seq(0,10,1) * 1000000,
#        col = adjustcolor("black", alpha.f = 0.2))

###############################################
pedLevels <- c(5,20,40,60,80)
directCosts <- c(203,111,49,28,21) / 2.95
medicaidDirectCosts <- directCosts*.16
nonMedicaidDirectCosts <- directCosts - medicaidDirectCosts
medicaidDirectCostsFed <- medicaidDirectCosts*.5
medicaidDirectCostsFl <- medicaidDirectCosts*.5
plot(pedLevels, medicaidDirectCosts,
     xlim=c(0,80), xlab="Pediatric immunization rate", ylab="Dollars (in millions)")


my4cols <- adjustcolor(c("darkred", "darkblue", "darkgreen"), alpha.f=0.5)
my4labels <- c("Direct Medicaid costs (paid by Massachusetts)", 
               "Direct Medicaid costs (paid by federal government)", 
               "Direct non-Medicaid costs")
polygonFun <- function(time, poly1, poly2, poly3){
  mm <- c(time, rev(time))
  poly1a <- c(rep(0, length(time)), rev(poly1))
  poly2a <- c(rep(0, length(time)), rev(poly2))
  poly3a <- c(rep(0, length(time)), rev(poly3))
  
  aa <- c(poly1, rev(poly1))
  bb <- c(poly2+poly1, rev(poly1))
  cc <- c(poly3+poly2+poly1, rev(poly2+poly1))
  
  plot(time, poly1, xaxt="n", type="n", ylim=c(0,max(cc, na.rm=TRUE)),
       xlab=("Pediatric immunization rate (%)"), ylab="Dollars (in millions)",
       main="Reduction in annual direct costs (by pay source)")
  axis(side=1, at=time, labels=time,
       tick=FALSE, line=0, cex=0.6)
  polygon(mm, poly1a, col=my4cols[1], border=NA)
  polygon(mm, bb, col=my4cols[2], border=NA)
  polygon(mm, cc, col=my4cols[3], border=NA)
  legend(x="topright", fill=rev(my4cols), 
         legend=rev(my4labels),
         border=FALSE, bty="n", cex=0.85)
  abline(h=seq(0,200,25), col=adjustcolor("black", alpha.f=0.2))
  abline(v=c(5,20,40,60,80), col=adjustcolor("black", alpha.f=0.2))
}

polygonFun(time= pedLevels,
           poly1= medicaidDirectCostsFl,
           poly2= medicaidDirectCostsFed,
           poly3= nonMedicaidDirectCosts)


par(mar=c(4,5,2,1))
############### EXCLUDING PRIVATE
my4cols <- adjustcolor(c("darkred", "darkblue"), alpha.f=0.5)
my4labels <- c("Medicaid costs paid by Massachusetts", 
               "Total Medicaid costs for Massachusetts")
polygonFun <- function(time, poly1, poly2){
  mm <- c(time, rev(time))
  poly1a <- c(rep(0, length(time)), rev(poly1))
  poly2a <- c(rep(0, length(time)), rev(poly2))
  
  aa <- c(poly1, rev(poly1))
  bb <- c(poly2+poly1, rev(poly1))
  
  plot(time, poly1, xaxt="n", type="n", ylim=c(0,max(bb, na.rm=TRUE)),
       xlab=("Pediatric immunization rate (%)"), ylab="Dollars (in millions)",
       main="Reduction in annual Medicaid costs", cex.axis=1.6,
       cex.lab=1.4)
  axis(side=1, at=time, labels=time,
       tick=FALSE, line=0, cex.axis=1.5)
  polygon(mm, poly1a, col=my4cols[1], border=NA)
  polygon(mm, bb, col=my4cols[2], border=NA)
  legend(x="topright", fill=rev(my4cols), 
         legend=rev(my4labels),
         border=FALSE, bty="n", cex=1.4)
  abline(h=seq(0,200,5), col=adjustcolor("black", alpha.f=0.2))
  abline(v=c(5,20,40,60,80), col=adjustcolor("black", alpha.f=0.2))
}

polygonFun(time= pedLevels,
           poly1= medicaidDirectCostsFl,
           poly2= medicaidDirectCostsFed)



