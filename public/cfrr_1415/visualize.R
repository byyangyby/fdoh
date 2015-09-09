library(RColorBrewer)

# SET WD
setwd('/home/joebrew/Documents/fdoh/public/cfrr_1415')

# READ IN RAW DATA FROM 2013/14 AND 2014/15
df <- read.csv('raw.csv')

# SUBSET TO ONLY INCLUDE 2014/15
df <- df[which(df$year == 2014),]

# CREATE COLUMN WITH MORE FORMAL TYPE
df$Type <- ifelse(df$type == 'elem',
                  'Elementary',
                  ifelse(df$type == 'mid',
                         'Middle',
                         ifelse(df$type == 'high',
                                'High',
                                ifelse(df$type == 'prek',
                                       'Head Start',
                                       ifelse(df$type == 'other',
                                              'Centers', 
                                              NA)))))

# DEFINE FUNCTION FOR PLOTTING
visualize <- function(type = 'elem',
                      var = 'cfrr',
                      bw = FALSE,
                      order_by_cfrr = TRUE){
  
  
  # subset
  sub_data <- df[which(df$type == type),]
  #order
  if(order_by_cfrr){
    sub_data <- sub_data[order(sub_data[,'cfrr']),]
  } else {
    sub_data <- sub_data[order(sub_data[,var]),]
  }
  
  # define color vector
  if(bw){
    cols <- rep('grey', nrow(sub_data))
  } else {
    cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(nrow(sub_data))
  }  
  
  # assign variable
  x <- sub_data[,var] * 100
  
  # plot
  bp <- barplot(x,
                border = NA,
                ylim = c(0, max(x) * 1.1),
                col = cols,
                names.arg = sub_data$school,
                las = 3,
                cex.names = 0.7,
                ylab = 'Percent')
  # labels
  text(x = bp[,1],
       y = x,
       pos = 3,
       labels = paste0(round(x, digits = 1)),
       col = adjustcolor('black', alpha.f = 0.6),
       cex = 0.6)
  # title
  title_var <- ifelse(var == 'ir', 
                      'Immunization rates',
                      'Consent form return rates')
  title(main = paste0(title_var, ' -- ', sub_data$Type[1], '\n',
                      '(2014-15)'))
}

# PLOT ALL
pdf('visuals_color.pdf',
    height = 11,
    width = 8.5)
par(mar = c(10, 4, 4,2))
par(mfrow = c(2,1))
for (j in c('elem', 'mid', 'high', 'prek', 'other')){
  for (i in c('cfrr', 'ir')){
    
    visualize(type = j,
              var = i,
              bw = FALSE)
  }
}

# Plot overall correlation
par(mfrow = c(1,1))
par(mar = c(5,4,4,2))
cols <- brewer.pal(5, 'Set3')
pchs <- 6:10
df$col <- cols[as.numeric(factor(df$type))]
#df$col <- 'grey'
df$pch <- pchs[as.numeric(factor(df$type))]

cfrr <- df$cfrr * 100
ir <- df$ir * 100
size <- df$pop / 400
plot(cfrr,
     ir,
     col = df$col,
     ylim = c(0, 100),
     xlim = c(0, 100),
     pch = 19,
     #pch = df$pch,
     xlab = 'Consent form return rate (%)',
     ylab = 'Immunization rate (%)',
     cex = size)

fit <- lm(ir ~ cfrr)
abline(fit,
       col = adjustcolor('black', alpha.f = 0.6),
       lwd = 2)
abline(a = 0, b = 1, lty = 2,
       col = adjustcolor('darkred', alpha.f = 0.2))

legend('topleft',
       #col = adjustcolor('black', alpha.f = 0.6),
       #pch = pchs,
       pch = 19,
       col = cols,
       legend = levels(factor(df$type)),
       title = 'Age group')
legend('top',
       #pch = 5,
       pch = 19,
       col = 'grey',
       pt.cex = c(100, 500, 1000, 1500) / 400,
       legend = c(100, 500, 1000, 1500),
       title = 'School population',
       ncol = 4)
legend('left',
       lty = c(2,1),
       col = adjustcolor(c('darkred', 'black'), alpha.f = 0.6),
       lwd = c(1,2),
       legend = c('Maximum', 'Line of best fit'),
       title = 'Trend lines')
title(main = 'Linear relationship between consent form return and immunization')

dev.off()