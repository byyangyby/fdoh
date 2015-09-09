library(gstat)
library(geoR)
library(rgdal)
library(scatterplot3d)
library(RColorBrewer)

setwd("C:/Users/BrewJR/Documents/fdoh/public/mosquito")

# Define color vector
my_colors <- colorRampPalette(c("darkgreen", "red"))(100)

SurfaceFun <- function(disease = "wnv",
                       boundary_shape = boundary){

  
  # getting coordinates of alachua boundary
  boundary_points <- boundary@polygons[[1]]@Polygons
  boundary_points <- boundary_points[[1]]@coords
  
  # Getting the site counts in question for recent trapping
  temp <- master[,which(grepl(disease ,colnames(master)) & 
                          grepl("site", colnames(master)))]
  temp <- as.data.frame(cbind(date=master$date, temp))
  temp <- temp[which(temp$date == 
                       max(temp$date[which(is.na(temp[,paste0(disease, "site1")]) == FALSE)])),]
  temp <- as.numeric(temp[-1])
  
  # Get trap locations and data values
  a <- data.frame("x" = loc$lon,
                  "y" = loc$lat,
                  "z" = temp)
  # Make into a geodata object
  b <- as.geodata(a)
  
  # Predict multiple points in Alachua County's boundary
  x <- seq(min(boundary_points[,1]), max(boundary_points[,1]), length = 100)
  y <- seq(min(boundary_points[,2]), max(boundary_points[,2]), length = 100)
  
  # Make a grid of those points
  pred.grid <- expand.grid(x,y)
  
  
  # kriging calculations
  kc <- krige.conv(geodata = b, coords = b$coords, data = b$data,
                   locations = pred.grid,
                   borders = boundary_points,
                   #borders = boundary@polygons,
                   # borders = ALACHUA BORDERS!,
                   krige = krige.control(type.krige = "ok",
                                         cov.pars = c(10, 3.33)))
  
  

  # Plot!
  # displaying predicted values
  image(kc, loc = pred.grid, 
        col = my_colors,
        xlab=NA, ylab=NA,
        xaxt = "n",
        yaxt = "n",
        xpd = NA,
        bty = "n")
  
  # Define percentiles for legend
  legtemp <-  round(quantile(kc$predict, probs = seq(0,1,, length = 10)))
  
  legend(x="topright",
         fill = my_colors[c(1,11,22,33,44,55,66,77,88,100)],
         legend = c(legtemp[1], NA, NA, legtemp[4], NA, NA, legtemp[7], NA, NA, legtemp[10]),
         border = FALSE,
         bty = "n",
         ncol = 1,
         y.intersp = 0.5,
         title = "Interpolation",
         cex = 0.75)
}



#   #x <- seq(-83, -82, by=0.01)
#   #y <- seq(29,30, by=0.01)
#   xv <- rep(x,each=14)
#   yv <- rep(y, 14)
#   in_mat <- as.matrix(cbind(xv,yv))

#   in_mat <- data.frame(in_mat)
#   names(in_mat) <- c("x", "y")
#   in_mat <- as.geodata(in_mat)
# 
#   
#   # Predict using kriging (another method)
#   mykrig <- krige.conv(geodata = b, coords = b$coords, data = b$data,
#                        locations = in_mat,
#                        # borders = ALACHUA BORDERS!,
#                        krige = krige.control(type.krige = "ok",
#                                              cov.pars = c(10, 3.33)))




# defining the grid
#pred.grid <-  expand.grid(seq(-83,-82, l=100), seq(29,30, l=100))

# # display 3d scatterplot
# s3d <- scatterplot3d(pred.grid$Var1, pred.grid$Var2, kc$predict,
#                      color = adjustcolor("black", alpha.f = 0.5),
#                      box = FALSE,
#                      grid = FALSE,
#                      type = "p",
#                      pch = ".")
# # add some points for the traps
# s3d$points3d(x = a$x,
#              y = a$y,
#              z = temp)

#   # http://www.leg.ufpr.br/geoR/geoRdoc/geoRintro.html
#   kc2 <- krige.bayes(geodata = b, coords = b$coords, data = b$data,
#                      locations = pred.grid,
#                      borders = x, prior = prior.control(phi.discrete = seq(0,5,l=51)), 
#                             output=output.control(n.predictive=2))
#   par(mfrow=c(2,2))
#   image(kc2, loc = pred.grid, main = "predicted", col=gray(seq(1,0.1,l=30)))
#   image(kc2, val ="variance", loc = pred.grid, 
#         main = "prediction variance", col=gray(seq(1,0.1,l=30)))
#   image(kc2, val = "simulation", number.col = 1, loc = pred.grid, 
#         main = "a simulation from\nthe predictive distribution", col=gray(seq(1,0.1,l=30)))
#   image(kc2, val = "simulation", number.col = 2,loc = pred.grid, 
#         main = "another simulation from \n the predictive distribution", col=gray(seq(1,0.1,l=30)))
#   par(mfrow=c(1,1))
#   
