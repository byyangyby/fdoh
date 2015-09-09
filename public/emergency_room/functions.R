#####
# Compass Rose
#####

#(from http://r-sig-geo.2731867.n2.nabble.com/How-to-diplasy-a-compass-rose-on-a-map-td4509034.html)
compass_rose <-function(x,y,rot=0,cex=1) { 
  oldcex<-par(cex=cex) 
  mheight<-strheight("M") 
  xylim<-par("usr") 
  plotdim<-par("pin") 
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1] 
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180 
  crspans<-rep(c(mheight*3,mheight/2),4) 
  xpoints<-cos(point.angles)*crspans*xmult+x 
  ypoints<-sin(point.angles)*crspans+y 
  polygon(xpoints,ypoints) 
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x 
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y 
  text(txtxpoints,txtypoints,c("E","N","W","S")) 
  par(oldcex) 
} 

#####
# COLLAPSE MAP INTO ONLY OUTER BOUNDARY
#####
collapse_map <- function(x){
  require(maptools)
  boundary <- unionSpatialPolygons(x, rep(1, length(x@polygons)))
}

library(RColorBrewer)
library(classInt)

#####
# CHOROPLETH MAP
#####
choro <- function(
  shape = NULL,
  boundary = NULL,
  main = NULL,
  var = NULL,
  color1 = "lightblue",
  color2 = "darkorange",
  legend_round = 2,
  legend_pos = "bottomleft",
  long_legend = TRUE,
  fixed_scale = NULL,
  lwd = 0.2,
  border = TRUE,
  legend_text_col = "black",
  add = FALSE){
  my_colors <- colorRampPalette(c(color1, "white", color2))(10)
  if(!is.null(fixed_scale)){
    if(length(fixed_scale) != 10){stop("Fixed scale must be of length 10")}
    my_quantiles <- fixed_scale
  } else{
    my_quantiles <- quantile(var, na.rm = TRUE, probs = seq(0,1, length = 10))
  }
  my_values <- vector(mode = "numeric", length = length(var))
  for (i in 1:length(var)){
    diffs <- (var[i] - as.numeric(my_quantiles))^2
    best <- which.min(diffs)[1]
    my_values[i] <- best
  }
  map_colors <- my_colors[my_values]
  plot(shape, col = map_colors, border = border, lwd = lwd,
       main = main, add = add)
  plot(boundary, add = TRUE)
  if(long_legend){
    legend_colors <- colorRampPalette(my_colors)(25)
    legend(legend_pos, # position
           legend = c(min(round(my_quantiles, digits = legend_round)),
                      rep(NA, 11),
                      median(round(my_quantiles, digits = legend_round)),
                      rep(NA, 11),
                      max(round(my_quantiles, digits = legend_round))),
           fill = legend_colors,
           cex = 0.75,
           y.intersp = 0.5,
           border=NA,
           bty = "n",
           text.col = legend_text_col)
  } else{
    legend_colors <- colorRampPalette(my_colors)(11)
    legend(legend_pos, # position
           legend = c(min(round(my_quantiles, digits = legend_round)),
                      rep(NA, 4),
                      median(round(my_quantiles, digits = legend_round)),
                      rep(NA, 4),
                      max(round(my_quantiles, digits = legend_round))),
           fill = legend_colors,
           cex = 0.75,
           y.intersp = 0.5,
           border=NA,
           bty = "n",
           text.col = legend_text_col)
  }
  
  compass_rose(x = -80.5 ,y = 30,rot=0,cex=0.25)
}

# choro(shape = fl,
#       boundary = boundary,
#       var = fl$lbw_rate2014,
#       long_legend = FALSE,
#       legend_pos = "center",
#       color1 = "lightblue",
#       color2 = "red")
# 
# for (i in 2000:2014){
#   choro(shape = fl, boundary = boundary, 
#         var = fl@data[,paste0("lbw_rate", i)])
#   Sys.sleep(1)
#   title(main = i)
# }