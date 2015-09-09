library(Hmisc)

#####
# FUNCTION FOR PRETTY BARPLOTS
#####
bar_fun <- function(var,
                    names.arg = NA,
                    xlab = NA,
                    ylab = NA,
                    col = adjustcolor("lightblue", alpha.f = 0.6),
                    border = NA,
                    cex.names = 0.76,
                    add_text = TRUE,
                    text_size = 0.75,
                    round_digits = 1,
                    percent = TRUE,
                    text_col = adjustcolor("black", alpha.f = 0.7),
                    grid_sequence = seq(0,100,20),
                    add_box = TRUE,
                    add_gridlines = TRUE,
                    add_ci = FALSE,
                    yplus = NULL,
                    yminus = NULL,
                    add = FALSE){
 # Draw plot
  bp <- barplot(var,
                names.arg = names.arg,
                ylab = ylab,
                xlab = xlab,
                col = col,
                border = border,
                ylim = c(0, max(var) * 1.1),
                cex.names = cex.names,
                add = add)
  # Add text
  if(add_text){
    text(x = bp[,1],
         y = var,
         labels = paste0(round(var, digits = round_digits),ifelse(percent, " %", "")),
         pos = 1,
         col = text_col,
         cex = text_size)
  }
  
  # Add gride lines
  if(add_gridlines){
    abline(h = grid_sequence,
           col = adjustcolor("black", alpha.f = 0.1),
           lty = 6)
  }
  # Add box
  if(add_box){
    box("plot")
  }
  
  # Add CI
  if(add_ci){
    if(is.null(yplus)|is.null(yminus)){
      stop("You must supply yplus and yminus values for confidence interval")
    } else {
      x_pos <- ifelse(add, -0.1, 0.1)
      errbar(x = bp[,1] + x_pos,
             y = var,
             yplus = yplus,
             yminus = yminus,
             add = TRUE,
             pch = NA,
             errbar.col = adjustcolor("darkred", alpha.f = 0.3))
    }
  }
}


#####
# FUNCTION FOR COMPUTING CONFIDENCE INTERVALS ON PROPORTIONS
# https://aghaynes.wordpress.com/2014/04/09/calculating-confidence-intervals-for-proportions/
#####

simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}
