##### 
# DEFINE FUNCTION FOR GETTING SIMPLE TIME SERIES DATAFRAMES
#####
get_ts <- function(school = NULL, 
                   grade = NULL,
                   race_bi = NULL,
                   lunch = NULL,
                   dist = FALSE){
  
  # Get data
  sub_screen <- screen
  
  # Subset based on conditions
  if(!is.null(school)){
    sub_screen <- sub_screen[which(sub_screen$school == school),]}
  if(!is.null(grade)){
    sub_screen <- sub_screen[which(sub_screen$Grade == grade),]}
  if(!is.null(race_bi)){
    sub_screen <- sub_screen[which(sub_screen$race_bi == race_bi),]}
  if(!is.null(lunch)){
    sub_screen <- sub_screen[which(sub_screen$lunch == lunch),]}
  
  if(!dist){
    temp <- sub_screen %>%
      group_by(year) %>%
      summarise(Z = mean(Z),
                bmi_percentile = mean(bmi_percentile),
                normal = length(which(cat == 'normal')),
                overweight = length(which(cat == 'overweight')),
                obese = length(which(cat == 'obese')),
                n = n())
    return(temp)
  } else{
    return_list <- list()
    for (i in unique(sort(sub_screen$year))){
      return_list[[paste0('year_',as.character(i))]] <- 
        sub_screen$bmi_percentile[which(sub_screen$year == i)]
    }
    return(return_list)
  }
}



#####
# MULTIPLOT FUNCTION
#####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}