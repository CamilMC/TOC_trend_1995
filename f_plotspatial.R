# Convinience function for plotting fennoscandian waterchemistry data
# and results using ggplot2 

f_plotspatial <- function(data,var,plottitle,col){
  require(ggplot2)
  pop <- ggplot(data) + 
    aes(Longitude,Latitude,var) + 
    geom_point(aes(colour=var)) + 
    ggtitle(plottitle) 
  pop + 
    theme(axis.line = element_line(colour = "black"),
          legend.title=element_blank(),
          panel.background=element_blank(), 
          panel.grid.major = element_blank()) + 
    scale_x_discrete(breaks=NULL) + 
    scale_y_discrete(breaks=NULL) + 
    scale_colour_gradient2(low="#4575B4",mid="#FFFFBF",high = "#D73027",midpoint = 0) + 
    borders(database = "world", regions = c("Norway","Sweden","Finland"), 
            fill = NA, colour = "grey50",xlim=c(0,35),ylim=c(55,73)) + 
    xlab("") + ylab("") 
}
