# Convinience function for plotting fennoscandian waterchemistry data
# and results using ggplot2 

f_plotspatial <- function(data,var,plottitle,lim = c(min(var),max(var)),midpoint = median(var)){
  require(ggplot2)
  pop <- ggplot(data) + 
    aes(longitude,latitude) + 
    geom_point(aes(colour=var)) + 
    ggtitle(plottitle) 
  pop <- pop + 
    theme(axis.line = element_line(colour = "black"),
          legend.title=element_blank(),
          panel.background=element_blank(), 
          panel.grid.major = element_blank()) + 
    scale_x_discrete(breaks=NULL) + 
    scale_y_discrete(breaks=NULL) + 
    scale_colour_gradient2(low="#4575B4",mid="#FFFFBF",high = "#D73027",midpoint = midpoint,limits=lim) + 
    borders(database = "world", regions = c("Norway","Sweden","Finland"), 
            fill = NA, colour = "grey50",xlim=c(0,35),ylim=c(55,73)) + 
    xlab("") + ylab("") +
    theme_void()
  return(pop)
}
