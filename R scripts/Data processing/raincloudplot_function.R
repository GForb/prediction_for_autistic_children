library(gridExtra)
give_column <- function(varname, data) {
  ind <- grep(varname, colnames(data))
  return(ind)
}

make_raincloudplot <- function(data, column, colour) {
  data <- as.data.frame(data)
  tmp <- ggplot(data, aes(x = 1.5, y = data[,column], colour = colour, na.rm = T)) + 
    labs(x = as.character(names(data[column])), y = "Value") +
    ggdist::stat_halfeye(
      colour = colour, 
      fill = colour,
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = colour) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA, 
      colour = colour
    ) +
    geom_point(
      size = 1.3,
      alpha = .3,
      colour = colour,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    coord_cartesian(xlim = c(1.2, NA), clip = "off")
  return(tmp)
}

make_raincloudplots <- function(data, columns, colour) {
  for (i in columns) {
    plt <- make_raincloudplot(data, i, colour)
    print(plt)
  }
}

#now merged with the next function - can be deleted
give_plotlist <- function(data, columns, colour) {
  plt_names <- list()
  plots <- list()
  for(i in 1:length(columns)){
    name <- paste("plot", i, sep="")
    plt_names[i] <- name
    plots[i] <- columns[i]
  }
  names(plots) <- plt_names
  plotlist <- lapply(plots, make_raincloudplot, data = data, colour = colour)
  return(plotlist)
}
#now merged with the next function - can be deleted
make_summary_plot <- function(plotlist){
  finalplot <- grid.arrange(grobs = plotlist, ncol = 6)
  return(finalplot)
}

#this works only if you install the package called gridExtra (function call inside function references to the package)
compare_raincloudplots <- function(data, columns, colour, figure_col_number){
  plt_names <- list()
  plots <- list()
  for(i in 1:length(columns)){
    name <- paste("plot", i, sep="")
    plt_names[i] <- name
    plots[i] <- columns[i]
  }
  names(plots) <- plt_names
  plotlist <- lapply(plots, make_raincloudplot, data = data, colour = colour)
  finalplot <- gridExtra::grid.arrange(grobs = plotlist, ncol = figure_col_number)
  return(finalplot)
}

compare_raincloudplots(clean_data, 6:11, "darkgreen", 3) #this does what i wanted it to do 
#suggested use: (also i can put the give_column function into the compare_raincloudplots and then we can get plots by variable namess)
compare_raincloudplots(clean_data, give_column("sdq", clean_data), "red", 6)

