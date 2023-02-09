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
    tmp <- make_raincloudplot(data, i, colour)
    print(tmp)
  }
}

make_raincloudplots(lsac_wave_1, give_column("sdq", gui_wave_1), "deeppink") #this call works now
