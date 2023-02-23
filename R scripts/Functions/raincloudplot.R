library(gridExtra)
library(ggdist)
library(ggthemes)
library(gifski)



make_raincloudplot <- function(column, col_label, colour) {
  data = tibble(column)
  col_name = colnames(data)
  tmp <- ggplot(data, aes(x = 1.5, y = .data[[col_name]],  colour = colour, na.rm = T)) + 
    labs(x = as.character(col_label), y = "Value") +
    ggdist::stat_halfeye(
      colour = colour, 
      fill = colour,
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
        seed = 1, width = .1 ,height =0.15
      )
    ) + 
    coord_cartesian(xlim = c(1.2, NA), clip = "off") +
    ggthemes::theme_few() +
    theme( title = element_text(), axis.title = element_text(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text())
  return(tmp)
}

make_raincloudplots <- function(data, colour) {
  plotlist <- map2(data, colnames(data), make_raincloudplot, colour = colour)
  return(plotlist)
}


#this works only if you install the package called gridExtra (function call inside function references to the package)
compare_raincloudplots <- function(data, colour, ncol_in_figure){
  plotlist <- make_raincloudplots(data, colour)
  finalplot <- gridExtra::grid.arrange(grobs = plotlist, ncol = ncol_in_figure)
  return(finalplot)
}


divide_by_wave <- function(data, n_o_waves){
  data_wave <- list()
  for (i in 1:n_o_waves) {
    subseted <- data |> 
      filter(wave == i)
    data_wave[i] <- list(subseted)
  }
  return(data_wave)
}


make_raincloudplot_wave <- function(data, n_o_waves, as_string_column, col_label, colour){
  data_wave <- divide_by_wave(data, n_o_waves)
  raincloud_wave <- lapply(data_wave, function(x){
    lapply(x[as_string_column], make_raincloudplot, col_label = col_label, colour = colour)
  })
  return(raincloud_wave)
}



#y <- make_raincloudplot_wave(gui_data, 3, "sdq_hyp_p", "Hyperactive", "pink")
#y


