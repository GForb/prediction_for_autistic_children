library(gridExtra)
library(ggdist)
library(ggthemes)
library(gifski)

set_labs <- function(variable, metadata){
  #returns a list with a minimum an maximum value 
  var <- metadata |> 
    filter(starts_with == variable)
  minimum <- var[, 2]
  maximum <- var[, 3]
  return(c(minimum, maximum))
}

make_raincloudplot <- function(column, col_label, colour, ylims = NULL) {
  data = tibble(column)
  col_name = colnames(data)
  tmp <- ggplot(data, aes(x = 1.5, y = .data[[col_name]],  colour = colour, na.rm = T)) + 
    #ylim(limz[[1]], limz[[2]])+
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
  if(!is.null(ylims)){
    tmp <- tmp + ylim(ylims)
  }
  
  return(tmp)
}

make_raincloudplots <- function(data, colour, var_metadata = NULL) {
  variables <- colnames(data)
  if(is.null(var_metadata)){
    ylims <- rep(NULL, length(ylims))
  }else{
    ylims <- lapply(variables, find_min_max, var_metadata)
  }

  plot_args <- tibble::tibble(column = as.list(data), 
                              col_label = variables, 
                              ylims = ylims)
  

  plotlist <- pmap(plot_args, make_raincloudplot, colour = colour)
  return(plotlist)
}


#this works only if you install the package called gridExtra (function call inside function references to the package)
compare_raincloudplots <- function(data, colour, ncol_in_figure){
  plotlist <- make_raincloudplots(data, colour)
  finalplot <- gridExtra::grid.arrange(grobs = plotlist, ncol = ncol_in_figure)
  return(finalplot)
}



make_raincloudplot_wave <- function(data, as_string_column, col_label, colour, var_metadata = NULL){

  data_wave <- divide_by_wave(data)
  
  if (is.null(var_metadata)) {
    ylims <- NULL
  } else {
    ylims <- find_min_max(as_string_column, var_metadata)
  }
  raincloud_wave <- lapply(data_wave, 
                           function(x){
    lapply(x[as_string_column], 
           make_raincloudplot, 
           col_label = paste(col_label, "Wave", unique(x$wave)), 
           colour = colour, 
           ylims = ylims)
  })
  return(raincloud_wave)
}




