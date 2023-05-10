library(patchwork)
library(cowplot)
library(gridExtra)
library(grid)

set_labs <- function(variable, metadata){
  #returns a list with a minimum an maximum value 
  var <- metadata |> 
    filter(starts_with == variable)
  minimum <- var[, 2]
  maximum <- var[, 3]
  return(c(minimum, maximum))
}

make_raincloudplot <- function(column, col_label, colour, ylims = NULL, x_col = NULL) {
  if(is.null(x_col)){
    x = 1.5
  } else {
    x = x_col
  }
  data = tibble(x, column)
  x_name = colnames(data)[1]
  y_name = colnames(data)[2]
  
    tmp <- ggplot(data, aes(
                            x = .data[[x_name]], 
                            y = .data[[y_name]], 
                            group = .data[[x_name]],  
                            colour = colour, 
                            na.rm = T)
                            ) + 
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
        width = 0.25, 
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

combined_raincloudplot_wave<- function(data, variable, colour, metadata) {
  raincloud_plots <- list()
  plotlist <- make_raincloudplot_wave(data, variable, label_var(variable, "label1", metadata = metadata), colour, var_metadata = metadata)
  wave <- max(data$wave)
  for(i in 1:length(plotlist)){
    raincloud_plots[[i]] <- plotlist[[i]][[1]]
  }
  for (i in 1:length(raincloud_plots)) {
    raincloud_plots[[i]] <- raincloud_plots[[i]] + theme_void()
  }
  
  # Combine the raincloud plots
  combined_plot <- do.call(grid.arrange, c(raincloud_plots, ncol = length(raincloud_plots)))
  
  # Create a tableGrob for Y-axis tick labels
  y_axis_labels <- tableGrob(data.frame(y = 1:40),
                             theme = ttheme_default(base_size = 14),
                             rows = NULL, cols = NULL)
  
  # Set the height of the tableGrob to match the combined plot
  height <- combined_plot$heights
  
  # Combine the combined plot and Y-axis tick labels
  combined_plot_with_y_axis <- arrangeGrob(y_axis_labels, combined_plot,
                                           heights = unit.c(unit(1, "line"), height))
  
  # Display the combined plot with Y-axis tick labels
  grid.newpage()
  grid.draw(combined_plot_with_y_axis)
  
  return(combined_plot_with_y_axis)
}

combined_raincloudplot_wave2 <- function(data, variable, col_label, colour, metadata=NULL) {
  if (is.null(var_metadata)) {
    ylims <- NULL
  } else {
    ylims <- find_min_max(variable, var_metadata)
  }
  
  
  data <- data[, c(variable, "wave")] |> 
    mutate(
      wave_factor = factor(wave, ordered = TRUE),
      x = as.integer(wave_factor)*2-0.5
    )
  
  plot <- make_raincloudplot(data[[variable]],col_label , colour, ylims = ylims, x_col = data["x"])
  
  return(plot)
}


  


