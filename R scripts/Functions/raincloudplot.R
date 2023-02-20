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
    #ggthemes::theme_fivethirtyeight() 
    theme(axis.title = element_text(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
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



# make_animated_plot <- function(study_data, column, col_label, color){
#   helper_make_raincloudplot <- function(study_data, column, col_label, colour) {
#     data = study_data |> 
#       select(ID, wave, column)
#     col_name = colnames(data[,3])
#     tmp <- ggplot(data, aes(x = 1.5, y = .data[[col_name]],  colour = colour, na.rm = T)) + 
#       labs(x = as.character(col_label), y = "Value") +
#       ggdist::stat_halfeye(
#         colour = colour,
#         fill = colour,
#         width = .6,
#         .width = 0,
#         justification = -.3,
#         point_colour = colour) +
#       geom_boxplot(
#         width = .25,
#         outlier.shape = NA,
#         colour = colour
#       ) +
#       geom_point(
#         size = 1.3,
#         alpha = .3,
#         colour = colour,
#         position = position_jitter(
#           seed = 1, width = .1 ,height =0.15
#         )
#       ) + 
#       coord_cartesian(xlim = c(1.2, NA), clip = "off") +
#       #ggthemes::theme_fivethirtyeight() 
#       theme(axis.title = element_text(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
#     return(tmp)
#   }
#   plot <- helper_make_raincloudplot(study_data, column, col_label, color)
#   print(plot)
#   animated.plot <- plot + 
#     gganimate::transition_time(study_data$wave) + 
#     labs(subtitle = "Wave: {frame_time}")
#   return(gganimate::animate(animated.plot, renderer = gifski_renderer(), height = 500, width = 800, fps = 20, duration = 10, end_pause = 90, res = 100))
# }

divide_by_wave <- function(data, n_o_waves){
  data_wave <- list()
  for (i in 1:n_o_waves) {
    subseted <- data |> 
      filter(wave == i)
    data_wave[i] <- list(subseted)
  }
  return(data_wave)
}
#workinprogress


# make_gif <- function(data, n_o_waves, column, col_label, colour){
#   data_wave <- divide_by_wave(data, n_o_waves)
#   plots <- list()
#   for(i in 1:n_o_waves){
#     present_data <- data_wave[[i]]
#     print(present_data)
#     column_sel <- magrittr::extract(column)
#     print(column_sel)
#     rc <- make_raincloudplot(column_sel, col_label, colour)
#     rc <- rc +
#       ggplot2::ggtitle(paste("Wave", i))
#     plots[i] <- rc
#   }
#   return(plots)
# }


make_gif2 <- function(data, n_o_waves, as_string_column, col_label, colour){
  data_wave <- divide_by_wave(data, n_o_waves)
  lapply(data_wave[as_string_column], print)
  raincloud_wave <- lapply(data_wave, function(x){
    lapply(x[as_string_column], make_raincloudplot, col_label = col_label, colour = colour)
  })
  return(raincloud_wave)
}

y <- make_gif2(gui_data, 3, "sdq_hyp_p", "Emotional", "pink")
y
#make_gif(gui_data, 3, gui_data$sdq_hyp_p, "hyper", "green")
#make_animated_plot(gui_data, gui_data$sdq_emot_p, "SDQ Emotional", "red")
