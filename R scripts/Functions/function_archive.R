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


#----------------------------------------------------
#----------------------------------------------------
  

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


#---------------------------------------------------
#---------------------------------------------------




