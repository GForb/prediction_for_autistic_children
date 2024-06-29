plot_many_ma <- function(data, my_colour = "black", diamond_height = 0.05) {
  
  diamond_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
    diamond <- create_diamond(data$position[i], data$ci.lb[i], data$ci.ub[i], height = diamond_height)
    diamond$group <- i
    diamond$metric <- data$metric[i]
    diamond
  }))
  
  
  # Create the plot
  ggplot() +
    # Add prediction interval whiskers
    geom_segment(data = data, aes(y = position, yend = position, x = pi.lb, xend = pi.ub),
                 linetype = "dotted", linewidth = 1, color = my_colour) +
    # Add caps to the prediction intervals
    geom_segment(data = data, aes(y = position - diamond_height / 2, yend = position + diamond_height / 2, x = pi.lb, xend = pi.lb),
                 linewidth = 1, color = my_colour) +
    geom_segment(data = data, aes(y = position - diamond_height / 2, yend = position + diamond_height / 2, x = pi.ub, xend = pi.ub),
                 linewidth = 1, color = my_colour) +
    # Add diamonds for confidence intervals
    geom_polygon(data = diamond_data, aes(x = x, y = y, group = group), fill = my_colour) +
    # Labels and theme adjustments
    labs(x = "Value", y = "Label") +
    theme_minimal() +
    scale_y_continuous(breaks = data$position, labels = data$label)
}


create_diamond <- function(position, ci.lb, ci.ub, height = 0.1) {
  data.frame(
    y = c(position, position - height, position, position + height),
    x = c(ci.ub, (ci.lb + ci.ub) / 2, ci.lb, (ci.lb + ci.ub) / 2)
  )
}



