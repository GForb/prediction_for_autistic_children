plot_many_ma_by_metric <- function(data, outcome = NULL, my_colour = "black", diamond_height = 0.1, vline_data = NULL, vline_pi = FALSE, rmse_stand = FALSE) {
  myOutcome = outcome
  if(is.null(outcome)) {
    myOutcome = data |> pull(outcome) |> unique()
  }

  
  plot_data <- data |> filter(outcome %in% myOutcome) |>  
    mutate(metric = factor(
      metric, 
      levels = c("rmse", "rmse_stand", "r_squared_transformed", "calib_itl", "calib_slope"), 
      labels = c("RMSE", "Standardised \n RMSE", "R-squared", "Calibration \n in the Large", "Calibration \n Slope")))
  
 
  if(is.null(vline_data)){
    metric  <-  plot_data$metric |> unique()
    if(rmse_stand){
      vline_x <-  c(1, 0,
                    data |> filter(metric == "r_squared_transformed", outcome %in% myOutcome) |> pull(est) |> max(na.rm  = TRUE),
                    data |> filter(metric == "rmse_stand", outcome %in% myOutcome) |> pull(est) |> min(na.rm  = TRUE))
    } else {
      vline_x <-  c(1, 0,
                    data |> filter(metric == "r_squared_transformed", outcome %in% myOutcome) |> pull(est) |> max(na.rm  = TRUE),
                    data |> filter(metric == "rmse", outcome %in% myOutcome) |> pull(est) |> min(na.rm  = TRUE))
    }
    vline_data <- tibble(metric = metric, vline_x = vline_x) 
  }

  print(plot_data |> filter(metric == "Standardised \n RMSE") |> select(label, position, metric, est, starts_with("ci"), starts_with("pi")))
  plot <- plot_data |>
    plot_many_ma(diamond_height = diamond_height, my_colour = my_colour) + 
    facet_grid(cols = vars(metric), scales = "free_x") + 
    geom_vline(data = vline_data, aes(xintercept = vline_x), linetype = "dashed", color = "red") +
    ggtitle(get_label(myOutcome, label_no = 2)) 
  
  
  if(vline_pi){
    plot <- plot + 
      geom_vline(data = vline_data, aes(xintercept = vline_lb), linetype = "dotted", color = "black") + 
      geom_vline(data = vline_data, aes(xintercept = vline_ub), linetype = "dotted", color = "black") 
    
  }
  
  return(plot)
}

plot_many_ma_by_outcome <- function(data, my_colour = "black", diamond_height = 0.1, vline_data = NULL, vline_pi = FALSE, scales = "free_x") {
  
  outcomes <- data |> pull(outcome) |> unique()
  names(outcomes) <- NULL
  outcome_max <- data |> group_by(outcome) |> summarise(vline_x = max(est, na.rm = TRUE))  
  outcome_labels <- outcomes |> get_label(label_no = 3)
  

  
  plot_data <- data |> 
    mutate(outcome = factor(get_label(outcome, label_no = 3)))
  
  if(is.null(vline_data)){
    vline_data <- tibble(outcome_label = outcome_labels,
                         outcome = outcomes) |> 
      left_join(outcome_max) |> 
      select(-outcome, outcome = outcome_label)
  }
 


  plot <- plot_data |>
    plot_many_ma(diamond_height = diamond_height, my_colour = my_colour) + 
    facet_grid(cols = vars(outcome), scales = scales) + 
    geom_vline(data = vline_data, aes(xintercept = vline_x), linetype = "dashed", color = "red") 
  
  if(vline_pi){
    plot <- plot + 
      geom_vline(data = vline_data, aes(xintercept = vline_lb), linetype = "dotted", color = "black") + 
      geom_vline(data = vline_data, aes(xintercept = vline_ub), linetype = "dotted", color = "black") 
    
  }
  
  return(plot)
}



plot_many_ma <- function(data, my_colour = "black", diamond_height = 0.05) {
  
  # Create diamond data
  diamond_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
    diamond <- create_diamond(data$position[i], data$ci.lb[i], data$ci.ub[i], data$est[i], height = diamond_height)
    diamond$group <- i
    diamond$metric <- data$metric[i]
    diamond$outcome <- data$outcome[i]
    diamond
  }))
  

  # Create the plot
  ggplot() +
    # Add prediction interval whiskers
    geom_segment(data = data, aes(y = position, yend = position, x = pi.lb, xend = pi.ub),
                 linetype = "dotted", linewidth = 1, color = my_colour) +
    # Add caps to the prediction intervals
    geom_segment(data = data, aes(y = position - diamond_height *0.75, yend = position + diamond_height *0.75, x = pi.lb, xend = pi.lb),
                 linewidth = 1, color = my_colour) +
    geom_segment(data = data, aes(y = position - diamond_height *0.75, yend = position + diamond_height *0.75, x = pi.ub, xend = pi.ub),
                 linewidth = 1, color = my_colour) +
    # Add diamonds for confidence intervals
    geom_polygon(data = diamond_data, aes(x = x, y = y, group = group), fill = my_colour) +
    # Labels and theme adjustments
    labs(x = "", y = "") +
    scale_y_continuous(breaks = data$position, labels = data$label)
}

create_diamond <- function(position, ci.lb, ci.ub, est, height = 0.1) {
  data.frame(
    y = c(position, position - height, position, position + height),
    x = c(ci.ub,est, ci.lb, est)
  )
}

plot_many_ma_multi_out <- function(data, my_colour = "black", diamond_height = 0.05) {
  
  # Create diamond data
  diamond_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
    diamond <- create_diamond(data$position[i], data$ci.lb[i], data$ci.ub[i], data$est[i], height = diamond_height)
    diamond$group <- i
    diamond$metric <- data$metric[i]
    diamond$outcome <- data$outcome[i]
    diamond$domain <- data$domain[i]
    diamond$scale <- data$scale[i]
    diamond
  }))
  
  print(diamond_data)
  
  
  # Create the plot
  ggplot(data = diamond_data, aes(y = position, colour = scale, fill = scale)) +
    # Add prediction interval whiskers
    geom_segment(data = data, aes(y = position, yend = position, x = pi.lb, xend = pi.ub),
                 linetype = "dotted", linewidth = 1) +
    # Add caps to the prediction intervals
    geom_segment(data = data, aes(y = position - diamond_height *0.75, yend = position + diamond_height *0.75, x = pi.lb, xend = pi.lb),
                 linewidth = 1) +
    geom_segment(data = data, aes(y = position - diamond_height *0.75, yend = position + diamond_height *0.75, x = pi.ub, xend = pi.ub),
                 linewidth = 1) +
    # Add diamonds for confidence intervals
    geom_polygon(data = diamond_data, aes(x = x, y = y, group = group)) +
    # Labels and theme adjustments
    labs(x = "", y = "") +
    scale_y_continuous(breaks = data$position, labels = data$label)
}

create_diamond <- function(position, ci.lb, ci.ub, est, height = 0.1) {
  data.frame(
    y = c(position, position - height, position, position + height),
    x = c(ci.ub,est, ci.lb, est)
  )
}
