# Load required packages
library(ggplot2)
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")

data_mt <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds")) |> 
  filter(model == "mt_ri_study_rs", intercept_est_method == "estimate") |> 
  mutate(label = get_label(outcome , label_no = 2),
         position = factor(label) |> as.numeric())

data_st <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds")) |> 
  filter(model == "st_ri_study", intercept_est_method == "estimate") |> 
  mutate(label = get_label(outcome , label_no = 2),
         position = factor(label) |> as.numeric())

data_mt |> filter(metric == "r_squared") |> plot_many_ma(my_colour = "black") + xlab("R squared") + ylab("Outcome")
data_mt |> filter(metric == "calib_slope") |> plot_many_ma(my_colour = "black") + xlab("Calibration Slope") + ylab("Outcome")
data_mt |> filter(metric == "calib_itl") |> plot_many_ma() + xlab("Calibration In the Large") + ylab("Outcome")



data_st |> filter(metric == "r_squared") |> plot_many_ma(my_colour = "turquoise") + xlab("R squared") + ylab("Outcome")
data_st |> filter(metric == "calib_slope") |> plot_many_ma(my_colour = "black") + xlab("Calibration Slope") + ylab("Outcome")
data_st |> filter(metric == "calib_itl") |> plot_many_ma() + xlab("Calibration In the Large") + ylab("Outcome")




# Function to create the diamond shape with adjustable height
create_diamond <- function(position, ci.lb, ci.ub, height = 0.1) {
  data.frame(
    y = c(position, position - height, position, position + height),
    x = c(ci.ub, (ci.lb + ci.ub) / 2, ci.lb, (ci.lb + ci.ub) / 2)
  )
}

# Height of the diamonds
diamond_height <- 0.05

# Create a new data frame for the diamond shapes
diamond_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
  diamond <- create_diamond(data$position[i], data$ci.lb[i], data$ci.ub[i], height = diamond_height)
  diamond$group <- i
  diamond
}))

my_colour <- "black"

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
