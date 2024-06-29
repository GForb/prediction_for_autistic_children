# Load necessary library
library(ggplot2)

# Define the range of x values
x_values <- seq(-1, 0.999, length.out = 1000)

# Calculate y values for the function -log(1-x)
y_values <- -log(1 - x_values)

fisher_z_values <- DescTools::FisherZ(x_values)

# Create a data frame for plotting
data <- data.frame(x = x_values, y = y_values, z = fisher_z_values)

# Plot the function using ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_line(aes(x = x, y = z), color = "red") +
  labs(title = "Plot of -log(1-x)",
       x = "x",
       y = "-log(1-x)") +
  theme_minimal() +
  xlim(-1, 1) +
  ylim(min(y_values), max(y_values))
