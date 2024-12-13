library(ggplot2)
library(patchwork)

# plot play PI



plots_folder <- here::here(thesis_plots, "Main Results")

# Loading data
pi_data_sdq <- readRDS(here(derived_data, "pi_data_sdq.Rds"))
pi_data_sdq <- pi_data_sdq |> mutate(round_base = round(base_spline1))

pi_data_cbcl <-  readRDS(here(derived_data, "pi_data_cbcl.Rds"))
pi_data_cbcl <- pi_data_cbcl |> mutate(round_base = round(base_spline1))

pi_data_vabs <-  readRDS(here(derived_data, "pi_data_vabs.Rds"))
pi_data_vabs <- pi_data_vabs |> mutate(round_base = round(base_spline1))

# Pick which dataset to use
pi_data <- pi_data_cbcl

plot_data <- pi_data |> 
  mutate(
         outcome = get_label(outcome, label_no = 3)) |> 
  select(outcome, round_base, pred, ID, starts_with("pi_lower"), starts_with("pi_upper"))  |> 
  pivot_longer(cols = -c("ID", "round_base", "outcome"), names_to = "what", values_to = "value") |>
  group_by(round_base, what, outcome) |>
  summarise(mean = mean(value)) |>
  pivot_wider(names_from = what, values_from = mean) 

# Reshape data to add alpha level as a variable and define transparency
plot_data_long <- plot_data |> 
  pivot_longer(
    cols = starts_with("pi_"),
    names_to = c(".value", "alpha"),
    names_pattern = "pi_(lower|upper)_(.*)"
  ) 
# Convert alpha to a factor
plot_data_long$alpha <- factor(plot_data_long$alpha, levels = c("0.05", "0.1", "0.25", "0.5"))


# Your main plot
main_plot <- ggplot(plot_data_long, aes(x = round_base)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = alpha)) +
  scale_fill_brewer(type = "seq", labels = c("95%", "90%", "75%", "50%")) +
  geom_line(aes(y = pred), color = "black", size = 1) +
  labs(y = "Prediction", x = "Baseline outcome", fill = "Prediction Interval Level") +
  facet_wrap(vars(outcome), nrow = 2, scales = "free")  # Adjust heights as needed

# Frequency plot
freq_plot <- ggplot(plot_data_long, aes(x = round_base)) +
  geom_bar() +
  facet_wrap(vars(outcome), nrow = 2, scales = "free") +
  labs(y = "Frequency", x = NULL) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),  # Removes facet labels in the frequency plot
    axis.text.x = element_text(angle = 45, hjust = 1)  # Adjusts x-axis text if needed
  )

# Combine the plots using patchwork
combined_plot <- main_plot / freq_plot +
  plot_layout(heights = c(3, 1)) 

# Display the combined plot


library(ggplot2)
library(dplyr)

N = length(unique(pi_data_sdq$ID))
# Prepare frequency data
freq_data <- pi_data |> 
  count(outcome, round_base) %>%
  mutate(lower = 0, upper = n/N*10, pred = NA, panel = "zFreq.",
    outcome = get_label(outcome, label_no = 3)) 

# Prepare main plot data with a similar structure
main_data <- plot_data_long %>%
  mutate(panel = "Prediction")
         

# Combine main and frequency data
combined_data <- bind_rows(main_data, freq_data) 
# Plot
plot <- ggplot(combined_data, aes(x = round_base)) +
  geom_ribbon(data = . %>% filter(panel == "Prediction"), 
              aes(ymin = lower, ymax = upper, fill = alpha), show.legend = TRUE) +
  geom_line(data = . %>% filter(panel == "Prediction"), 
            aes(y = pred), color = "black", size = 1) +
  geom_bar(data = . %>% filter(panel == "zFreq."), 
           aes(y = upper), stat = "identity", fill = "grey50", color = "black") +
  facet_grid(panel ~ outcome, scales = "free", switch = "y", space = "free_y") +
  scale_fill_brewer(type = "seq", labels = c("95%", "90%", "75%", "50%")) +
  labs(y = NULL, x = "Baseline outcome", fill = "Prediction Interval Level") +
  theme(
    strip.placement = "outside",
    strip.background.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot

