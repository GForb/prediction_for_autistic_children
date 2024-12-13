library(ggplot2)
library(patchwork)

# plot play PI



plots_folder <- here::here(thesis_plots, "Main Results")


# SDQ - load data
sdq_cutoffs <- tibble(
  outcome = c(
    "sdq_cond_p",
    "sdq_emot_p",
    "sdq_hyp_p",
    "sdq_peer_p",
    "sdq_pro_p"
  ),
  cutoff = c(4, 5, 8, 4, 6),
  max = 10,
  min = 0
) |> 
  mutate(outcome_label = get_label(outcome, label_no = 3))
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")

analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete, autism != "post baseline")
analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) 

pi_data_sdq <- get_pi_data(analysis_spec, data = analysis_data_wide, minmax_values = sdq_cutoffs |> select(outcome, min, max)) |> 

pi_data_sdq <- pi_data_sdq |> mutate(round_base = round(base_spline1))

# Getting CBCL data
cbcl_cutoffs <- tibble(
  outcome = c(
    "cbcl_aff",
    "cbcl_anx",
    "cbcl_som",
    "cbcl_adhd",
    "cbcl_odd",
    "cbcl_con"
  ),
  cutoff = c(7, 6, NA, 6, 8, 14),
  n_items = c(13, 6, 7, 7, 5, 17),
  max = n_items * 2,
  min = 0
) |> 
  mutate(outcome_label = get_label(outcome, label_no = 3))
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete)
analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) 

pi_data_cbcl <- get_pi_data(analysis_spec, data = analysis_data_wide, minmax_values = cbcl_cutoffs |> select(outcome, min, max))

pi_data_cbcl <- pi_data_cbcl |> mutate(round_base = round(base_spline1))


plot_data <- pi_data_sdq |> 
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
freq_data <- pi_data_sdq |> 
  count(outcome, round_base) %>%
  mutate(lower = 0, upper = n/N*5, pred = NA, panel = "zFreq.",
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
  facet_grid(panel ~ outcome, scales = "free_y", switch = "y", space = "free_y") +
  scale_fill_brewer(type = "seq", labels = c("95%", "90%", "75%", "50%")) +
  labs(y = NULL, x = "Baseline outcome", fill = "Prediction Interval Level") +
  theme(
    strip.placement = "outside",
    strip.background.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot

