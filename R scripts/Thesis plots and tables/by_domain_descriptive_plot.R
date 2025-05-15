box_plot_by_domain <- function(plot_data) {
  plot_data |> ggplot2::ggplot(aes(y = Score, fill = domain)) + 
    geom_boxplot(show.legend = FALSE) +  # Remove legend for geom_boxplot
    geom_hline(aes(yintercept = cutoff, color = "High probability of disorder"), linetype = "dashed") +  # Add legend for geom_hline
    scale_fill_viridis_d(option = "viridis") +
    facet_grid(cols = vars(domain)) +
    scale_color_manual(name = "", values = "red") +  # Customize the color and name for geom_hline
    theme(legend.position = "top",
          strip.text = element_text(angle = 45),
          axis.title.x = element_blank(),     # Remove x-axis title
          axis.text.x = element_blank(),      # Remove x-axis text
          axis.ticks.x = element_blank(),     # Remove x-axis ticks
          axis.line.x = element_blank())
}


cbcl_cutoffs <- tibble(
  domain = c(
    "cbcl_aff",
    "cbcl_anx",
    "cbcl_som",
    "cbcl_adhd",
    "cbcl_odd",
    "cbcl_con"
  ),
  cutoff = c(7, 6, NA, 6, 8, 14)
)

analysis_data <- readRDS(here(derived_data, "pooled_cbcl.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_out <- analysis_data |> filter(wave == out_wave)
cbcl_levels <- analysis_data_out |> select(starts_with("cbcl")) |> colnames()
cbcl_labels <- get_label(cbcl_levels, label_no = 3)

plot_data_out <- analysis_data_out|> mutate(study = "Overall") |> 
  pivot_longer(cols = starts_with("cbcl"), names_to = "domain", values_to = "Score") |> 
  left_join(cbcl_cutoffs) |> 
  mutate(domain = factor(domain, 
                         levels = cbcl_levels,
                         labels = cbcl_labels)) |> 
  mutate(study = factor(
    study, 
    levels = study_levels,
    labels = study_labels
  ))


cbcl_plot <- plot_data_out |> box_plot_by_domain()
ggsave(cbcl_plot, filename = file.path(thesis_plots, "Main Results",  "cbcl_out_domain_plot.png"), width = 12, height = 9, unit = "cm")

sdq_cutoffs <- sdq_cutoffs |> select(domain = outcome, cutoff)
analysis_data <- readRDS(here(derived_data, "pooled_sdq.Rds"))|> 
  filter(base_all_complete, out_all_complete) 
analysis_data_out <- analysis_data |> filter(wave == out_wave)

sdq_levels <- analysis_data_out |> select(starts_with("sdq")) |> colnames()
sdq_labels <- get_label(sdq_levels, label_no = 3)

plot_data_out_sdq <- analysis_data_out|> mutate(study = "zOverall") |> 
  pivot_longer(cols = starts_with("sdq"), names_to = "domain", values_to = "Score") |> 
  left_join(sdq_cutoffs) |> # these are saved in a csv and loaded with config
  mutate(domain = factor(domain, 
                         levels = sdq_levels,
                         labels = sdq_labels)) |> 
  mutate(study = factor(
    study, 
    levels = study_levels,
    labels = study_labels
  ))

sdq_plot <- plot_data_out_sdq |> box_plot_by_domain()

ggsave(sdq_plot, filename = file.path(thesis_plots, "Main Results",  "sdq_out_domain_plot.png"), width = 12, height = 9, unit = "cm")
