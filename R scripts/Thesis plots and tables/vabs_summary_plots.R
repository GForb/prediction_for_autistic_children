analysis_data <- readRDS(here(derived_data, "pooled_vabs.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)

vabs_levels <- c("vabs_com_ae", "vabs_dls_ae", "vabs_soc_ae")
vabs_labels <- c("Communication", "Daily Living Skills", "Socialisation")

study_levels <- c("EDX", "ELENA", "EpiTED", "Pathways", "zOverall")
study_labels <- c("EDX", "ELENA", "EpiTED", "Pathways", "Overall")

plot_data_base <- analysis_data_base |> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_base) |> select(ID, study, starts_with("sdq")) |> 
  pivot_longer(cols = starts_with("sdq"), names_to = "domain", values_to = "Score") |> 
  left_join(sdq_cutoffs) |> 
  mutate(domain = factor(domain, 
                         levels = sdq_levels,
                         labels = sdq_labels)) |> 
  mutate(study = factor(
    study, 
    levels = study_levels,
    labels = study_labels
  ))

plot_data_out <- analysis_data_out|> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_out) |> select(ID, study, starts_with("sdq")) |> 
  pivot_longer(cols = starts_with("sdq"), names_to = "domain", values_to = "Score") |> 
  left_join(sdq_cutoffs) |> 
  mutate(domain = factor(domain, 
                         levels = sdq_levels,
                         labels = sdq_labels)) |> 
  mutate(study = factor(
    study, 
    levels = study_levels,
    labels = study_labels
  ))

analysis_data_ages <- analysis_data_wide |> select(ID, study, base_age, out_age, fu_length) 

plot_data_ages <- analysis_data_ages|> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_ages)|> 
  pivot_longer(c(base_age, out_age, fu_length), names_to = "domain", values_to = "Score") |> 
  mutate(domain = factor(
    domain,
    levels = c("base_age", "out_age", "fu_length"),
    labels = c("Age at Baseline", "Age at Outcome", "Follow-up Length")),
    study = factor(
      study, 
      levels = study_levels,
      labels = study_labels
    ))

box_plot_by_study_domain <- function(plot_data) {
  plot_data |> ggplot2::ggplot(aes(y = Score, fill = study)) + 
    geom_boxplot(show.legend = FALSE) +  # Remove legend for geom_boxplot
    geom_hline(aes(yintercept = cutoff, color = "High or Very High (low or very low for Pro-social domain)"), linetype = "dashed") +  # Add legend for geom_hline
    scale_fill_viridis_d(option = "viridis") +
    facet_grid(rows = vars(domain), cols = vars(study)) +
    scale_color_manual(name = "", values = "red") +  # Customize the color and name for geom_hline
    theme(legend.position = "top",
          strip.text = element_text(angle = 45),
          axis.title.x = element_blank(),     # Remove x-axis title
          axis.text.x = element_blank(),      # Remove x-axis text
          axis.ticks.x = element_blank(),     # Remove x-axis ticks
          axis.line.x = element_blank())
}

sdq_base_plot <- plot_data_base |> box_plot_by_study_domain()
sdq_base_plot

sdq_out_plot <- plot_data_out |> box_plot_by_study_domain()

ages_plot <- plot_data_ages |>  ggplot2::ggplot(aes(y = Score, fill = study)) + 
  geom_boxplot(show.legend = FALSE) +  # Remove legend for geom_boxplot
  scale_fill_viridis_d(option = "viridis") +
  facet_grid(rows = vars(domain), cols = vars(study), scales = "free_y") +
  theme(legend.position = "top",
        strip.text = element_text(angle = 45),
        axis.title.x = element_blank(),     # Remove x-axis title
        axis.text.x = element_blank(),      # Remove x-axis text
        axis.ticks.x = element_blank(),     # Remove x-axis ticks
        axis.line.x = element_blank()) +
  ylab("Years")

ggsave(sdq_base_plot, filename = file.path(thesis_plots, "Main Results",  "vabs_base_plot.png"), width = 14, height = 18, unit = "cm")
ggsave(sdq_out_plot, filename = file.path(thesis_plots, "Main Results",  "vabs_out_plot.png"), width = 14, height = 18, unit = "cm")
ggsave(ages_plot, filename = file.path(thesis_plots, "Main Results",  "vabs_ages_plot.png"), width = 14, height = 9, unit = "cm")

ggsave(ages_plot, filename = file.path(thesis_plots, "Main Results",  "vabs_ages_plot_conf.png"), width = 24, height = 13, unit = "cm")

