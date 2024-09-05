analysis_data <- readRDS(here(derived_data, "pooled_sdq.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline") 

analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline") 

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)

sdq_levels <- c("sdq_cond_p", "sdq_emot_p", "sdq_hyp_p", "sdq_peer_p", "sdq_pro_p")
sdq_labels <- c("Conduct", "Emotional", "Hyperactivity", "Peer", "Pro-social")

study_levels <- c("ALSPAC", "GUI", "MCS", "Quest", "SNAP", "TEDS", "k_families", "lsac_b", "lsac_k", "zOverall")
study_labels <- c("ALSPAC", "GUI", "MCS", "Quest", "SNAP", "TEDS", "1K Fam", "LSAC B", "LSAC K", "Overall")
  
# cutoffs taken from ...  https://www.sciencedirect.com/science/article/pii/S0890856709605438
sdq_cutoffs <- tibble(domain = sdq_levels, 
                      cutoff = c(4, 5, 8, 4, 6))

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
  mutate(
    study = factor(
      study, 
      levels = study_levels,
      labels = study_labels
    ),
  cutoff = NA) |> 
  pivot_longer(c(base_age, out_age, fu_length), names_to = "domain", values_to = "Score") |> 
  mutate(domain = factor(
    domain,
    levels = c("base_age", "out_age", "fu_length"),
    labels = c("Age at Baseline", "Age at Outcome", "Follow-up Length")))

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

ggsave(sdq_base_plot, filename = file.path(thesis_plots, "Main Results",  "sdq_base_plot.png"), width = 14, height = 18, unit = "cm")
ggsave(sdq_out_plot, filename = file.path(thesis_plots, "Main Results",  "sdq_out_plot.png"), width = 14, height = 18, unit = "cm")
ggsave(ages_plot, filename = file.path(thesis_plots, "Main Results",  "sdq_ages_plot.png"), width = 14, height = 9, unit = "cm")


