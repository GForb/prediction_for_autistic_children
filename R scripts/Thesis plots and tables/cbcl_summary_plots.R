

analysis_data <- readRDS(here(derived_data, "pooled_cbcl.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)
cbcl_levels <- analysis_data_out |> select(starts_with("cbcl")) |> colnames()
cbcl_labels <- get_label(cbcl_levels, label_no = 3)

study_levels <- analysis_data_out |> pull(study) |> unique() |> c("zOverall")
study_labels <- analysis_data_out |> pull(study) |> unique() |> c("Overall")

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
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



plot_data_base <- analysis_data_base |> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_base) |> select(ID, study, starts_with("cbcl")) |> 
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

plot_data_out <- analysis_data_out|> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_out) |> select(ID, study, starts_with("cbcl")) |> 
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

analysis_data_ages <- analysis_data_wide |> select(ID, study, base_age, out_age, fu_length) 

plot_data_ages <- analysis_data_ages|> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_ages)|> 
  pivot_longer(c(base_age, out_age, fu_length), names_to = "domain", values_to = "Score") |> 
  mutate(domain = factor(
    domain,
    levels = c("base_age", "out_age", "fu_length"),
    labels = c("Base", "Out", "Flw-up")),
    study = factor(
      study, 
      levels = study_levels,
      labels = study_labels
    ))

box_plot_by_study_domain <- function(plot_data) {
  plot_data |> ggplot2::ggplot(aes(y = Score, fill = study)) + 
    geom_boxplot(show.legend = FALSE) +  # Remove legend for geom_boxplot
    geom_hline(aes(yintercept = cutoff, color = "High probability of disorder"), linetype = "dashed") +  # Add legend for geom_hline
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

cbcl_base_plot <- plot_data_base |> box_plot_by_study_domain()
cbcl_base_plot

cbcl_out_plot <- plot_data_out |> box_plot_by_study_domain()

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
  ylab("Years") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))

ggsave(cbcl_base_plot, filename = file.path(thesis_plots, "Main Results",  "cbcl_base_plot.png"), width = 14, height = 18, unit = "cm")
ggsave(cbcl_out_plot, filename = file.path(thesis_plots, "Main Results",  "cbcl_out_plot.png"), width = 14, height = 18, unit = "cm")
ggsave(ages_plot, filename = file.path(thesis_plots, "Main Results",  "cbcl_ages_plot.png"), width = 14, height = 6, unit = "cm")

ggsave(ages_plot, filename = file.path(thesis_plots, "Main Results",  "cbcl_ages_plot_conf.png"), width = 24, height = 13, unit = "cm")

