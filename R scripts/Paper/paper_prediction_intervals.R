library(patchwork)

plots_folder <- here::here(here(outputs, "Paper"))

pi_data_sdq <- readRDS(here(derived_data, "pi_data_sdq.Rds"))

sdq_plot <- make_pi_plot_paper(pi_data_sdq, cutoffs = sdq_cutoffs |> select(outcome = outcome_label, cutoff), legend = TRUE) +
  ggtitle("SDQ") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 10, 2))
sdq_plot 

report_coverage(pi_data_sdq)

# CBCL - load data

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
pi_data_cbcl <- readRDS(here(derived_data, "pi_data_cbcl.Rds")) |> 
  filter(!(outcome %in% c("cbcl_odd", "cbcl_som")))
  
cbcl_cutoffs_mod <- cbcl_cutoffs |> filter(!(outcome %in% c("cbcl_odd", "cbcl_som")))
cbcl_plot <- make_pi_plot_paper(pi_data_cbcl, cutoffs = cbcl_cutoffs_mod |> select(outcome = outcome_label, cutoff)) +
  ggtitle("CBCL") 
  
  
cbcl_plot

report_coverage(pi_data_cbcl)

# VABS - load data

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
pi_data_vabs <- readRDS(here(derived_data, "pi_data_vabs.Rds"))

vabs_plot <- make_pi_plot_paper(pi_data_vabs) +
  ggtitle("VABS") +
  theme(legend.position = "none")
vabs_plot


(cbcl_plot/sdq_plot/vabs_plot) +
  plot_annotation(tag_levels = 'A')

ggsave(here::here(plots_folder, paste0("prediction_intervals.png")), width = 18, height = 21, units = "cm")
