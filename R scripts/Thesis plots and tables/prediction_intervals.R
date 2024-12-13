

plots_folder <- here::here(thesis_plots, "Main Results")

pi_data_sdq <- readRDS(here(derived_data, "pi_data_sdq.Rds"))

sdq_plot <- make_pi_plot(pi_data_sdq, cutoffs = sdq_cutoffs |> select(outcome = outcome_label, cutoff), legend = TRUE)
sdq_plot
ggsave(here::here(plots_folder, paste0("pi_sdq.png")), width = 16, height = 18, units = "cm")

report_coverage(pi_data_sdq)

# CBCL - load data

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
pi_data_cbcl <- readRDS(here(derived_data, "pi_data_cbcl.Rds"))

cbcl_plot <- make_pi_plot(pi_data_cbcl, cutoffs = cbcl_cutoffs |> select(outcome = outcome_label, cutoff))
cbcl_plot
ggsave(here::here(plots_folder, paste0("pi_cbcl.png")), width = 16, height = 18, units = "cm")

report_coverage(pi_data_cbcl)

# VABS - load data

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
pi_data_vabs <- readRDS(here(derived_data, "pi_data_vabs.Rds"))

vabs_plot <- make_pi_plot(pi_data_vabs)
vabs_plot

ggsave(here::here(plots_folder, paste0("pi_vabs.png")), width = 16, height = 18, units = "cm")

report_coverage(pi_data_vabs)

