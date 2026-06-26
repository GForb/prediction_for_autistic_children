library(patchwork)

plots_folder <- here::here(here(outputs, "Paper"))

pi_data_sdq_raw <- readRDS(here(derived_data, "pi_data_sdq.Rds"))

pi_data_sdq_raw |> select(base_spline1, outcome) |> 
  group_by(outcome) |> 
  summarise(q5 = quantile(base_spline1, 0.05),
            q95 = quantile(base_spline1, 0.95))

pi_data_sdq <- pi_data_sdq_raw |> filter(!(outcome == "sdq_cond_p" & base_spline1 > 8))
sdq_plot <- make_pi_plot_paper(pi_data_sdq, cutoffs = sdq_cutoffs |> select(outcome = outcome_label, cutoff), legend = TRUE) +
  ggtitle("SDQ") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 10, 2))
sdq_plot 

report_coverage(pi_data_sdq)

# CBCL - load data

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
pi_data_cbcl_raw <- readRDS(here(derived_data, "pi_data_cbcl.Rds")) |> 
  filter(!(outcome %in% c("cbcl_odd", "cbcl_som")))


pi_data_cbcl_raw |> select(base_spline1, outcome) |> 
  group_by(outcome) |> 
  summarise(q5 = quantile(base_spline1, 0.05),
            q95 = quantile(base_spline1, 0.95))

pi_data_cbcl <- pi_data_cbcl_raw |> 
  filter(!(outcome == "cbcl_con" & base_spline1 > 15),
         !(outcome == "cbcl_aff" & base_spline1 > 12),
         !(outcome == "cbcl_anx" & base_spline1 > 9),
        !(outcome == "cbcl_adhd" & base_spline1 > 12)
         )


  
cbcl_cutoffs_mod <- cbcl_cutoffs |> filter(!(outcome %in% c("cbcl_odd", "cbcl_som")))
cbcl_plot <- make_pi_plot_paper(pi_data_cbcl, cutoffs = cbcl_cutoffs_mod |> select(outcome = outcome_label, cutoff)) +
  ggtitle("CBCL") +
  scale_x_continuous(breaks = seq(0, 16, 2))
  
  
cbcl_plot

report_coverage(pi_data_cbcl)

# VABS - load data

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
pi_data_vabs_raw <- readRDS(here(derived_data, "pi_data_vabs.Rds")) 


pi_data_vabs |> select(base_spline1, outcome) |> 
  group_by(outcome) |> 
  summarise(q5 = quantile(base_spline1, 0.05),
            q90 = quantile(base_spline1, 0.95))

pi_data_vabs <- pi_data_vabs_raw |> 
  filter(base_spline1 < 12.5) # limit to 95% of data for all outcomes

vabs_plot <- make_pi_plot_paper(pi_data_vabs) +
  ggtitle("VABS") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 12, 3))
vabs_plot


(cbcl_plot/sdq_plot/vabs_plot) +
  plot_annotation(tag_levels = 'A')

ggsave(here::here(plots_folder, paste0("prediction_intervals.png")), width = 18, height = 21, units = "cm")
