
# SDQ

analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete, autism != "post baseline")

pi_data_sdq <- get_pi_data(
  analysis_spec,
  data = analysis_data_wide,
  minmax_values = sdq_cutoffs |> select(outcome, min, max),
  results_folder = results_folder_sdq
)
saveRDS(pi_data_sdq, here(derived_data, "pi_data_sdq.Rds"))
# CBCL


analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete)

pi_data_cbcl <- get_pi_data(
  analysis_spec,
  data = analysis_data_wide,
  minmax_values = cbcl_cutoffs |> select(outcome, min, max),
  results_folder = results_folder_cbcl
)
saveRDS(pi_data_cbcl, here(derived_data, "pi_data_cbcl.Rds"))

# VABS
vabs_cutoffs <- tibble(
  outcome = c(
    "vabs_soc_ae",
    "vabs_com_ae",
    "vabs_dls_ae"
  ),
  cutoff = c(NA, NA, NA),
  max = 100,
  min = 0
  
) |> 
  mutate(outcome_label = get_label(outcome, label_no = 3))

analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds")) |> filter(base_all_complete, out_all_complete)
pi_data_vabs <- get_pi_data(
  analysis_spec,
  data = analysis_data_wide,
  minmax_values = vabs_cutoffs |> select(outcome, min, max),
  results_folder = results_folder_vabs
)

saveRDS(pi_data_vabs, here(derived_data, "pi_data_vabs.Rds"))
