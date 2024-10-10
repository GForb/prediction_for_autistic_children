analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete)
analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl.Rds")) |> filter(base_all_complete, out_all_complete, all_complete)  # exclude people missing a single value in one outcome.

# Sensitivity analysis
analysis_data_long_all_short_fu <- analysis_data_long |>
  filter(fu_length < 5 & study != "TOGO1")
analysis_data_long_all_no_pa  <- analysis_data_long |> 
  filter(base_iq_full_scale >50 | (is.na(base_iq_full_scale) & base_iq_perceptual > 50))

analysis_data_long_all_no_pa |> filter(wave ==0) |> count(study)
analysis_data_long_all_short_fu |> filter(wave ==0) |> count(study)



analysis_data_long_mi <- readRDS(here(derived_data, "cbcl_imputed_ml.Rds"))


analyses_datasets <- list(
  st = analysis_data_wide,
  mt = analysis_data_long,
  mt_mi = analysis_data_long_mi,
  mt_short_fu = analysis_data_long_all_short_fu,
  mt_no_pa = analysis_data_long_all_no_pa
)

analysis_ns <- imap(analyses_datasets, function(x, idx)
  tibble(
    dataset = idx,
    n_obs = nrow_analysis_data(x),
    n_ids = nids_analysis_data(x)
  )) |> bind_rows()
analysis_ns

outcome <- "cbcl"
iwalk(analyses_datasets, function(x, idx) {
  file_name <- paste0(outcome, "_", idx)
  saveRDS(x, here(derived_data, paste0(file_name, ".Rds")))
  if(is.data.frame(x)){
    write_dta(x, here(derived_data, paste0(file_name, ".dta")))
  } else {
    write_dta(x[[1]], here(derived_data, paste0(file_name, ".dta")))
  }
})
