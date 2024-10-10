results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds"))


analysis_spec$data_name |> unique()

# Datasets (taken from run_cbcl_models)

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete) 

analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl_spline.Rds"))
analysis_data_long_mi <- readRDS(here(derived_data, "cbcl_imputed_ml.Rds")) 

analysis_data_long_mi <- c(list(analysis_data_long), analysis_data_long_mi)

# Sensitivity analysis
analysis_data_long_all_no_pa <- 
  analysis_data_long |> filter(base_iq_full_scale >50 | (is.na(base_iq_full_scale) & base_iq_perceptual > 50))

analysis_data_long_all_short_fu <- analysis_data_long |>
  filter(fu_length < 5 & study != "TOGO1")

analyses_datasets <- list(
  st = analysis_data_wide,
  mt = analysis_data_long,
  mt_mi = analysis_data_long_mi,
  mt_no_pa = analysis_data_long_all_no_pa,
  mt_short_fu = analysis_data_long_all_short_fu
)

# Adding data to analysis spec
analysis_spec <- analysis_spec |> 
  rowwise() |> 
  mutate(data = analyses_datasets[[data_name]] |> list()) |> 
  ungroup()

analysis_spec |> filter(multiple_imputed_data) |>  select(data) |> print(n = 100)

analysis_spec |> slice(1) |> pull(model_function)

cc_analysis_sepc <-  analysis_spec |> filter(is.na(multiple_imputed_data)) 



tictoc::tic()
model_results <- analysis_spec  |> run_many_models_model_only()
tictoc::toc()

saveRDS(model_results, here::here(results_folder, "cbcl_model_only.rds"))


# Loading results from log files.

