results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds"))


analysis_spec |> slice(1) |> pull(model_function)

analysis_spec$multiple_imputed_data

analysis_spec$data_name

# Datasets (taken from run_sdq_models)
analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete, autism != "post baseline")
analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds")) |> filter(base_all_complete,
                                                                              out_all_complete,
                                                                              autism != "post baseline",
                                                                              all_complete)  # exclude people missing a single value in one outcome.

# Sensitivity analysis
analysis_data_long_all_autistic <- readRDS(here(derived_data, "pooled_sdq.Rds")) |> 
  filter(base_all_complete, out_all_complete, all_complete)

analysis_data_long_all_many_fu <- analysis_data_long |> 
  filter(!(study %in% c("SNAP", "Quest", "k_families")))
analysis_data_long_all_short_fu <- analysis_data_long |> 
  filter(fu_length < 4)

analysis_data_long_res_aut <- analysis_data_long |> filter(autism == "childhood, researcher")

analysis_data_wide_mi <- readRDS(here(derived_data, "sdq_imputed_wide_fcs_glm.Rds"))

analysis_data_wide_mi <- c(list(analysis_data_wide), analysis_data_wide_mi)
analysis_data_long_mi <- readRDS(here(derived_data, "sdq_imputed_ml.Rds"))
analysis_data_long_mi <- c(list(analysis_data_long), analysis_data_long_mi)

analyses_datasets <- list(
  st = analysis_data_wide,
  st_mi = analysis_data_wide_mi,
  mt = analysis_data_long,
  mt_mi = analysis_data_long_mi,
  mt_all_aut = analysis_data_long_all_autistic,
  mt_many_fu = analysis_data_long_all_many_fu,
  mt_short_fu = analysis_data_long_all_short_fu,
  mt_res_aut = analysis_data_long_res_aut
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
model_results <- analysis_spec  |>   run_many_models_model_only()
tictoc::toc()

saveRDS(model_results, here::here(results_folder, "sdq_model_only.rds"))


# Loading results from log files.
