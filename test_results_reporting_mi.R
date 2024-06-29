results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
model_name_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) |> 
  mutate(file_name = paste0(analysis_name,".rds"))


model_name_spec_rest <- model_name_spec |> filter(multiple_imputed_data == TRUE) 

model_name_spec_no_mi<- model_name_spec |> filter(is.na(multiple_imputed_data)) |> 
  filter(outcome == "vabs_dls_ae", intercept_est == "average", predictor_set == "pred_init") 

results_example_filename <- model_name_spec_rest |> slice(1) |> pull(file_name) 
results_example <- readRDS(here::here(results_folder, results_example_filename))

results <- run_meta_analysis(model_name_spec_rest |> slice(1) |> pull(file_name) , model_name_spec_rest |> slice(1) |> pull(multiple_imputed_data) )

