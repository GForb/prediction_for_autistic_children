model_name_spec_raw <- readRDS(here::here(results_folder, "analysis_spec.rds"))

cv_name_spec <- model_name_spec_raw |> 
  select(-intercept_est) |> 
  mutate(intercept_est = "cv") |> 
  mutate(file_name = paste0(analysis_name, "_", intercept_est, ".rds"))

model_name_spec <-  model_name_spec_raw  |> 
  separate_longer_delim(cols = "intercept_est", delim = " ") |> 
  mutate(file_name = paste0(analysis_name, "_", intercept_est, ".rds")) 

# process in the case of multiple pred columns


results_all <- get_meta_analysis_df(model_name_spec) 
results_cv <- get_internal_validation_df(cv_name_spec)

results_cv$long_results |> count(metric)