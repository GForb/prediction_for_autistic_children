# Meta-analyse
# Create full results table
# Return results table with meta-analysis as column...
# Use this to create consistent set of results

create_full_results_table <- function(results_folder) {
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
  
  full_results <- results_all$wide_results |> 
    left_join(results_cv$wide_results, by = c("analysis_name"), suffix = c("", "_cv"))


  full_results_long <- results_all$long_results |> 
    left_join(results_cv$long_results, by = c("analysis_name", "metric"), suffix = c("", "_cv")) |> 
    select(metric, est, est_cv, se, tau,  everything())
  
  
  saveRDS(full_results, file = here::here(results_folder, "results_meta_analysis.rds"))
  saveRDS(full_results_long, file = here::here(results_folder, "results_meta_analysis_long.rds"))
  
  
  outcomes <- unique(full_results$outcome)
  for(myOut in outcomes){
    full_results |>   
      arrange(intercept_est_method) |>
      filter(outcome == myOut) |> 
      select(outcome  ,   model , predictor_set,     intercept_est_method, r_squared_transformed, everything(), -analysis_name, -starts_with("meta_analysis"), -starts_with("tau") ) |> 
      print(n = 24) 
  }
}