# Meta-analyse
# Create full results table
# Return results table with meta-analysis as column...
# Use this to create consistent set of results

create_full_results_table <- function(results_folder) {
  model_name_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) |> 
    separate_longer_delim(cols = "intercept_est", delim = " ") |> 
    mutate(file_name = paste0(analysis_name, "_", intercept_est, ".rds"))

  # process in the case of multiple pred columns
  
  
  results_all <- get_meta_analysis_df(model_name_spec) 
  
  full_results <- results_all$wide_results
  full_results_long <- results_all$long_results
  saveRDS(full_results, file = here::here(results_folder, "results_meta_analysis.rds"))
  saveRDS(full_results_long, file = here::here(results_folder, "results_meta_analysis_long.rds"))
  
  
  outcomes <- unique(full_results$outcome)
  for(myOut in outcomes){
    full_results |>   
      arrange(intercept_est_method) |>
      filter(outcome == myOut) |> 
      select(outcome  ,   model , predictor_set,     intercept_est_method, r_squared, everything(), -analysis_name, -starts_with("meta_analysis"), -starts_with("tau") ) |> 
      print(n = 24) 
  }
}