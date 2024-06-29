results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")


model_names <- c("results_reg_init", "results_reg","results_reg_ri")
outcomes <- c("sdq_emot_p", "sdq_cond_p", "sdq_hyp_p", "sdq_pro_p", "sdq_peer_p") # 
intercept_est_methods <- c("average", "estimate")



model_name_spec = expand_grid(model_names, intercept_est_methods, outcomes) |>
  mutate(analysis_name = paste0(model_names, "_", outcomes, "_int_", intercept_est_methods),
         file_name = paste0(analysis_name,".rds"))



full_results <- get_meta_analysis_df(model_name_spec) 
saveRDS(full_results, file = here::here(results_folder, "results_meta_analysis.rds"))

  full_results |>   
    arrange(outcome) |> 
    select(outcome  ,   model ,      intercept_est_method, `r-squared`, everything() ) |> 
    print(n = 24) 
    
for(myOut in outcomes){
  full_results |>   
    arrange(intercept_est_method) |>
    filter(outcome == myOut) |> 
    select(outcome  ,   model ,      intercept_est_method, `r-squared`, everything() ) |> 
    print(n = 24) 
}