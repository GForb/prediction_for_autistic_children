# devtools::install_github("GForb/IPDPredictR")
# library(IPDPredictR)
# detach("package:IPDPredictR", unload = TRUE, character.only = TRUE)
# 


cbcl_wide_data <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(study != "TOGO2")
cbcl_long_data <- readRDS(here(derived_data, "pooled_cbcl.Rds")) |> filter(study != "TOGO2")

nrow(cbcl_wide_data)
analysis_data_wide <- cbcl_wide_data |> filter(out_all_complete, base_all_complete)
nrow(analysis_data_wide)

results_folder <- here::here(data_and_outputs, "Results", "CBCL_exc_TOGO2", "Prelim")
log_folder <- here::here(results_folder, "Logs")
# Regression

outcomes <- c("cbcl_aff", "cbcl_anx", "cbcl_som", "cbcl_adhd", "cbcl_odd", "cbcl_con") # 
intercept_est_methods <- c("average", "estimate")

for(outcome in outcomes){
  for(intercept_est in intercept_est_methods){
    
    baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")
    non_outcome_baseline <- stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome))
    
    model_name <- "results_reg_init"
    model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
    
    model_pred_reg <- model_pred_reg_factory(
      intercept_est = intercept_est,
      model_options = "nocons",
      model_code = glue::glue("regress out_{outcome} study_* base_age out_age {baseline_outcomes_string} base_sex"),
      outcome = outcome,
      log_file = here::here(log_folder, paste0(model_full_name, ".log"))
    )
    results_reg_init <- model_pred_reg(analysis_data_wide)
    saveRDS(results_reg_init, here::here(results_folder, paste0(model_full_name, ".rds")))
    
    
    model_name <- "results_reg"
    model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
    
    model_pred_reg <- model_pred_reg_factory(
      intercept_est = intercept_est,
      model_options = "nocons",
      model_code = glue::glue("regress out_{outcome} study_* base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex"),
      outcome = outcome,
      log_file = here::here(log_folder, paste0(model_full_name, ".log"))
      
    )
    results_reg <- model_pred_reg(analysis_data_wide)
    saveRDS(results_reg, here::here(results_folder, paste0(model_full_name, ".rds")))
    
    
    
    model_name <- "results_reg_ri"
    model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
    
    model_pred_reg <- model_pred_reg_factory(
      intercept_est = intercept_est,
      model_options = "nocons",
      model_code = glue::glue("mixed out_{outcome}  base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex || ID:"),
      outcome = outcome,
      log_file = here::here(log_folder, paste0(model_full_name, ".log"))
      
    )
    results_reg <- model_pred_reg(analysis_data_wide)
    saveRDS(results_reg, here::here(results_folder, paste0(model_full_name, ".rds")))
    
    
    
    # # Random intercept - no interaction with age
    # 
    # model_name <- "results_ri"
    # model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
    # 
    # model_pred_ri <- model_pred_vabs_gsem_factory(
    #   pred_waves = "0 -1", 
    #   outcome = outcome, 
    #   model_code = glue::glue("gsem ({outcome} <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex ///
    #                           base_vabs_dq {non_outcome_baseline} M1[ID]@1)"),
    #   model_options = "nocons",
    #   intercept_est = intercept_est,
    #   log_file = here::here(log_folder, paste0(model_full_name, ".log"))
    #   
    #   
    # ) 
    # 
    # results_ri <- model_pred_ri(dls_data)
    # saveRDS(results_ri, here::here(results_folder, paste0(model_full_name, ".rds")))
    # 
    # ## Random intercept, including interaction with vabs_dq
    # 
    # model_name <- "results_ri_int"
    # model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
    # 
    # model_pred_ri_int <- model_pred_vabs_gsem_factory(
    #   pred_waves = "0 -1", 
    #   outcome = outcome, 
    #   model_code = glue::glue("gsem ({outcome} <- study_* age_spline1 age_spline2 ///
    #                           base_vabs_dq base_sex ///
    #                           c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
    #                           c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq ///
    #                            {non_outcome_baseline} M1[ID]@1)"),
    #   model_options = "nocons",
    #   intercept_est = intercept_est,
    #   log_file = here::here(log_folder, paste0(model_full_name, ".log"))
    #   
    # ) 
    # 
    # results_ri_int <- model_pred_ri_int(dls_data)
    # saveRDS(results_ri_int, here::here(results_folder, paste0(model_full_name, ".rds")))
    # 
    # # Random slope
    # 
    # model_name <- "results_rs"
    # model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
    # 
    # model_pred_rs_int <- model_pred_vabs_gsem_rs_factory(
    #   pred_waves = "0 -1", 
    #   outcome = outcome, 
    #   model_code = glue::glue("gsem ({outcome} <- study_* age_spline1 age_spline2 ///
    #                           c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
    #                           c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq ///
    #                           base_vabs_dq {non_outcome_baseline} M1[ID]@1 c.age_c#M2[ID]@1)"),
    #   model_options = "nocons",
    #   intercept_est = intercept_est,
    #   log_file = here::here(log_folder, paste0(model_full_name, ".log"))
    #   
    # ) 
    # 
    # Don't run as currently no convergence
    #  results_rs <- model_pred_rs_int(dls_data)
    # saveRDS(results_rs, here::here(results_folder, "results_rs.rds"))
    
  }
}