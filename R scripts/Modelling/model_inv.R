results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
outcome <- "sdq_pro_p"
intercept_est <- "average"
model_name <- "mt_ri_study_rs"
model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est,".rds")
model_predictions <- readRDS(here::here(results_folder, model_full_name))
 model_name_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) |> 
   mutate(file_name = paste0(analysis_name,".rds"))

 full_results <- readRDS(here::here(results_folder, "results_meta_analysis.rds")) 
 
run_meta_analysis(model_full_name)

report_model(model_predictions)

  meta_analysis <- IPDPredictR:::meta_analyse_predictions_cont(predictions = model_predictions, study_var_name = "study") 
  
  by_study <- IPDPredictR:::get_performance_by_study(
    by_study_predictions_df = model_predictions, 
    study_var_name = "study", 
    evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred)
  
  all_studies <- by_study$study |> unique()
  tau3 <- meta_analysis$results_df |> filter(metric == "r-squared") |> pull(tau2) |> format(scientific=F, digits = 2)
  tau3_text = glue::glue("tau-squared = {tau3}")
 
  plot_for <- function(results, my_studies) {
    print(my_studies[[1]])
    metafor::forest(results, 
                    addpred = TRUE, 
                    refline = NULL , 
                    slab = my_studies[[1]],
                    xlab = "R-squared", 
                    alim = c(0,1) , 
                    xlim = c(-0.2, 1.2),
                    header= "Study", 
                    mlab = tau3_text)  
  }
  all_studies_list = list(all_studies)   
print(all_studies_list)
plot_for(meta_analysis$results_list[[3]], all_studies_list)
  
  tau1 <- meta_analysis$results_df |> filter(metric == "calib_slope") |> pull(tau2) |> format(scientific=F, digits = 2)
  tau1_text = glue::glue("tau-squared = {tau1}")
  
  
  metafor::forest(meta_analysis$results_list[[1]], 
                  addpred = TRUE, 
                  refline = 1 , 
                  xlab = "Calibration Slope",  
                  header= "Study", 
                  mlab = tau1_text)
  
  tau2 <- meta_analysis$results_df |> filter(metric == "calib_itl") |> pull(tau2) |> format(scientific=F, digits = 2)
  tau2_text = glue::glue("tau-squared = {tau2}")
  
  
  metafor::forest(meta_analysis$results_list[[2]], 
                  addpred = TRUE, 
                  refline = 0 , 
                  xlab = "Calibration in the Large", 
                  xlim = c(-3,3) , 
                  header= "Study", 
                  mlab = tau2_text)
  
  IPDPredictR:::calibration_plot_cont(model_predictions, study_var_name = "study") |> print()
  
  
  
