report_model <- function(analysis_name, results_folder) {
  model_name <-  analysis_name
  full_results <- readRDS(here::here(results_folder, "results_meta_analysis.rds")) 
  
  model_results_df <- full_results |> filter(analysis_name == model_name)
  file_name <- paste0(model_name, ".rds")
  model_predictions <- readRDS(here::here(results_folder, file_name))
  
  
  by_study <- IPDPredictR:::get_performance_by_study(
    by_study_predictions_df = model_predictions, 
    study_var_name = "study", 
    evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred)
  
  studies <- by_study$study |> unique()

  create_forrest_plots(model_results_df = model_results_df, studies = studies)

  IPDPredictR:::calibration_plot_cont(model_predictions, study_var_name = "study") |> print()
  
}

create_forrest_plots <- function(model_results_df, studies) {
  tau <- model_results_df |> select(starts_with("tau")) |> pivot_longer(cols = everything(), names_prefix = "tau_", values_to = "tau") # 
  meta_analysis <- model_results_df |> select(starts_with("meta_analysis")) |> pivot_longer(cols = everything(), names_prefix = "meta_analysis_", values_to = "meta_analysis")
  
  forrest_plot_data <- bind_cols(tau, meta_analysis |> select(-name) ) |> 
    mutate(studies = list(studies))
  
  pwalk(forrest_plot_data, create_forrest_plot)
  
}

create_forrest_plot <- function(name, tau, meta_analysis, studies) {

  if(name == "r-squared") {
    name <- "r_squared"
  }
  
  tau_formatted <- tau |>  format(scientific=F, digits = 2)
  tau_text = glue::glue("tau = {tau_formatted}")
  
  xlab <-  list(calib_slope = "Calibration Slope",
           calib_itl = "Calibration in the Large",
           r_squared = "R-squared",
           rmse = "Root Mean Squared Error")
  
  alim <-  list(calib_slope = c(0.25,1.75),
                calib_itl = c(-1.5,1.5),
                r_squared = c(0,1),
                rmse = c(0,3))
  
  xlim <-  list(calib_slope = c(-0.1, 2.7),
               calib_itl = c(-2.1, 2.7),
               r_squared = c(-0.2, 1.4),
               rmse = c(-0.6,3.6))
  refline <-  list(calib_slope = 1,
                   calib_itl = 0,
                   r_squared = NULL,
                   rmse = NULL)
  

  metafor::forest(meta_analysis, 
                  addpred = TRUE, 
                  refline = refline[[name]] , 
                  xlab = xlab[[name]], 
                  slab = c("lsac_k", "lsac_b", "TEDS" ,  "SNAP"  , "Quest" , "MCS"  ,  "GUI" ,   "ALSPAC" ,"1k_fam"),
                  alim = alim[[name]] , 
                  xlim = xlim[[name]],
                  header= "Study", 
                  mlab = tau_text)
  

}
 
