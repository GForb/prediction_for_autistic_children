report_model <- function(analysis_name, results_folder, outcome) {
  model_name <-  analysis_name
  full_results <- readRDS(here::here(results_folder, "results_meta_analysis.rds")) 
  file_name <- paste0(model_name, ".rds")
  
  model_results_df <- full_results |> 
    mutate(analysis_name = paste0(analysis_name, "_", intercept_est_method))|> 
    filter(analysis_name == model_name)
  
  # need to add aggregate results column to this df.
  
  print(model_results_df)
  
  print(file_name)
  model_predictions <- readRDS(here::here(results_folder, file_name))
  
  if(!is.null(model_predictions$imp_no)){
    model_predictions <- model_predictions |> 
      group_by(ID, study) |>  
      summarise(pred = mean(pred), actual = mean(actual)) |> 
      ungroup() 
  }
  model_predictions <- model_predictions |> as.data.frame()
  
  by_study <- IPDPredictR:::get_performance_by_study(
    by_study_predictions_df = model_predictions, 
    study_var_name = "study", 
    evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred)
  
  studies <- by_study$study |> unique()

  create_forrest_plots(model_results_df = model_results_df, studies = studies, outcome = outcome)

  calibration_plot_cont(model_predictions, study_var_name = "study") |> print()
  
}

create_forrest_plots <- function(model_results_df, studies, outcome) {
  tau <- model_results_df |> select(starts_with("tau")) |> pivot_longer(cols = everything(), names_prefix = "tau_", values_to = "tau") # 
  
  meta_analysis <- model_results_df |> 
    select(starts_with("meta_analysis")) |> 
    pivot_longer(cols = everything(), 
                 names_prefix = "meta_analysis_", 
                 values_to = "meta_analysis")
  
  forrest_plot_data <- bind_cols(tau, meta_analysis |> select(-name) ) |> 
    mutate(studies = list(studies))
  
  if(outcome == "CBCL") {
    pwalk(forrest_plot_data, create_forrest_plot_cbcl)
    
  } else if(outcome == "VABS") {
    pwalk(forrest_plot_data, create_forrest_plot_vabs)
    
  } else if(outcome == "SDQ") {
    pwalk(forrest_plot_data, create_forrest_plot_sdq)
  }
  
}

create_forrest_plot_cbcl <- function(name, tau, meta_analysis, studies) {
  tau_formatted <- tau |>  format(scientific=F, digits = 2)
  tau_text = glue::glue("tau = {tau_formatted}")
  
  print(name)
  
  xlab <-  list(calib_slope = "Calibration Slope",
           calib_itl = "Calibration in the Large",
           r_squared_transformed = "R-squared (transformed)",
           r_squared = "R-squared",
           rmse = "Root Mean Squared Error")
  
  alim <-  list(calib_slope = c(0.25,1.75),
                calib_itl = c(-1.5,1.5),
                r_squared = c(0,1),
                r_squared_transformed = c(0,1),
                rmse = c(0,5))
  
  xlim <-  list(calib_slope = c(-0.1, 2.7),
               calib_itl = c(-2.1, 2.7),
               r_squared = c(-0.2, 1.4),
               r_squared_transformed = c(-0.2, 1.4),
               rmse = c(-0.6,5.6))
  
  refline <-  list(calib_slope = 1,
                   calib_itl = 0,
                   r_squared = NULL,
                   r_squared_transformed = NULL,
                   rmse = NULL)
  
  transf <- list(calib_slope = NULL,
                 calib_itl = NULL,
                 r_squared = NULL,
                 r_squared_transformed = back_transform_rsq,
                 rmse = NULL)
  
  print(studies)
  metafor::forest(meta_analysis, 
                  addpred = TRUE, 
                  refline = refline[[name]] , 
                  xlab = xlab[[name]], 
                  alim = alim[[name]] , 
                  xlim = xlim[[name]],
                  transf = transf[[name]],
                  slab = c("TOGO2","TOGO1" ,"SSC" ,"Pathways", "ELENA"),
                  header= "Study", 
                  mlab = tau_text)
  

}

create_forrest_plot_vabs <- function(name, tau, meta_analysis, studies) {
  tau_formatted <- tau |>  format(scientific=F, digits = 2)
  tau_text = glue::glue("tau = {tau_formatted}")
  
  print(name)
  
  xlab <-  list(calib_slope = "Calibration Slope",
                calib_itl = "Calibration in the Large",
                r_squared_transformed = "R-squared (transformed)",
                r_squared = "R-squared",
                rmse = "Root Mean Squared Error")
  
  alim <-  list(calib_slope = c(0.5,1.5),
                calib_itl = c(-3,3),
                r_squared = c(0,1),
                r_squared_transformed = c(0,1),
                rmse = c(0,4))
  
  xlim <-  list(calib_slope = c(-0.1, 2.2),
                calib_itl = c(-5.5, 7.2),
                r_squared = c(-0.2, 1.4),
                r_squared_transformed = c(-0.5, 1.6),
                rmse = c(-2.5,7.5))
  
  refline <-  list(calib_slope = 1,
                   calib_itl = 0,
                   r_squared = NULL,
                   r_squared_transformed = NULL,
                   rmse = NULL)
  
  transf <- list(calib_slope = NULL,
                 calib_itl = NULL,
                 r_squared = NULL,
                 r_squared_transformed = back_transform_rsq,
                 rmse = NULL)
  
  print(studies)
  metafor::forest(meta_analysis, 
                  addpred = TRUE, 
                  refline = refline[[name]] , 
                  xlab = NA, 
                  alim = alim[[name]] , 
                  xlim = xlim[[name]],
                  transf = transf[[name]],
                  slab =  c("Pathways","EpiTED","ELENA", "EDX"),
                  header= "Study", 
                  mlab = tau_text)
  
  
}


create_forrest_plot_sdq <- function(name, tau = NULL, meta_analysis, studies) {
  tau_formatted <- tau |>  format(scientific=F, digits = 2)
  if(!is.null(tau)) {
    tau_text = glue::glue("tau = {tau_formatted}")
  } else {
    tau_text = NULL
  }

  print(name)
  
  xlab <-  list(calib_slope = "Calibration Slope",
                calib_itl = "Calibration in the Large",
                r_squared_transformed = "R-squared",
                r_squared = "R-squared",
                rmse = "Root Mean Squared Error")
  
  alim <-  list(calib_slope = c(0.25,1.75),
                calib_itl = c(-1.5,1.5),
                r_squared = c(0,1),
                r_squared_transformed = c(0,1),
                rmse = c(1,3))
  
  xlim <-  list(calib_slope = c(-0.1, 2.7),
                calib_itl = c(-2.1, 2.7),
                r_squared = c(-0.6, 1.4),
                r_squared_transformed = c(-0.6, 1.4),
                rmse = c(0.4,4.6))
  
  refline <-  list(calib_slope = 1,
                   calib_itl = 0,
                   r_squared = NULL,
                   r_squared_transformed = NULL,
                   rmse = NULL)
  
  transf <- list(calib_slope = NULL,
                 calib_itl = NULL,
                 r_squared = NULL,
                 r_squared_transformed = back_transform_rsq,
                 rmse = NULL)
  
  print(studies)
  print(refline[[name]])
  metafor::forest(meta_analysis, 
                  addpred = TRUE, 
                  refline = refline[[name]] , 
                  xlab = NA, 
                  alim = alim[[name]] , 
                  xlim = xlim[[name]],
                  slab = c("LSAC K", "LSAC B", "1K Familes",    "TEDS",    "SNAP",  "Quest",   "MCS",   "GUI", "ALSAPC"),
                  transf = transf[[name]],
                  header= "Study", 
                  mlab = tau_text)
  
  
}



