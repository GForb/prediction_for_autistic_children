get_meta_analysis_df <- function(model_name_spec) {
  
  if(is.null(model_name_spec$multiple_imputed_data)) {
    list_results <- lapply(model_name_spec$file_name, run_meta_analysis)
  } else {
    list_results <- map2(
      model_name_spec$file_name, 
      model_name_spec$multiple_imputed_data, 
      run_meta_analysis)
  }
  results_df <- bind_rows(list_results)

  model_info <- model_name_spec |> 
    select(model = model_name, 
           intercept_est_method  = intercept_est, 
           outcome, predictor_set, 
           file_name, analysis_name)
  
  print(results_df, n = 100)
  
  # Extract df
  long_results <- results_df |> 
    left_join(model_info, by = "file_name") |>
    select(-file_name) |>
    mutate(tau = sqrt(tau2)) |> 
    select(-tau2) |> 
    back_transform_rsq_cols() 
  
  wide_results <- long_results |> 
    IPDPredictR:::compress_columns() |> 
    mutate(summary = paste0(round(est, 2), " ", ci, " [", round(tau, 2), "]")) |> 
    select(outcome, model,predictor_set, intercept_est_method, metric, summary, meta_analysis, tau, analysis_name) |> 
    pivot_wider(names_from = metric, values_from = all_of(c("summary", "meta_analysis", "tau")))  |> 
    arrange(outcome) |> 
    select(outcome  ,   model ,  predictor_set,    intercept_est_method,  everything() ) |> 
    rename_with(.fn = ~ str_remove(.x, "summary_"), .cols = starts_with("summary")) 
  
  return(list(wide_results = wide_results, long_results = long_results))
}


#     pivot_wider(names_from = metric, values_from = all_of(c("summary", "meta_analysis"))) |>


run_meta_analysis <- function(results_name, multiple_imputed_data = NULL) {
  print(results_name)
  meta_analysis_df <- tibble(metric = c("calib_slope", "calib_itl", "r_squared", "r_squared_transformed" , "rmse"))
  
  
  try({
    results <- readRDS(here::here(results_folder, results_name))
    if(check_results(results)){
      if(is.null(multiple_imputed_data))  {
        multiple_imputed_data <- FALSE
      } else  if(is.na(multiple_imputed_data))  {
        multiple_imputed_data <- FALSE
      } else {
        multiple_imputed_data <- TRUE
      }
      
      if(multiple_imputed_data){
        meta_analysis_df <- run_meta_analysis_mi(results)
      } else {
        meta_analysis_df <- run_meta_analysis_single_data(results)
      }
    }
  })
  
  meta_analysis_df <- meta_analysis_df |> mutate(file_name = results_name) 
  return(meta_analysis_df)
    
}

run_meta_analysis_mi <- function(results) {
  n_imp = max(results$imp_no)
  
  # 1. Meta-analyse each imputed dataset separately
  analyse_imp_rep <- function(my_imp_no) {
    results |> filter(imp_no == my_imp_no) |> run_meta_analysis_single_data() |> mutate(imp_no = my_imp_no)
  }
  results_list <- map(1:n_imp, analyse_imp_rep) 
  results_df <- bind_rows(results_list) |> pool_est_all_metrics()
  
  # Obtain by study results - pool performance metrics at the study level, then meta-analyse.
  # Calculate performance metrics on each imputed dataset
  # Pool
  # Meta-analyse pooled performance.
  
  meta_analysis_list  <- IPDPredictR:::meta_analyse_predictions_cont_mi(predictions = results, study_var_name = "study", imp_indicator_name = "imp_no") 
  meta_analysis <- tibble(meta_analysis = meta_analysis_list$results_list)
  by_study <- tibble(by_study = list(meta_analysis_list$by_study))
  # 2. Create a by study object and a meta-analysis object - these will *not* match the headline results (i wonder how far out!) but can be used for quick plotting 
  
  results_df <- bind_cols(results_df, by_study, meta_analysis)
  return(results_df)

}

run_meta_analysis_single_data <- function(results) {
    meta_analysis_list <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results, study_var_name = "study") 
    meta_analysis_df <- meta_analysis_list$results_df |> tibble()
    meta_analysis <- tibble(meta_analysis = meta_analysis_list$results_list)
    
    by_study <- tibble(by_study = list(meta_analysis_list$by_study))
    
    meta_analysis_df <- meta_analysis_df |> bind_cols(meta_analysis) |> bind_cols(by_study)
  
    return(meta_analysis_df)
}



check_results <- function(results) {
  if(is.null(results$pred) | is.null(results$actual)) {
    return(FALSE)
  } else if(any(is.na(results$pred)) | any(is.na(results$actual))) {
      return(FALSE)
  } else {
    return(TRUE)
  }

}

process_results_df  <- function(model_names, intercept_est_methods, outcomes, predictor_set, df) {
  if(is.null(df)) {
    tibble(metric = c("r-squared", "calib_itl", "calib_slope"),
           outcome = outcomes, 
           model = model_names, 
           predictor_set = predictor_set,
           intercept_est_method = intercept_est_methods,
           summary = NA) |> 
      pivot_wider(names_from = metric, values_from = summary) 
    
  } else {
    df |> mutate(
      outcome = outcomes, 
      model = model_names, 
      predictor_set = predictor_set,
      intercept_est_method = intercept_est_methods) |> 
      IPDPredictR:::compress_columns() |> 
      mutate(summary = paste0(round(est, 2), " ", ci, " ", pi, " [", round(tau2, 2), "] ")) |> 
      select(outcome, model,predictor_set, intercept_est_method, metric, summary) |> 
      pivot_wider(names_from = metric, values_from = summary) 
  }
}

# This funciton is not used. It implements a "pooled prediction" approach to evaluating model performance. 
# Wood 2015 finds this to lead to substantial positive bias in model performance. May be useful in future for a compariosn of methods.
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4515100/

get_aggregate_results <- function(results) {
  aggregate_results <- results |> 
    group_by(ID, study) |>  
    summarise(pred = mean(pred), actual = mean(actual)) |> 
    ungroup() |> 
    as.data.frame()
  ma_aggregate <- run_meta_analysis_single_data(aggregate_results)
  
}