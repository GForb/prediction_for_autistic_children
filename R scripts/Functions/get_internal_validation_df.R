get_internal_validation_df  <- function(model_name_spec) {
  
  model_name_spec$file_name |> print()
  
  if(is.null(model_name_spec$multiple_imputed_data)) {
    list_results <- lapply(model_name_spec$file_name, process_cv_results)  # process_cv_results
  } else {
    list_results <- map2(model_name_spec$file_name, model_name_spec$multiple_imputed_data, process_cv_results)
  }
  results_df <- bind_rows(list_results)
  
  model_info <- model_name_spec |> 
    select(model = model_name, 
           intercept_est_method  = intercept_est, 
           outcome, predictor_set, 
           file_name, analysis_name)
  
  
  long_results <- results_df |> 
    left_join(model_info, by = "file_name") |>
    select(-file_name) |> 
    mutate(intercept_est_method = "internal validation") |> 
    back_transform_rsq_cols()
  
  wide_results <- long_results |> 
    mutate(summary = round(est, 2)) |> 
    select(outcome, model,predictor_set, intercept_est_method, metric, summary, analysis_name) |> 
    pivot_wider(names_from = metric, values_from = all_of(c("summary")))  |> 
    arrange(outcome) |> 
    select(outcome  ,   model ,  predictor_set,    intercept_est_method,  everything() ) |> 
    rename_with(.fn = ~ str_remove(.x, "summary_"), .cols = starts_with("summary")) 
  

  
  return(list(wide_results = wide_results, long_results = long_results))
  
}

process_cv_results <- function(results_name, multiple_imputed_data = NULL) {
  results_df <- tibble(metric = c("calib_slope", "calib_itl", "r_squared", "r_squared_transformed" , "rmse"))
  
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
      results_df <- evaluate_cross_validation(results, mi = multiple_imputed_data)
    }
  })
  results_df <- results_df |> 
    mutate(file_name = results_name)
  return(results_df)
  
  
}

evaluate_cross_validation <- function(results, mi = FALSE) {
  if(mi){
    results_df <- results |> 
      group_by(fold, validation_rep, imp_no) |> 
      aggregate_cv_results()
  } else {
    results_df <- results |> 
      group_by(fold, validation_rep) |> 
      aggregate_cv_results()
  }

  
  return(results_df)
}

aggregate_cv_results <- function(grouped_data) {

summarised_data <- grouped_data |> 
    summarise(performance = list(summarise_performance(pred, actual))) |> 
    unnest(cols = c(performance)) |> 
    ungroup() 

  summarised_data |> 
    pivot_longer(cols = -c(fold, validation_rep), names_to = "metric", values_to = "value") |> 
    group_by(metric) |> 
    summarise(est = mean(value)) |> 
    ungroup()
}

summarise_performance <- function(pred, actual) {
  IPDPredictR:::evaluate_performance_cont_obs_pred(actual = actual, pred = pred) |> 
    select(-se) |> 
    pivot_wider(names_from = "metric", values_from = "coef")
}





