run_model <- function(model_function, analysis_name = NULL, multiple_imputed_data = FALSE, ...) {
  arg_list <- list(...)
  func_args <- names(formals(model_function))
  matched_args <- arg_list[names(arg_list) %in% func_args]
  
  
  if(multiple_imputed_data) {
    if(is.null(arg_list$data)){
      stop("data must be one of the args passed to run model. If called via run many models it must be a column in the spcification tibble")
    }
    
    run_mi_model <- function(dataset, index) {
      dataset |> mutate(imp_no = as.numeric(index))
      matched_args$data <- dataset
      if(!is.null(matched_args$log_file)){
        matched_args$log_file <- paste0(matched_args$log_file, "_", index)
      } 
      print(paste0("Running MI model for imputation ", index))
      results <- do.call(model_function, matched_args)
      print(paste0("Completed MI model for imputation ", index))
      results$imp_no <- index |> as.numeric()
      return(results)
    }
    
    results_list <- imap(arg_list$data, run_mi_model)
    results <- bind_rows(results_list)
    
    
  } else {
    results <- do.call(model_function, matched_args)
  }
  results |> colnames() |>  print()
  
  if(!is.null(results$pred_cv)){
    results_cv <- results |> filter(validation == "cv") |> 
      rename(pred = pred_cv) |> 
      mutate(int_est = "_cv")
  } else {
    results_cv <- tibble(pred = NA, actual = NA, int_est = "")

  }

  
  results <-  results |> 
    filter(validation != "cv") |> 
    select(-pred_cv) 
  if(results |> select(starts_with("pred")) |> ncol() > 0){
    results <- results |> 
      pivot_longer(cols = starts_with("pred"), names_to = "int_est", values_to = "pred") |> 
      mutate(int_est = str_remove(int_est, "pred")) 
  } else {
    results <- tibble(pred = NA, actual = NA, int_est = "")
  }
  results <- results |> bind_rows(results_cv)
  
    
  
  
  int_est_methods <- results$int_est |> unique()
  
  print("int est methods:")
  print(int_est_methods)
  
  if(!is.null(analysis_name)) {
    for (my_int_est in int_est_methods) {
      filename = paste0(analysis_name,my_int_est, ".rds")
      results_filtered <- results |> filter(int_est == my_int_est) |> 
        mutate(analysis_name = paste0(analysis_name, my_int_est))
      print(my_int_est)
      results_filtered |> pull(pred) |> mean() |> print()
      results_filtered |> 
        saveRDS( here::here(results_folder, filename))

    }

  } 
  return(results)

}

run_many_models <- function(specification_tibble) {
  pmap(specification_tibble, function(...) {
    args <- list(...)
    model_function <- args$model_function
    args$model_function <- NULL
    do.call(run_model, c(list(model_function), args))
  })
}
