run_model <- function(model_function, analysis_name = NULL, multiple_imputed_data = FALSE, ...) {
  arg_list <- list(...)
  func_args <- names(formals(model_function))
  matched_args <- arg_list[names(arg_list) %in% func_args]
  
  print(arg_list$analysis_name)
  
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
  try({
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
  
  })
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

run_many_models_model_only <- function(specification_tibble) {
  pmap(specification_tibble, function(...) {
    args <- list(...)
    model_function <- args$model_function
    args$model_function <- NULL
    do.call(run_model_only, c(list(model_function), args))
  })
}

run_model_only <- function(model_function, analysis_name = NULL, multiple_imputed_data = FALSE, ...) {
  print(analysis_name)
  arg_list <- list(...)
  func_args <- names(formals(model_function))
  if(is.na(multiple_imputed_data)){
    multiple_imputed_data <- FALSE
  }
  if(multiple_imputed_data){
    mi_data_list_list <- arg_list$data
    names(mi_data_list_list) <- 1:length(mi_data_list_list)
    length(mi_data_list_list) |> print()
    mi_data <- mi_data_list_list |> 
      imap(\(x, idx) x |> 
             mutate(mi_m = as.numeric(idx) - 1, 
                                 mi_id = row_number()) |>
      mutate(across(where(is.factor), as.numeric))) |> 
      bind_rows()

    arg_list$data <- mi_data
  }
  matched_args <- arg_list[names(arg_list) %in% func_args] |> c(model_only = TRUE)
  
  print(arg_list$analysis_name)
  
  results <- do.call(model_function, matched_args)


  return(results)
  
}
# length(arg_list$data) |> print()
# mi_data_list <- arg_list$data |> 
#   imap(\(x, idx) x |> mutate(mi_m = idx, mi_id = row_number()))
# 
# mi_data <- bind_rows(mi_data_list)
# arg_list$multiple_imputed_data
# print(arg_list$data )

