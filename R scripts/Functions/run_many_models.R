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

  
  
  
  
  if(!is.null(analysis_name)) {
    results$analysis_name <- analysis_name
    saveRDS(results, here::here(results_folder, paste0(analysis_name, ".rds")))
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
