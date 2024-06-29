check_values <- function(data, metadata = var_metadata) {

  
  variables_to_check <- metadata[,1]
  data_to_check <- data |> 
    dplyr::select(any_of(variables_to_check))
  
  var_min_max <- metadata[,1:3]
  colnames(var_min_max) <- c("variable_to_check", "min", "max")
  
  # Running checks ----
  results <- purrr::pmap(var_min_max, check_data, data = data)
  
  
  # Processing results ----
  results <- results |> unlist()
  
  if (length(results) == 0) {
    test_passed <- TRUE
    results_string <- "Values in range for all checked variables"
    var_out_of_range <- NULL
    data_min_max <- NULL
  } else {
    test_passed <- FALSE
    failed_test <- "The following variables had values out of range: "
    results_string <- paste(results, collapse = ", ")
    results_string <- paste("Variables with values out of range:", results_string)
    var_out_of_range <- results
    
    data_min_max <- data |> 
      select(all_of(var_out_of_range)) |> 
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |> 
      group_by(variable) |> 
      summarise(data_min = min(value, na.rm = TRUE),  data_max = max(value, na.rm = TRUE)) |> 
      left_join(metadata |> select(variable = variable_name, min, max), by = "variable") |>  
    print()
    
  }
  print(results_string)
  if(!test_passed){
    stop("values out of range")
  }


}

check_data <- function(variable_to_check, min, max, data) {
  columns_to_check <- data |> select(any_of(variable_to_check))
  failing_vars <- character(0)
  if(ncol(columns_to_check) > 0) {
    failing_vars <- check_columns(columns_to_check, min, max)
  }
  return(failing_vars)
}

check_columns <- function(data, min, max) {
  names <- colnames(data)
  failed_vars <- apply(data, MARGIN = 2, FUN = check_column, min = min, max = max)
  failing_vars <- names[failed_vars]
  return(failing_vars)
}

check_column <- function(column, min, max) {
  test_failed <- FALSE
  var_min <- min(column, na.rm = TRUE)
  var_max <- max(column, na.rm = TRUE)
  
  if(var_min < min & !is.na(min)) {
    test_failed <- TRUE
  }
     
 if(var_max > max & !is.na(max)) {
    test_failed <- TRUE
  }
  return(test_failed)
}

check_single_obs_per_wave <- function(data) {
  data %>%
    dplyr::group_by(ID, wave) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L) 
}

