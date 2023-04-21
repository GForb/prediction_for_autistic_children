check_values <- function(data, metadata = var_metadata) {

  
  variables_to_check <- metadata[,1]
  data_to_check <- data |> 
    dplyr::select(starts_with(variables_to_check))
  
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
  } else {
    test_passed <- FALSE
    failed_test <- "The following variables had values out of range: "
    results_string <- paste(results, collapse = ", ")
    results_string <- paste("Variables with values out of range:", results_string)
    var_out_of_range <- results
  }
  print(results_string)
  return(list(passed = test_passed, 
              var_out_of_range = var_out_of_range))
}

check_data <- function(variable_to_check, min, max, data) {
  columns_to_check <- data |> select(starts_with(variable_to_check))
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
  if(var_min < min | var_max > max) {
    test_failed <- TRUE
  }
  return(test_failed)
}