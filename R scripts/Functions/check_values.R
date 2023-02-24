
check_values <- function(data, metadata) {
  test_passed <- TRUE
  failed_test <- "The following variables had values out of range: "
  failed_the_test <- list()
  
  variables_to_check <- metadata[,1]
  
  data_to_check <- data |> 
    dplyr::select(starts_with(variables_to_check))
  
  pmap(metadata, check_variable, data = data)
    
  lappply
    
  
  if(test_passed){
    return(print("Successfully passed test"))
  }else{
    return(print(paste0(failed_test, failed_the_test)))
  }
}

check_variable <- function(variable_to_check, min, max, data) {
  
  columns_to_check <- data |> slect(starts_with(variable_to_check))
  if(ncol(check_col) >1) {
    check_cols(columns_to_check, min, max)
  }
  
}

check_columns <- function(data, min, max) {
  failed_vars <- list()
  
  failed_vars <- apply(data, MARGIN = 2, FUN = check_column, min = min, max = max)
 
  
  return(failed_vars)
}

check_column <- function(variables, min, max) {
  for (j in 1:ncol(data)) {
    var_min <- min(tibble[,j], na.rm = TRUE)
    var_max <- max(tibble[,j], na.rm = TRUE)
    if(var_min < min | var_max > max) {
      failed_the_test <- append(failed_vars, colnames(data[,j]))
    }
  }
}