#var_metadata <- var_metadata[-2, ]

find_matching_column <- function(variable_name, data) {
  starts_with <- substr(variable_name, 1, nchar(variable_name)-2)
  matching_column <- which(grepl(paste0("^", starts_with), data$starts_with))
  return(matching_column)
}

label_var <- function(as_string_variable, which_label, metadata = var_metadata) {
  index <- find_matching_column(as_string_variable, metadata)
  column_ind <- 0
  if(which_label == "label2"){
    column_ind <- 6
  }else{
    column_ind <- 5
  }
  return(metadata[index, column_ind])
}


  
#find_matching_column("sdq_pro_p", var_metadata)

#label_var("sdq_pro_p", "label1")
