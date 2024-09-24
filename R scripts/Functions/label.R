
get_label <- Vectorize(function(variable_name, metadata = var_metadata, label_no = 1) {
  if(sum(metadata$variable_name == variable_name) == 0){
    variable_name <- stringr::str_remove(variable_name, "base_")
    variable_name <- stringr::str_remove(variable_name, "out_")
  }
  
  if(sum(metadata$variable_name == variable_name) == 0){
    label <- variable_name
  } else {
     my_var_name <- variable_name
      var_data <- metadata |> 
        filter(variable_name == my_var_name)
     label <-  var_data[,glue::glue("label{label_no}")]
     while(label== "" & label_no >1) {
       label_no = label_no - 1
       label <- var_data[,glue::glue("label{label_no}")]
     }
  }
  return(label)
}, vectorize.args = "variable_name") 


