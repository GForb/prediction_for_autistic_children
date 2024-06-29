
get_label <- Vectorize(function(variable_name, metadata = var_metadata, label_no = 1) {
  if(sum(metadata$variable_name == variable_name) == 0){
    variable_name <- stringr::str_remove(variable_name, "base_")
    variable_name <- stringr::str_remove(variable_name, "out_")
  }
  
  if(sum(metadata$variable_name == variable_name) == 0){
    return(variable_name)
  } else {
    if (label_no ==1){
      metadata$label1[metadata$variable_name == variable_name]
    } else {
      metadata$label2[metadata$variable_name == variable_name]
    }
  }

}, vectorize.args = "variable_name") 


