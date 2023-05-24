# takes in 5 variables and creates a rmd document 
# dataset is the name of the dataset in a string
# variables is a vector of strings
# colour is an R colour string
# template is a parameter rmarkdown file to be used to produce the document
# output_file is the file name for saving the document in the outputs folder

get_variables <- function(report_type, metadata = var_metadata) {
  variable_names <- dplyr::pull(metadata, starts_with)
  elements_to_modify <- variable_names[startsWith(variable_names, "sdq_")]
  if(report_type == "parent"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_p"), variable_names)
  }else if(report_type == "teacher"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_t"), variable_names)
  }else if(report_type == "other parent"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_p2"), variable_names)
  }else if(report_type == "parent living elsewhere"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_ple"), variable_names)
  }else if(report_type == "child"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_c"), variable_names)
  }else if(report_type == "mother"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_m"), variable_names)
  }else if(report_type == "father"){
    modified_variable_names <- ifelse(variable_names %in% elements_to_modify, paste0(variable_names, "_f"), variable_names)
  }else{
    stop("Invalid argument! Please provide a valid reporter (e.g. parent)")
  }
  return(modified_variable_names)
}


create_doc <- function(dataset, 
                       variables = c("age", "sdq_tot_p", "sdq_cond_p", "sdq_hyp_p", "sdq_emot_p", "sdq_peer_p", "sdq_pro_p"), 
                       colour = "red", 
                       template = here::here("Rmarkdown/descriptive_report_template.rmd"),
                       output_file = paste0("Dataset summaries/", dataset, ".html")) {

  parameters <- list(
                     name_dataset = dataset, 
                     variables = variables, 
                     colour = colour)
  
  output_file = file.path(outputs, output_file)
  
  rmarkdown::render(template, 
                    output_format = "html_document", 
                    output_file = output_file, 
                    params  = parameters)
}


