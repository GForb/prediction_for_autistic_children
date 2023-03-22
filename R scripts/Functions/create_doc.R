# takes in 5 variables and creates a rmd document 
# dataset is the name of the dataset in a string
# variables is a vector of strings
# colour is an R colour string
# template is a parameter rmarkdown file to be used to produce the document
# output_file is the file name for saving the document in the outputs folder


create_doc <- function(dataset, 
                       variables, 
                       colour, 
                       template,
                       output_file) {

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


