# takes in 5 variables and creates a quarto document 
# path is a string
# dataset is the name of an R object in a string format
# variables is a vector of strings
# colour is an R colour string
#filename is the name of the document

create_doc <- function(path, 
                       dataset, 
                       variables, 
                       colour, 
                       filename) {

  parameters <- list(data = path, name_dataset = dataset, variables = variables, colour = colour)
  custom_doc <- file.path(here::here("Test/paramstest.rmd"))
  rmarkdown::render(custom_doc, 
                    output_format = "html_document", 
                    output_file = filename, 
                    params  = parameters)
}


