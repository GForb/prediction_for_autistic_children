create_doc <- function(path, dataset, variables, colour, filename) {
  # takes in 5 variables and creates a quarto document 
  # path is a string
  # dataset is the name of an R object in a string format
  # variables is a vector of strings
  # colour is an R colour string
  #filename is the name of the document
  parameters <- list(data = path, name_dataset = dataset, variables = variables, colour = colour)
  custom_doc <- file.path(here::here("Test/paramstest.rmd"))
  rmarkdown::render(custom_doc, 
                    output_format = "html_document", 
                    output_file = filename, 
                    params  = parameters)
}

create_doc("R scripts/Data Processing/import_gui.R", 
           "gui_data", 
           c("age", "sex", "sdq_emot_p", "sdq_cond_p", "sdq_hyp_p", "sdq_peer_p", "sdq_pro_p"), 
           "red", 
           "I hope this works.html")

