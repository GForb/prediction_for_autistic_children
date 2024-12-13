create_doc <- function(dataset, 
                       template = here::here("Rmarkdown/descriptive_report_template.rmd"),
                       output_file = paste0("Dataset summaries/", dataset, ".html"),
                       outcome = "") {
  title  <- stringr::str_replace_all(str_to_title(dataset), "_", " ")
  parameters <- list(set_title = title,
                     name_dataset = dataset,
                     outcome = outcome)
  
  output_file = file.path(outputs, output_file)
  
  rmarkdown::render(template, 
                    output_format = "html_document", 
                    output_file = output_file, 
                    params  = parameters)
}


