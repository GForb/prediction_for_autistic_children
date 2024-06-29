run_results_report <- function(template, output_file_name) {
  output_file  <-  file.path(outputs, "Results",  output_file_name)
  
  rmarkdown::render(template, 
                    output_format = "html_document", 
                    output_file = output_file)
}
