
template = here::here("Rmarkdown/vabs_results.rmd")
output_file_name  <-  "vabs_results.html"

output_file  <-  file.path(outputs, "Results",  output_file_name)

rmarkdown::render(template, 
                  output_format = "html_document", 
                  output_file = output_file)