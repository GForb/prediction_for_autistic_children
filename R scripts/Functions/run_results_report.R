run_results_report <- function(template, output_file_name) {
  output_file  <-  file.path(outputs, "Results",  output_file_name)
  
  rmarkdown::render(template, 
                    output_format = "html_document", 
                    output_file = output_file)
}


run_results_plot <- function(outcome) {
  plots_template <- here::here("Rmarkdown/ma_plots_template.rmd")
  create_doc(dataset = NULL, 
             template = plots_template, 
             outcome = outcome, 
             output_file = glue::glue("Results/{outcome}_results_plots.html"))
}