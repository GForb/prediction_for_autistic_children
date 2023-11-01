studies <- c("gui")
template <- here::here("Rmarkdown/descriptive_report_template.rmd")
variables = c("sdq_tot_p", "sdq_tot_t")
colour = "green"


create_doc(
  set_title = "GUI descriptives",
  dataset = "gui", 
  variables = c("sdq_tot_p", "sdq_tot_t"), 
  colour = "green", 
  template = here::here("Rmarkdown/descriptive_report_template.rmd"))

walk(studies, create_doc, template = template, variables = variables, colour = colour)
