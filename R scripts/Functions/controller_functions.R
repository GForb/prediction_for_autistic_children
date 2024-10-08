
 # Functions ----
process <- function(study_name) {
  source(here(data_processing_scripts, paste0("import_", study_name, ".R")))

}

# Running analysis
descriptive_template <- here::here("Rmarkdown/descriptive_report_template.rmd")


pool <- function(outcome_name) {
  source(here(data_processing_scripts, paste0("pool_", outcome_name, ".R")))
}

run_models <- function(outcome_name) {
  source(here(modelling_scripts, paste0("run_", outcome_name, "_models.R")))
}

report_all <- function(outcome_name) {
  source(here(modelling_scripts, paste0(outcome_name, "_full_results_report.R")))
}
