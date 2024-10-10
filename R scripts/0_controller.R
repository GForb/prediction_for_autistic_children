# See file controller_functions for helper functions

## SDQ
process("ALSPAC")
process("gui")
process("TEDS")
process("mcs")
process("ThousandFamiliesR")
process("SNAP")
process("Quest")
process("lsac_k")
process("lsac_b")


# Running garbage clean after processing data as it is memory intensive
rm(list = ls())
gc()
source(here::here("R scripts", "config.R"))


## VABS & CBCL
process("ELENA")
process("EDX")
process("EpiTED")
process("TOGO2")
process("TOGO1")
process("pathways")
process("ssc")
process("TRAILS")

# Running garbage clean after processing data as it is memory intensive
rm(list = ls())
gc()
source(here::here("R scripts", "config.R"))


# Descriptive Reports
descriptive_template <- here::here("Rmarkdown/descriptive_report_template.rmd")
## SDQ
create_doc(dataset = "ALSPAC", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "gui", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "TEDS", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "mcs", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "k_families", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "SNAP", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "Quest", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "lsac_k", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "lsac_b", template = descriptive_template, outcome = "sdq")

## CBCL
#create_doc(dataset = "edx_cbcl", template = descriptive_template, outcome = "cbcl")
create_doc( dataset = "elena_cbcl", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "pathways_cbcl", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "togo1", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "togo2", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "TRAILS_CC", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "TRAILS_POP", template = descriptive_template, outcome = "cbcl")







# VABS Pipeline   ----

## Pooling ----
pool("vabs")
create_doc(dataset = "pooled_vabs", template = descriptive_template, outcome = "vabs")

## Multiple Imputaiton ----
tictoc::tic()
source(here(modelling_scripts, "vabs_mi_multilevel.R"))
tictoc::toc() 

##  Running models with validation ----
tictoc::tic()
run_models("vabs")
tictoc::toc() 

##  Meta-analysing results ----
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
tictoc::tic()
set.seed(42345234)
create_full_results_table(results_folder)
tictoc::toc() 

# Running models for model reporting  ----
##  Running models for model reporting
source(here::here(modelling_scripts, "run_model_only_vabs.R"))


# CBCL Pipeline ----

## Pooling ----
pool("cbcl")
create_doc(dataset = "pooled_cbcl", template = descriptive_template, outcome = "cbcl")

## Multiple Imputaiton ----
tictoc::tic()
source(here(modelling_scripts, "cbcl_mi_multilevel.R"))
tictoc::toc() 

##  Running models with validation ----


source(here(data_processing_scripts, "save_analysis_datasets_cbcl.R"))

tictoc::tic()
run_models("cbcl")
tictoc::toc() 

##  Meta-analysing results ----
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")

tictoc::tic()
set.seed(366345634)
create_full_results_table(results_folder)
tictoc::toc() 

##  Running models for model reporting
tictoc::tic()
source(here::here(modelling_scripts, "run_model_only_cbcl.R"))
tictoc::toc() 

# SDQ Pipeline  ----

# Pooling   ----  
pool("sdq")
create_doc(dataset = "pooled_sdq", template = descriptive_template, outcome = "sdq")

## Multiple Imputation ----
tictoc::tic()
source(here(modelling_scripts, "sdq_mi_fcs_glm.R"))
tictoc::toc() 
tictoc::tic()
source(here(modelling_scripts, "sdq_mi_multilevl.R"))
tictoc::toc() 

##  Running models with validation ----
tictoc::tic()
run_models("sdq")
tictoc::toc() 

##  Meta-analysing results ----
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
tictoc::tic()
set.seed(949493)
create_full_results_table(results_folder)
tictoc::toc() 

##  Running models for model reporting
source(here::here(modelling_scripts, "run_model_only_sdq.R"))

# Reporting
source(here::here(thesis_reporting, "n_fup_tables.R"))
source(here::here(thesis_reporting, "ages_tables.R"))

source(here::here(thesis_reporting, "cbcl_descriptive_table.R"))
source(here::here(thesis_reporting, "sdq_descriptive_table.R"))
source(here::here(thesis_reporting, "vabs_descriptive_table.R"))

source(here::here(thesis_reporting, "report_model_only.R"))

source(here::here(thesis_reporting, "main_results_table.R"))

source(here::here(thesis_reporting, "sensitivity_analysis_plots_and_tables.R"))



# ISCB Charts

template = here::here("Rmarkdown/iscb_charts.Rmd")
output_file_name  <-  "iscb_charts.html"
run_results_report(template, output_file_name)
