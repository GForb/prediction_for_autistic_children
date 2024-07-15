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



# Descriptive Reports
## SDQ
descriptive_template <- here::here("Rmarkdown/descriptive_report_template.rmd")

create_doc(dataset = "ALSPAC", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "gui", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "TEDS", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "mcs", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "k_families", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "SNAP", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "Quest", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "lsac_k", template = descriptive_template, outcome = "sdq")
create_doc(dataset = "lsac_b", template = descriptive_template, outcome = "sdq")

## VABS
create_doc(dataset = "edx_vabs", template = descriptive_template, outcome = "vabs")
create_doc(dataset = "epited", template = descriptive_template, outcome = "vabs")
create_doc(dataset = "elena_vabs", template = descriptive_template, outcome = "vabs")
create_doc(dataset = "pathways_vabs", template = descriptive_template, outcome = "vabs")

## CBCL
create_doc(dataset = "edx_cbcl", template = descriptive_template, outcome = "cbcl")
create_doc( dataset = "elena_cbcl", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "pathways_cbcl", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "togo1", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "togo2", template = descriptive_template, outcome = "cbcl")
create_doc(dataset = "ssc", template = descriptive_template, outcome = "cbcl")




pool("vabs")
create_doc(dataset = "pooled_vabs", template = descriptive_template, outcome = "vabs")


tictoc::tic()
run_models("cbcl")
tictoc::toc() 

tictoc::tic()
run_models("vabs")
tictoc::toc() 
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
tictoc::tic()
create_full_results_table(results_folder)
tictoc::toc() 

results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Prelim")
tictoc::tic()
create_full_results_table(results_folder)
tictoc::toc() 


results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
tictoc::tic()
create_full_results_table(results_folder)
tictoc::toc() 

report_all("vabs")
template = here::here("Rmarkdown/vabs_results.rmd")
output_file_name  <-  "vabs_results.html"
run_results_report(template, output_file_name)



pool("cbcl")
create_doc(dataset = "pooled_cbcl", template = descriptive_template, outcome = "cbcl")
run_models("cbcl")
report_all("cbcl")
template = here::here("Rmarkdown/cbcl_results.rmd")
output_file_name  <-  "cbcl_results.html"
run_results_report(template, output_file_name)


pool("sdq")
create_doc(dataset = "pooled_sdq", template = descriptive_template, outcome = "sdq")


tictoc::tic()
run_models("sdq")
tictoc::toc() #4713 seconds

set.seed(12345)
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
create_full_results_table(results_folder)


#create_full_results_table(results_folder, model_names, outcomes, intercept_est_methods)

template = here::here("Rmarkdown/sdq_results.rmd")
output_file_name  <-  "sdq_results.html"
run_results_report(template, output_file_name)



run_results_plot("SDQ")
run_results_plot("CBCL")
run_results_plot("VABS")

