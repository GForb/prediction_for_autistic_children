
results_folder <- here::here(results_folder_main, "VABS", "Prelim")


outcome <- "vabs_dls_ae"
model_name <- paste0("st_fi_study_", outcome, "_pred3_mi_estimate_cv")

report_model(analysis_name = model_name, outcome = "VABS", results_folder = results_folder)


outcome <- "vabs_soc_ae"
model_name <- paste0("st_fi_study_", outcome, "_pred3_mi_estimate_cv")

report_model(analysis_name = model_name, outcome = "VABS", results_folder = results_folder)

outcome <- "vabs_com_ae"
model_name <- paste0("st_fi_study_", outcome, "_pred3_mi_estimate_cv")

report_model(analysis_name = model_name, outcome = "VABS", results_folder = results_folder)