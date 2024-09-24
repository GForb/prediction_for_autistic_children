results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")


model_name_spec_raw <- readRDS(here::here(results_folder, "analysis_spec.rds"))
my_spec <- model_name_spec_raw |> filter(predictor_set == "pred3_mt", outcome %in% c("sdq_pro_p", "sdq_hyp_p", "sdq_emot_p", "sdq_cond_p"))

tictoc::tic()
create_full_results_table(results_folder, my_spec)
tictoc::toc()


predictions <- readRDS(here::here(results_folder, "mt_fi_study_rs_sdq_pro_p_pred3_mt_estimate.rds"))
study_var_name <- "study"
imp_indicator_name <-  "imp_no"

result_file$study |> unique() 
result_file$imp_no |> unique()
predictions <- predictions |> as.data.frame()
study_vec <- predictions[,study_var_name]
imp_no_vec <- predictions[,imp_indicator_name] 

predictions$study_imp = paste0(study_vec, "_imp", imp_no_vec)
predictions$study_imp |> unique()