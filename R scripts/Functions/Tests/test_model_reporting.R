results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
outcome <- "sdq_cond_p"
intercept_est <- "average"
model_name <- "mt_ri_study_rs"
model_full_name <- paste0(model_name, "_", outcome, "_int_", intercept_est)
file_name <- paste0(model_full_name, ".rds")



full_results <- readRDS(here::here(results_folder, "results_meta_analysis.rds")) 

model_predictions <- readRDS(here::here(results_folder, file_name))


by_study <- IPDPredictR:::get_performance_by_study(
  by_study_predictions_df = model_predictions, 
  study_var_name = "study", 
  evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred)

studies <- by_study |> pull(study) |> unique()


report_model(results_folder = results_folder, analysis_name = model_full_name)


model_results <- full_results |> filter(analysis_name == model_full_name)

tau2 <- model_results |> select(starts_with("tau2")) |> pivot_longer(cols = everything(), names_prefix = "tau2_")

meta_analysis <- model_results |> select(starts_with("meta_analysis")) |> pivot_longer(cols = everything(), names_prefix = "meta_analysis_")

create_forrest_plots(model_results_df = model_results, studies = studies)