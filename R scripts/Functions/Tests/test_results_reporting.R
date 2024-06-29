results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
model_name_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) |> 
  mutate(file_name = paste0(analysis_name,".rds"))


full_results <- get_meta_analysis_df(model_name_spec) 

full_results 

results_name <- "st_ri_study_sdq_emot_p_pred_init_int_average.rds"
results <- readRDS(here::here(results_folder, results_name))

my_ma <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results, study_var_name = "study") 

df <- run_meta_analysis("st_ri_study_sdq_emot_p_pred_init_int_average.rds")


pivot_wider(df |> select(metric, est, meta_analysis, by_study, file_name), names_from = metric, values_from = c("est", "meta_analysis" ))