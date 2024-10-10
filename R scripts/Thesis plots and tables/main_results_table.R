# SDQ ----
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
         main_analysis = case_when(
           predictor_set == "pred3_mt" ~ TRUE & intercept_est_method == "estimate_cv",
           TRUE ~ FALSE)
  ) 

main_results <-  results |> filter(main_analysis, metric != "r_squared") |> 
  mutate(label  = get_label(outcome, label_no = 3))

main_results |> save_main_results_hux_table(outcome = "sdq")

# VABS ----
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
         main_analysis = case_when(
           predictor_set == "pred3_mt" ~ TRUE & intercept_est_method == "estimate_cv" & model == "mt_fi_study_ri",
           TRUE ~ FALSE)
  ) 
main_results <-  results |> filter(main_analysis, metric != "r_squared") |> 
  mutate(label  = get_label(outcome, label_no = 3))

main_results |> save_main_results_hux_table(outcome = "vabs")
# 
# # CBCL ----
# results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
# 
# # Fascinating - need to make sens plot
# 
# raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))
# 
# results <- raw_results_long |> 
#   mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
#          main_analysis = case_when(
#            predictor_set == "pred3_mt" ~ TRUE & intercept_est_method == "estimate_cv",
#            TRUE ~ FALSE)
#   ) 
# 
# recorded_results <- results |> filter(!is.na(est))
# recorded_results |> count(model)
# recorded_results |> count(predictor_set)
# recorded_results |> filter(predictor_set== "pred3a_mt") |> count(outcome)
# main_results <-  results |> filter(main_analysis, metric != "r_squared") |> 
#   mutate(label  = get_label(outcome, label_no = 3))
# 
# 
# 
# # Table - extract into function
# main_results |> save_main_results_hux_table(outcome = "sdq")