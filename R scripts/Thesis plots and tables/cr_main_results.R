
# SDQ ----
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
         main_analysis = case_when(
           predictor_set == "pred3_mt" ~ TRUE & intercept_est_method == "estimate_cv",
           TRUE ~ FALSE)
  ) 

main_results_sdq <-  results |> filter(main_analysis, metric != "r_squared") |> 
  mutate(label  = get_label(outcome, label_no = 3))

saveRDS(main_results_sdq, file = here::here(results_folder, "main_results_sdq.rds"))

results_with_late_diag <- results |> filter(suffix == "_all_aut", intercept_est_method == "estimate_cv", metric != "r_squared") |> 
  mutate(label  = get_label(outcome, label_no = 3))
saveRDS(results_with_late_diag, file = here::here(results_folder, "results_sdq_all_aut.rds"))

# VABS ----
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
         main_analysis = case_when(
           predictor_set == "pred3_mt" ~ TRUE & intercept_est_method == "estimate_cv" & model == "mt_fi_study_ri",
           TRUE ~ FALSE)
  ) 
main_results_vabs <-  results |> filter(main_analysis, metric != "r_squared") |> 
  mutate(label  = get_label(outcome, label_no = 3))

standard_rmse <- main_results_vabs |> 
  filter(metric == "rmse") |> 
  select(outcome, est, ci.lb, ci.ub, pi.lb, pi.ub) |> 
  pivot_longer(cols = c(est, ci.lb, ci.ub, pi.lb, pi.ub), names_to = "est_type", values_to = "est_value") |> 
  mutate(est_value = est_value / 2) |> 
  pivot_wider(names_from = est_type, values_from = est_value) |> 
  mutate(metric = "rmse_stand") |> 
  mutate(label  = get_label(outcome, label_no = 3)) 

main_results_vabs <- bind_rows(main_results_vabs, standard_rmse) 


saveRDS(main_results_vabs, file = here::here(results_folder, "main_results_vabs.rds"))

# CBCL ----
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")

# Fascinating - need to make sens plot

raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |>
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
         main_analysis = case_when(
           predictor_set == "pred3_mt" ~ TRUE & 
             intercept_est_method == "estimate_cv" & 
             ((model == "mt_fi_study_rs" & outcome != "cbcl_som") |
                (model == "mt_fi_study_ri" & outcome == "cbcl_som")),
           TRUE ~ FALSE)
  )

recorded_results <- results |> filter(!is.na(est))
recorded_results |> count(model)
recorded_results |> count(predictor_set)
recorded_results |> filter(predictor_set== "pred3a_mt") |> count(outcome)

cbcl_n_items<- cbcl_cutoffs |>
  select(outcome, n_items) 

main_results <-  results |> filter(main_analysis, metric != "r_squared") |>
  mutate(label  = get_label(outcome, label_no = 3))  

standard_rmse <- main_results |> 
  filter(metric == "rmse") |> 
  select(outcome, est, ci.lb, ci.ub, pi.lb, pi.ub) |> 
  pivot_longer(cols = c(est, ci.lb, ci.ub, pi.lb, pi.ub), names_to = "est_type", values_to = "est_value") |> 
  left_join(cbcl_n_items) |> 
  mutate(est_value = est_value / (n_items*2)*10) |> 
  pivot_wider(names_from = est_type, values_from = est_value) |> 
  mutate(metric = "rmse_stand") |> 
  select(-n_items) |>
  mutate(label  = get_label(outcome, label_no = 3))  


main_results_cbcl <- bind_rows(main_results, standard_rmse) 

saveRDS(main_results_cbcl, file = here::here(results_folder, "main_results_cbcl.rds"))

