plots_folder <- here::here(thesis_plots, "Main Results")

dp <- 2

# SDQ ----

# Sensitivity analysis: Table
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")


raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))
results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))
  ) |> 
  filter(
    intercept_est_method == "estimate_cv" | 
      (intercept_est_method ==  "average" & predictor_set == "pred3_mt"), 
    predictor_set %in% c("pred3_mt", "pred1_mt", "pred1")) |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix))

results |> select(analysis_type) |> unique()

outcomes <- raw_results_long$outcome |> unique()

sens_labels <- tibble(
  analysis_type = c("mt_fi_study_rs_pred3_mt_estimate_cv_", 
                    "mt_fi_study_rs_pred1_mt_estimate_cv__main", 
                    "mt_ri_study_rs_pred1_mt_estimate_cv_",
                    "mt_fi_study_ri_pred1_mt_estimate_cv_",
                    "st_fi_study_pred1_estimate_cv_",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__1pred",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__3pred",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__all_aut",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__many_fu",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__short_fu"
  ),
  label = c("Primary analysis",
            "Predictor set 1 Only",
            "Random intercept \n for study",
            "Random intercept \n for ID only",
            "Single timepoint model",
            "Predictions made \n off single assessment",
            "Predictions made \n off  3 assessments",
            "Include post baseline \n diagnosis",
            "Include people with \n 2+ observations",
            "Exclude follow up \n > 4 years"
  ),
  order = 10:1
)

sensitivity_results <- results |> 
  left_join(sens_labels) |> 
  filter(!is.na(label), metric != "r_squared")

# Sensitivity table
tables <- map(outcomes, make_sens_table)
names(tables) <- outcomes
iwalk(tables, save_sens_hux_table)

walk(outcomes, make_sensitivty_analysis_plot)


save_sens_delta_tables(sensitivity_results,
                       myOutcome  = "sdq",
                       models = c(9, 8, 7),
                       time = c(6, 5, 4),
                       pop = c(3, 2, 1))


# VABS ----
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))
  ) |> 
  filter(
    intercept_est_method == "estimate_cv" | 
    (intercept_est_method ==  "average" & predictor_set == "pred3_mt"), 
  predictor_set %in% c("pred3_mt","pred2_mt", "pred1_mt", "pred1")) |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix))

results |> select(analysis_type) |> unique()

outcomes <- raw_results_long$outcome |> unique()


sens_labels <- tibble(
  analysis_type = c("mt_fi_study_ri_pred3_mt_estimate_cv_", 
                    "mt_fi_study_ri_pred2_mt_estimate_cv_", 
                    "mt_fi_study_ri_pred1_mt_estimate_cv__2pred",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__1pred",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__3pred",
                    "st_fi_study_pred1_estimate_cv_",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__many_fu",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__short_fu",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__no_pa"
  ),
  label = c("Primary analysis",
            "Predictor set 2",
            "Predictor set 1 Only",
            "Predictions made off single assessment",
            "Predictions made off 3 assessments",
            "Single timepoint model",
            "Include only 2+ follow up",
            "Follow up less than 5 years",
            "Exclude IQ < 50"
  ),
  order = 9:1
)

sensitivity_results <- results |> 
  left_join(sens_labels) |> 
  filter(!is.na(label), metric != "r_squared")


# Sensitivity table
tables <- map(outcomes, make_sens_table)
names(tables) <- outcomes
iwalk(tables, save_sens_hux_table)

walk(outcomes, make_sensitivty_analysis_plot)


save_sens_delta_tables(sensitivity_results,
                       myOutcome =  "vabs",
                       models = c(8, 7),
                       time = c(6, 5, 4),
                       pop = c(3, 2, 1))


# CBCL ----
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")

results <- raw_results_long |>
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))
  ) |>
  filter(
    intercept_est_method == "estimate_cv") |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix))

results |> select(analysis_type) |> unique()

outcomes <- raw_results_long$outcome |> unique()

sensitivity_results <- results |>
  left_join(sens_labels) |>
  filter(!is.na(label), metric != "r_squared")

sens_labels <- tibble(
  analysis_type = c("mt_fi_study_ri_pred3_mt_estimate_cv_",
                    "mt_fi_study_ri_pred2_mt_estimate_cv_",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__2pred",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__1pred",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__3pred",
                    "st_fi_study_pred1_estimate_cv_",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__many_fu",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__short_fu",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__no_pa"
  ),
  label = c("Primary analysis",
            "Predictor set 2",
            "Predictor set 1 Only",
            "Predictions made off single assessment",
            "Predictions made off 3 assessments",
            "Single timepoint model",
            "Include only 2+ follow up",
            "Follow up less than 5 years",
            "Exclude IQ < 50"
  ),
  order = 9:1
)

# Sensitivity table
tables <- map(outcomes, make_sens_table)
names(tables) <- outcomes
iwalk(tables, save_sens_hux_table)

walk(outcomes, make_sensitivty_analysis_plot)



