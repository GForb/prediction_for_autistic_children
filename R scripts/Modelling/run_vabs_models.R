

analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds")) |> filter(base_all_complete, out_all_complete)
analysis_data_long <- readRDS(here(derived_data, "pooled_vabs.Rds"))|> filter(base_all_complete, out_all_complete)
analysis_data_wide_mi <- readRDS(here(derived_data, "vabs_imputed_wide.Rds")) 

results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")

# Single timepoint analysis - complete case --------------------------------------------

outcomes <- c("vabs_dls_ae", "vabs_com_ae", "vabs_soc_ae")
intercept_est_methods <- c("average", "estimate")

baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")

analysis_spec_single_timepoint <- expand_grid(outcomes, intercept_est_methods) |> 
  rename(outcome = outcomes, intercept_est = intercept_est_methods) |> 
  rowwise() |> 
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred_init = glue::glue("base_age out_age {baseline_outcomes_string} base_sex") |> as.character(),       
    pred1 = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex") |> as.character()
  ) |> 
  ungroup() |> 
  pivot_longer(cols = starts_with("pred"), names_to = "predictor_set", values_to = "predictors") |> 
  mutate(
    model_function = list(model_pred_reg_fi_study),
    analysis_name = glue::glue("st_fi_study_{outcome}_{predictor_set}_int_{intercept_est}") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    do_file = here::here(do_folder, paste0(analysis_name, ".do")),
    data = list(analysis_data_wide),
    model_name = "st_fi_study"
  )

st_results <- run_many_models(analysis_spec_single_timepoint)


# Single timepoint analysis with multiple imputation --------------------------------------------
analysis_spec_single_timepoint_mi <- expand_grid(outcomes, intercept_est_methods) |> 
  rename(outcome = outcomes, intercept_est = intercept_est_methods) |> 
  rowwise() |> 
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred1 = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex") |> as.character(),
    pred2 = glue::glue("{pred1} i.base_adi_65 base_ados_css_rrb base_ados_css_sa c.base_iq_full_scale##i.base_iq_standard") |> as.character(),
    pred3 = glue::glue("{pred2} base_ethnicity base_maternal_education") |> as.character()
  ) |> 
  ungroup() |> 
  pivot_longer(cols = starts_with("pred"), names_to = "predictor_set", values_to = "predictors") |> 
  mutate(
    model_function = list(model_pred_reg_fi_study),
    analysis_name = glue::glue("st_fi_study_{outcome}_{predictor_set}_int_{intercept_est}_mi") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    do_file = here::here(do_folder, paste0(analysis_name, ".do")),
    data = list(analysis_data_wide_mi),
    model_name = "st_fi_study_mi",
    multiple_imputed_data = TRUE
  )
tictoc::tic()
st_results_mi <- run_many_models(analysis_spec_single_timepoint_mi)
tictoc::toc()

# Single timepoint analysis with multiple imputation - complete data only - running as control --------------------------------------------
analysis_spec_single_timepoint_mi_complete <- expand_grid(outcomes, intercept_est_methods) |> 
  rename(outcome = outcomes, intercept_est = intercept_est_methods) |> 
  rowwise() |> 
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred1 = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex") |> as.character()
  ) |> 
  ungroup() |> 
  pivot_longer(cols = starts_with("pred"), names_to = "predictor_set", values_to = "predictors") |> 
  mutate(
    model_function = list(model_pred_reg_fi_study),
    analysis_name = glue::glue("st_fi_study_{outcome}_{predictor_set}_int_{intercept_est}_mi_complete") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    do_file = here::here(do_folder, paste0(analysis_name, ".do")),
    data = list(analysis_data_wide_mi),
    model_name = "st_fi_study_mi",
    multiple_imputed_data = TRUE
  )
tictoc::tic()
st_results_mi_complete <- run_many_models(analysis_spec_single_timepoint_mi_complete)
tictoc::toc()


# Multi timepoint analysis - complete case --------------------------------------------


analysis_spec_multi_timepoint <- expand_grid(outcomes, intercept_est_methods) |> 
  rename(outcome = outcomes, intercept_est = intercept_est_methods) |> 
  mutate(non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
         predictors   = glue::glue("age_spline1 age_spline2 ///
                                      base_vabs_dq base_sex ///
                                      c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
                                      c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
                                       {non_outcome_baseline}") |> as.character(),
         mt_fi_study_ri = list(model_pred_gsem_fi_study_ri_id),
         mt_fi_study_rs = list(model_pred_gsem_fi_study_rs_id)) |> 
  pivot_longer(cols = starts_with("mt"), names_to = "model_name", values_to = "model_function") |>
  mutate(
    analysis_name = glue::glue("{model_name}_{outcome}_int_{intercept_est}") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    do_file = here::here(do_folder, paste0(analysis_name, ".do")),
    data = list(analysis_data_long),
    pred_waves = "0 -1",
    out_wave = 2,
    predictor_set = "pred1"
  )

tictoc::tic()
mt_results_soc <- run_many_models(analysis_spec_multi_timepoint) 
tictoc::toc()

analysis_spec <- bind_rows(
  analysis_spec_single_timepoint, 
  analysis_spec_multi_timepoint, 
  analysis_spec_single_timepoint_mi,
  st_results_mi_complete
) |> 
  select(-data) 

analysis_spec |> 
  saveRDS(here::here(results_folder, "analysis_spec.rds")) 
