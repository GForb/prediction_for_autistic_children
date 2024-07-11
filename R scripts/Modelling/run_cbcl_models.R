set.seed(123456)

# Todo: Add base ados module 1
# Make analysis spec repeat for multiple datasets.

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete)
analysis_data_wide_mi <- readRDS(here(derived_data, "cbcl_imputed_wide.Rds")) 

analysis_data_no_low_iq <- analysis_data_wide |> filter(base_iq_full_scale > 50 | is.na(base_iq_full_scale) & base_iq_perceptual > 50) # add base ados module 1.
analysis_data_no_long_follow_up <- analysis_data_wide |> filter(fu_length < 5)


results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Prelim")
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")

# Single timepoint analysis - complete case --------------------------------------------

outcomes <- c("cbcl_aff", "cbcl_anx", "cbcl_som", "cbcl_adhd", "cbcl_odd", "cbcl_con")
intercept_est_methods <- c("average", "estimate")

baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")

analysis_spec_single_timepoint <- tibble(outcome = outcomes, 
                                         intercept_est = "average estimate estimate_cv") |> 
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
    analysis_name = glue::glue("st_fi_study_{outcome}_{predictor_set}") |> as.character(),
    log_file = here::here(log_folder, analysis_name),
    do_file = here::here(do_folder, analysis_name),
    data = list(analysis_data_wide),
    model_name = "st_fi_study"
  )

st_results <- run_many_models(analysis_spec_single_timepoint)


# Single timepoint analysis with multiple imputation --------------------------------------------
analysis_spec_single_timepoint_mi <- tibble(outcome = outcomes, 
                                            intercept_est = "average estimate estimate_cv") |> 
  rowwise() |> 
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred1 = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex") |> as.character(),
    pred2 = glue::glue("{pred1} i.base_adi_65 base_ados_css_rrb base_ados_css_sa c.base_iq_full_scale##i.base_iq_standard base_vabs_abc_ss") |> as.character(),
    pred3 = glue::glue("{pred2} base_ethnicity base_maternal_education") |> as.character()
  ) |> 
  ungroup() |> 
  pivot_longer(cols = starts_with("pred"), names_to = "predictor_set", values_to = "predictors") |> 
  mutate(
    model_function = list(model_pred_reg_fi_study),
    analysis_name = glue::glue("st_fi_study_{outcome}_{predictor_set}_mi") |> as.character(),
    log_file = here::here(log_folder, analysis_name),
    do_file = here::here(do_folder, analysis_name),
    data = list(analysis_data_wide_mi),
    model_name = "st_fi_study_mi",
    multiple_imputed_data = TRUE
  )
tictoc::tic()
st_results_mi <- run_many_models(analysis_spec_single_timepoint_mi)
tictoc::toc()




# Multi timepoint analysis - complete case --------------------------------------------

analysis_spec <- bind_rows(
  analysis_spec_single_timepoint, 
  analysis_spec_single_timepoint_mi,
) |> 
  select(-data) 

analysis_spec |> 
  saveRDS(here::here(results_folder, "analysis_spec.rds")) 
