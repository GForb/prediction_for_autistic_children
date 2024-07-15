set.seed(123456)

analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete)
analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds"))|> filter(base_all_complete, out_all_complete)
analysis_data_wide_mi <- readRDS(here(derived_data, "sdq_imputed_wide.Rds")) 

results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")

# Single timepoint analysis - complete case --------------------------------------------

outcomes <- c("sdq_pro_p", "sdq_hyp_p", "sdq_emot_p", "sdq_cond_p", "sdq_peer_p")
baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")

common_analysis_spec <- tibble(outcome = outcomes, intercept_est = "average estimate estimate_cv") |> 
  rowwise() |> 
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred_init = glue::glue("base_age out_age {baseline_outcomes_string} base_sex") |> as.character(),       
    pred1 = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex") |> as.character(),
    pred2 = glue::glue("{pred1} base_ld") |> as.character(),
    pred3 = glue::glue("{pred2} base_ethnicity base_maternal_education base_imd_decile base_maternal_mh") |> as.character(),
    pred1_mt = glue::glue("age_spline1 age_spline2 ///
                                       base_sex ///
                                      c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
                                       {non_outcome_baseline}") |> as.character(),
    mt_ri_study_ri = list(model_pred_gsem_fi_study_ri_id),
    mt_ri_study_rs = list(model_pred_gsem_fi_study_rs_id),
    st_fi_study = list(model_pred_reg_fi_study),
    st_ri_study = list(model_pred_reg_fi_study) 
  ) |> 
  ungroup() |> 
  pivot_longer(cols = starts_with("pred"), names_to = "predictor_set", values_to = "predictors") |>
  pivot_longer(cols = starts_with(c("mt", "st")), names_to = "model_name", values_to = "model_function") 


# to make add data
analysis_spec_single_timepoint <- common_analysis_spec |> 
  filter(model_name %in% c("st_fi_study", "st_ri_study"),
         predictor_set %in% c("pred1", "pred_init")) |> 
  filter(!(predictor_set == "pred_init" & model_name == "st_fi_study")) |>
  add_analysis_name(log_folder = log_folder, do_folder = do_folder) |> 
  mutate(data = list(analysis_data_wide))
 
st_results <- run_many_models(analysis_spec_single_timepoint)



# Single timepoint analysis with multiple imputation --------------------------------------------
analysis_spec_single_timepoint_mi <- common_analysis_spec |> 
  filter(model_name  == "st_ri_study",
         predictor_set %in% c("pred2", "pred3")) |> 
  
  add_analysis_name(log_folder = log_folder, do_folder = do_folder, suffix = "_mi") |> 
  mutate(
    data = list(analysis_data_wide_mi),
    multiple_imputed_data = TRUE
  )
  
tictoc::tic()
st_results_mi <- run_many_models(analysis_spec_single_timepoint_mi)
tictoc::toc()

# Multi timepoint analysis - complete case --------------------------------------------


analysis_spec_multi_timepoint <- common_analysis_spec |> 
  filter(model_name %in% c("mt_ri_study_ri", "mt_ri_study_rs"),
         predictor_set == "pred1_mt") |>
  mutate(
    data = list(analysis_data_long),
    pred_waves = "0 -1",
    out_wave = 2,
  ) |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder)


tictoc::tic()
mt_results <- run_many_models(analysis_spec_multi_timepoint) 
tictoc::toc()

outcomes <- c("vabs_dls_ae", "vabs_com_ae", "vabs_soc_ae")

analysis_spec_multi_timepoint_3pred <- analysis_spec_multi_timepoint |> 
  mutate(pred_waves = "0 -1 -2") |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder, suffix = "_3pred")


tictoc::tic()
mt_results_3pred <- run_many_models(analysis_spec_multi_timepoint_3pred) 
tictoc::toc()




analysis_spec <- bind_rows(
  analysis_spec_single_timepoint, 
  analysis_spec_single_timepoint_mi,
  analysis_spec_multi_timepoint, 
  analysis_spec_multi_timepoint_3pred
) |> 
  select(-data) 

analysis_spec |> 
  saveRDS(here::here(results_folder, "analysis_spec.rds")) 
