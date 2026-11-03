
analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds")) |> filter(base_all_complete, out_all_complete) 
analysis_data_long_mi <- readRDS(here(derived_data, "vabs_imputed_ml.Rds")) 

analysis_data_long <- readRDS(here(derived_data, "pooled_vabs_spline.Rds"))


# Sensitivity analysis



analyses_datasets <- list(
  st = analysis_data_wide,
  mt_mi = analysis_data_long_mi[[1]]
)

analysis_ns <- imap(
  analyses_datasets, 
  function(x, idx) tibble(
    dataset = idx, 
    n_obs = nrow(x),
    n_ids = x$ID |> unique() |> length()
)) |> bind_rows()
analysis_ns



results_folder <- results_folder_vabs
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")

# Defining analysis specs

# Single timepoint analysis - complete case --------------------------------------------

outcomes <- c("vabs_com_ae", "vabs_soc_ae", "vabs_dls_ae")
baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")

common_analysis_spec <- tibble(outcome = outcomes, intercept_est = "average estimate estimate_cv") |>
  rowwise() |>
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred_init = glue::glue("base_age out_age {baseline_outcomes_string} base_sex") |> as.character(),
    pred1 = glue::glue(
      "base_spline* ///
      c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_sex ///
      c.base_spline1#c.base_vabs_dq c.base_spline2#c.base_vabs_dq base_vabs_dq  ///
        base_age out_age {non_outcome_baseline}"
    ) |> as.character(),
    pred1_mt = glue::glue(
      "age_spline1 age_spline2 ///
       base_sex age_spline1Xsex age_spline2Xsex ///
       base_vabs_dq age_spline1Xdq age_spline2Xdq  ///
       {non_outcome_baseline} "
    ) |> as.character(),

    pred2_mt = glue::glue(
      "{pred1_mt} base_adi_65 base_ados_css_rrb base_ados_css_sa base_iq_full_scale base_iq_standard base_iqXmethod"
    ) |> as.character(),
    pred3_mt = glue::glue("{pred2_mt} base_ethnicity base_maternal_education") |> as.character(),
    mt_fi_study_ri = list(model_pred_gsem_fi_study_ri_id),
    st_fi_study = list(model_pred_reg_fi_study),
  ) |>
  ungroup() |>
  pivot_longer(cols = starts_with("pred"),
               names_to = "predictor_set",
               values_to = "predictors") |>
  pivot_longer(cols = starts_with(c("mt", "st")),
               names_to = "model_name",
               values_to = "model_function")

# Complete case analysis 
analysis_spec_single_timepoint_cc <- common_analysis_spec |> 
  filter(model_name == c("st_fi_study"),
         predictor_set %in% c("pred1")) |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder) |> 
  mutate(data = list(analysis_data_wide), data_name = "st")
 
# Multi timepoint analysis - with multiple imputation --------------------------------------------


analysis_spec_multi_timepoint_mi_pred3 <- common_analysis_spec |> 
  filter(model_name %in% c("mt_fi_study_ri"),
         predictor_set == "pred3_mt") |>
  mutate(
    data = list(analysis_data_long_mi),
    data_name = "mt_mi",
    pred_waves = "0 -1",
    out_wave = 2,
    multiple_imputed_data = TRUE
  ) |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder)


analysis_spec <- bind_rows(
  analysis_spec_single_timepoint_cc, 
  analysis_spec_multi_timepoint_mi_pred3,
) |> 
  select(-data) 

analysis_spec |> 
  saveRDS(here::here(results_folder, "analysis_spec.rds")) 



set.seed(5614)
tictoc::tic()
mt_results <- run_many_models(analysis_spec_single_timepoint_cc) 
tictoc::toc()




set.seed(6541)

tictoc::tic()
# Note primary analysis is v slow to run
# mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_mi_pred3) 
tictoc::toc()

# Additional analysis
# see run_vabs_models for full list of analyses run


