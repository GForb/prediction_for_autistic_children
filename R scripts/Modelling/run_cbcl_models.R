analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete)
analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl.Rds")) |> filter(base_all_complete, out_all_complete, all_complete)  # exclude people missing a single value in one outcome.

# Sensitivity analysis
analysis_data_long_all_short_fu <- analysis_data_long |>
  filter(fu_length < 5 & study != "TOGO1")
analysis_data_long_all_no_pa  <- analysis_data_long |> 
  filter(base_iq_full_scale >50 | (is.na(base_iq_full_scale) & base_iq_perceptual > 50))

analysis_data_long_all_no_pa |> filter(wave ==0) |> count(study)
analysis_data_long_all_short_fu |> filter(wave ==0) |> count(study)



analysis_data_long_mi <- readRDS(here(derived_data, "cbcl_imputed_ml.Rds"))


analyses_datasets <- list(
  st = analysis_data_wide,
  mt = analysis_data_long,
  mt_mi = analysis_data_long_mi[[1]],
  mt_short_fu = analysis_data_long_all_short_fu,
  mt_no_pa = analysis_data_long_all_no_pa
)

analysis_ns <- imap(analyses_datasets, function(x, idx)
  tibble(
    dataset = idx,
    n_obs = nrow(x),
    n_ids = x$ID |> unique() |> length()
  )) |> bind_rows()
analysis_ns



results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")

# Defining analysis specs

# Single timepoint analysis - complete case --------------------------------------------

outcomes <- analysis_data_long |> select(starts_with("cbcl")) |> colnames()
baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")

common_analysis_spec <- tibble(outcome = outcomes, intercept_est = "average estimate estimate_cv") |>
  rowwise() |>
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred_init = glue::glue("base_age out_age {baseline_outcomes_string} base_sex") |> as.character(),
    pred1 = glue::glue(
      "base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex"
    ) |> as.character(),
    pred2 = glue::glue(
      "{pred1} base_iq_full_scale base_ados_css_rrb base_ados_css_sa base_vabs_abc_ss base_adi_65 base_iqXmethod base_iq_standard"
    ) |> as.character(),
    pred3 = glue::glue("{pred2} base_ethnicity base_maternal_education") |> as.character(),
    pred_init_mt = glue::glue("age_spline1  ///
       base_sex ///
       {non_outcome_baseline}") |> as.character(),
    pred1_mt = glue::glue(
      "age_spline1 age_spline2 ///
                                       base_sex ///
                                      c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
                                       {non_outcome_baseline}"
    ) |> as.character(),
    pred2_mt = glue::glue(
      "{pred1_mt} base_iq_full_scale base_ados_css_rrb base_ados_css_sa base_vabs_abc_ss base_adi_65 base_iqXmethod base_iq_standard"
    ) |> as.character(),
    pred2a_mt = glue::glue(
      "{pred1_mt} base_iq_full_scale base_ados_css_rrb base_ados_css_sa base_vabs_abc_ss base_adi_65"
    ) |> as.character(),
    pred3_mt = glue::glue("{pred2_mt} base_ethnicity base_maternal_education") |> as.character(),
    pred3a_mt = glue::glue("{pred2a_mt} base_ethnicity base_maternal_education") |> as.character(),
    mt_ri_study_ri = list(model_pred_gsem_ri_study_ri_id),
    mt_fi_study_ri = list(model_pred_gsem_fi_study_ri_id),
    mt_ri_study_rs = list(model_pred_gsem_ri_study_rs_id),
    mt_fi_study_rs = list(model_pred_gsem_fi_study_rs_id),
    st_fi_study = list(model_pred_reg_fi_study),
    st_ri_study = list(model_pred_gsem_ri_study)
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
  filter(
    model_name %in% c("st_ri_study", "st_fi_study"),
    predictor_set %in% c("pred1", "pred_init")
  ) |>
  filter(!(predictor_set == "pred_init" &
             model_name == "st_fi_study")) |>
  add_analysis_name(log_folder = log_folder, do_folder = do_folder) |>
  mutate(data = list(analysis_data_wide), data_name = "st")

analysis_spec_multi_timepoint_cc <- common_analysis_spec |>
  filter(
    model_name %in% c("mt_ri_study_rs", "mt_fi_study_rs", "mt_fi_study_ri"),
    predictor_set %in% c("pred_init_mt", "pred1_mt")
  ) |>
  add_analysis_name(log_folder = log_folder, do_folder = do_folder) |>
  mutate(
    data = list(analysis_data_long),
    data_name = "mt",
    pred_waves = "0 -1",
    out_wave = 2
  )


analysis_spec_multi_timepoint_cc_datasets <- common_analysis_spec |>
  filter(model_name == "mt_fi_study_rs", predictor_set == "pred1_mt") |>
  mutate(
    data_main = list(analysis_data_long),
    data_short_fu = list(analysis_data_long_all_short_fu),
    data_long_no_pa = list(analysis_data_long_all_no_pa)
  ) |>
  pivot_longer(
    cols = starts_with("data_"),
    names_to = "data_set",
    values_to = "data",
    names_prefix = "data_"
  ) |>
  mutate(
    pred_waves = "0 -1",
    out_wave = 2,
    suffix = glue::glue("_{data_set}")
  ) |>
  add_analysis_name(log_folder = log_folder,
                    do_folder = do_folder,
                    suffix = suffix) |>
  mutate(
    data_name = case_when(
      data_set == "main" ~ "mt",
      data_set == "long_no_pa" ~ "mt_no_pa",
      TRUE ~ paste0("mt_" , data_set)
    )
  )


analysis_spec_multi_timepoint_cc_nfu <- common_analysis_spec |>
  filter(model_name == "mt_fi_study_rs", predictor_set == "pred1_mt") |>
  mutate(
    data = list(analysis_data_long),
    data_name = "mt",
    pred_waves1 = "0",
    pred_waves2 = "0 -1",
    pred_waves3 = "0 -1 -2"
  ) |>
  pivot_longer(
    cols = starts_with("pred_waves"),
    names_to = "n_pred_waves",
    values_to = "pred_waves",
    ,
    names_prefix = "pred_waves"
  ) |>
  mutate(out_wave = 2,
         suffix = glue::glue("_{n_pred_waves}pred")) |>
  add_analysis_name(log_folder = log_folder,
                    do_folder = do_folder,
                    suffix = suffix)





# Multi timepoint analysis - with multiple imputation --------------------------------------------


analysis_spec_multi_timepoint_mi_pred2 <- common_analysis_spec |>
  filter(model_name %in% c("mt_fi_study_rs", "mt_fi_study_ri"),
         predictor_set == "pred2_mt") |>
  mutate(
    data = list(analysis_data_long_mi),
    data_name = "mt_mi",
    pred_waves = "0 -1",
    out_wave = 2,
    multiple_imputed_data = TRUE
  ) |>
  add_analysis_name(log_folder = log_folder, do_folder = do_folder)

analysis_spec_multi_timepoint_mi_pred3 <- common_analysis_spec |>
  filter(model_name %in% c("mt_fi_study_rs", "mt_fi_study_ri"),
         predictor_set == "pred3_mt") |>
  mutate(
    data = list(analysis_data_long_mi),
    data_name = "mt_mi",
    pred_waves = "0 -1",
    out_wave = 2,
    multiple_imputed_data = TRUE
  ) |>
  add_analysis_name(log_folder = log_folder, do_folder = do_folder)

analysis_spec_multi_timepoint_mi_pred3a <- common_analysis_spec |>
  filter(model_name %in% c("mt_fi_study_rs", "mt_fi_study_ri"),
         predictor_set == "pred3a_mt") |>
  mutate(
    data = list(analysis_data_long_mi),
    data_name = "mt_mi",
    pred_waves = "0 -1",
    out_wave = 2,
    multiple_imputed_data = TRUE
  ) |>
  add_analysis_name(log_folder = log_folder, do_folder = do_folder)


#analysis_spec_single_timepoint_cc, analysis_spec_multi_timepoint_cc

analysis_spec <- bind_rows(analysis_spec_single_timepoint_cc,
                           analysis_spec_multi_timepoint_cc) |>
  select(-data)

analysis_spec <- bind_rows(
  analysis_spec_single_timepoint_cc,
  analysis_spec_multi_timepoint_cc,
  analysis_spec_multi_timepoint_cc_datasets,
  analysis_spec_multi_timepoint_cc_nfu,
  analysis_spec_multi_timepoint_mi_pred3,
  analysis_spec_multi_timepoint_mi_pred2,
  analysis_spec_multi_timepoint_mi_pred3a
) |>
  select(-data)

analysis_spec |>
  saveRDS(here::here(results_folder, "analysis_spec.rds"))

# Running models
set.seed(5614)
tictoc::tic()
mt_results <- run_many_models(analysis_spec_single_timepoint_cc)
tictoc::toc()

# Running models
set.seed(5614)
tictoc::tic()
mt_results <- run_many_models(analysis_spec_multi_timepoint_cc)
tictoc::toc()


set.seed(6146146)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_cc_datasets)
tictoc::toc()

set.seed(645164)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_cc_nfu)
tictoc::toc()



set.seed(6541)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_mi_pred3)
tictoc::toc()

set.seed(516546)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_mi_pred2)
tictoc::toc()


set.seed(516546)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_mi_pred3a)
tictoc::toc()



# Additional analysis

# Poisson
# Allowing for hetrogeneity in residual variance.
