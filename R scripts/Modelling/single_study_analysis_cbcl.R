# For SDQ:

# 1: In a rich, longitudianl environment is the difference between single timepoint and multimepoint still small
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Single Study")
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")



# Load wide data
baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")



analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete, study == "TRAILS")
analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl.Rds")) |> filter(base_all_complete, out_all_complete, all_complete, study == "TRAILS")  


# Load wide data
outcomes <- analysis_data_long |> select(starts_with("cbcl")) |> colnames()
baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")


# 337 obs in data, 19 parameters

common_analysis_spec <- tibble(outcome = outcomes, intercept_est = "average") |>
  rowwise() |>
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred1 = glue::glue(
      "base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex"
    ) |> as.character(),
    pred1_mt = glue::glue(
      "age_spline1 age_spline2 ///
                                       base_sex ///
                                      c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
                                       {non_outcome_baseline}"
    ) |> as.character(),
    mt_fi_study_rs = list(model_pred_gsem_fi_study_rs_id),
    mt_fi_study_ri = list(model_pred_gsem_fi_study_ri_id),
    st_fi_study = list(model_pred_reg_fi_study),
    cv_only = TRUE
  ) |>
  ungroup() |>
  pivot_longer(cols = starts_with("pred"),
               names_to = "predictor_set",
               values_to = "predictors") |>
  pivot_longer(cols = starts_with(c("mt", "st")),
               names_to = "model_name",
               values_to = "model_function")


analysis_spec_single_timepoint_cc <- common_analysis_spec |> 
  filter(model_name == "st_fi_study",
         predictor_set == "pred1") |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder) |> 
  mutate(data = list(analysis_data_wide), data_name = "st")

analysis_spec_multi_timepoint_cc <- common_analysis_spec |> 
  filter(model_name %in% c("mt_fi_study_rs","mt_fi_study_ri"),
         predictor_set == "pred1_mt") |>
  mutate(
        data = list(analysis_data_long),
        pred_waves = "0 -1",
        out_wave = 2,
  ) |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder) 


analysis_spec_multi_timepoint_cc_nfu <- common_analysis_spec |> 
  filter(model_name == "mt_fi_study_rs",
         predictor_set == "pred1_mt") |>
  mutate(
    data = list(analysis_data_long),
    pred_waves = "0 -1 -2") |> 
  mutate(
    out_wave = 2,
    suffix = glue::glue("_3pred")
  ) |> 
  add_analysis_name(log_folder = log_folder, do_folder = do_folder, suffix = suffix)


analysis_spec <- bind_rows(
    analysis_spec_single_timepoint_cc, 
    analysis_spec_multi_timepoint_cc,
    analysis_spec_multi_timepoint_cc_nfu,
  ) |> 
  select(-data) 

analysis_spec |> 
  saveRDS(here::here(results_folder, "analysis_spec.rds")) 

set.seed(5614)
tictoc::tic()
mt_results <- run_many_models(analysis_spec_single_timepoint_cc) 
tictoc::toc()

set.seed(6146146)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_cc) 
tictoc::toc()

set.seed(2934875)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_cc_nfu) 
tictoc::toc()



create_full_results_table(results_folder,cv_only = TRUE)
full_results <- readRDS(here::here(results_folder,"results_meta_analysis.rds"))
outcomes <- unique(full_results$outcome)

for(myOut in outcomes){
  full_results |>   
    arrange(intercept_est_method) |>
    filter(outcome == myOut) |> 
    select(outcome  , analysis_name,rmse, r_squared_transformed) |> 
    print(n = 24) 
}
# Fit single timepoint model to MCS data
# Load long data
# Fit longitudinal model to MCS data
# Comapre performance via cross validation

# 2: With good measurement, do adding predicors, still lead to little benefit.

# Load long data
# Run multiple imputation on just MCS data
# Fit model with all predictors to MCS data (maybe limit)
# Load long data
# Fit model with all some predictors to MCS data (maybe limit)
# Compare performance.

# Crucial Q: Can I validate with one study.
