
sdq_wide_data <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(autism != "post baseline")
sdq_long_data <-  
  readRDS(here(derived_data, "pooled_sdq.Rds")) |> filter(autism != "post baseline") |> 
  mutate(sdq_pro_p = 10 - sdq_pro_p)
  

nrow(sdq_wide_data)
analysis_data_wide <- sdq_wide_data |> filter(out_all_complete, base_all_complete)
nrow(analysis_data_wide)

analysis_data_long <- sdq_long_data |> filter(out_all_complete, base_all_complete)

results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
log_folder <- here::here(results_folder, "Logs")
# Regression

outcomes <- c("sdq_emot_p", "sdq_cond_p", "sdq_hyp_p", "sdq_pro_p", "sdq_peer_p")
intercept_est_methods <- c("average", "estimate") 

 

# for(outcome in outcomes){
#   for(intercept_est in intercept_est_methods){

outcome <- "sdq_emot_p"
intercept_est <- "average"

# each analysis requires:
# a model function
# a set of predictors
# an outcome
# an intercept estimation method
# a name

# Single timepoint analysis are different to multi-timepoint analyses


baseline_outcomes_string <- paste0("base_", outcomes) |> paste(collapse = " ")


  
outcome <- "sdq_pro_p" #c("sdq_emot_p", "sdq_cond_p", "sdq_hyp_p", "sdq_pro_p", "sdq_peer_p")
intercept_est <- c("average")


analysis_spec_single_timepoint_simple <- expand_grid(outcome, intercept_est) |>
  rowwise() |>
  mutate(
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    pred_init = glue::glue("base_age out_age {baseline_outcomes_string} base_sex") |> as.character(),
    pred_full = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age {non_outcome_baseline} base_sex") |> as.character()
  ) |>
  ungroup() |>
  pivot_longer(cols = starts_with("pred"), names_to = "predictor_set", values_to = "predictors") |>
  mutate(
    model_function = list(model_pred_gsem_ri_study),
    analysis_name = glue::glue("st_ri_study_{outcome}_{predictor_set}_int_{intercept_est}") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    data = list(analysis_data_wide),
    model_name = "st_ri_study"
  )

st_results <- run_many_models(analysis_spec_single_timepoint_simple)

analysis_spec_multi_timepoint_simple <- expand_grid(outcome, intercept_est) |>
  mutate(
    model_function = list(model_pred_gsem_ri_study_rs_id),
    model_name = "mt_ri_study_rs",
    non_outcome_baseline = stringr::str_remove_all(string = baseline_outcomes_string, pattern = paste0("base_", outcome)),
    predictors   = glue::glue("age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex  {non_outcome_baseline} ") |> as.character(),
    analysis_name = glue::glue("{model_name}_{outcome}_int_{intercept_est}") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    data = list(analysis_data_long),
    pred_waves = "0 -1",
    out_wave = 2,
    predictor_set = "pred_full"
  )


mt_results_simple <- run_many_models(analysis_spec_multi_timepoint_simple)

