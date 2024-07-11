# Runs a series of models to chekc all of the different mdoels I have stata code for.

# Load SDQ data
sdq_wide_data <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(autism != "post baseline")
sdq_long_data <- readRDS(here(derived_data, "pooled_sdq.Rds")) |> filter(autism != "post baseline")

nrow(sdq_wide_data)
analysis_data_wide <- sdq_wide_data |> filter(out_all_complete, base_all_complete)
nrow(analysis_data_wide)

analysis_data_long <- sdq_long_data |> filter(out_all_complete, base_all_complete)

results_folder <- here::here(data_and_outputs, "Results", "Test")
log_folder <- here::here(results_folder, "Logs")
do_folder <- here::here(results_folder, "Do Files")

outcomes <- c("sdq_emot_p")
intercept_est_methods <- "average estimate estimate_cv"


analysis_spec_single_timepoint <- tibble(outcome = "sdq_emot_p", 
                                         intercept_est = "average estimate estimate_cv") |> 
  mutate(
    predictors = glue::glue("base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age  base_sex") |> as.character(),
    st_ri_study = list(model_pred_gsem_ri_study),
    st_fi_study = list(model_pred_reg_fi_study)
  ) |> 
  pivot_longer(cols = starts_with("st"), names_to = "model_name", values_to = "model_function") |>
  mutate(
    analysis_name = glue::glue("{model_name}_{outcome}") |> as.character(),
    log_file = here::here(log_folder, analysis_name),
    do_file = here::here(do_folder, analysis_name),
    data = list(analysis_data_wide),
    predictor_set = "pred_full"
  )

tictoc::tic()
st_results <- run_many_models(analysis_spec_single_timepoint)
tictoc::toc()


analysis_spec_multi_timepoint <- tibble(outcome = "sdq_emot_p", 
                                        intercept_est = "average estimate estimate_cv") |> 
  mutate(
         predictors = glue::glue("age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex ") |> as.character(),
         mt_fi_study_ri = list(model_pred_gsem_fi_study_ri_id),
         mt_fi_study_rs = list(model_pred_gsem_fi_study_rs_id),
         mt_ri_study_rs = list(model_pred_gsem_ri_study_rs_id),
         mt_ri_study_rs = list(model_pred_gsem_ri_study_rs_id)) |> 
  pivot_longer(cols = starts_with("mt"), names_to = "model_name", values_to = "model_function") |>
  mutate(
    analysis_name = glue::glue("{model_name}_{outcome}") |> as.character(),
    log_file = here::here(log_folder, paste0(analysis_name, ".log")),
    data = list(analysis_data_long),
    pred_waves = "0 -1",
    out_wave = 2,
    predictor_set = "pred_full"
  )

tictoc::tic()
mt_results <- run_many_models(analysis_spec_multi_timepoint)
tictoc::toc()


analysis_spec <- bind_rows(analysis_spec_single_timepoint, analysis_spec_multi_timepoint) |> 
  select(-data) 

analysis_spec |> 
  saveRDS(here::here(results_folder, "analysis_spec.rds")) 

# Load a particular model


create_full_results_table(results_folder)

full_results <- readRDS(here::here(results_folder, "results_meta_analysis.rds"))
full_results_long <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds"))

full_results_long <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds"))
full_results_long |> print(n = 100)

full_results |> select(model,intercept_est_method,  starts_with("r_squared"), starts_with("rmse"), starts_with("calib_slope"), starts_with("calib_itl"))

full_results_long |> select(metric, est, est)

single_model <- readRDS(here::here(results_folder, "st_ri_study_sdq_emot_p_average.rds"))

single_model |> 
  pivot_longer(cols = starts_with("pred"), names_to = "int_est", values_to = "pred") 

analysis_spec |> 
  mutate(file_name = paste0(analysis_name,".rds")) |> 
  pivot_longer(cols = starts_with("pred"), names_to = "int_est", values_to = "pred") |> 
  mutate(int_est_method2 = int_est_method2 |> substr(5, length(int_est_method2)))