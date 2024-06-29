# devtools::install_github("GForb/IPDPredictR")
# library(IPDPredictR)
# detach("package:IPDPredictR", unload = TRUE, character.only = TRUE)


vabs_wide_data <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))
vabs_long_data <- readRDS(here(derived_data, "pooled_vabs.Rds"))


vabs_long_data |> count(study, out_wave)
vabs_long_data |> count(study, base_wave)

dls_data <- vabs_long_data |> filter(in_dls ==1, !is.na(vabs_dls_ae))
dls_wide <- vabs_wide_data |> filter(in_dls ==1, !is.na(out_vabs_dls_ae))

# Regression
model_pred_reg <- model_pred_reg_factory(
  intercept_est = intercept_est,
  model_options = "nocons",
  model_code = glue::glue("regress out_{outcome} study_* base_age out_age base_vabs_dls_ae base_vabs_com_ae base_vabs_soc_ae base_sex"),
  outcome = outcome,
  log_file = here::here(log_folder, paste0(model_full_name, ".log"))
)
results_reg <- model_pred_reg(dls_wide)

IPDPredictR:::evaluate_performance_cont_obs_pred(predicted = results_reg$pred, actual = results_reg$actual) # aggregate

IPDPredictR:::calibration_plot_cont(results_reg, study_var_name = "study")

meta_analysis <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results_reg, study_var_name = "study") 
meta_analysis$results_df

by_study <- IPDPredictR:::get_performance_by_study(
  by_study_predictions_df = results_int, 
  study_var_name = "study", 
  evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred)# aggregate


# Random intercept - no interaction with age
model_pred_ri <- model_pred_vabs_gsem_factory(
  pred_waves = "0 -1", 
  outcome = "vabs_dls_ae", 
  model_code = "gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex base_vabs_dq base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)",
  model_options = "nocons",
  intercept_est = "estimate"
  
) 
results <- model_pred_ri(dls_data)

IPDPredictR:::evaluate_performance_cont_obs_pred(predicted = results$pred, actual = results$actual) # aggregate

IPDPredictR:::calibration_plot_cont(results, study_var_name = "study")

meta_analysis <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results, study_var_name = "study") 
meta_analysis$results_df

## Random intercept, including interaction with vabs_dq

model_pred_ri_int <- model_pred_vabs_gsem_factory(
  pred_waves = "0 -1", 
  outcome = "vabs_dls_ae", 
  model_code = "gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 c.age_spline1#i.base_sex c.age_spline2#i.base_sex  c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq base_vabs_dq base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)",
  model_options = "nocons",
  intercept_est = "estimate"
) 
# Think about addin inverse variance weighting to the esitmate of the intercept when average is given

results_int <- model_pred_ri_int(dls_data)

by_study <- IPDPredictR:::get_performance_by_study(
  by_study_predictions_df = results_int, 
  study_var_name = "study", 
  evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred)# aggregate

studies <- by_study |> filter(metric == "calib_slope") |> pull(study)

meta_analysis <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results_int, study_var_name = "study") 

meta_analysis$results_df

tau1 <- meta_analysis$results_df |> filter(metric == "calib_slope") |> pull(tau2) |> format(scientific=F, digits = 2)
tau1_text = glue::glue("tau-squared = {tau1}")
metafor::forest(meta_analysis$results_list[[1]], addpred = TRUE, refline = 1 , slab = studies, xlab = "Calibration Slope",  header= "Study", mlab = tau1_text)
tau2 <- meta_analysis$results_df |> filter(metric == "calib_itl") |> pull(tau2) |> format(scientific=F, digits = 2)
tau2_text = glue::glue("tau-squared = {tau2}")
metafor::forest(meta_analysis$results_list[[2]], addpred = TRUE, refline = 0 , slab = studies, xlab = "Calibration in the Large", xlim = c(-3,3) , header= "Study", mlab = tau2_text)

  tau3 <- meta_analysis$results_df |> filter(metric == "r-squared") |> pull(tau2) |> format(scientific=F, digits = 2)
tau3_text = glue::glue("tau-squared = {tau3}")
metafor::forest(meta_analysis$results_list[[3]], addpred = TRUE, refline = NULL , slab = studies,  xlab = "R-squared", xlim = c(0,1.2) , header= "Study", mlab = tau3_text)



IPDPredictR:::calibration_plot_cont(results_int, study_var_name = "study")

## Random slope

model_pred_rs_int <- model_pred_vabs_gsem_rs_factory(
  pred_waves = "0 -1", 
  outcome = "vabs_dls_ae", 
  model_code = "gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 c.age_spline1#i.base_sex c.age_spline2#i.base_sex  c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq base_vabs_dq base_vabs_soc_ae base_vabs_com_ae M1[ID]@1 c.age_c#M2[ID]@1)",
  model_options = "nocons",
  intercept_est = "estimate"
) 


results_rs <- model_pred_rs_int(dls_data)

by_study <- IPDPredictR:::get_performance_by_study(
  by_study_predictions_df = results_rs, 
  study_var_name = "study", 
  evaluate_performance =IPDPredictR:::evaluate_performance_cont_obs_pred) # aggregate

studies <- by_study |> filter(metric == "calib_slope") |> pull(study)

meta_analysis <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results_int, study_var_name = "study") 

meta_analysis$results_df


