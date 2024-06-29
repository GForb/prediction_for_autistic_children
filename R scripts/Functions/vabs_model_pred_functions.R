

model_pred_vabs_gsem_factory <- function(pred_waves, outcome, model_code, model_options, intercept_est, log_file) {
  function(data) {
    model_pred_vabs_gsem(
      data = data, 
      pred_waves = pred_waves, 
      outcome = outcome, 
      model_code = model_code, 
      model_options = model_options, 
      intercept_est= intercept_est,
      log_file = log_file
      )
  }
}


model_pred_vabs_gsem <- function(data, pred_waves, outcome, model_code, model_options, intercept_est, log_file) {
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- "mkspline age_spline = age_c, nknots(3) cubic"
  out_wave <- 1
  wave_var <- "wave"
  study_var <- "study"
  id_var <- "ID"

  analysis_code <- glue::glue("
    get_all_preds_gsem {study_var}, ///
      pred_var_name(pred) ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) ///
    	predict_function(predict_multiwave_gsem_ri) ///
    	predict_args(wave_var({wave_var}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	id_var({id_var}))
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 model_code = model_code,
                 model_options = model_options)
}

model_pred_vabs_gsem_rs_factory <- function(pred_waves, outcome, model_code, model_options, intercept_est, log_file) {
  function(data) {
    model_pred_vabs_gsem_rs(
      data = data, 
      pred_waves = pred_waves, 
      outcome = outcome, 
      model_code = model_code, 
      model_options = model_options, 
      intercept_est= intercept_est,
      log_file = log_file
    )
  }
}
model_pred_vabs_gsem_rs <- function(data, pred_waves, outcome, model_code, model_options, intercept_est, log_file) {
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- "mkspline age_spline = age_c, nknots(3) cubic"
  out_wave <- 1
  wave_var <- "wave"
  study_var <- "study"
  id_var <- "ID"

  analysis_code <- glue::glue("
   get_all_preds_gsem {study_var}, ///
      pred_var_name(pred) ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) ///
    	predict_function(predict_multiwave_gsem_rs_uv) ///
    	predict_args(wave_var({wave_var}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	id_var({id_var})) 
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 model_code = model_code,
                 model_options = model_options)
  

}

