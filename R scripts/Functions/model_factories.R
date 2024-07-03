
# Multi-timepoint models

## Multi-timepoint model with random intercept and slope, random study

model_pred_gsem_ri_study_rs_id <- function(data, outcome, predictors, intercept_est, log_file,  pred_waves, out_wave, do_file = NULL) {
  wave_var <- "wave"
  id_var <- "ID"
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- glue::glue("mkspline age_spline = age_c, nknots(3) cubic")
  
  model_code <- glue::glue("gsem ({outcome} <- {predictors} STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)")
  simple_model_options <- "var(M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)"
  run_model <- glue::glue("
    {model_code}, var(STUDY[study]  M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
    mat simple = e(b)
    {model_code}, from(simple)
  ")
  
  
  analysis_code <- glue::glue("
  get_all_preds_gsem {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	random_study ///
    	random_slope ///
    	simple_model({model_code}) ///
    	simple_model_options({simple_model_options})
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 run_model = run_model,
                 do_file = do_file)
  
}


## Multi-timepoint model with random intercept, random study

model_pred_gsem_ri_study_ri_id <- function(data, outcome, predictors, intercept_est, log_file, pred_waves, out_wave, do_file = NULL) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- glue::glue("mkspline age_spline = age_c, nknots(3) cubic")
  
  wave_var <- "wave"
  study_var <- "study"
  id_var <- "ID"
  
  model_code <- glue::glue("gsem ({outcome} <- {predictors} STUDY[study]@1 M1[ID]@1)")
  run_model <- glue::glue("
    {model_code}
    matrix all = e(b)
    ")
  
  analysis_code <- glue::glue("
    get_all_preds_gsem {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options(from(all)) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	random_study
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 run_model = run_model,
                 do_file = do_file)
  
}

# Multi-timpoint models for fixed study



model_pred_gsem_ri_study_rs_id <- function(data, outcome, predictors, intercept_est, log_file,  pred_waves, out_wave, do_file = NULL) {
  wave_var <- "wave"
  id_var <- "ID"
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- glue::glue("mkspline age_spline = age_c, nknots(3) cubic")
  
  model_code <- glue::glue("gsem ({outcome} <- {predictors} STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)")
  simple_model_options <- "var(M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)"
  run_model <- glue::glue("
    {model_code}, var(STUDY[study]  M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
    mat simple = e(b)
    {model_code}, from(simple)
  ")
  
  
  analysis_code <- glue::glue("
  get_all_preds_gsem {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	random_study ///
    	random_slope ///
    	simple_model({model_code}) ///
    	simple_model_options({simple_model_options})
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 run_model = run_model,
                 do_file = do_file)
  
}


## Multi-timepoint model with random intercept, fixed study

model_pred_vabs_gsem <- function(data, pred_waves, outcome, model_code, model_options, intercept_est, log_file, do_file = NULL) {
  out_wave <- 1

  
  analysis_code <- glue::glue("
    get_all_preds_gsem {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 model_code = model_code,
                 model_options = model_options,
                 do_file = do_file)
}

model_pred_gsem_fi_study_ri_id <- function(data, outcome, predictors, intercept_est, log_file, pred_waves, out_wave, do_file = NULL) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- glue::glue("mkspline age_spline = age_c, nknots(3) cubic")
  
  wave_var <- "wave"
  study_var <- "study"
  id_var <- "ID"
  
  model_code <- glue::glue("gsem ({outcome} <- study_* {predictors}  M1[ID]@1)")
  model_options <- "nocons"
  
  run_model <- glue::glue("
    {model_code}, {model_options}
    ")

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
                 run_model = run_model,
                 do_file = do_file)
}
## Fixed study, random slope for ID

model_pred_gsem_fi_study_rs_id <- function(data, outcome, predictors, intercept_est, log_file, pred_waves, out_wave, do_file = NULL) {
  wave_var <- "wave"
  id_var <- "ID"
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_gsem.do")
  make_spline <- glue::glue("mkspline age_spline = age_c, nknots(3) cubic")
  model_options <- "nocons"
  
  mixed_model <- glue::glue("mixed {outcome} study_* {predictors}, noconstant   || ID: age_c")
  mixed_model_options <- "cov(unstructured)"
  
  model_options <- "nocons"
  
  model_code <- glue::glue("gsem ({outcome} <- study_* {predictors}  M1[ID]@1 c.age_c#M2[ID]@1)")
  run_model <- glue::glue("
    mixed_first_gsem_rs , ///
			gsem_code({model_code}) ///
    gsem_options({model_options}) ///
			mixed_code({mixed_model}) ///
    mixed_model_options({mixed_model_options})
  ")
  
  
  analysis_code <- glue::glue("
  get_all_preds_gsem {study_var}, ///
      pred_var_name(pred) ///
      model_code({model_code}) ///
      model_options({model_options}) ///
    	intercept_est({intercept_est}) ///
    	predict_function(predict_multiwave_gsem_rs_uv) ///
    	predict_args(wave_var({wave_var}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	id_var({id_var})) ///
    	mixed_model({mixed_model}) ///
    	mixed_model_options(\"{mixed_model_options}\") 
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 run_model = run_model,
                 do_file = do_file)
  
}


## Single timepoint, fixed study

model_pred_reg_fi_study <- function(data, outcome, predictors, intercept_est, log_file, do_file = NULL) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_reg.do")
  make_spline <- glue::glue("mkspline base_spline = base_{outcome}, nknots(3) cubic")
  
  model_code <- glue::glue("regress (out_{outcome} study_*  {predictors} )")
  model_options <- "nocons"
  run_model <- glue::glue("{model_code}, {model_options}")
  
  analysis_code <- glue::glue("
   get_all_preds_reg {study_var}, ///
      pred_var_name(pred) ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) 
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 run_model = run_model,
                 do_file = do_file)
}



## Single timepoint, fixed study
model_pred_reg_factory <- function(outcome, model_code, model_options, intercept_est, log_file) {
  function(data) {
    model_pred_regress(
      data = data, 
      outcome = outcome, 
      model_code = model_code, 
      model_options = model_options, 
      intercept_est= intercept_est,
      log_file = log_file
    )
  }
}


model_pred_regress <- function(data, outcome, model_code, model_options, intercept_est, log_file, do_file = NULL) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs_reg.do")
  make_spline <- glue::glue("mkspline base_spline = base_{outcome}, nknots(3) cubic")
  
  analysis_code <- glue::glue("
   get_all_preds_reg {study_var}, ///
      pred_var_name(pred) ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) 
    ")
  
  run_stata_code(data = data,
                 log_file = log_file,
                 stata_prog_source = stata_prog_source,
                 make_spline = make_spline,
                 analysis_code = analysis_code,
                 model_code = model_code,
                 model_options = model_options,
                 do_file = do_file)
  
}

run_stata_code <- function(data, log_file, stata_prog_source, make_spline, run_model, analysis_code, do_file = NULL) {

  stata_code <- glue::glue("
    
    cap log close results_log
    log using \"{log_file}\", text replace name(results_log)
    
    qui do \"{stata_prog_source}\"
    
    {make_spline}

*********************** Running model on whole populaiton ***********************
    {run_model}

*********************** Validating ***********************
       
    {analysis_code}
    
    log close results_log

  ")
  
  if(!is.null(do_file)) {
    print("writing do file")
    print(do_file)
    writeLines(stata_code, do_file)
  }
  
  print("THIS IS THE STATA CODE")
  print(stata_code)
  cat("\n \n \n")
  
  
  results <-  RStata::stata(stata_code, data.in = data, data.out = TRUE)

  results  |> select(ID, study, pred = starts_with("pred"), actual = starts_with("actual"), everything())
  
}



