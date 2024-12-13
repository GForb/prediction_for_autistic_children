

# Multi-timepoint models

## Multi-timepoint model with random intercept and slope, random study

model_pred_gsem_ri_study_rs_id <- function(data,
                                           outcome,
                                           predictors,
                                           intercept_est,
                                           log_file,
                                           pred_waves,
                                           out_wave,
                                           do_file = NULL,
                                           model_only = FALSE, cv_only = FALSE) {
  wave_var <- "wave"
  id_var <- "ID"
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue(
    "cap drop age_spline1
                             cap drop age_spline2
                            mkspline age_spline = age_c, nknots(3) cubic"
  )
  
  model_code <- glue::glue("gsem ({outcome} <- {predictors} STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)")
  simple_model_options <- "var(M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)"
  run_model <- glue::glue(
    "
    {model_code}, var(STUDY[study]  M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
    mat simple = e(b)
    {model_code}, from(simple)
  "
  )
  
  run_model_only <- run_model
  run_model_only_mi <- glue::glue(
    "
    mi estimate, cmdok: model_code}, var(STUDY[study]  M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
    mat simple = e(b)
    mi estimate, cmdok: {model_code}, from(simple)
  "
  ) 
  
  
  analysis_code <- glue::glue(
    "
  get_all_preds {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	random_study ///
    	random_slope ///
    	simple_model({model_code}) ///
    	simple_model_options({simple_model_options})
    "
  )
  
  
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    int_valid_code = int_valid_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
}


## Multi-timepoint model with random intercept, random study

model_pred_gsem_ri_study_ri_id <- function(data,
                                           outcome,
                                           predictors,
                                           intercept_est,
                                           log_file,
                                           pred_waves,
                                           out_wave,
                                           do_file = NULL,
                                           model_only = FALSE, cv_only = FALSE) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue(
    "cap drop age_spline1
                             cap drop age_spline2
                            mkspline age_spline = age_c, nknots(3) cubic"
  )
  
  wave_var <- "wave"
  study_var <- "study"
  id_var <- "ID"
  
  model_code <- glue::glue("gsem ({outcome} <- {predictors} STUDY[study]@1 M1[ID]@1)")
  run_model <- glue::glue("
    {model_code}
    matrix all = e(b)
    ")
  
  run_model_only <- run_model
  run_model_only_mi <- glue::glue(
    "
    mi estimate, cmdok:{model_code}
  "
  ) 
  
  analysis_code <- glue::glue(
    "
    get_all_preds {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options(from(all)) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	random_study
    "
  )
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
  
}

# Multi-timpoint models for fixed study



model_pred_gsem_ri_study_rs_id <- function(data,
                                           outcome,
                                           predictors,
                                           intercept_est,
                                           log_file,
                                           pred_waves,
                                           out_wave,
                                           do_file = NULL,
                                           model_only = FALSE, cv_only = FALSE) {
  wave_var <- "wave"
  id_var <- "ID"
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue(
    "cap drop age_spline1
                             cap drop age_spline2
                            mkspline age_spline = age_c, nknots(3) cubic"
  )
  
  model_code <- glue::glue("gsem ({outcome} <- {predictors} STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)")
  simple_model_options <- "var(M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)"
  run_model <- glue::glue(
    "
    {model_code}, var(STUDY[study]  M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
    mat simple = e(b)
    {model_code}, from(simple)
  "
  )
  
  run_model_only <- run_model
  run_model_only_mi <- glue::glue(
    "
    mi estimate, cmdok: {model_code}, var(STUDY[study]  M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
    mat simple = e(b)
    mi estimate, cmdok: {model_code}, from(simple)
  "
  )
  
  
  analysis_code <- glue::glue(
    "
  get_all_preds {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	random_study ///
    	random_slope ///
    	simple_model({model_code}) ///
    	simple_model_options({simple_model_options})
    "
  )
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
  
}


## Multi-timepoint model with random intercept, fixed study



model_pred_gsem_fi_study_ri_id <- function(data,
                                           outcome,
                                           predictors,
                                           intercept_est,
                                           log_file,
                                           pred_waves,
                                           out_wave,
                                           do_file = NULL,
                                           model_only = FALSE, cv_only = FALSE) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue(
    "cap drop age_spline1
                             cap drop age_spline2
                            mkspline age_spline = age_c, nknots(3) cubic"
  )
  
  wave_var <- "wave"
  study_var <- "study"
  id_var <- "ID"
  
  model_code <- glue::glue("gsem ({outcome} <- study_* {predictors}  M1[ID]@1)")
  model_options <- "nocons"
  
  run_model <- glue::glue("
    {model_code}, {model_options}
    ")
  
  run_model_only <- run_model
  run_model_only_mi <- glue::glue(
    "
    mi estimate, cmdok: {model_code}, {model_options}
  "
  )
  

  
  analysis_code <- glue::glue(
    "
    get_all_preds {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    "
  )
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
}
## Fixed study, random slope for ID

model_pred_gsem_fi_study_rs_id <- function(data,
                                           outcome,
                                           predictors,
                                           intercept_est,
                                           log_file,
                                           pred_waves,
                                           out_wave,
                                           do_file = NULL,
                                           model_only = FALSE, cv_only = FALSE) {
  wave_var <- "wave"
  id_var <- "ID"
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue(
    "cap drop age_spline1
                             cap drop age_spline2
                            mkspline age_spline = age_c, nknots(3) cubic"
  )
  model_options <- "nocons"
  
  mixed_model <- glue::glue("mixed {outcome} study_* {predictors}, noconstant   || ID: age_c")
  mixed_model_options <- "cov(unstructured)"
  
  model_options <- "nocons"
  
  model_code <- glue::glue("gsem ({outcome} <- study_* {predictors}  M1[ID]@1 c.age_c#M2[ID]@1)")
  run_model <- glue::glue(
    "
    mixed_first_gsem_rs , ///
			gsem_code({model_code}) ///
    gsem_options({model_options}) ///
			mixed_code({mixed_model}) ///
    mixed_model_options({mixed_model_options})
  "
  )
  
  run_model_only <- glue::glue(" {mixed_model}, {mixed_model_options}")
  run_model_only_mi <- glue::glue("mi estimate: {run_model_only}")
  
  
  analysis_code <- glue::glue(
    "
  get_all_preds {study_var}, ///
      model_code({model_code}) ///
      model_options({model_options}) ///
    	intercept_est({intercept_est}) ///
    	out_wave({out_wave}) ///
    	predictor_waves({pred_waves}) ///
    	mixed_model({mixed_model}) ///
    	mixed_model_options(\"{mixed_model_options}\")
    "
  )
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
  
}

# Single timepoint models
## Single timepoint, fixed study

model_pred_reg_fi_study <- function(data,
                                    outcome,
                                    predictors,
                                    intercept_est,
                                    log_file,
                                    do_file = NULL,
                                    model_only = FALSE, cv_only = FALSE) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue("mkspline base_spline = base_{outcome}, nknots(3) cubic")
  
  model_code <- glue::glue("regress out_{outcome} study_*  {predictors} ")
  model_options <- "nocons"
  run_model <- glue::glue("{model_code}, {model_options}")
  
  run_model_only <- run_model
  run_model_only_mi <- glue::glue("mi estimate: {run_model_only}")
  
  
  analysis_code <- glue::glue(
    "
   get_all_preds {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	model_options({model_options}) ///
    	single_time_point
    "
  )
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
}

model_pred_gsem_ri_study <- function(data,
                                     outcome,
                                     predictors,
                                     intercept_est,
                                     log_file,
                                     do_file = NULL,
                                     model_only = FALSE, cv_only = FALSE) {
  study_var <- "study"
  stata_prog_source <- here::here("Stata", "prediction_progs.do")
  make_spline <- glue::glue("mkspline base_spline = base_{outcome}, nknots(3) cubic")
  
  model_code <- glue::glue("gsem (out_{outcome} <- STUDY[study]  {predictors} )")
  run_model <- model_code
  
  run_model_only <- run_model
  run_model_only_mi <- glue::glue("mi estimate, cmdok: {run_model_only}")
  
  
  analysis_code <- glue::glue(
    "
   get_all_preds {study_var}, ///
      model_code({model_code}) ///
    	intercept_est({intercept_est}) ///
    	single_time_point ///
    	random_study
    "
  )
  
  run_stata_code(
    data = data,
    log_file = log_file,
    stata_prog_source = stata_prog_source,
    make_spline = make_spline,
    analysis_code = analysis_code,
    run_model = run_model,
    do_file = do_file,
    model_only = model_only, cv_only = cv_only,
    run_model_only = run_model_only,
    run_model_only_mi = run_model_only_mi
  )
}
