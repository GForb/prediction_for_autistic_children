
run_stata_code <- function(
    data, 
    log_file, 
    stata_prog_source, 
    make_spline, 
    run_model, 
    analysis_code, 
    do_file = NULL, 
    model_only = FALSE, cv_only = FALSE,
    run_model_only,
    run_model_only_mi) {
  
  if(model_only){
    run_stata_code_model_only(
    data, 
    log_file, 
    make_spline, 
    do_file,
    run_model_only,
    run_model_only_mi)
    
  } else if(cv_only) {
    run_stata_code_cv_only(
    data, 
    log_file, 
    stata_prog_source, 
    make_spline, 
    run_model, 
    analysis_code, 
    do_file)
  } else {
    

  
 
  
  int_valid_code <- glue::glue(
    "{analysis_code} ///
    cross_validation ///
    	n_cv_folds(5) ///
	    n_reps(2)")
  
  
  
  run_stata_code_inner <- function(code, name) {

      stata_code <- glue::glue("
      
      cap log close results_log
      log using \"{log_file}_{name}.log\", text replace name(results_log)
      
      qui do \"{stata_prog_source}\"
      
      {make_spline}
  
  *********************** Running model on whole populaiton ***********************
      {run_model}
  
  *********************** Validating ***********************
         
      {code}
      
      log close results_log
  
    ")
      
    
    if(!is.null(do_file)) {
      print("writing do file")
      print(do_file)
      writeLines(stata_code, glue::glue("{do_file}_{name}.do"))
    }
    
    print("THIS IS THE STATA CODE")
    print(stata_code)
    cat("\n \n \n")
    
    
    results <-  RStata::stata(stata_code, data.in = data, data.out = TRUE) |> 
      mutate(validation = name)
    
    results  |> select(ID, validation, study, starts_with("pred"), actual = starts_with("actual"), everything())
  }
  
  
  
  
  
  results <- imap(list(cv = int_valid_code, iecv = analysis_code), run_stata_code_inner)
  bind_rows(results)
  }
}

run_stata_code_model_only <- function(
    data, 
    log_file, 
    make_spline, 
    do_file = NULL,
    run_model_only,
    run_model_only_mi) {
  
  
  stata_code <- glue::glue("
      
    cap log close results_log
    log using \"{log_file}_model_only.log\", text replace name(results_log)
    
    {make_spline}
  
  *********************** Running model on whole populaiton ***********************
  capture confirm variable mi_m
  if !_rc {{
  tempfile temp
  save  `temp'
     mi import flong, id(mi_id) m(mi_m)
     {run_model_only_mi}
  }}
  else {{
      {run_model_only}
   }}

    mat A = r(table)
    matrix At =  A'	
    clear
 
    svmat2 At, names(col)  rnames(coef)
    gen N = `e(N)'

    log close results_log

    ")
  
  
  if(!is.null(do_file)) {
    print("writing do file")
    do_file <- glue::glue("{do_file}_model_only.do")
    print(do_file)
    writeLines(stata_code, do_file)
  }
  
  results <-  RStata::stata(stata_code, data.in = data, data.out = TRUE) 
  
  return(results)
  
}

run_stata_code_cv_only <- function(  data, 
                                     log_file, 
                                     stata_prog_source, 
                                     make_spline, 
                                     run_model, 
                                     analysis_code, 
                                     do_file){
  
  
  int_valid_code <- glue::glue(
    "{analysis_code} ///
    cross_validation ///
    	n_cv_folds(10) ///
	    n_reps(10)")
   name <- "cv"
    stata_code <- glue::glue("
      
      cap log close results_log
      log using \"{log_file}_{name}.log\", text replace name(results_log)
      
      qui do \"{stata_prog_source}\"
      
      {make_spline}
  
  *********************** Running model on whole populaiton ***********************
      {run_model}
  
  *********************** Validating ***********************
         
      {int_valid_code}
      
      log close results_log
  
    ")
    
    
    if(!is.null(do_file)) {
      print("writing do file")
      print(do_file)
      writeLines(stata_code, glue::glue("{do_file}_{name}.do"))
    }
    
    print("THIS IS THE STATA CODE")
    print(stata_code)
    cat("\n \n \n")
    
    
    results <-  RStata::stata(stata_code, data.in = data, data.out = TRUE) |> 
      mutate(validation = name)
    
    results  |> select(ID, validation, study, starts_with("pred"), actual = starts_with("actual"), everything())
  
  
}
