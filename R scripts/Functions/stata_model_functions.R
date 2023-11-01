

# Univariate random intercept ---------------------------------------------

stata_results_many_outcomes <- name <- function(stata_model_fun, timepoints, data, outcomes) {
  results_list <- lapply(outcomes, stata_results_many_timpoints, data=data, timepoints = timepoints, stata_model_fun = stata_model_fun)
  results_df <- results_list |> dplyr::bind_rows(.id = "outcome") |> select(outcome, everything())
  results_df$outcome <- outcomes[results_df$outcome]
  return(results_df)
}

stata_results_many_timpoints <- function(outcome, timepoints, data, stata_model_fun) {
  results_list <- lapply(timepoints, univariate_random_int_1tp, data=data, outcome = outcome, stata_model_fun = stata_model_fun)
  results_list |> dplyr::bind_rows(.id = "timepoints") |> select(timepoints, everything())
}

univariate_random_int_1tp <- function(n_timepoints, data, outcome, stata_model_fun) {
  results <- stata_model_fun(data = data, 
                             outcome = outcome, 
                             n_timepoints = n_timepoints) |> 
    IPDPredictR:::meta_analyse_predictions(IPDPredictR:::evaluate_performance_cont_obs_pred)
}


stata_univariate_random_intercept <- function(data, outcome, n_timepoints) {
  
    data_pre_processing <-  get_data_preprocessing()
    predictors <-   "i.studyid   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_sdq_emot_p base_age base_sex"
    predictors <- predictors |> stringr::str_remove(glue::glue("base_{outcome}"))
    model <- glue::glue('
                        gsem ({outcome} <-  {predictors} M1[ID]@1) if train_data
                        ')
    
    predict <-  glue::glue('
      cap prog drop my_predict
      prog define my_predict
      syntax namelist, 
      	preserve
      	keep if relative_wave <=0  & relative_wave >=  1- {n_timepoints} 
      	di \"Number with predictor outcomes\"
      	count if sdq_emot_p != .
      	tempvar random_part 
      	predict `random_part\' , latent
      	tempfile tempfile
  	    keep if relative_wave ==0
  	    save `tempfile\'
  	    restore
      	merge m:1 studyid ID using `tempfile\', keepusing(`random_part\')
      	drop _merge
      	tempvar fixed
      	predict `fixed\', fixedonly
      	gen `namelist\' = `fixed\' + `random_part\'
      end
      
      keep if test_data
      my_predict pred
      rename {outcome} actual
      rename studyid study
      keep if relative_wave ==1
      keep actual study pred
      
      ')
    
    reg_pred <- RStata::stata(paste(data_pre_processing, model, predict, sep = "\n"),  data.in = data, data.out = TRUE)
    colnames(reg_pred) <- c("actual", "study" ,"pred")
    
    return(reg_pred)
}





get_data_preprocessing <- function() {
  "
    encode studyid, gen(studyid2)
    drop studyid
    rename studyid2 studyid
"
}