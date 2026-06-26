# Model parameters:

sdq_results <- process_results_for_outcome("sdq")
st_sdq <- sdq_results |> 
  filter(model_name  == "st_fi_study",  predictor_set == "pred1") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(coef, starts_with("sdq")) 

# Load the sdq data
sdq_params <- st_sdq
outcome_name <- "sdq_pro_p"
predict_sdq <- function(individual_data, outcome_name, sdq_params, sudy_intercept = NULL) {
  params <- sdq_params |> pull(outcome_name)
  names(params) <- sdq_params$coef
  if(is.null(sudy_intercept)){
    study_intercepts <- params[names(params) |> startsWith("study")]
    intercept = mean(study_intercepts)
  } else {
    intercept <- params[sudy_intercept]
  }
  
  pred <- intercept
  return(pred)
}

predict_sdq(1, "sdq_pro_p", st_sdq)
  
sdq_results |> 
  filter(model_name  == "st_fi_study",  predictor_set == "pred1") 


st_sdq |> print(n = 100)

# have inputs

# create spline term for predictor

# make prediction

# for now, select someone from data to save having to caclulate spline terms. Even create a simple mapping table.

# Make predictions... 