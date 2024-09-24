model_R_reg_fi_study <- function(data, outcome, predictors, intercept_est, log_file = NULL){
  if(!is.null(log_file)){
    sink(paste0(log_file, ".txt"))
  }
  predictors_string <- get_predictors_fixed_study(data, predictors)
  
  model_formula = as.formula(paste0("out_", outcome, " ~ ", paste(predictors_vector, collapse = "+")))
  print(model_formula)
  
  model_function <- function(data){
    lm(formula = model_formula, data = data)
  }

  print("Overall Model")
  summary(model) |> print()
  
  studies <- data$study |> unique()
  
  # Internal-external cross validaiton - with different intercept re-estimateion
  for(hold_out_study in studies){
    train_data <- data |> filter(study != hold_out_study)
    test_data <- data |> filter(study == hold_out_study)
    
    model <- model_function(train_data)
    summary(model) |> print()
    fixed_pred <- predict_no_study(model, test_data)
    
    # int_est = "average"
    study_estimates <- broom::tidy(model) |> 
      filter(grepl("^study_*", term), !is.na(estimate)) |> 
      mutate(weight = 1/std.error^2,
             contrib = estimate*weight)
    intercept <- sum(study_estimates$contrib)/sum(study_estimates$weight)
    test_data$pred <- fixed_pred + intercept
    
    # int_est = "estimate"
    inttercept <- IPDPredictR:::predict_intercepts(model, test_data = test_data, cluster_var = "study") |>
      pull(pred_intercept)
    test_data$pred <- fixed_pred + intercept
    
    
    # int_est = estimate_cv
    for(i in 1:n_reps){
      test_data_with_fold <- test_data |> add_fold(n_folds = 5)
      for(my_fold in 1:n_folds){
        intercept_est_data <- test_data_with_fold |> filter(fold != my_fold)
        inttercept <- IPDPredictR:::predict_intercepts(model, test_data = intercept_est_data, cluster_var = "study") |> 
          pull(pred_intercept)
        fixed_pred <- predict_no_study(model, test_data |> filter(fold == my_fold))
        test_data[fold == my_fold, "pred"] <- predict_no_study(model, test_data |> filter(fold == my_fold))
      }
    }
    data[study == hold_out_study, "pred"] <- test_data$pred

  }  
  
  data_with_pred_actual <- data |> mutate(actual = data[, outcome])
  # What do I want to do
  # 1. fit model
  # 2. make predictions (no study) - is this in IPDPredicR 
  
  
  if(!is.null(log_file)){
    sink()
  }
  return(data_with_pred_actual)
}

# predictions when study is unobserved

# I need to predict an intercept (using complete data)
# Then use that intercept to predict new data
# How do I handle fixed study in Stata.

# I fit a constrained gsem model, fixing all parameters at there estimated values and re-estimating the intercept.
# Very clever Gordon. Now do that for a mixed model.




get_predictors_fixed_study <- function(data, predictors) {
  studies_vector <- data |> select(starts_with("study_")) |> colnames()
  print(studies_vector)
  predictors_vector <- c(studies_vector, strsplit(predictors, " ")[[1]])
  predictors_string <- paste(predictors_vector, collapse = " + ") |> paste("-1")
  return(predictors_string)
}



add_fold <- function(data, n_folds) {
  n <- nrow(data)
  length_fold <- round(n/n_folds)
  my_sample <- sample.int(n)
  for(fold in 1:(n_folds-1)){
    start <- length_fold*(fold-1)+1
    end <- length_fold*fold
    fold_sample <- my_sample[start:end]
    data[fold_sample, "fold"] <- fold
  }
  start <- length_fold*(n_folds-1)+1
  end <- length(my_sample)
  data[my_sample[start:end], "fold"] <- n_folds
  return(data)
}

predict_no_study <- function(newdata, model) {
  predictors <- dplyr::select(newdata, -starts_with("study_"))
  predictor_names <- colnames(predictors)
  betas <- model$coef[predictor_names]
  pred_fixed <- as.matrix(predictors) %*% betas
  return(pred_fixed)
}

 