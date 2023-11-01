evaluate_imputations_MI_Val <- function(jomo_imp_object, model_function, evaluate_performance, orig_data) {
  n_imuptations = max(jomo_imp_object$Imputation)
  
  train_data = rep(orig_data$train_data, n_imuptations +1)
  test_data = rep(orig_data$test_data, n_imuptations+ 1)
  
  jomo_imp_object <- jomo_imp_object |> mutate(train_data = train_data, test_data = test_data)
  
  list_of_imputed_datasets <- split(jomo_imp_object, jomo_imp_object$Imputation)[-1]
  # Need way of identifying whether train or test data
  
  
  
  # 1. Get list of fitted models
  train_data_list <- map(list_of_imputed_datasets, function(x) dplyr::filter(x, train_data)) 
  test_data_list <- map(list_of_imputed_datasets, ~filter(.x, test_data)) 
  
  performance_list <- map2(train_data_list, test_data_list, function(x, y) ProfacSims:::model_evaluate_pipeline(model_function, x, y, evaluate_performance))
  
  # 2. 
}