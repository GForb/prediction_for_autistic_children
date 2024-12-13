get_calib_data_by_study <- function(predictions_df) {
  if(!is.null(predictions_df$imp_no)) {
    grouped_df <- predictions_df |> 
      group_by(study, imp_no) 
  } else {
    grouped_df <- predictions_df |> 
      group_by(study)
  }
  
  grouped_df |> 
    summarise(
      coef = get_calib_coef(pred, actual),
      intercept = get_calib_intercept(pred, actual)
    ) |> 
    ungroup() |> 
    group_by(study) |> 
    summarise(
      coef = mean(coef),
      intercept = mean(intercept)
    )
}

get_calib_coef <- function(predicted, actual) {
  result <- IPDPredictR:::evaluate_performance_cont_obs_pred_calib_slope_int(actual = actual, predicted = predicted)
  return(result$coef)
}

get_calib_intercept <- function(predicted, actual) {
  result <- IPDPredictR:::evaluate_performance_cont_obs_pred_calib_slope_int(actual = actual, predicted = predicted)
  return(result$intercept)
}
  
