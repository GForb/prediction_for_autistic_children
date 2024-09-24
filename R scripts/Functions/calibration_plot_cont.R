calibration_plot_cont <- function(predictions_df, study_var_name, by_study_calibration = NULL) {
  if (is.null(by_study_calibration)) {
    calib_results <- IPDPredictR:::get_performance_by_study(
      study_var_name = study_var_name, 
      by_study_predictions_df = predictions_df, 
      evaluate_performance =  IPDPredictR:::evaluate_performance_cont_obs_pred_calib_slope_int) 
    
  } else {
    calib_results <- by_study_calibration 
  }
  
  predictions_df <- predictions_df |> as.data.frame()
  predictions_df[,study_var_name] <- factor(predictions_df[,study_var_name])
  plot <- predictions_df |> 
    ggplot(aes(x = pred, y = actual)) +
    
    # Points
    geom_point(size = 0.25) +
    
    # Calibration line
    geom_abline(data = calib_results, aes(
      intercept = intercept, 
      slope = coef, 
      linetype = "Calibration line"), color = "black", size = 0.7) +
    
    # Smoothed Calibration line
    geom_smooth(aes(linetype = "Smoothed Calibration"), method="auto", se=FALSE, fullrange=FALSE, level=0.95, size = 0.7, colour = "blue") +
    
    # Perfect Calibration line
    geom_abline(aes(linetype = "Perfect calibration", intercept = 0, slope = 1), color = "red", size = 0.7) +
    
    # Faceting by study
    facet_wrap(facets = vars(study), nrow = 2) +
    
    # Custom linetypes
    scale_linetype_manual(
      name = "", 
      values = c("Calibration line" = "solid", "Smoothed Calibration" = "solid", "Perfect calibration" = "dashed")) +
    
    # Labels
    labs(y = "Observed", x = "Predicted") +
    
    # Legend settings

    
    # Theme settings
    theme_bw() +
    theme(legend.position = "top") 
  
  
  return(plot)
}


# guides(linetype = guide_legend(override.aes = list(
#   color = c("black", "blue", "red"),      # Colors match the lines
#   linetype = c("solid", "solid", "dashed"),  # Line types in correct order
#   size = c(0.7, 0.7, 0.7)               # Line sizes match
# ))) +
