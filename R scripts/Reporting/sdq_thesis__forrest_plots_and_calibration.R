# Thesis plots

# Main analysis 
plots_folder <- here::here(thesis_plots, "Main Results")
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")

# Plot main results
coulours <- c(VABS = "turquoise4", CBCL = "turquoise4", SDQ = "turquoise4")

main_results |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  ggtitle("") +
  theme_linedraw(base_size = 12)

ggsave(file = here::here(plots_folder, "sdq_main_results.png"), width = 18, height = 8, units = "cm")

myOutcome <- "sdq_hyp_p"
myMetric <- "rmse"

# Forrest plots for each outcome
plot_forrestplot_grid <- function(myOutcome){
  
  titles <- list(calib_slope = "Calibration Slope",
                 calib_itl = "Calibration in the Large",
                 r_squared_transformed = "R-squared",
                 r_squared = "R-squared",
                 rmse = "Root Mean Squared Error")
  png(file = here::here(plots_folder, paste0("forrestplot_", myOutcome, ".png")), width = 24, height = 16, "cm", res = 300)
  font_size = 0.95
  par(mfrow = c(2, 2), mar = c(2, 4, 2.5, 4), 
      cex = font_size,        # Overall scaling of text
      cex.main = font_size,   # Font size for titles
      cex.lab = font_size,    # Font size for axis labels
      cex.axis = font_size)   # Font size for axis tick labels
  for(myMetric in c("calib_itl", "calib_slope" , "rmse", "r_squared_transformed")){
    results_data <- main_results |> filter(outcome == myOutcome, metric == myMetric) 
    ma <-  results_data  |> pull(meta_analysis) |> first()
    tau <- results_data  |> pull(tau) 
    create_forrest_plot_sdq(myMetric, tau = tau, meta_analysis = ma, studies = "")
    title(main = titles[[myMetric]])
  }
  par(mfrow=c(1,1), mar = c(5,4,4,2))
  dev.off()
  
}

outcomes <- main_results$outcome |> unique()
walk(outcomes, plot_forrestplot_grid)



# Calibration plot for each outcome
save_calib_plot <- function(myOutcome) {
  results_data <- main_results |> filter(outcome == myOutcome, metric == "calib_slope") 
  predictions_df <- readRDS(here::here(results_folder, paste0(results_data$analysis_name,"_",results_data$intercept_est_method,  ".rds"))) 
  predictions_df_sampled <-     predictions_df |> 
    group_by(ID) %>%           # Group by ID
    slice_sample(n = 1) %>%    # Randomly select one row per group (ID)
    ungroup()     
  
  calib_data <- predictions_df |> get_calib_data_by_study()
  
  calibration_plot_cont(predictions_df_sampled, "study", by_study_calibration = calib_data) + 
    theme_bw(base_size = 11) +
    theme(legend.position = "top") 
  
  ggsave(file = here::here(plots_folder, paste0("calib_plot_", myOutcome, ".png")), width = 14.5, height = 10, units = "cm")
}
walk(outcomes, save_calib_plot)


