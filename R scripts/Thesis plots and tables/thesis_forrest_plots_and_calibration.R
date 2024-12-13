# Plotting functions

plot_forrestplot_grid <- function(myOutcomeDomain, myOutcome, main_results){
  
  forrest_plot_functions = list(sdq = create_forrest_plot_sdq,
                                cbcl = create_forrest_plot_cbcl,
                                vabs = create_forrest_plot_vabs)
  
  my_forrest_function <- forrest_plot_functions[[myOutcome]]
  
  titles <- list(calib_slope = "Calibration Slope",
                 calib_itl = "Calibration in the Large",
                 r_squared_transformed = "R-squared",
                 r_squared = "R-squared",
                 rmse = "Root Mean Squared Error")
  png(file = here::here(plots_folder, paste0("forestplot_", myOutcomeDomain, ".png")), width = 24, height = 16, "cm", res = 300)
  font_size = 0.95
  par(mfrow = c(2, 2), mar = c(2, 4, 2.5, 4), 
      cex = font_size,        # Overall scaling of text
      cex.main = font_size,   # Font size for titles
      cex.lab = font_size,    # Font size for axis labels
      cex.axis = font_size)   # Font size for axis tick labels
  for(myMetric in c("calib_itl", "calib_slope" , "rmse", "r_squared_transformed")){
    results_data <- main_results |> filter(outcome == myOutcomeDomain, metric == myMetric) 
    ma <-  results_data  |> pull(meta_analysis) |> first()
    tau <- results_data  |> pull(tau) 
    my_forrest_function(myMetric, tau = tau, meta_analysis = ma, studies = "")
    title(main = titles[[myMetric]])
  }
  par(mfrow=c(1,1), mar = c(5,4,4,2))
  dev.off()
  
}

save_calib_plot <- function(myOutcome, results_folder, main_results) {
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

# Main analysis 
coulours <- c(VABS = "turquoise4", CBCL = "turquoise4", SDQ = "turquoise4")

plots_folder <- here::here(thesis_plots, "Main Results")

# SDQ
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
main_results <- readRDS(here::here(results_folder, "main_results_sdq.rds")) 

# Forrest plots for each outcome
outcomes <- main_results$outcome |> unique()
walk(outcomes, function(myOutcome) plot_forrestplot_grid(myOutcomeDomain = myOutcome, myOutcome = "sdq", main_results = main_results))
walk(outcomes, function(myOutcome) save_calib_plot(myOutcome, results_folder, main_results))

# CBCL
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
main_results <- readRDS(here::here(results_folder, "main_results_cbcl.rds")) 

anx <- main_results |> filter(metric == "calib_slope", outcome == "cbcl_anx")

anx_by_study <- anx |> pull(by_study) 

ma_list <- anx |> pull(meta_analysis)

ma_calib_slope <- ma_list[[1]]

anx_by_study[[1]] |> filter(metric == "calib_slope")

# Forrest plots for each outcome
outcomes <- main_results$outcome |> unique()
walk(outcomes, function(myOutcome) plot_forrestplot_grid(myOutcomeDomain = myOutcome, myOutcome = "cbcl", main_results = main_results))
walk(outcomes, function(myOutcome) save_calib_plot(myOutcome, results_folder, main_results))


# VABS
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
main_results <- readRDS(here::here(results_folder, "main_results_vabs.rds")) 

# Forrest plots for each outcome
outcomes <- main_results$outcome |> unique()
walk(outcomes, function(myOutcome) plot_forrestplot_grid(myOutcomeDomain = myOutcome, myOutcome = "vabs", main_results = main_results))
walk(outcomes, function(myOutcome) save_calib_plot(myOutcome, results_folder, main_results))


# SDQ - inc post_base_diangosis
plots_folder <- here::here(thesis_plots, "SDQ Post diag")

results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
results <- readRDS(here::here(results_folder, "results_sdq_all_aut.rds")) 

# Forrest plots for each outcome
outcomes <- results$outcome |> unique()
walk(outcomes, function(myOutcome) plot_forrestplot_grid(myOutcomeDomain = myOutcome, myOutcome = "sdq", main_results = results))
walk(outcomes, function(myOutcome) save_calib_plot(myOutcome, results_folder, results))
