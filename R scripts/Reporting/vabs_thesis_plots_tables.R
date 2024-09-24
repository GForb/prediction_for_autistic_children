# Thesis plots

# Main analysis 
plots_folder <- here::here(thesis_plots, "Main Results")
tables_folder <- here::here(thesis_tables, "Main Results")
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")


raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))
raw_results <- readRDS(file = here::here(results_folder, "results_meta_analysis.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set)),
         main_analysis = case_when(
           predictor_set == "pred3_mt" ~ TRUE & intercept_est_method == "estimate_cv" & model == "mt_fi_study_ri",
           TRUE ~ FALSE)
         ) 

#            intercept_est_method == "estimate_cv" & predictor_set == "pred1_mt"  & model == "mt_fi_study_ri" & suffix ==  "_main" ~ TRUE,

main_results <-  results |> filter(main_analysis, metric != "r_squared") |> 
  mutate(label  = get_label(outcome, label_no = 3))

dp <- 2

# Table
main_results_table  <- main_results |> 
  mutate(
    est_str = paste0(round(est, dp)),
    ci_str = paste0(round(ci.lb, dp), "- ", round(ci.ub, dp)),
    pi_str = paste0(round(pi.lb, dp), "- ", round(pi.ub, dp)),
    outcome_label = get_label(outcome, label_no = 3)
  ) |> 
  select(label, metric, est_str, ci_str, pi_str) |> 
  pivot_longer(cols = c("est_str", "ci_str", "pi_str"), names_to = "summary", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value) |> 
  mutate(summary = case_when(
    summary == "est_str" ~ "Estimate",
    summary == "ci_str" ~ "95% CI",
    summary == "pi_str" ~ "95% PI"
  )) |> 
  select(label, summary, calib_itl, calib_slope, rmse, everything())

write_csv(main_results_table, file.path(tables_folder, "vabs_main_analysis.csv"))

# Plot main results
coulours <- c(VABS = "turquoise4", CBCL = "darkorange", SDQ = "darkorchid")

main_results |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  ggtitle("") +
  theme_linedraw(base_size = 12)

ggsave(file = here::here(plots_folder, "vabs_main_results.png"), width = 18, height = 8, units = "cm")



# Forrest plots for each outcome
plot_forrestplot_grid <- function(myOutcome){
  
  titles <- list(calib_slope = "Calibration Slope",
                 calib_itl = "Calibration in the Large",
                 r_squared_transformed = "R-squared",
                 r_squared = "R-squared",
                 rmse = "Root Mean Squared Error")
  png(file = here::here(plots_folder, paste0("forrestplot_", myOutcome, ".png")), width = 24, height = 16, "cm", res = 300)
  font_size = 0.95
  par(mfrow = c(2, 2), mar = c(2, 4, 5, 4), 
      cex = font_size,        # Overall scaling of text
      cex.main = font_size,   # Font size for titles
      cex.lab = font_size,    # Font size for axis labels
      cex.axis = font_size)   # Font size for axis tick labels
  for(myMetric in c("calib_itl", "calib_slope" , "rmse", "r_squared_transformed")){
    results_data <- main_results |> filter(outcome == myOutcome, metric == myMetric) 
    ma <-  results_data  |> pull(meta_analysis) |> first()
    tau <- results_data  |> pull(tau) 
    create_forrest_plot_vabs(myMetric, tau = tau, meta_analysis = ma, studies = "")
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

# Sensitivity analysis: Table

# Sensitivity analysis plot (rmse only)
sens_labels <- tibble(
  analysis_type = c("mt_fi_study_ri_pred3_mt_estimate_cv_", 
                    "mt_fi_study_ri_pred2_mt_estimate_cv_", 
                    "mt_fi_study_ri_pred1_mt_estimate_cv__2pred",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__1pred",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__3pred",
                    "st_fi_study_pred1_estimate_cv_",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__many_fu",
                    "mt_fi_study_ri_pred1_mt_estimate_cv__short_fu",
                    "mt_fi_study_rs_pred1_mt_estimate_cv__no_pa"
  ),
  label = c("Primary analysis",
            "Predictor set 2",
            "Predictor set 1 Only",
            "Predictions made off single assessment",
            "Predictions made off 3 assessments",
            "Single timepoint model",
            "Include only 2+ follow up",
            "Follow up less than 5 years",
            "Exclude IQ < 50"
  ),
  order = 9:1
)

# change sensitivity analysis to run on single timepoint models.
# Check for other analysis needed.

sensitivity_results <- results |> 
  filter(
    intercept_est_method == "estimate_cv" | 
      (intercept_est_method ==  "average" & predictor_set == "pred3_mt"), 
    predictor_set %in% c("pred3_mt","pred2_mt", "pred1_mt", "pred1")) |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix)) |> 
  left_join(sens_labels) |> 
  filter(!is.na(label), metric != "r_squared")

# Making plots
walk(outcomes, make_sensitivty_analysis_plot)

# Making tables

walk(outcomes, make_sens_table)

sens_labels

primary_results <- sensitivity_results |> filter(label == "Primary analysis") |> select(outcome, metric, prim_est = est, prim_tau = tau)

modelling_table <- sensitivity_results |> filter(order %in% c(8,7)) |> make_sens_table3(primary_results = primary_results, table_name = "vabs_modelling")
timepoints_table <- sensitivity_results |> filter(order %in% c(6,5,4)) |> make_sens_table3(primary_results = primary_results,table_name = "vabs_timepoints")
population_table <- sensitivity_results |> filter(order %in% c(3,2,1)) |> make_sens_table3(primary_results = primary_results, table_name = "vabs_population")



sens_labels

