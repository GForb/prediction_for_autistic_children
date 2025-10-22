
get_cutoff <- function(risk, cutoff, rmse) {
  z_score <- qnorm(risk)
  threshold_pred <- cutoff + z_score * rmse
  return(threshold_pred)
}

exceedance_prob <- Vectorize(function(prediction, rmse, threshold) {
  # Calculate the probability that the true value exceeds the threshold
  prob <- 1 - pnorm(threshold, mean = prediction, sd = rmse)
  return(prob)
})



get_dc_plot <- function(myOutcome, results_folder, prediction_data) {
  cutoff <- var_metadata |> filter(variable_name == myOutcome) |> pull(cutoff)
  raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))
  rmse <- raw_results_long |> 
    filter(metric == "rmse", model == "st_fi_study", intercept_est_method == "estimate_cv", predictor_set == "pred1", outcome == myOutcome) |> 
    pull(est)
  
  print(cutoff)
  print(rmse)
  
  prediction_data_with_bin_out <- prediction_data |> filter(outcome == myOutcome) |> 
    rename(outcome_name = outcome) |> 
    mutate(outcome = case_when(actual > cutoff ~ 1, TRUE ~ 0),
    risk_from_normal = exceedance_prob(pred, rmse, cutoff),
    base_over_cut = case_when(baseline > cutoff ~ 1, TRUE ~ 0)
    ) |> 
    select(prediction_model = risk_from_normal, baseline_only = baseline, everything())
  
  prediction_data_with_bin_out$outcome_name |> unique() |> print()
  mean(prediction_data_with_bin_out$base_over_cut) |> print()
  
  mean(prediction_data_with_bin_out$outcome) |> print()
  nrow(prediction_data_with_bin_out) |> print()
  DescTools::Cstat(x = prediction_data_with_bin_out$risk_from_normal, resp = prediction_data_with_bin_out$outcome) |> print()
  
  dc <- dcurves::dca(
    outcome ~ prediction_model + base_over_cut,
    thresholds = seq(0.1, 0.5, by = 0.1), 
    data = prediction_data_with_bin_out
  ) 
  
  # my_plot <- dc |> dcurves:::plot.dca(type = "net_benefit", smooth = TRUE) 
  # my_plot <-  plot + ggplot2::ggtitle(myOutcome)
  return(dc)
}

# Set up outcome

results_folder <- results_folder_sdq
analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) 


outcomes <- analysis_spec$outcome |> unique()
filenames <- analysis_spec |> 
  filter(predictor_set == "pred1_mt", suffix == "_main") |> 
  mutate(filename = paste0(analysis_name, "_", "estimate_cv", ".rds")) |> 
  pull(filename)

load_data <- function(myOutcome, filename){
  data <- readRDS(here::here(results_folder, filename)) |> as.data.frame()
  data |> mutate(outcome = myOutcome,
                 baseline = data[,paste0("base_", myOutcome)])
}
prediction_data <- map2(outcomes, filenames, load_data) |> bind_rows()

plots <- map(outcomes, function(myOutcome) get_dc_plot(myOutcome, results_folder, prediction_data)) 
names(plots) <- outcomes

my_plots <- plots |> map(plot)
my_plots
ggpubr::ggarrange(
  plotlist = my_plots,
  common.legend = TRUE,
  labels = outcomes |> get_label(label_no = 3),
  vjust = 0.25,
  legend = "top"
) |> ggpubr::annotate_figure(
  top =  "Net benifit from a naieve approach, approach will likely overestimate net-benifit of model or using baseline score",
    bottom = ggpubr::text_grob(
      "To do this properly, predicted probabilites of the outcome need to be estimated during IECV. 
      Sensitivity, specificity and prevalence can then be calculated and combined in a trivariate meta-analysis. 
      This is described on p414 and 492 in Richard Riley's IPD metanalysis book. Pro-social will also be wrong due to reverse scoring",
  color = "red")
  )

# Per 100 people screened, 

# Screening using prediction tool would result in 5 more 'true positives' being identified or 50 less false positives being identified. for hyperactivity difference is 2.