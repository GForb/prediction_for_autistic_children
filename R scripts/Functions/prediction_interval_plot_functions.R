
get_pi_data <- function(analysis_spec, data, minmax_values = NULL, results_folder) {
  analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds")) 
  
  outcomes <- analysis_spec$outcome |> unique()
  
  all_results <- map(outcomes, 
                     function(myOutcome) get_predictions(myOutcome, analysis_spec, data)
  ) |> 
    bind_rows()
  
  # Fit model, make predictions, estimate standard error.
  
  # Get RMSE from pooled results
  raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))
  rmse <- raw_results_long |> filter(metric == "rmse", model == "st_fi_study", intercept_est_method == "estimate_cv", predictor_set == "pred1") |> select(rmse = est, outcome)
  
  n <- nrow(analysis_data_wide)
  p = 11 # number of predictors, better to extract from model,
  
  predictions <- all_results |> left_join(rmse) |> 
    mutate(mse = rmse^2,
           se2 = se^2,
           var_pred = mse + se2,
           se_pred = sqrt(var_pred),
           df = n - p - 2,
           se_ifl_percent = (se_pred/rmse-1)*100,
           alpha95 = 0.05,
           alpha90 = 0.1,
           alpha75 = 0.25,
           alpha50 = 0.5) |> 
    tibble() |> 
    pivot_longer(cols = starts_with("alpha"), names_to = NULL, values_to = "alpha") |> 
    mutate(pi_mult = qt((1-alpha/2),df),
           pi_lower = pred - pi_mult*se_pred,
           pi_upper = pred + pi_mult*se_pred)
          
  
  if(!is.null(minmax_values)){
    predictions <- predictions |>
      left_join(minmax_values) |> 
      mutate(
        pi_lower = case_when(pi_lower < min & !is.na(min) ~ min, TRUE ~ pi_lower),
        pi_upper = case_when(pi_upper > max  & !is.na(max)~ max, TRUE ~ pi_upper)
      )
  }
  
  predictions <- predictions |>
    mutate(
      covered_low = case_when(actual > pi_lower ~ 1, TRUE ~ 0),
      covered_high = case_when(actual < pi_upper ~ 1, TRUE ~ 0),
      covered = covered_low * covered_high
    ) |>
    select(-pi_mult) |>
    pivot_wider(
      names_from = alpha,
      values_from = c(
        "pi_lower",
        "pi_upper",
        "covered",
        "covered_low",
        "covered_high",
        "alpha"
      )
    ) 
  
  return(predictions)
  
}

make_pi_plot <- function(predictions, cutoffs = NULL, legend = TRUE) {
  plot_data <- predictions |> 
    mutate(round_base = round(base_spline1),
           outcome = get_label(outcome, label_no = 3)) |> 
    select(outcome, round_base, pred, ID, starts_with("pi_lower"), starts_with("pi_upper"))  |> 
    pivot_longer(cols = -c("ID", "round_base", "outcome"), names_to = "what", values_to = "value") |>
    group_by(round_base, what, outcome) |>
    summarise(mean = mean(value)) |>
    pivot_wider(names_from = what, values_from = mean) 
  
  # Reshape data to add alpha level as a variable and define transparency
  plot_data_long <- plot_data |> 
    pivot_longer(
      cols = starts_with("pi_"),
      names_to = c(".value", "alpha"),
      names_pattern = "pi_(lower|upper)_(.*)"
    ) 
  # Convert alpha to a factor
  plot_data_long$alpha <- factor(plot_data_long$alpha, levels = c("0.05", "0.1", "0.25", "0.5"))
  
  # Plot with varying transparency for each ribbon
  plot <- ggplot(plot_data_long, aes(x = round_base)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = alpha)) +
    scale_fill_brewer(type = "seq", labels = c("95%", "90%", "75%", "50%")) +
    geom_line(aes(y = pred), color = "black", size = 1) +
    labs(y = "Prediction", x = "Baseline outcome", fill = "Prediction Interval") +
    facet_wrap(vars(outcome), nrow = 2, scales = "free")  
  
  
  if(!is.null(cutoffs)){
    cutoffs <- cutoffs |> mutate(LineName = "Likely disorder cutoff")
    plot <- plot +   
      geom_hline(data = cutoffs, aes(yintercept = cutoff, linetype = LineName), color = "red") +
      geom_vline(data = cutoffs, aes(xintercept = cutoff), linetype = "dashed", color = "red") +
      scale_linetype_manual(values = 2)  +
      labs(linetype = NULL) 
    }
  
  if(legend){
    plot <- plot  + theme(legend.position = "top")
  } else {
    plot <- plot  + theme(legend.position = "none")
  }

  
  
  return(plot)
  
}

get_predictions <- function(myOutcome, analysis_spec, data) {
  model <- analysis_spec |> filter(model_name == "st_fi_study", predictor_set == "pred1") |> filter(outcome == myOutcome) 
  
  data <- analysis_data_wide
  outcome <- model$outcome
  predictors <- model$predictors
  stata_code <- glue::glue("
  mkspline base_spline = base_{outcome}, nknots(3) cubic
  regress out_{outcome} {predictors}
  predict pred
  predict se, stdp
  gen actual = out_{outcome}
                         ")
  results <-  RStata::stata(stata_code, data.in = data, data.out = TRUE) 
  
  predictions_raw <- results |> select(ID, actual, pred, se, base_spline1)|> 
    mutate(outcome = myOutcome)
}

shift_legend3 <- function(p) { # function from stackoverflow
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  plot <- lemon::reposition_legend( p, "center", panel=names(pnls), plot = TRUE )
  return(plot)
}

report_coverage <- function(pi_data) {
  print("Coverage of upper interval")
  pi_data |> select(outcome, starts_with("covered_high")) |> pivot_longer(-"outcome") |> group_by(outcome,name) |> summarise(coverage = mean(value)) 
  
  print("Coverage of upper interval for predicitons < 5")
  pi_data |> filter(pred < 5) |> select(outcome, starts_with("covered_high")) |> pivot_longer(-"outcome") |> group_by(outcome, name) |> summarise(coverage = mean(value), n = n())
  
  print("Coverage of lower interval for predicitons < 5")
    pi_data |> select(outcome, starts_with("covered_low")) |> pivot_longer(-"outcome") |> group_by(outcome,name) |> summarise(coverage = mean(value))
    
  print("Coverage of lower interval for predicitons > 5")
  pi_data |> filter(pred > 5) |> select(outcome, starts_with("covered_low")) |> pivot_longer(-"outcome") |> group_by(outcome,name) |> summarise(coverage = mean(value), n = n())
  
  print("Overall Coverage")
  pi_data |> select(outcome, starts_with("covered_0")) |> pivot_longer(-"outcome") |> group_by(outcome,name) |> summarise(coverage = mean(value), n = n())
  
}
