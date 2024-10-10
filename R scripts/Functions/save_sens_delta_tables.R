save_sens_delta_tables <- function(sens_results, models, time, pop, myOutcome){
  primary_results <- sens_results |> filter(label == "Primary analysis") |> select(outcome, metric, prim_est = est, prim_tau = tau)
  
  modelling_table <- sens_results |> 
    filter(order %in% models) |> 
    make_sens_delta_table(primary_results = primary_results) |> 
    save_sens_delta_hux_table(outcome = myOutcome, what = "models")
  
  timepoints_table <- sens_results |> 
    filter(order %in% time) |> make_sens_delta_table(primary_results = primary_results) |> 
    save_sens_delta_hux_table(outcome = myOutcome, what = "time")
 
   population_table <- sens_results |> 
    filter(order %in% pop) |> make_sens_delta_table(primary_results = primary_results) |> 
    save_sens_delta_hux_table(outcome = myOutcome, what = "pop")
  
  return(list(modelling_table, timepoints_table, population_table))
}


make_sens_delta_table <- function(table_data, primary_results, table_name = "") {
  round1dp_metrics_est <- c("calib_itl", "rmse")
  round1dp_metrics_tau <- c("")
  
  sens_table <- table_data |> 
    left_join(primary_results) |> 
    mutate(
      delta_est = case_when(
        metric %in% round1dp_metrics_est ~ (est - prim_est) |> round(1),
        TRUE ~ (est - prim_est) |> round(2)),
      delta_tau = case_when(
        metric %in% round1dp_metrics_tau ~ (tau - prim_tau) |> round(1),
        TRUE ~ (tau - prim_tau) |> round(2)),
      est = case_when(
        metric %in% round1dp_metrics_est ~ (est) |> round(1),
        TRUE ~ (est) |> round(2)),
      tau = case_when(
        metric %in% round1dp_metrics_tau ~ (tau) |> round(1),
        TRUE ~ (tau) |> round(2)),
    ) |> 
    mutate(sum_str_est = paste0(est, " (", delta_est, ")"),
           sum_str_tau = paste0(tau, " (", delta_tau, ")")) |>
    select(label, outcome, order, metric, est = sum_str_est, tau = sum_str_tau) |> 
    pivot_wider(names_from = metric, values_from = c("est", "tau"), names_glue = "{metric}_{.value}") |> 
    mutate(outcome_label = get_label(outcome, label_no = 3)) |>
    select(label, outcome_label, order, 
           starts_with("rmse"),
           starts_with("r_squared_transformed"),
           starts_with("calib_itl"),
           starts_with("calib_slope")) |>
    arrange(desc(order), outcome_label) |> 
    select(-order)
  
  sens_table <-  rbind(
    c("","","RMSE", "RMSE", "$R^2$", "$R^2$", "Calibration In The Large", "Calibration In The Large", "Calibration Slope", "Calibration Slope"),
    c("","",rep(c("Est", "$\\Delta$"), 4)),
    sens_table
  )
  
  #write_csv(sens_table, file.path(tables_folder, paste0("sens_", table_name, ".csv")))
  return(sens_table)
}


save_sens_delta_hux_table <- function(sens_delta_table, outcome, what = ""){
  print(what)
  print("doing it")
  n_rows <- nrow(sens_delta_table)
  n_outcomes <- sens_delta_table |> filter(outcome_label != "") |> pull(outcome_label) |> unique() |> length()
  n_analysis <- sens_delta_table |> filter(label != "") |> pull(label) |> unique() |> length()
  border_rows <- 1:n_analysis*n_outcomes+2
  
  hux_table <-  sens_delta_table |> 
    huxtable::hux(add_colnames = FALSE) |> 
    huxtable::set_bottom_border(row = 2, value = 0.5) |>
    huxtable::set_bottom_border(row = border_rows, value = 0.5) |>
    huxtable::merge_repeated_rows(col = 1)  |> 
    huxtable::merge_across(row = 1, col = 3:4) |> 
    huxtable::merge_across(row = 1, col = 5:6) |>
    huxtable::merge_across(row = 1, col = 7:8) |>
    huxtable::merge_across(row = 1, col = 9:10) 
  
  outcome_label <- toupper(outcome)
  print(what)
  what_label <- case_when(
    what == "models" ~ "invovling different modelling approaches",
    what == "time" ~ "using different numbers of timepoints to make predicitons",
    what == "pop" ~ "including different analaysis populations",
    TRUE ~ "FIX WHAT"
  )
  
  caption <- glue::glue("Differences in model performance between the primary analysis for the {outcome_label} and sensitiviity analysis {what_label}.")
  ht <- hux_table |> save_hux_table(
    file_name = paste0(outcome,"_sens_delta_", what, ".tex"),
    caption = caption,
    label = paste0(outcome,"_sens_delta_", what))
  
  print(ht)
  return(ht)
}


