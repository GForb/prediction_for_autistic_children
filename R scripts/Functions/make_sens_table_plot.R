make_sens_table <- function(myOutcome) {
  sens_table <- sensitivity_results |> 
    filter(outcome == myOutcome) |> 
    mutate(
      est_str = paste0(round(est, dp)),
      ci_str = paste0(round(ci.lb, dp), "- ", round(ci.ub, dp)),
      pi_str = paste0(round(pi.lb, dp), "- ", round(pi.ub, dp)),
    ) |> 
    select(label, metric, est_str, ci_str, pi_str, order) |> 
    pivot_longer(cols = c("est_str", "ci_str", "pi_str"), names_to = "summary", values_to = "value") |> 
    pivot_wider(names_from = metric, values_from = value) |> 
    mutate(summary = case_when(
      summary == "est_str" ~ "Estimate",
      summary == "ci_str" ~ "95% CI",
      summary == "pi_str" ~ "95% PI"
    )) |> 
    arrange(-order) |> 
    select(label, summary, calib_itl, calib_slope, rmse,  everything(), -order) 
  
  write_csv(sens_table, file.path(tables_folder, paste0("sens_", myOutcome, ".csv")))
  return(table)
}


make_sens_table2 <- function(table_data, primary_results, table_name) {
  sens_table <- table_data |> 
    left_join(primary_results) |> 
    mutate(
      delta_est = (est - prim_est) |> round(2),
       per_delta_est = case_when(is.finite(delta_est/prim_est) ~ (delta_est/prim_est * 100) |> round(),
                                 TRUE ~ 0),
       delta_tau = (tau - prim_tau) |> round(2),
       per_delta_tau = case_when(is.finite(delta_tau/prim_tau) ~ (delta_tau/prim_tau * 100) |> round(),
                                 TRUE ~ 0)
    ) |> 
    select(label, outcome, order, metric, delta_est, per_delta_est, delta_tau, per_delta_tau) |> 
    mutate(sum_str_est = paste0(delta_est, " (", per_delta_est, ")"),
           sum_str_tau = paste0(delta_tau, " (", per_delta_tau, ")")) |>
    select(label, outcome, order, metric, est = sum_str_est, tau = sum_str_tau) |> 
    pivot_wider(names_from = metric, values_from = c("est", "tau"), names_glue = "{metric}_{.value}") |> 
    mutate(outcome_label = get_label(outcome, label_no = 3)) |>
    select(label, outcome_label, order, 
           starts_with("calib_itl"),
           starts_with("calib_slope"),
           starts_with("rmse"),
           starts_with("r_squared_transformed")) |>
    arrange(desc(order), outcome_label) |> 
    select(-order)
  
  write_csv(sens_table, file.path(tables_folder, paste0("sens_", table_name, ".csv")))
  return(sens_table)
}

make_sens_table3 <- function(table_data, primary_results, table_name) {
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
           starts_with("calib_itl"),
           starts_with("calib_slope"),
           starts_with("rmse"),
           starts_with("r_squared_transformed")) |>
    arrange(desc(order), outcome_label) |> 
    select(-order)
  
  write_csv(sens_table, file.path(tables_folder, paste0("sens_", table_name, ".csv")))
  return(sens_table)
}


make_sensitivty_analysis_plot <- function(myOutcome) {
  plot_data <- sensitivity_results |> 
    filter(outcome == myOutcome) |> 
    mutate(
      position = order
    )
  
  vline_data <- plot_data |> 
    filter(label == "Primary analysis") |> 
    select(metric, vline_x = est, vline_lb = pi.lb, vline_ub = pi.ub) |> 
    mutate(metric = factor(
      metric, 
      levels = c("calib_itl", "calib_slope", "rmse", "r_squared_transformed"), 
      labels = c("Calibration \n in the Large", "Calibration \n Slope",  "RMSE", "R-squared")))
  
  plot_data |> plot_many_ma_by_metric(outcome = myOutcome, my_colour = "darkorchid", vline_data = vline_data, vline_pi = TRUE)
  
  ggsave(file = here::here(plots_folder, glue::glue("sens_plot_{myOutcome}.png")), width = 18, height = 20, units = "cm")
  
}
