save_main_results_hux_table <- function(main_results,outcome,  dp = 2){
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
      summary == "ci_str" ~ "95\\% CI",
      summary == "pi_str" ~ "95\\% PI"
    )) |> 
    select(label, summary, rmse, r_squared_transformed, calib_itl, calib_slope) |> 
    arrange(label)
  
  
  n_outcomes <- main_results_table$label |> unique() |> length()
  border_rows <- 1:n_outcomes*3+1
  main_results_table <- rbind(
    c("", "", "RMSE", "$R^2$", "Calibration In-The-Large", "Calibration Slope"),
    main_results_table
    )
  
  hux_table <-  main_results_table |> 
    huxtable::hux(add_colnames = FALSE) |> 
    huxtable::set_bottom_border(row = 1, value = 0.5) |>
    huxtable::set_bottom_border(row = border_rows, value = 0.5) |>
    huxtable::merge_repeated_rows(col = 1)  
  
  outcome_upper <- toupper(outcome)
  
  hux_table |> save_hux_table(
    file_name = paste0(outcome,"_main_results.tex"),
    caption = glue::glue("Results for model validation of the primary analysis of the {outcome_upper}."),
    label = paste0(outcome,"_main_results"))
  # 
  
}