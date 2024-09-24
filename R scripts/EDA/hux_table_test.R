library(huxtable)

tables_folder <- here::here(thesis_tables, "Main Results")
table <- read_csv(here::here(tables_folder, "sens_sdq_modelling.csv"))

heder_row <- tibble::tribble(
  ~label, ~outcome_label, ~calib_itl_est, ~calib_itl_tau, ~calib_slope_est, ~calib_slope_tau, ~rmse_est, ~rmse_tau, ~r_squared_transformed_est, ~r_squared_transformed_tau,
  "", "", "Calibration In-the-Large", "Calibration In-the-Large", "Calibration Slope", "Calibration Slope", "RMSE", "RMSE", "$R^2$", "$R^2$",
  "", "", "Est. ($\\Delta$)", "Tau ($\\Delta$)", "Est. ($\\Delta$)", "Tau ($\\Delta$)", "Est. ($\\Delta$)", "Tau ($\\Delta$)", "Est. ($\\Delta$)", "Tau ($\\Delta$)")


table <- heder_row |> 
  bind_rows(table)

nrows <- table |> nrow()
n_outcomes <- unique(table$outcome_label) |> length()-1
n_groups <-  (nrows-2)/n_outcomes
group_tops <- (1:n_groups -1)*n_outcomes + 2 + 1
ncol <- table |> ncol()


ht <- table |> 
  hux(add_colnames = FALSE) |> 
  set_bold(, row = 1:2) |> 
  merge_repeated_rows(col = 1)  |> 
  merge_cells(1, 3:4) |> 
  merge_across(1, 5:6) |> 
  merge_across(1, 7:8) |> 
  merge_across(1, 9:10) |> 
  set_top_border(row = 1, value = 0.8) |>
  set_bottom_border(row = final(1), value = 0.8) |> 
  set_align(value = "centre", col = 3:ncol(table)) |> 
  set_width(value = 1) |> 
  set_wrap(col = 1, value = TRUE) |> 
  set_wrap(col = 2:ncol, value = FALSE) |> 
  set_valign( col = 1, value = "middle") 


for(myRow in group_tops){
  ht <- ht |> set_top_border(row = myRow, value = 0.5)
}


ht |> save_hux_table("sens_sdq_modelling.tex", 
                     "sens_sdq_modelling", 
                     "Sensitivity analysis of SDQ modelling")




