
plots_folder <- here::here(thesis_plots, "Main Results")


# SDQ ----
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
main_results_sdq <- readRDS(here::here(results_folder, "main_results_sdq.rds")) 
  

main_results_sdq |> save_main_results_hux_table(outcome = "sdq")

# Plotting
main_results_sdq |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "sdq_main_results.png"), width = 18, height = 8, units = "cm")


# VABS ----
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
main_results_vabs <- readRDS(here::here(results_folder, "main_results_vabs.rds")) |> 
  filter(metric != "rmse") 

  

main_results_vabs |> save_main_results_hux_table(outcome = "vabs") 
main_results_vabs |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "vabs_main_results.png"), width = 18, height = 8, units = "cm")



# CBCL ----
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
main_results_cbcl <- readRDS(here::here(results_folder, "main_results_cbcl.rds")) |> 
  filter(metric != "rmse") 


main_results_cbcl |> save_main_results_hux_table(outcome = "cbcl") 

main_results_cbcl |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "cbcl_main_results.png"), width = 18, height = 8, units = "cm")

main_results_bne <- bind_rows(main_results_sdq |> mutate(scale = "SDQ"), 
                              main_results_cbcl |> filter(metric != "rmse") |> mutate(metric = case_when(metric == "rmse_stand" ~ "rmse",
                                                                                                                         TRUE ~ metric),
                                                                                      scale = "CBCL")
                              ) |> 
  filter(metric %in% c("rmse", "r_squared_transformed")) |> 
  select(outcome, metric, est, ci.lb, ci.ub, pi.lb, pi.ub, scale) |> 
  mutate(label = get_label(outcome, label_no = 2)) |> 
  mutate( 
    position =  case_when(
      outcome == "sdq_hyp_p" ~ 11,
      outcome == "cbcl_adhd" ~ 10,
      outcome == "sdq_emot_p" ~ 9,
      outcome == "cbcl_anx" ~ 8,
      outcome == "cbcl_aff" ~ 7,
      outcome == "sdq_cond_p" ~ 6,
      outcome == "cbcl_con" ~ 5,
      outcome == "cbcl_odd" ~ 4,
      outcome == "sdq_peer_p" ~ 3,
      outcome == "sdq_pro_p" ~ 2,
      outcome == "cbcl_som" ~ 1),
    domain = case_when(
      outcome == "sdq_hyp_p" ~ "A",
      outcome == "cbcl_adhd" ~ "A",
      outcome == "sdq_emot_p" ~ "B",
      outcome == "cbcl_anx" ~ "B",
      outcome == "cbcl_aff" ~ "B",
      outcome == "sdq_cond_p" ~ "C",
      outcome == "cbcl_con" ~ "C",
      outcome == "cbcl_odd" ~ "C",
      outcome == "sdq_peer_p" ~ "D",
      outcome == "sdq_pro_p" ~ "D",
      outcome == "cbcl_som" ~ "E") |> as.factor()) |> 
  mutate(metric = factor(
    metric, 
    levels = c("r_squared_transformed", "rmse", "rmse_stand", "calib_itl", "calib_slope"), 
    labels = c("R-squared", "Standardised \n RMSE", "Standardised \n RMSE", "Calibration \n in the Large", "Calibration \n Slope")))




main_results_bne |> 
  plot_many_ma_multi_out(my_colour = "turquoise4", diamond_height = 0.2) +
  facet_grid(cols = vars(metric), rows = vars(domain), scales = "free", space = "free_y", as.table = TRUE) 
