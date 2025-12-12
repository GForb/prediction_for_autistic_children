
plots_folder <- here::here(thesis_plots, "Main Results")


# Loading Data ----
results_folder <- results_folder_sdq
main_results_sdq <- readRDS(here::here(results_folder, "main_results_sdq.rds")) 

results_folder <- results_folder_vabs
main_results_vabs <- readRDS(here::here(results_folder, "main_results_vabs.rds")) |> 
  filter(metric != "rmse") 

results_folder <- results_folder_cbcl
main_results_cbcl <- readRDS(here::here(results_folder, "main_results_cbcl.rds")) |> 
  filter(metric != "rmse") 



# CBCL vs SDQ ----

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

main_results_bne |> filter(metric == "R-squared") |>   
  mutate(pi.lb = ifelse(pi.lb <0, 0, pi.lb)) |> 
  plot_many_ma_multi_out(my_colour = "turquoise4", diamond_height = 0.2) +
  facet_grid(rows = vars(domain), scales = "free", space = "free_y", as.table = TRUE) +
  theme_bw(base_size = 24) + 
  theme(legend.position = "top") + 
  labs(x = "R-squared", y = "") 

main_results_vabs |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |>   
  mutate(pi.lb = ifelse(pi.lb <0, 0, pi.lb)) |> 
  filter(metric == "r_squared_transformed") |> 
  plot_one_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  theme_bw(base_size = 24) +
  labs(title = "", x = "R-sqaured")

