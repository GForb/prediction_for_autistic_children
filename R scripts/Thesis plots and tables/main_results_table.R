
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

# Tables ----
main_results_sdq |> save_main_results_hux_table(outcome = "sdq")
main_results_vabs |> save_main_results_hux_table(outcome = "vabs") 
main_results_cbcl |> save_main_results_hux_table(outcome = "cbcl") 

# Plotting ----
main_results_sdq |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "sdq_main_results.png"), width = 18, height = 7, units = "cm")

main_results_vabs |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "vabs_main_results.png"), width = 18, height = 7, units = "cm")

main_results_cbcl |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "cbcl_main_results.png"), width = 18, height = 7, units = "cm")

