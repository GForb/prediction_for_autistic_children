library(patchwork)

plots_folder <- here::here(here(outputs, "Paper"))


# Loading Data ----
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
main_results_sdq <- readRDS(here::here(results_folder, "main_results_sdq.rds")) 

results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
main_results_vabs <- readRDS(here::here(results_folder, "main_results_vabs.rds")) |> 
  filter(metric != "rmse") 

results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
main_results_cbcl <- readRDS(here::here(results_folder, "main_results_cbcl.rds")) |> 
  filter(metric != "rmse") 

# Plotting ----
sdq_plot <- main_results_sdq |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  ggtitle("SDQ")


vabs_plot <-  main_results_vabs |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("VABS")


cbcl_plot <- main_results_cbcl |> 
  mutate( 
    position =  label |>  factor() |>  as.numeric()
  ) |> 
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("CBCL")


(cbcl_plot/sdq_plot/vabs_plot) +
  plot_annotation(tag_levels = 'A')

ggsave(file = here::here(plots_folder, "main_results.png"), width = 18, height = 21, units = "cm")
