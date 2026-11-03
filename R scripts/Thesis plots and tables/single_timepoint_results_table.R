# Results for the single timepoint model sensitivity analysis (st_fi_study, predictor set 1), reported in the same way as the main results.
# Combines cr_main_results.R (selecting results) and main_results_table.R (tables and plots).

plots_folder <- here::here(thesis_plots, "Main Results")

get_single_timepoint_results <- function(results_folder) {
  readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds")) |>
    mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))) |>
    filter(model == "st_fi_study",
           predictor_set == "pred1",
           suffix == "",
           intercept_est_method == "estimate_cv",
           metric != "r_squared") |>
    mutate(label  = get_label(outcome, label_no = 3))
}

# SDQ ----
results_folder <- results_folder_sdq
single_timepoint_results_sdq <- get_single_timepoint_results(results_folder)
saveRDS(single_timepoint_results_sdq, file = here::here(results_folder, "single_timepoint_results_sdq.rds"))

# VABS ----
results_folder <- results_folder_vabs
single_timepoint_results_vabs <- get_single_timepoint_results(results_folder)

standard_rmse <- single_timepoint_results_vabs |>
  filter(metric == "rmse") |>
  select(outcome, est, ci.lb, ci.ub, pi.lb, pi.ub) |>
  pivot_longer(cols = c(est, ci.lb, ci.ub, pi.lb, pi.ub), names_to = "est_type", values_to = "est_value") |>
  mutate(est_value = est_value / 2) |>
  pivot_wider(names_from = est_type, values_from = est_value) |>
  mutate(metric = "rmse_stand") |>
  mutate(label  = get_label(outcome, label_no = 3))

single_timepoint_results_vabs <- bind_rows(single_timepoint_results_vabs, standard_rmse)
saveRDS(single_timepoint_results_vabs, file = here::here(results_folder, "single_timepoint_results_vabs.rds"))

# CBCL ----
results_folder <- results_folder_cbcl
single_timepoint_results_cbcl <- get_single_timepoint_results(results_folder)

cbcl_n_items <- cbcl_cutoffs |>
  select(outcome, n_items)

standard_rmse <- single_timepoint_results_cbcl |>
  filter(metric == "rmse") |>
  select(outcome, est, ci.lb, ci.ub, pi.lb, pi.ub) |>
  pivot_longer(cols = c(est, ci.lb, ci.ub, pi.lb, pi.ub), names_to = "est_type", values_to = "est_value") |>
  left_join(cbcl_n_items) |>
  mutate(est_value = est_value / (n_items*2)*10) |>
  pivot_wider(names_from = est_type, values_from = est_value) |>
  mutate(metric = "rmse_stand") |>
  select(-n_items) |>
  mutate(label  = get_label(outcome, label_no = 3))

single_timepoint_results_cbcl <- bind_rows(single_timepoint_results_cbcl, standard_rmse)
saveRDS(single_timepoint_results_cbcl, file = here::here(results_folder, "single_timepoint_results_cbcl.rds"))

# Tables ----
single_timepoint_description <- "the single timepoint model sensitivity analysis"

single_timepoint_results_sdq |>
  save_main_results_hux_table(outcome = "sdq", file_suffix = "single_timepoint_results", analysis_description = single_timepoint_description)
single_timepoint_results_vabs |>
  filter(metric != "rmse") |>
  save_main_results_hux_table(outcome = "vabs", file_suffix = "single_timepoint_results", analysis_description = single_timepoint_description)
single_timepoint_results_cbcl |>
  filter(metric != "rmse") |>
  save_main_results_hux_table(outcome = "cbcl", file_suffix = "single_timepoint_results", analysis_description = single_timepoint_description)

# Plotting ----
single_timepoint_results_sdq |>
  mutate(
    position =  label |>  factor() |>  as.numeric()
  ) |>
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "sdq_single_timepoint_results.png"), width = 18, height = 7, units = "cm")

single_timepoint_results_vabs |>
  filter(metric != "rmse") |>
  mutate(
    position =  label |>  factor() |>  as.numeric()
  ) |>
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "vabs_single_timepoint_results.png"), width = 18, height = 7, units = "cm")

single_timepoint_results_cbcl |>
  filter(metric != "rmse") |>
  mutate(
    position =  label |>  factor() |>  as.numeric()
  ) |>
  plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = TRUE) +
  ggtitle("")

ggsave(file = here::here(plots_folder, "cbcl_single_timepoint_results.png"), width = 18, height = 7, units = "cm")

# Plotting main and single timepoint results together ----
# Each outcome domain has two rows, main results above single timepoint results, with a gap between domains.
main_results_sdq <- readRDS(here::here(results_folder_sdq, "main_results_sdq.rds"))
main_results_vabs <- readRDS(here::here(results_folder_vabs, "main_results_vabs.rds"))
main_results_cbcl <- readRDS(here::here(results_folder_cbcl, "main_results_cbcl.rds"))

combine_main_single_timepoint <- function(main_results, single_timepoint_results) {
  bind_rows(
    main_results |> mutate(analysis = "main"),
    single_timepoint_results |> mutate(analysis = "single timepoint")
  ) |> 
    mutate(
      domain_position = label |> factor() |> as.numeric(),
      position = 3 * domain_position - if_else(analysis == "main", 1, 2),
      label = paste0(label, " (", analysis, ")")
    )
}

plot_main_single_timepoint <- function(main_results, single_timepoint_results, outcome_name, rmse_stand = FALSE) {
  plot_data <- combine_main_single_timepoint(main_results, single_timepoint_results)
  if (rmse_stand) {
    plot_data <- plot_data |> filter(metric != "rmse")
  }
  n_domains <- plot_data$domain_position |> max()
  
  plot_data |> 
    plot_many_ma_by_metric(my_colour = "turquoise4", diamond_height = 0.2, rmse_stand = rmse_stand, show_vline = FALSE) +
    ggtitle("")
  
  ggsave(file = here::here(plots_folder, paste0(outcome_name, "_main_vs_single_timepoint_results.png")), width = 18, height = 2 + 2.5 * n_domains, units = "cm")
}

plot_main_single_timepoint(main_results_sdq, single_timepoint_results_sdq, "sdq")
plot_main_single_timepoint(main_results_vabs, single_timepoint_results_vabs, "vabs", rmse_stand = TRUE)
plot_main_single_timepoint(main_results_cbcl, single_timepoint_results_cbcl, "cbcl", rmse_stand = TRUE)
