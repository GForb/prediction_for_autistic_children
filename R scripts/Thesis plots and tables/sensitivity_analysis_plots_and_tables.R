plots_folder <- here::here(thesis_plots, "Main Results")

dp <- 2
myColour = "turquoise4"


sens_labels_all <- tibble(
  label = c(
    "Primary\nanalysis",
    "Predictor\nset 1 and 2",
    "Predictor\nset 1 Only",
    "Random\nintercept\nfor study",
    "Random \nintercept\nfor ID only",
    "Single\ntimepoint \nmodel",
    "Predictions\nwith 1\nassessment",
    "Predictions\nwith 3\nassessments",
    "Include post baseline\ndiagnosis",
    "Only people with\n2+ observations",
    "Follow up\nless than\n5 years",
    "Follow up\nless than\n4 years",
    "Exclude\nIQ < 50"
  ),
  analysis_type_cbcl = c(
    "mt_fi_study_rs_pred3_mt_estimate_cv_",
    "mt_fi_study_rs_pred2_mt_estimate_cv_",
    "mt_fi_study_rs_pred1_mt_estimate_cv_",
    "mt_ri_study_rs_pred1_mt_estimate_cv_",
    "mt_fi_study_ri_pred1_mt_estimate_cv_",
    "st_fi_study_pred1_estimate_cv_",
    "mt_fi_study_rs_pred1_mt_estimate_cv__1pred",
    "mt_fi_study_rs_pred1_mt_estimate_cv__3pred",
    "all aut not run",
    "many fu not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__short_fu",
    "exc 4 yr fu not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__long_no_pa"
  ),
  analysis_type_cbcl_som = c(
    "mt_fi_study_ri_pred3_mt_estimate_cv_",
    "mt_fi_study_ri_pred2_mt_estimate_cv_",
    "mt_fi_study_ri_pred1_mt_estimate_cv_",
    "mt_ri_study_ri_pred1_mt_estimate_cv_",
    "mt_fi_study_rs_pred1_mt_estimate_cv_",
    "st_fi_study_pred1_estimate_cv_",
    "mt_fi_study_rs_pred1_mt_estimate_cv__1pred",
    "mt_fi_study_rs_pred1_mt_estimate_cv__3pred",
    "all aut not run",
    "many fu not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__short_fu",
    "exc 4 yr fu not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__long_no_pa"
  ),
  analysis_type_sdq = c(
    "mt_fi_study_rs_pred3_mt_estimate_cv_",
    "pred 1 and 2 not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__main",
    "mt_ri_study_rs_pred1_mt_estimate_cv_",
    "mt_fi_study_ri_pred1_mt_estimate_cv_",
    "st_fi_study_pred1_estimate_cv_",
    "mt_fi_study_rs_pred1_mt_estimate_cv__1pred",
    "mt_fi_study_rs_pred1_mt_estimate_cv__3pred",
    "mt_fi_study_rs_pred1_mt_estimate_cv__all_aut",
    "mt_fi_study_rs_pred1_mt_estimate_cv__many_fu",
    "exc 5 yr fu not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__short_fu",
    "pa not run"
  ),
  analysis_type_vabs = c(
    "mt_fi_study_ri_pred3_mt_estimate_cv_",
    "mt_fi_study_ri_pred2_mt_estimate_cv_",
    "mt_fi_study_ri_pred1_mt_estimate_cv__2pred",
    "random intercept study not run",
    "no random slope not run",
    "st_fi_study_pred1_estimate_cv_",
    "mt_fi_study_ri_pred1_mt_estimate_cv__1pred",
    "mt_fi_study_ri_pred1_mt_estimate_cv__3pred",
    "all aut not run",
    "mt_fi_study_rs_pred1_mt_estimate_cv__many_fu",
    "mt_fi_study_ri_pred1_mt_estimate_cv__short_fu",
    "exc 4 yr fu not run",
    "mt_fi_study_ri_pred1_mt_estimate_cv__no_pa"
  ),
  order = 13:1
)





# SDQ ----
# Sensitivity analysis: Table
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")


raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))
results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))
  ) |> 
  filter(
    intercept_est_method == "estimate_cv" | 
      (intercept_est_method ==  "average" & predictor_set == "pred3_mt"), 
    predictor_set %in% c("pred3_mt", "pred1_mt", "pred1")) |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix))

results |> select(analysis_type) |> unique()

outcomes <- raw_results_long$outcome |> unique()

sens_labels <- sens_labels_all |> select(label, analysis_type = analysis_type_sdq, order)



sensitivity_results <- results |> 
  left_join(sens_labels) |> 
  filter(!is.na(label), metric != "r_squared") |> 
  arrange(order) |> 
  mutate(order = match(order, unique(order))) # removing gaps from order var

# Sensitivity table
tables <- map(outcomes, function(myOutcome) make_sens_table(myOutcome, sensitivity_results))
names(tables) <- outcomes
iwalk(tables, save_sens_hux_table)

walk(outcomes, make_sensitivty_analysis_plot)


save_sens_delta_tables(sensitivity_results,
                       myOutcome  = "sdq",
                       models = c(9, 8, 7),
                       time = c(6, 5, 4),
                       pop = c(3, 2, 1))
metrics <- sensitivity_results |> pull(metric) |> unique()

vline_data <-  sensitivity_results |> filter(label == "Primary\nanalysis") |> 
  select(metric, outcome, vline_x = est, vline_lb = pi.lb, vline_ub = pi.ub) |> 
  mutate(outcome = factor(get_label(outcome, label_no = 3)))


walk(metrics, function(myMetric) {
  plot <- sensitivity_results |> 
    filter(metric == myMetric) |> 
    mutate(position = order) |> 
    plot_many_ma_by_outcome(vline_data = vline_data |> filter(metric == myMetric), vline_pi = TRUE, my_colour = myColour) 
  plot + scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) 
  ggsave(here::here(plots_folder, paste0("sens_plot_sdq_", myMetric, ".png")), width = 16, height = 21, units = "cm")
})

# VABS ----
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds"))

results <- raw_results_long |> 
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))
  ) |> 
  filter(
    intercept_est_method == "estimate_cv" | 
    (intercept_est_method ==  "average" & predictor_set == "pred3_mt"), 
  predictor_set %in% c("pred3_mt","pred2_mt", "pred1_mt", "pred1")) |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix))

results |> select(analysis_type) |> unique()

outcomes <- raw_results_long$outcome |> unique()


sens_labels <- sens_labels_all |> select(label, analysis_type = analysis_type_vabs, order)


sensitivity_results <- results |> 
  left_join(sens_labels) |> 
  filter(!is.na(label), metric != "r_squared") |> 
  arrange(order) |> 
  mutate(order = match(order, unique(order))) # removing gaps from order var



# Sensitivity table
tables <- map(outcomes, function(myOutcome) make_sens_table(myOutcome, sensitivity_results))
names(tables) <- outcomes
iwalk(tables, save_sens_hux_table)

save_sens_delta_tables(sensitivity_results,
                       myOutcome =  "vabs",
                       models = c(8, 7),
                       time = c(6, 5, 4),
                       pop = c(3, 2, 1))


metrics <- sensitivity_results |> pull(metric) |> unique()

vline_data <-  sensitivity_results |> filter(label == "Primary\nanalysis") |> 
  select(metric, outcome, vline_x = est, vline_lb = pi.lb, vline_ub = pi.ub) |> 
  mutate(outcome = factor(get_label(outcome, label_no = 3)))


walk(metrics, function(myMetric) {
  plot <- sensitivity_results |> 
    filter(metric == myMetric) |> 
    mutate(position = order) |> 
    plot_many_ma_by_outcome(vline_data = vline_data |> filter(metric == myMetric), vline_pi = TRUE, my_colour = myColour) 
  plot + scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) 
  ggsave(here::here(plots_folder, paste0("sens_plot_vabs_", myMetric, ".png")), width = 16, height = 21, units = "cm")
})



# CBCL ----
results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
raw_results_long <- readRDS(file = here::here(results_folder, "results_meta_analysis_long.rds")) |> 
  select(outcome, metric, est, ci.lb, ci.ub, pi.lb, pi.ub, model, predictor_set, intercept_est_method, analysis_name)

cbcl_n_items<- tibble(
  outcome = c(
    "cbcl_aff",
    "cbcl_anx",
    "cbcl_som",
    "cbcl_adhd",
    "cbcl_odd",
    "cbcl_con"
  ),
  n_items = c(13, 6, 7, 7, 5, 17)
)

standard_rmse <- raw_results_long |> 
  filter(metric == "rmse") |> 
  pivot_longer(cols = c(est, ci.lb, ci.ub, pi.lb, pi.ub), names_to = "est_type", values_to = "est_value") |> 
  left_join(cbcl_n_items) |> 
  mutate(est_value = est_value / (n_items*2)) |> 
  pivot_wider(names_from = est_type, values_from = est_value) |> 
  mutate(metric = "rmse_stand") |> 
  select(-n_items)



raw_results_mod<- bind_rows(raw_results_long, standard_rmse) |> 
  filter(metric != "rmse")



results <- raw_results_mod |>
  mutate(suffix = str_remove_all(analysis_name, paste0(model, "_", outcome, "_", predictor_set))
  ) |>
  filter(
    intercept_est_method == "estimate_cv") |> 
  mutate(analysis_type = paste0(model, "_", predictor_set, "_", intercept_est_method, "_", suffix))

results |> select(analysis_type) |> unique()

outcomes <- raw_results_long$outcome |> unique()

sens_labels <- sens_labels_all |> select(label, analysis_type = analysis_type_cbcl, order)

sens_labels_somatic  <- sens_labels_all  |> select(label, analysis_type = analysis_type_cbcl_som, order) |> 
  mutate(outcome = "cbcl_som")

outcomes_no_som <- outcomes[outcomes != "cbcl_som"]
sens_labels_w_out <- map(outcomes_no_som, function(myOutcome) sens_labels |> mutate(outcome = myOutcome)) |> bind_rows()

sens_labels <- bind_rows(sens_labels_w_out, sens_labels_somatic)

sensitivity_results <- results |> 
  left_join(sens_labels) |> 
  filter(!is.na(label), metric != "r_squared")  |> 
  arrange(order) |> 
  mutate(order = match(order, unique(order))) # removing gaps from order var

  
  
sensitivity_results |> filter(metric == "rmse_stand") |>  count(order)

# Sensitivity table
tables <- map(outcomes, function(myOutcome) make_sens_table(myOutcome, sensitivity_results))
names(tables) <- outcomes
iwalk(tables, save_sens_hux_table)

walk(outcomes, make_sensitivty_analysis_plot)

vline_data <-  sensitivity_results |> filter(label == "Primary\nanalysis") |> 
  select(metric, outcome, vline_x = est, vline_lb = pi.lb, vline_ub = pi.ub) |> 
  mutate(outcome = factor(get_label(outcome, label_no = 3)))

metrics <- sensitivity_results |> pull(metric) |> unique()
walk(metrics, function(myMetric) {
  plot <- sensitivity_results |> 
    filter(metric == myMetric) |> 
    mutate(position = order) |> 
    plot_many_ma_by_outcome(vline_data = vline_data |> filter(metric == myMetric), vline_pi = TRUE, my_colour = myColour) 
  plot + scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) 
  ggsave(here::here(plots_folder, paste0("sens_plot_cbcl_", myMetric, ".png")), width = 16, height = 21, units = "cm")
})



