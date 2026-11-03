# Compares the main results from the revised run (controller v4) with the thesis/publication run.
# Requires config_v4.R to be sourced, and cr_main_results.R to have been run for the revised results.
# Saves a comparison table to the Revised outputs Results folder.

thesis_results_folders <- list(
  sdq = here::here(results_folder_main, "SDQ", "Thesis"),
  cbcl = here::here(results_folder_main, "CBCL", "Thesis"),
  vabs = here::here(results_folder_main, "VABS", "Thesis")
)
revised_results_folders <- list(
  sdq = results_folder_sdq,
  cbcl = results_folder_cbcl,
  vabs = results_folder_vabs
)

compare_cols <- c("est", "ci.lb", "ci.ub", "pi.lb", "pi.ub", "tau")

read_main_results <- function(outcome_measure, folders) {
  readRDS(here::here(folders[[outcome_measure]], paste0("main_results_", outcome_measure, ".rds"))) |>
    mutate(measure = outcome_measure) |>
    select(measure, outcome, label, metric, model, all_of(compare_cols))
}

measures <- c("sdq", "cbcl", "vabs")
thesis_results <- map(measures, read_main_results, folders = thesis_results_folders) |> bind_rows()
revised_results <- map(measures, read_main_results, folders = revised_results_folders) |> bind_rows()

# Checking both runs report the same set of models, outcomes and metrics
join_keys <- c("measure", "outcome", "label", "metric", "model")
missing_revised <- anti_join(thesis_results, revised_results, by = join_keys)
missing_thesis <- anti_join(revised_results, thesis_results, by = join_keys)
if (nrow(missing_revised) > 0 | nrow(missing_thesis) > 0) {
  warning("Thesis and revised main results do not contain the same rows")
  print(missing_revised)
  print(missing_thesis)
}

comparison <- inner_join(thesis_results, revised_results, by = join_keys, suffix = c("_thesis", "_revised")) |>
  mutate(
    est_diff = est_revised - est_thesis,
    est_pct_change = 100 * est_diff / abs(est_thesis),
    ci_width_thesis = ci.ub_thesis - ci.lb_thesis,
    ci_width_revised = ci.ub_revised - ci.lb_revised,
    # Change in estimate relative to the width of the thesis confidence interval
    est_diff_rel_ci = est_diff / ci_width_thesis,
    revised_in_thesis_ci = est_revised >= ci.lb_thesis & est_revised <= ci.ub_thesis,
    tau_diff = tau_revised - tau_thesis,
    pi_width_diff = (pi.ub_revised - pi.lb_revised) - (pi.ub_thesis - pi.lb_thesis)
  ) |>
  arrange(measure, metric, outcome)

# Results table ----
comparison_table <- comparison |>
  mutate(
    thesis = sprintf("%.3f (%.3f, %.3f)", est_thesis, ci.lb_thesis, ci.ub_thesis),
    revised = sprintf("%.3f (%.3f, %.3f)", est_revised, ci.lb_revised, ci.ub_revised)
  ) |>
  select(measure, label, metric, thesis, revised, est_diff, est_pct_change, est_diff_rel_ci,
         revised_in_thesis_ci, tau_thesis, tau_revised, pi_width_diff)

print(comparison_table, n = Inf, width = Inf)

write_csv(comparison_table, here::here(outputs, "Results", "main_results_thesis_vs_revised.csv"))

# Summary by measure and metric ----
comparison_summary <- comparison |>
  group_by(measure, metric) |>
  summarise(
    n = n(),
    max_abs_diff = max(abs(est_diff)),
    mean_abs_diff = mean(abs(est_diff)),
    max_abs_diff_rel_ci = max(abs(est_diff_rel_ci)),
    n_outside_thesis_ci = sum(!revised_in_thesis_ci),
    .groups = "drop"
  )

print(comparison_summary, n = Inf, width = Inf)

# Largest changes, relative to the thesis confidence interval width
comparison |>
  slice_max(abs(est_diff_rel_ci), n = 10) |>
  select(measure, label, metric, est_thesis, est_revised, est_diff, est_diff_rel_ci) |>
  print(width = Inf)
