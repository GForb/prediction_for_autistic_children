

make_single_study_table <- function(target, target_label, full_results) {
  table_data <- full_results |> select(outcome, analysis_name, target = any_of(target)) |> 
    left_join(scale_max) |>
    mutate(
      target =  target |> round(2) ,
      target_char = target |> as.character(),
      model = analysis_name |> str_remove(outcome)
    ) |>
    select(outcome, model, target_char) |>
    pivot_wider(names_from = model, values_from = target_char) |> 
    select(outcome, 
           primary_analysis = mt_fi_study_rs__pred1_mt,
           single_tp = st_fi_study__pred1,
           primary_analysis_3pred = mt_fi_study_rs__pred1_mt_3pred,
           primary_analysis_pred_set3 = mt_fi_study_rs__pred3_mt
    ) |> 
    mutate(outcome = get_label(outcome, label_no = 1),
           primary_analysis_pred_set3 = case_when(
             is.na(primary_analysis_pred_set3) ~ "",
             TRUE ~ primary_analysis_pred_set3)
    )
  
  
  caption <-  glue::glue("{target_label} for analysis conducted using data from the a single study for the SDQ and CBCL. 
                                        Analysis of the SDQ were conducted using data from the MCS, for the CBCL TRAILS was used. RMSE calculated using 10 fold cross validation repated 10 time. 
                                        All analysis excpet the final column are using predictor set 1 only.")
  
  rbind(
    c(
      "Outcome",
      "Random Slope",
      "Single Timepoint",
      "Random slope - 3 assessments",
      "Random slope predictor set 3"
    ),
    table_data) |> 
    huxtable::hux(add_colnames = FALSE) |> 
    huxtable::set_bottom_border(row = 1, value = 0.5) |>
    huxtable::set_bottom_border(row = 6, value = 0.5) |>
    huxtable::set_width(1) |> 
    huxtable::set_wrap(row = 1, value = TRUE) |> 
    save_hux_table(file_name =glue::glue({"single_study_{target}.tex"}),
                   caption = caption,
                   label = glue::glue("single_study_{target}"), 
                   htb = TRUE)
  
}

scale_max <- bind_rows(
  cbcl_cutoffs |> select(outcome, max),
  sdq_cutoffs |> select(outcome, max) 
)

full_results <- map(c("SDQ", "CBCL"),
                    function(study_name) {
                      results_folder <- here::here(data_and_outputs, "Results", study_name, "Single Study")
                      readRDS(here::here(results_folder,"results_meta_analysis.rds"))
                    }) |> 
  bind_rows() |> 
  left_join(scale_max) |>
  mutate(
    rmse_stand =  (rmse / max *10))

make_single_study_table("rmse_stand", "Standardised RMSE", full_results)
make_single_study_table("r_squared_transformed", "$R^2$", full_results)
make_single_study_table("calib_slope", "Calibration slope", full_results)
make_single_study_table("calib_itl", "Calibration in-the-large", full_results)


