analysis_data <- readRDS(here(derived_data, "pooled_vabs.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)


base_table <- analysis_data_base |> select(ID, study, starts_with("vabs")) |>  create_descriptive_table()
out_table <- analysis_data_out |> select(ID, study, starts_with("vabs")) |>  create_descriptive_table()

order_n = tibble(
  variable = c("base_sex", 
               "base_iq_full_scale", "base_iq_standard",
               "base_ados_css_rrb", "base_ados_css_sa",
               "base_adi_65", "base_maternal_education", "base_ethnicity"), 
  order_no = 1:8)
base_predictors_table <- analysis_data_base |> 
  select(
    ID, study,  base_sex, 
    base_adi_65, base_ados_css_rrb, base_ados_css_sa, base_iq_full_scale, base_iq_standard, base_maternal_education, base_ethnicity) |>  
  create_descriptive_table(order = order_n) 

base_table |> save_descriptive_hux_table(outcome_str = "vabs", what = "base")
base_predictors_table |> save_descriptive_hux_table(outcome_str = "vabs", what = "pred")
out_table |> save_descriptive_hux_table(outcome_str = "vabs", what = "out")


# 
# write_csv(base_table, file.path(thesis_tables, "vabs_descriptive_table_base.csv"))
# write_csv(base_predictors_table, file.path(thesis_tables, "vabs_descriptive_table_predictors.csv"))
# write_csv(out_table, file.path(thesis_tables, "vabs_descriptive_table_outcomes.csv"))
