analysis_data <- readRDS(here(derived_data, "pooled_cbcl.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)

base_table <- analysis_data_base |> select(ID, study, starts_with("cbcl")) |>  create_descriptive_table()
out_table <- analysis_data_out |> select(ID, study, starts_with("cbcl")) |>  create_descriptive_table()

order_n = tibble(
  variable = c("base_sex", 
               "base_iq_full_scale", "base_iq_standard",
               "base_ados_css_rrb", "base_ados_css_sa", "base_vabs_abc_ss",
               "base_adi_65", "base_maternal_education", "base_ethnicity"), 
  order_no = 1:8)
base_predictors_table <- analysis_data_base |> 
  select(
    ID, study,  base_sex, 
    base_adi_65, base_ados_css_rrb, base_ados_css_sa, base_iq_full_scale, base_iq_standard, base_maternal_education, base_ethnicity) |>  
  create_descriptive_table(order = order_n) 


base_table_output <- base_table |> save_descriptive_hux_table(outcome_str = "cbcl", what = "base")
base_predictors_table |> save_descriptive_hux_table(outcome_str = "cbcl", what = "pred")
out_table |> save_descriptive_hux_table(outcome_str = "cbcl", what = "out")



