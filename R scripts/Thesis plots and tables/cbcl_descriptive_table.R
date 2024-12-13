analysis_data <- readRDS(here(derived_data, "pooled_cbcl.Rds"))|> 
  filter(base_all_complete, out_all_complete) 

# source for cutoffs: Swedish clinical sample https://pmc.ncbi.nlm.nih.gov/articles/PMC8297893/
cbcl_cutoffs <- tibble(
  variable = c(
    "cbcl_aff",
    "cbcl_anx",
    "cbcl_som",
    "cbcl_adhd",
    "cbcl_odd",
    "cbcl_con"
  ),
  cutoff = c(7, 6, NA, 6, 8, 14),
  n_items = c(13, 6, 7, 7, 5, 17)
)

analysis_data_base <- analysis_data |> filter(wave == base_wave)

# Code for ICCs - not used. Note ANOVA Rhos are way higher.
fishmethods::clus.rho(analysis_data_base$age, analysis_data_base$study)
fishmethods::clus.rho(analysis_data_base$cbcl_aff, analysis_data_base$study)
fishmethods::clus.rho(analysis_data_base$cbcl_anx, analysis_data_base$study)



analysis_data_out <- analysis_data |> filter(wave == out_wave)

base_table <- analysis_data_base |> select(ID, study, starts_with("cbcl")) |>  create_descriptive_table_outcome(cutoffs = cbcl_cutoffs)
out_table <- analysis_data_out |> select(ID, study, starts_with("cbcl")) |>  create_descriptive_table_outcome(cutoffs = cbcl_cutoffs)

order_n = tibble(
  variable = c("base_sex", 
               "base_iq_full_scale", "base_iq_standard",
               "base_ados_css_rrb", "base_ados_css_sa", "base_vabs_abc_ss",
               "base_adi_65", "base_maternal_education", "base_ethnicity"), 
  order_no = 1:9)
base_predictors_table <- analysis_data_base |> 
  select(
    ID, study,  base_sex, 
    base_adi_65, base_ados_css_rrb, base_ados_css_sa, base_iq_full_scale, base_iq_standard,base_vabs_abc_ss, base_maternal_education, base_ethnicity) |>  
  create_descriptive_table(order = order_n) 


base_table_output <- base_table |> save_descriptive_hux_table(outcome_str = "cbcl", what = "base", outcome_table = TRUE, col1_width = 0.12, col2_width = 0.18, htb = TRUE)
base_predictors_table |> save_descriptive_hux_table(outcome_str = "cbcl", what = "pred", htb = TRUE)
out_table |> save_descriptive_hux_table(outcome_str = "cbcl", what = "out", outcome_table = TRUE, col1_width = 0.12, col2_width = 0.18, htb = TRUE)


