data <-   readRDS(here(derived_data, "pooled_sdq.Rds")) 
outcome_names <- var_metadata |> filter(outcome ==1) |> pull(variable_name)


data |> 
  filter(wave == out_wave) |>
  select(ID, age, study, any_of(c(outcome_names, "fu_length"))) |> 
  get_counts_table_by_study()