analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline")

n_obs_data <- readRDS(here(derived_data, "pooled_sdq.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline", wave == base_wave) |> 
  select(ID, n_obs)



ages <- 
  analysis_data_wide |> 
  mutate(study = "zOverall") |> 
  bind_rows(analysis_data_wide) |> 
  select(ID, study, base_age, out_age, fu_length) |> 
  left_join(n_obs_data) |> 
  pivot_longer(cols = c(base_age, out_age, fu_length, n_obs), names_to = "predictor", values_to = "value") |> 
  group_by(study, predictor) |>
  summarise(med = median(value),
            uq = quantile(value, 0.75),
            lq = quantile(value, 0.25)) |> 
  mutate(text = paste0(round(med, 1), " (", round(lq, 1), ", ", round(uq, 1), ")")) |> 
  select(-med, -uq, -lq) |> 
  ungroup() 

ages_table <- ages |> pivot_wider(names_from = predictor, values_from = text) |> select(study, base_age, out_age, fu_length, n_obs)



write_csv(ages_table, file.path(thesis_tables, "sdq_fu_table.csv"))
