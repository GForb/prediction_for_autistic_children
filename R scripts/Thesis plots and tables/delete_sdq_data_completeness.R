analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline")


predictor_completeness <- 
  analysis_data_wide |> 
  mutate(study = "zOverall") |> 
  bind_rows(analysis_data_wide) |> 
  select(ID, study, base_sex, base_ld,
         base_ethnicity, base_imd_decile, base_maternal_mh, base_maternal_education,  ) |> 
  pivot_longer(cols = starts_with("base"), names_to = "predictor", values_to = "value") |> 
  group_by(study, predictor) |>
  summarise(n = n(),
            n_complete = sum(!is.na(value)),
            prop_complete = n_complete/n) |> 
  mutate(text = paste0(n_complete, " (", round(prop_complete*100), "\\%)")) |> 
  select(-n, -n_complete, -prop_complete) |> 
  ungroup() 

predictor_completeness_by_study <- predictor_completeness |> 
  pivot_wider(names_from = study, values_from = text) |> 
  mutate(predictor_label = get_label(predictor, label = 1)) |> 
  mutate(order = c(3,6,2,4,5,1)) |> 
  arrange(order) |> 
  select(predictor_label, everything(), -order, -predictor)

write_csv(predictor_completeness_by_study, file.path(thesis_tables, "sdq_predictor_completeness.csv"))

predictor_completeness_by_var <- predictor_completeness |>   pivot_wider(names_from = predictor, values_from = text) |> 
  select(Study = study, 
         Sex = base_sex, 
         LearningDisability = base_ld, 
         Ethnicity = base_ethnicity, 
         IMD = base_imd_decile, 
         MaternalEducation = base_maternal_education, 
         MaternalMentalHealth = base_maternal_mh)

write_csv(predictor_completeness_by_var, file.path(thesis_tables, "sdq_predictor_completeness_by_var.csv"))
