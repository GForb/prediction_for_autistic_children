
save_folder <- here(derived_data, "SDQ_gen_pop")

studies = c("gui", "mcs", "lsac_b", "lsac_k")
pooled_data = tibble()
for(study in studies){
  load(here(derived_data, paste0(study, ".Rdata")))
  data <- get(paste0(study, "_data"))
  pooled_data <- bind_rows(pooled_data, data)
}

pooled_data |> filter(autistic_any_wave ==1) |> select(study) |> table()

nrow_pooled <- nrow(pooled_data)


pooled_data <- pooled_data |> mutate(base_wave = case_when(study  ==  "gui" ~ 2,
                                                           study  ==  "mcs" ~ 3,
                                                           study == "lsac_b" ~ 7,
                                                           study == "lsac_k" ~ 5),
                                     out_wave = base_wave + 1,
                                     relative_wave = wave - base_wave,
                                     age_cent = (age - mean(age, na.rm = TRUE))/sd(age, na.rm = TRUE)) |> 
  rename(studyid = study)

# mutate(study_mcs = case_when(study == "mcs" ~ 1,
#                              TRUE ~ 0),
#        study_gui = case_when(study == "gui" ~ 1,
#                              TRUE ~ 0),
#        study_lsac_k = case_when(study == "lsac_k" ~ 1,
#                                 TRUE ~ 0) ,
#        study_lsac_b = case_when(study == "lsac_b" ~ 1,
#                                 TRUE ~ 0) ) |> 



# Sampling non-autistic participants
non_autistic_ids <- pooled_data |> filter(autistic_any_wave !=1 | is.na(autistic_any_wave)) |> 
  select(ID, studyid) |> 
  unique()

set.seed(1000)
sample <- non_autistic_ids %>%
  group_by(studyid) %>%
  sample_n(size=1000) |> 
  mutate(non_autistic_eda_sample1000 = 1)

pooled_data <-  pooled_data |> left_join(sample)
pooled_data |> select(non_autistic_eda_sample1000) |> table(useNA = "always")
pooled_data |> 
  group_by(studyid, wave) |> 
  summarise(n = sum(!is.na(non_autistic_eda_sample1000))) |> 
  ungroup() |> 
  pivot_wider(names_from = studyid, values_from = n)

# Forming base- and outcome datasets
base_data <- pooled_data |> filter(wave == base_wave)
out_data <- pooled_data |> filter(wave == out_wave) 

out_data_mod <- out_data |> 
  select(ID, studyid, age, starts_with("sdq"), starts_with("study")) |> 
  rename_with(
    ~ paste0("y_", .x, recycle0 = TRUE),
    starts_with("sdq")
  ) |> 
  rename(y_age = age)

single_timepoint_data <- base_data |> full_join(out_data_mod, by = "ID") |> 
  mutate(studyid = case_when(!is.na(studyid.x) ~ studyid.x,
                            TRUE ~ studyid.y) )|> 
  select(-studyid.x, -studyid.y) |> 
  mutate(fu_length = y_age -age)

# Adding base predictors to pooled data

base_data_renamed <- pooled_data |> 
  filter(wave == base_wave) |> 
  select(ID, studyid, starts_with("sdq"), age, age_cent, sex ) |> 
  rename_with(
    ~ paste0("base_", .x, recycle0 = TRUE),
    c(-ID, -studyid)  
  )


pooled_data <- pooled_data |> 
  left_join(base_data_renamed) |> 
  mutate()

pooled_data |> filter(studyid == "mcs") |> select( sex, wave) |> table( useNA = "always")



pooled_data_non_autistic <- pooled_data |> 
  filter(autistic_any_wave !=1 | is.na(autistic_any_wave)) |> 
  mutate(train_data = case_when(non_autistic_eda_sample1000 ==1 ~ 1,
                                TRUE ~0),
         test_data = 1-train_data)

table(pooled_data_non_autistic$train_data, pooled_data_non_autistic$test_data)

nrow(pooled_data)
nrow(pooled_data_non_autistic)
nrow(base_data)
nrow(out_data)
nrow(single_timepoint_data)
nrow(single_timepoint_data |> filter(!is.na(studyid)))



saveRDS(pooled_data, file = here(save_folder, "sdq_gen_pop_all.rds"))

saveRDS(pooled_data_non_autistic, file = here(save_folder, "sdq_gen_pop_non_autistic.rds"))


pooled_data_non_autistic1000 <- pooled_data_non_autistic |> filter(non_autistic_eda_sample1000 ==1) 
saveRDS(pooled_data_non_autistic1000, file = here(save_folder, "sdq_gen_pop_non_autistic1000.rds"))
haven::write_dta(data = pooled_data_non_autistic1000, path = here(save_folder, "sdq_gen_pop_non_autistic1000.dta"))

pooled_data_test <- pooled_data_non_autistic |> filter(non_autistic_eda_sample1000 !=1 | is.na(non_autistic_eda_sample1000))
saveRDS(pooled_data_test, file = here(save_folder, "sdq_gen_pop_non_autistic_test.rds"))
haven::write_dta(data = pooled_data_test, path = here(save_folder, "sdq_gen_pop_non_autistic_test.dta"))

# Creating test, and train base and outcome data sets
train_data_1tp <- single_timepoint_data |> filter(non_autistic_eda_sample1000 ==1)

saveRDS(train_data_1tp, file = here(save_folder, "sdq_eda_1tp_train_data.rds"))
haven::write_dta(train_data_1tp, path = here(save_folder, "sdq_eda_1tp_train_data.dta"))


# Processing test data
test_data_1tp <- single_timepoint_data |> 
  filter(non_autistic_eda_sample1000 !=1 | is.na(non_autistic_eda_sample1000)) |> 
  filter(autistic_any_wave !=1 | is.na(autistic_any_wave)) 
saveRDS(test_data_1tp, file = here(save_folder, "sdq_eda_1tp_test_data.rds"))





