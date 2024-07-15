pooled_data_sdq <- pool_datasets(
  c("ALSPAC",
    "GUI", 
    "k_families",
    "lsac_b",
    "lsac_k",
    "mcs",
    "Quest", 
    "SNAP",
    "TEDS"
    ), 
  include_acc = TRUE)

# Processing autism status data 

pooled_data_sdq$pooled_data_long |> 
  group_by(study) |> 
  summarise(mean_obs = mean(n_obs))

pooled_data_sdq$pooled_data_long <- pooled_data_sdq$pooled_data_long |> 
  mutate(base_ld = case_when(is.na(base_ld)  & !is.na(base_iq_full_scale) ~ if_else(base_iq_full_scale <70, 1,0),
                              TRUE ~ base_ld))

pooled_data_sdq$pooled_data_wide <- pooled_data_sdq$pooled_data_wide |> 
  mutate(base_ld = case_when(is.na(base_ld) & !is.na(base_iq_full_scale) ~ if_else(base_iq_full_scale <70, 1,0),
                              TRUE ~ base_ld))

# Think about implications of most trajectory information being between individual in the longitiudinal models...
pooled_data_sdq$pooled_data_wide  |> count(autism)
pooled_data_sdq$pooled_data_wide  |> filter(is.na(autism)) |> count(study)
pooled_data_sdq$pooled_data_wide |> count(study)
pooled_data_sdq$pooled_data_wide |> filter(base_sex ==2) |> count(study)

pooled_data_sdq$pooled_data_long |> count(study, base_wave)
pooled_data_sdq$pooled_data_long |> count(wave)


pooled_data_sdq$pooled_data_acc  |> count(study)

pooled_data_sdq$pooled_data_acc |> filter(is.na(autism))  |> count(study)

saveRDS(pooled_data_sdq$pooled_data_long, here(derived_data, "pooled_sdq.Rds"))
saveRDS(pooled_data_sdq$pooled_data_wide, here(derived_data , "pooled_sdq_wide.Rds"))
saveRDS(pooled_data_sdq$pooled_data_acc, here(derived_data, "pooled_sdq_acc.Rds"))

haven::write_dta(pooled_data_sdq$pooled_data_long, here(derived_data, "pooled_sdq.dta"))
haven::write_dta(pooled_data_sdq$pooled_data_wide, here(derived_data, "pooled_sdq_wide.dta"))