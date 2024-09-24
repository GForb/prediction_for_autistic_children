pooled_data_cbcl <- pool_datasets(
  c("pathways_cbcl", # checked
    "elena_cbcl", # checked
    "ssc", # checked
    "togo1", # checked
    "togo2"), # checked
  include_acc = TRUE)

# leave out "edx_cbcl" until I have item level data.


# what a shit show
pooled_data_cbcl$pooled_data_long |> 
  group_by(study) |> 
  summarise(mean_obs = mean(n_obs))

# Think about implications of most trajectory information being between individual in the longitiudinal models...


pooled_data_cbcl$pooled_data_wide |> count(study)
pooled_data_cbcl$pooled_data_long |> count(study, base_wave)

pooled_data_cbcl$pooled_data_long |> filter(study == "SSC") |> count(wave) 

pooled_data_cbcl$pooled_data_long |> 
  filter(study == "Pathways") |> 
  select(wave, base_wave) |> 
  count(wave)

pooled_data_cbcl$pooled_data_acc <- pooled_data_cbcl$pooled_data_acc |> 
  mutate(study = case_when(study == "pathways_cbcl" ~ "Pathways",
                           study == "elena_cbcl" ~ "ELENA",
                           study == "ssc" ~ "SSC",
                           study == "togo1" ~ "TOGO1",
                           study == "togo2" ~ "TOGO2"),
         ID = paste0(study, "_", old_ID))



saveRDS(pooled_data_cbcl$pooled_data_long |> select(-"0"), here(derived_data, "pooled_cbcl.Rds"))
saveRDS(pooled_data_cbcl$pooled_data_wide |> select(-"0"), here(derived_data , "pooled_cbcl_wide.Rds"))
saveRDS(pooled_data_cbcl$pooled_data_acc, here(derived_data, "pooled_cbcl_acc.Rds"))

haven::write_dta(pooled_data_cbcl$pooled_data_long |> select(-"0"), here(derived_data, "pooled_cbcl.dta"))
haven::write_dta(pooled_data_cbcl$pooled_data_wide |> select(-"0"), here(derived_data, "pooled_cbcl_wide.dta"))