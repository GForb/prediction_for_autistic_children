pooled_data_cbcl <- pool_datasets(
  c("pathways_cbcl", # checked
    "elena_cbcl", # checked
    "ssc", # checked
    "togo1", # checked
    "togo2",
    "TRAILS"), # checked
  include_acc = TRUE)

# leave out "edx_cbcl" until I have item level data.


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


# Saving data with splines

analysis_data_long <- pooled_data_cbcl$pooled_data_long |> 
  filter(base_all_complete, out_all_complete, all_complete) |> 
  select(-`0`)

spline_stata_code_cbcl <- "
    mkspline age_spline = age_c , nknots(3) cubic
    gen age_spline1Xsex = age_spline1*base_sex
    gen age_spline2Xsex = age_spline2*base_sex
    su age_spline*"

spline_stata_code_cbcl2 <- "di 1"

analysis_data_long_spline <- RStata::stata(
   spline_stata_code_cbcl, 
   data.in = analysis_data_long, 
   data.out = TRUE)


saveRDS(analysis_data_long_spline, here::here(derived_data, "pooled_cbcl_spline.Rds"))
