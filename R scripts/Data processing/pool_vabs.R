
pooled_data_vabs <- pool_datasets(c("pathways_vabs", "elena_vabs", "epited", "edx_vabs"))



pooled_data_vabs$pooled_data_wide <- pooled_data_vabs$pooled_data_wide |> 
  mutate(in_dls = case_when(!is.na(out_vabs_dls_ae) & !is.na(base_vabs_dls_ae) & !is.na(base_vabs_com_ae) & !is.na(base_vabs_soc_ae) ~ 1,
                            TRUE ~ 0),
         in_com = case_when(!is.na(out_vabs_com_ae) & !is.na(base_vabs_dls_ae) & !is.na(base_vabs_com_ae) & !is.na(base_vabs_soc_ae) ~ 1,
                            TRUE ~ 0),
         in_soc = case_when(!is.na(out_vabs_soc_ae) & !is.na(base_vabs_dls_ae) & !is.na(base_vabs_com_ae) & !is.na(base_vabs_soc_ae) ~ 1,
                            TRUE ~ 0),
         base_iq_standard = case_when(!is.na(base_iq_full_scale) ~ base_iq_standard,
                                      is.na(base_iq_full_scale) ~ NA))|> 
          rowwise() |> 
          mutate(base_vabs_dq = mean(c(base_vabs_dls_ae, base_vabs_com_ae, base_vabs_soc_ae))/base_age*10) |> 
          ungroup() 

# Single impuation for standard scores based on base_vabs_dq
pooled_data_vabs$pooled_data_wide

pooled_data_vabs$pooled_data_long <- pooled_data_vabs$pooled_data_long |> 
  left_join(pooled_data_vabs$pooled_data_wide |> select(ID, in_dls, in_com, in_soc, base_vabs_dq), by = "ID") |> 
  mutate(base_vabs_dq_dec = base_vabs_dq/10,
         base_iq_standard = case_when(!is.na(base_iq_full_scale) ~ base_iq_standard,
                                      is.na(base_iq_full_scale) ~ NA))

complete_data_long <- pooled_data_vabs$pooled_data_long  |> filter(base_all_complete, out_all_complete)
complete_data_wide <- pooled_data_vabs$pooled_data_wide  |> filter(base_all_complete, out_all_complete)

pooled_data_vabs$pooled_data_acc <- pooled_data_vabs$pooled_data_acc |> 
  mutate(study = case_when(study == "pathways_vabs" ~ "Pathways",
                           study == "elena_vabs" ~ "ELENA",
                           study == "epited" ~ "EpiTED",
                           study == "edx_vabs" ~ "EDX"),
         ID = paste0(study, "_", old_ID))

saveRDS(pooled_data_vabs$pooled_data_long, here(derived_data, "pooled_vabs.Rds"))
saveRDS(pooled_data_vabs$pooled_data_wide, here(derived_data, "pooled_vabs_wide.Rds"))
saveRDS(pooled_data_vabs$pooled_data_acc, here(derived_data, "pooled_vabs_acc.Rds"))

haven::write_dta(pooled_data_vabs$pooled_data_long, here(derived_data, "pooled_vabs.dta"))
haven::write_dta(pooled_data_vabs$pooled_data_wide, here(derived_data, "pooled_vabs_wide.dta"))

haven::write_dta(complete_data_wide, here(derived_data, "pooled_vabs_wide_complete.dta"))
haven::write_dta(complete_data_long, here(derived_data, "pooled_vabs_long_complete.dta"))


analysis_data_long <-pooled_data_vabs$pooled_data_long |> 
  filter(base_all_complete, out_all_complete, all_complete) # exclude people missing a single value in one outcome.

# Missing data is at level 2: The cluster level (cluster = individual) - this is the script to use to investigate how to impute missing data.
# 1% of data are partially complete - to drop or not to drop? - wont introduce bias


spline_stata_code <- "
    mkspline age_spline = age_c , nknots(3) cubic
    gen age_spline1Xsex = age_spline1*base_sex
    gen age_spline2Xsex = age_spline2*base_sex
    gen age_spline1Xdq = age_spline1*base_vabs_dq/10
    gen age_spline2Xdq = age_spline2*base_vabs_dq/10
    su age_spline*  
"

analysis_data_long <- RStata::stata(spline_stata_code, data.in = analysis_data_long, data.out = TRUE) 

saveRDS(analysis_data_long, here::here(derived_data, "pooled_vabs_spline.Rds"))


