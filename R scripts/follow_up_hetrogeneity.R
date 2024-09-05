analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete)

study_char <- analysis_data_wide |> group_by(study) |> 
  summarise(
    ld = mean(base_ld, na.rm = TRUE), 
    fu_length = mean(fu_length),
    n_obs = mean(n_obs, na.rm = TRUE)
  ) |> 
  mutate(year_of_birth = c(2006, 1991, 1997, 2001, 2002, 1991, 1995, 2004, 1999))
study_char

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete)

study_char <- analysis_data_wide |> group_by(study) |> 
  summarise(
    fu_length = mean(fu_length),
    n_obs = mean(n_obs, na.rm = TRUE)
    
  ) 
study_char

analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds")) |> filter(base_all_complete, out_all_complete)

study_char <- analysis_data_wide |> group_by(study) |> 
  summarise(
    fu_length = mean(fu_length),
    n_obs = mean(n_obs, na.rm = TRUE)
    
  ) 

study_char