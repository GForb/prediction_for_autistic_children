data_raw_vabs  <- readxl::read_xlsx(here::here(data_folder, "ELENA_DATA_VABS_20250130_protege.xlsx")) 
data_raw_cbcl <- readxl::read_xlsx(here::here(data_folder, "ELENA_DATA_20240613_protege.xlsx")) 



cbcl_iqs <- tibble(
  test_label = unique(c(data_raw_cbcl$calcul_bestQDV0, data_raw_cbcl$calcul_bestQDV2)), 
  test = c("Non-verbal", "Perceptual", "Fluid reasoning", "FSIQ", "Non-standard", 
           "Non-standard", "Fluid reasoning", "Fluid reasoning", "Vineland DLS SS", 
           "Perceptual", "Non-standard", "Non-standard", "Non-standard", "Perceptual")) |> 
  mutate(standard = case_when(test == "Non-standard" ~ 0,
                              TRUE ~ 1),
         treat_as = case_when(test == "FSIQ" ~ "base_iq_full_scale",
                              test == "Perceptual" ~ "base_iq_perceptual",
                              test == "Fluid reasoning" ~ "base_iq_perceptual",
                              test == "Vineland DLS SS" ~ "base_vabs_abc_ss",
                              TRUE ~ "base_iq_full_scale"))


       
vabs_iqs <- tibble(
  test_label = unique(c(data_raw_vabs$calcul_bestQDV0, data_raw_vabs$calcul_bestQDV2)), 
  test = c(
    "Non-verbal",
    "Perceptual",
    "Non-standard",
    "Fluid reasoning",
    "FSIQ",
    "Fluid reasoning",
    "Non-standard",
    "Non-standard",
    "Perceptual",
    "Non-standard",
    "Non-verbal",
    "Perceptual",
    "Non-standard",
    "Fluid reasoning",
    "Non-standard",
    "Perceptual",
    "FSIQ",
    "Vineland DLS SS",
    "Perceptual",
    "Non-standard"
  )) |>
  mutate(standard = case_when(test == "Non-standard" ~ 0,
                              TRUE ~ 1),
         treat_as = case_when(test == "FSIQ" ~ "base_iq_full_scale",
                              test == "Perceptual" ~ "base_iq_perceptual",
                              test == "Fluid reasoning" ~ "base_iq_perceptual",
                              test == "Vineland DLS SS" ~ "base_vabs_abc_ss",
                              TRUE ~ "base_iq_full_scale"))




#All trues or NA all good.
left_join(vabs_iqs, cbcl_iqs, by = "test_label") |> 
  mutate(test_check = test.x == test.y,
         standard_check = standard.x == standard.y,
         treat_as_check = treat_as.x == treat_as.y) 