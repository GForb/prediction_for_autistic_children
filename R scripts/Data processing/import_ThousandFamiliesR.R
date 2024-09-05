# Aim: Create file in long format with one row per participant and wave

data_folder <- here::here(raw_data, "ThousandFamilies")

data  <- haven::read_sav(here::here(data_folder, "v1. Predicting autistic children's outcomes.sav"))
colnames(data) 

predictors <- data |> select(
  ID = ID_updated,
  base_sex = W1_childgender,
  base_hearing_impairment = W1_Hearing,
  base_visual_impairment = W1_Visual,
  base_imd_decile = W1_IMDDecile_R,
  base_subjective_poverty = W1_Financial,
  learning_disability_cat = W1_IDlevel,
  base_ethnicity = W1_Ethnicity,
  base_maternal_education = W1_Education,
  base_maternal_mh = W1_K6total,
  base_vabs_abc_ss = W1_VABSscoresstandardisedABCscore
) |> 
  mutate(base_sex = base_sex - 1,
         base_ethnicity = case_when(base_ethnicity >= 20 ~ 1,
                                    base_ethnicity <= 19 ~ 0),
         learning_disability = 1,
         base_visual_impairment = 2 - base_visual_impairment,
         base_hearing_impairment = 2- base_hearing_impairment,
         base_maternal_education = case_when(base_maternal_education <= 4 ~ 0,
                                             base_maternal_education >=5 ~ 1))

wave1_data <- data |> 
  select(
  ID = ID_updated,
  sdq_emot_p = W1_pemotion,
  sdq_cond_p = W1_pconduct, 
  sdq_hyp_p = W1_phyper,
  sdq_peer_p = W1_ppeer,
  sdq_pro_p = W1_pprosoc,
  age = W1_RCHage_yr
) |> 
  mutate(wave = 0)
 

wave2_data <- data |> select(
  ID = ID_updated,
  age = W2ChildAge,
  sdq_emot_p = W2pemotion,
  sdq_cond_p = W2pconduct, 
  sdq_hyp_p = W2phyper,
  sdq_peer_p = W2ppeer,
  sdq_pro_p = W2pprosoc
) |> 
  mutate(wave = 1)

data_all <- bind_rows(wave1_data, wave2_data) |> left_join(predictors)
  



# Renaming Variables ----


k_families_data <- data_all |> mutate(study = "k_families",
                                      country = "UK",
                                      base_wave = 0, 
                                      out_wave = 1,
                                      autism = "childhood, parent report",
                                      base_ld = 1) 

particpant_accounting <- get_age_range_data_sdq(wave1_data = wave1_data, wave2_data = wave2_data) |>  mutate(autism = "childhood, parent report") 
particpant_accounting |> count(include)
check_values(k_families_data)

saveRDS(k_families_data, file = file.path(derived_data, "k_families.Rds"))

saveRDS(particpant_accounting, file = file.path(derived_data, "k_families_acc.Rds"))


# -------------------------------------------------------------------------

