
wave_1_folder <- file.path(raw_data, "SSC", "SSC Version 15.3 Phenotype Dataset", "Proband Data")
wave_2_folder <- file.path(raw_data, "SSC", "SSCIAN_FollowUp_Dataset")

ssc_vabs1 <- read_csv(file.path(wave_1_folder, "vineland_ii.csv")) |> 
  select(ID = individual, base_vabs_abc_ss = composite_standard_score)
ssc_descriptive1 <- read_csv(file.path(wave_1_folder, "ssc_core_descriptive.csv"))
ssc_cbcl1_raw <- read_csv(file.path(wave_1_folder, "cbcl_6_18.csv"))
ssc_ados1_words <- read_csv(file.path(wave_1_folder, "ados_1.csv")) |> 
  mutate(words = case_when(algorithm == "some-words" ~ 1,
                           algorithm == "no-words" ~0,
                           TRUE ~ NA)) |> 
  select(ID = individual, words)  

ssc_adi <- read_csv(file.path(raw_data, "SSC/SSC Version 15.3 Phenotype Dataset/Proband Data/adi_r.csv")) |> 
  mutate(base_adi_65 = process_adi65(q65_friendships_current)) |>
  select(ID = individual, base_adi_65 ) 

ssc_cbcl2 <- read_csv(file.path(raw_data, "SSC/SSCIAN_FollowUp_Dataset/cbcl_6_18.csv"))
ssc_med_hist2 <- read_csv(file.path(raw_data, "SSC/SSCIAN_FollowUp_Dataset/asd_dx_med_hx.csv"))

ssc_hearing_vision_raw <- read_csv(file.path(wave_1_folder, "medhx_hearing_vision_allergies.csv"))
ssc_wave_2_age <- read_csv(file.path(wave_2_folder, "ssc_follow_up_cbcl_age_at_eval.csv")) |> 
  mutate(age = age_at_eval/12) |> 
  select(ID = sfari_id, age)
  

ssc_hearing_vision <-  ssc_hearing_vision_raw |> 
  rename(ID = individual) |> 
  mutate(base_visual_impairment = case_when(vision == "no" ~ 0,
                                       vision == "yes"  ~ 1),
         base_hearing_impairment = case_when(hearing == "no" ~ 0,
                                        hearing == "yes"  ~ 1) ) |> 
  select(ID, base_visual_impairment, base_hearing_impairment) 
  

colnames(ssc_hearing_vision) 

# Selecting and renaming relevant CBCL scores



predictors <- ssc_descriptive1 |> 
  rename(ID = individual) |> 
  left_join(ssc_ados1_words, by = "ID") |> 
  mutate(words = case_when(ados_module ==1 ~ words,
                           ados_module = NA ~ NA,
                           ados_module > 1 ~ 1)) |> 
  mutate(age = age_at_ados/12,
         sex = case_when(sex == "male" ~ 0,
                         sex == "female" ~ 1),
        ethnicity = case_when(race == "white" ~ 1,
                              race == "" ~ NA,
                              TRUE ~ 0),
        ados_social_affect = as.numeric(ados_social_affect),
        ados_css_sa = calculate_css_sa(ados_social_affect, module = ados_module, words = words),
        ados_css_rrb = calculate_css_rrb(ados_restricted_repetitive, module = ados_module, words = words),
        iq_standard = 2-ssc_diagnosis_full_scale_iq_type) |> 
  select(ID,
         base_sex = sex,
         base_ethnicity = ethnicity,
         base_iq_full_scale = ssc_diagnosis_full_scale_iq,
         base_iq_standard = iq_standard,   # 1 standard, 2 ratio
         base_ados_css_sa = ados_css_sa,
         base_ados_css_rrb = ados_css_rrb) |> 
  left_join(ssc_adi, by = "ID") |>
  left_join(ssc_hearing_vision, by = "ID") |> 
  left_join(ssc_vabs1, by = "ID") 

edit_cbcl_item_namesSSC <- Vectorize(function(old_name) {
  parts <- unlist(strsplit(old_name, "_"))
  item_number <- substr(parts[1], 2, nchar(parts[1])) |> as.numeric()
  if(item_number == 56){
    item_number <- paste0(56, parts[2])
  }

  new_name <- paste0("cbcl_item_", item_number)
  
  return(new_name)
})



cbcl_scores1 <- ssc_cbcl1_raw |> 
  select(ID = individual,
         cbcl_aff = affective_problems_total,
         cbcl_anx = anxiety_problems_total, 
         cbcl_som = somatic_prob_total,
         cbcl_adhd = add_adhd_total,
         cbcl_odd = oppositional_defiant_total,
         cbcl_con = conduct_problems_total) |> 
  left_join(ssc_descriptive1 |> select(ID = individual, age = age_at_ados)) |> 
  mutate(cbcl_odd = case_when(cbcl_odd == 89 ~ NA,
                              TRUE ~ cbcl_odd),
         wave = 0,
         age = age/12) 


cbcl_scores2 <- ssc_cbcl2 |> 
  filter(role == "proband") |> 
  select(ID = sfari_id,
         cbcl_aff = dsm5_depress_sum,
         cbcl_anx = dsm5_anxdisord_sum, 
         cbcl_som = dsm5_somaticpr_sum,
         cbcl_adhd = dsm5_adhd_sum,
         cbcl_odd = dsm5_opposit_sum,
         cbcl_con = dsm5_conduct_sum) |> 
  left_join(ssc_wave_2_age, by = "ID") |> 
  mutate(wave = 1,
         ID = sub("-", ".", ID))

cbcl_item_scores <- ssc_cbcl2 |> 
  rename_with(edit_cbcl_item_namesSSC, .cols = starts_with("q")) |> 
  select(ID = sfari_id, starts_with("cbcl_item")) |> 
  calc_cbcl_dsmIV_domains()

wave1_data <- cbcl_scores1 |> 
  right_join(predictors, by = "ID") 
  
wave2_data <-  cbcl_scores2 |> 
  left_join(predictors, by = "ID") 

age_range_data <- get_age_range_data_cbcl(wave1_data, wave2_data)

age_range_data |> count(include)


# Renaming Variables ----

data_all <- bind_rows(wave1_data, wave2_data) |> 
  left_join(age_range_data |> select(ID, include), by = "ID") |>
  filter(include == "include") |> 
  select(ID, wave, age, everything(), -include)



# Adding study level variables ----
ssc_data <- data_all |> mutate(study = "SSC",
                                   country = "USA",
                               base_wave = 0, 
                               out_wave = 1,
                               autism  = 1) 

check_values(ssc_data)

saveRDS(ssc_data, file = file.path(derived_data, "ssc.Rds"))


particpant_accounting <- age_range_data |> 
  select(ID, include) |> 
  mutate(study = "SSC")
saveRDS(particpant_accounting, file = here::here(derived_data, "ssc_acc.Rds"))


# -------------------------------------------------------------------------

