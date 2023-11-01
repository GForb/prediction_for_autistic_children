
wave_1_folder <- file.path(raw_data, "SSC", "SSC Version 15.3 Phenotype Dataset", "Proband Data")
wave_2_folder <- file.path(raw_data, "SSC", "SSCIAN_FollowUp_Dataset")

ssc_descriptive1 <- read_csv(file.path(wave_1_folder, "ssc_core_descriptive.csv"))
ssc_cbcl1_raw <- read_csv(file.path(wave_1_folder, "cbcl_6_18.csv"))
ssc_cbcl2 <- read_csv(file.path(raw_data, "SSC/SSCIAN_FollowUp_Dataset/cbcl_6_18.csv"))
ssc_med_hist2 <- read_csv(file.path(raw_data, "SSC/SSCIAN_FollowUp_Dataset/asd_dx_med_hx.csv"))



# Selecting and renaming relevant CBCL scores

cbcl_scores1 <- ssc_cbcl1_raw |> 
  mutate(cbcl_ext_total = rule_breaking_total + aggressive_behavior_total,
         cbcl_int_total = somatic_complaints_total + anxious_depressed_total + withdrawn_total) |> 
  mutate(cbcl_ext_total_check = cbcl_ext_total - externalizing_problems_total,
         cbcl_int_total_check = cbcl_int_total - internalizing_problems_total)


cbcl_scores1$cbcl_ext_total_check |> table()
cbcl_scores1$cbcl_int_total_check |> table()

cbcl_scores1 <- cbcl_scores1 |>   
  select(ID = individual, 
     cbcl_ext_tscore = externalizing_problems_t_score,
     cbcl_int_tscore = internalizing_problems_t_score,
     cbcl_ext_total, cbcl_int_total) 

cbcl_scores2 <- ssc_cbcl2 |> 
  filter(role == "proband") |> 
  mutate(wave = 2,
         cbcl_ext_total = ach_rulebreak_sum + ach_aggressive_sum,
         cbcl_int_total = ach_somatic_sum + ach_anxdep_sum + ach_withdep_sum) |> 
  mutate(cbcl_ext_total_check = cbcl_ext_total - ach_external_sum,
         cbcl_int_total_check = cbcl_int_total - ach_internal_sum)


cbcl_scores2$cbcl_ext_total_check |> table()
cbcl_scores2$cbcl_int_total_check |> table()

cbcl_scores2 <- cbcl_scores2 |> 
  select(ID = sfari_id, 
         cbcl_ext_total,
         cbcl_ext_tscore = ach_external_t_score,
         cbcl_int_total, 
         cbcl_int_tscore = ach_internal_t_score)  
  
# Defining wave variable

# Processing autism variables ----

# autism_status = 1: Autism reported & not diagnosed by a medical professional
# Autism_status = 2: Autism reported diagnosed by a medical professional

ssc_wave_1 <- ssc_descriptive1 |> 
  mutate(wave = 1) |> 
  select(ID = individual, 
         ados_css, 
         sex, 
         diagnosis_ados, 
         iq_full_scale = ssc_diagnosis_full_scale_iq,
         iq_verbal = ssc_diagnosis_verbal_iq,
         iq_performance = ssc_diagnosis_nonverbal_iq,
         age = age_at_ados) |> 
  left_join(cbcl_scores1, by = "ID") |> 
  mutate(sex = case_when(sex == "male" ~ 1,
                        sex ==  "female" ~ 2),
         age = age/12,
         wave = 1)
 


ssc_wave_2 <- ssc_med_hist2 |> 
  select(ID = sfari_id, age = age_at_eval) |> 
  full_join(cbcl_scores2, by = "ID") |> 
  mutate(wave =2,
         age = age/12,
         ID = str_replace(ID, "-", ".")) 


# Renaming Variables ----

data_all <- bind_rows(ssc_wave_1, ssc_wave_2)

# Adding study level variables ----
ssc_data <- data_all |> mutate(study = "ssc",
                                   country = "USA")

check_values(ssc_data)

save (ssc_data, file = file.path(derived_data, "ssc.Rdata"))
