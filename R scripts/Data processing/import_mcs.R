

#importing data; wave 1 corresponds to sweep at age 5 ; we have sdq from age 3
mcs_wave_1_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_cm_derived.dta"))
mcs_wave_2_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_cm_derived.dta"))
mcs_wave_3_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_cm_derived.dta"))
mcs_wave_4_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_derived.dta"))
mcs_wave_5_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_derived.dta"))

mcs_wave_1_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_cm_cognitive_assessment.dta"))
mcs_wave_2_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_hhgrid.dta")) |> filter(DCNUM00 !=-1) # filter to get the cohort member
mcs_wave_3_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_cm_interview.dta"))
mcs_wave_4_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_interview.dta"))
mcs_wave_5_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_cognitive_assessment.dta"))

mcs_wave_1_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_parent_cm_interview.dta"))
mcs_wave_2_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_parent_cm_interview.dta"))
mcs_wave_3_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_parent_cm_interview.dta"))
mcs_wave_4_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_parent_cm_interview.dta"))
mcs_wave_5_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_parent_cm_interview.dta"))

mcs_wave_1_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_cm_interview.dta"))
mcs_wave_2_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_cm_interview.dta"))
mcs_wave_3_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_hhgrid.dta")) |>  
  filter(ECNUM00 >0) |> 
  select(MCSID, ECNUM00, ECSEX0000)
mcs_wave_4_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_hhgrid.dta")) |> filter(FCNUM00 >0) |> 
  select(MCSID, FCNUM00, FHCSEX00)
mcs_wave_5_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_interview.dta"))

parent_interview <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_parent_derived.dta"))


# Finding specific predictors
imd_data <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13//mcs5_geographically_linked_data.dta")) |> 
  select(MCSID, base_imd_decile = EIMDSCOE)



ethnicity_data <- haven::read_stata(file.path(raw_data, "MCS/Sweep 1/UKDA-4683-stata/stata/stata13/mcs1_cm_derived.dta")) |> 
  mutate(ID = paste0(MCSID,"_", ACNUM00),
         base_ethnicity = case_when(ADC06E00==1 ~ 1,
                                    ADC06E00 > 1 ~ 0,
                                    ADC06E00 < 1 ~ NA)) |> 
  select(ID, base_ethnicity) 


parent_interview_raw <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_parent_interview.dta")) 
parent_data <- parent_interview_raw |> 
  mutate(base_maternal_mh = case_when(EPPHDE00 >= 0 & EPPHHO00 >= 0 & EPPHRF00 >= 0 &EPPHEE00 >= 0 & EPPHWO00 >= 0 & EPPHNE00 >= 0 ~ EPPHDE00 + EPPHHO00 + EPPHRF00 + EPPHEE00 + EPPHWO00 +  EPPHNE00 -6, # -6 as scores range from 1 to 36
                                                 TRUE ~ NA),
         base_subjective_poverty = case_when(EPMAFI00 > 0 ~ EPMAFI00,
                                             EPMAFI00 < 0 ~ NA)) |> 
  select(MCSID,EPNUM00, base_maternal_mh, base_subjective_poverty) |> 
  mutate(mother_id =  paste0(MCSID, "_", EPNUM00))

ld_data <-  readRDS(file = here(raw_data,"MCS",  "mcs_ID"))

# subjective poverty: EPMAFI00 

# Education

mothers <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_hhgrid.dta")) |> 
  select(MCSID, CHCREL00, CPNUM00, CHPSEX00) |> 
  filter(CHCREL00 ==7, CHPSEX00 ==2) |> 
  mutate(mother_id = paste0(MCSID, "_", CPNUM00))

parental_eudcation_sweep1 <- haven::read_stata(file.path(raw_data, "MCS/Sweep 1/UKDA-4683-stata/stata/stata13/mcs1_parent_interview.dta"))  |> 
  select(MCSID,APNUM00, AELIG00, APLFTE00, APACQU00, APVCQU00) |> 
  mutate(mother_id = paste0(MCSID, "_", APNUM00))  |> 
  select(-MCSID)

maternal_education <-  mothers |> left_join(parental_eudcation_sweep1, by = "mother_id") |> 
  mutate(base_maternal_education = case_when(APACQU00 %in% c(1,2,3) ~ 1,
                                            APACQU00 %in% c(95, 96) & APLFTE00 > 18 ~ 1,
                                            APVCQU00 %in% c(1,2) ~ 1, # Vocational qualificaitons
                                            APACQU00 < 0 ~ NA,
                                            APACQU00 %in% c(4,5,6) ~ 0,
                             APACQU00 %in% c(95, 96) & APLFTE00 <= 18 ~ 0),
                            ) |> 
  select(MCSID, base_maternal_education, mother_id)

family_level_predictors <- 
  maternal_education |> 
  left_join(parent_data |> select(mother_id, starts_with("base")), by = "mother_id") |> 
  full_join(imd_data, by = "MCSID")


# Joining datasets
mcs_wave_1 <- dplyr::full_join(mcs_wave_1_age, mcs_wave_1_sdq)
mcs_wave_2 <- dplyr::full_join(mcs_wave_2_age, mcs_wave_2_sdq)
mcs_wave_3 <- dplyr::full_join(mcs_wave_3_age, mcs_wave_3_sdq)
mcs_wave_4 <- dplyr::full_join(mcs_wave_4_age, mcs_wave_4_sdq)
mcs_wave_5 <- dplyr::full_join(mcs_wave_5_age, mcs_wave_5_sdq)

#selecting primary caregiver as informant for asd status
mcs_wave_1_asd_status <- mcs_wave_1_asd |> 
  filter(CRESP00 == 1) 
mcs_wave_2_asd_status <- mcs_wave_2_asd |> 
  filter(DRESP00 == 1)
mcs_wave_3_asd_status <- mcs_wave_3_asd |> 
  filter(ERESP00 == 1)
mcs_wave_4_asd_status <- mcs_wave_4_asd |> 
  filter(FRESP00 == 1)


#joining asd data 
mcs_wave_1 <- dplyr::full_join(mcs_wave_1, mcs_wave_1_asd_status)
mcs_wave_2 <- dplyr::full_join(mcs_wave_2, mcs_wave_2_asd_status, join_by("MCSID", "DCNUM00")) 
mcs_wave_3 <- dplyr::full_join(mcs_wave_3, mcs_wave_3_asd_status)
mcs_wave_4 <- dplyr::full_join(mcs_wave_4, mcs_wave_4_asd_status)

#joining sex data
mcs_wave_1 <- dplyr::full_join(mcs_wave_1, mcs_wave_1_sex)
mcs_wave_2 <- dplyr::full_join(mcs_wave_2, mcs_wave_2_sex)
mcs_wave_3 <- dplyr::full_join(mcs_wave_3, mcs_wave_3_sex)
mcs_wave_4 <- dplyr::full_join(mcs_wave_4, mcs_wave_4_sex)
mcs_wave_5 <- dplyr::full_join(mcs_wave_5, mcs_wave_5_sex)

mcs_wave_1$wave <- 1
mcs_wave_2$wave <- 2
mcs_wave_3$wave <- 3
mcs_wave_4$wave <- 4
mcs_wave_5$wave <- 5


# renaming variables 
mcs_wave_1_selected <- mcs_wave_1 |> 
  rename(
         sdq_emot_p = CEMOTION,
         sdq_cond_p = CCONDUCT, 
         sdq_hyp_p = CHYPER,
         sdq_peer_p = CPEER,
         sdq_pro_p = CPROSOC,
         sdq_tot_p = CEBDTOT, 
         age = CHCAGE00,
         base_sex = CHCSEX00
         ) |>
  mutate(q_autism = CPAUTS00, 
         ID = paste(MCSID, CCNUM00, sep = "_")) |> 
  mutate(age  = na_if(age, -1),
         age = age/365.25) 

mcs_wave_1_selected <- mcs_wave_1_selected |> 
  mutate(autism = case_when(CPAUTS00 == 1 ~ 1))

wave1_sex <- mcs_wave_1_selected |> select(ID, base_sex)

mcs_wave_2_selected <- mcs_wave_2 |> 
  rename(
         sdq_emot_p = DDEMOTION,
         sdq_cond_p = DDCONDUCT, 
         sdq_hyp_p = DDHYPER,
         sdq_peer_p = DDPEER,
         sdq_pro_p = DDPROSOC,
         sdq_tot_p = DDDEBDTOT, 
         sdq_imp_p = DDIMPACT, 
         age = DHCAGE00) |> 
  mutate(
         q_autism = DPAUTS00, ID = paste(MCSID, DCNUM00, sep = "_")) |> 
  mutate(age  = na_if(age, -1),
         age = age/365.25) 
  

  mcs_wave_2_selected <- mcs_wave_2_selected  |> 
    mutate(autism = case_when(DPAUTS00 == 1 | DPRASN0A == 4 | DPRASN0B == 4 | DPRASN0C == 4 | DPRASN0D == 4 | DPRASN0E == 4 | DPRASN0F == 4 | DPRASN0G == 4 | DPRASN0H == 4 | 
                                DPRASN0I == 4 | DPRASN0J == 4 | DPRASN0K == 4 | DPRASN0L == 4 | DPRASN0M == 4 | DPRASN0N == 4 | DPRASN0O == 4 | DPRASN0P == 4 | DPRASN0Q == 4 | 
                                DPRASX0A == 4 | DPRASX0B == 4 | DPRASX0C == 4 | DPRASX0D == 4 | DPRASX0E == 4 | DPRASX0F == 4 | DPRASX0G == 4 | DPRASX0H == 4 | DPRASX0I == 4 | 
                                DPRASX0J == 4 | DPRASX0K == 4 | DPRASX0L == 4 | DPRASX0M == 4 | DPRASX0N == 4 | DPRASX0O == 4 | DPRASX0P == 4 | DPRASX0Q == 4 | DPRASX0R == 4 | 
                                DPRASX0S == 4 | DPRASX0T == 4 | DPRASX0U == 4 | DPRASX0V == 4 | DPRASX0W == 4 | DPRASX0X == 4 | DPRASX0Y == 4 | DPRASX0Z == 4 | DPRASX1A == 4 | 
                                DPRASX1B == 4 | DPRASX1C == 4 | DPRASX1D == 4 | DPRSEN0A == 4 | DPRSEN0B == 4 | DPRSEN0C == 4 | DPRSEN0D == 4 | DPRSEN0E == 4 | DPRSEN0F == 4 | 
                                DPRSEN0G == 4 | DPRSEN0H == 4 | DPRSEN0I == 4 | DPRSEN0J == 4 | DPRSEN0K == 4 | DPRSEN0L == 4 | DPRSEN0M == 4 | DPRSNX0A == 4 | DPRSNX0B == 4 | 
                                DPRSNX0C == 4 | DPRSNX0D == 4 | DPRSNX0E == 4 | DPRSNX0F == 4 | DPRSNX0G == 4 | DPRSNX0H == 4 | DPRSNX0I == 4 | DPRSNX0J == 4 | DPRSNX0K == 4 | 
                                DPRSNX0L == 4 | DPRSNX0M == 4 | DPRSNX0N == 4 | DPRSNX0O == 4 | DPRSNX0P == 4 | DPRSNX0Q == 4 | DPRSNX0R == 4 | DPRSNX0S == 4 | DPRSNX0T == 4 | 
                                DPRSNX0U == 4 | DPRSNX0V == 4 | DPRSNX0W == 4 | DPRSNX0X == 4 | DPRSNX0Y == 4 | DPRSNX0Z == 4 | DPRSNX1A == 4 | DPRSNX1B == 4 | DPRSNX1C == 4 | 
                                DPRSNX1D == 4 ~ 1)) 
mcs_wave_3_selected <- mcs_wave_3 |> 
  rename(
         sdq_emot_p = EEMOTION,
         sdq_cond_p = ECONDUCT, 
         sdq_hyp_p = EHYPER,
         sdq_peer_p = EPEER,
         sdq_pro_p = EPROSOC,
         sdq_tot_p = EEBDTOT, 
         age = EMCS5AGE,
         sex = ECSEX0000,
         ) |> 
  mutate(q_autism = EPAUTS00, ID = paste(MCSID, ECNUM00, sep = "_")) |> 
  mutate(autism = case_when(EPAUTS00 == 1 | EPRASN0D == 1 | EPRASX0D == 1 | EPRSEN0D == 1 | EPRSEX0D == 1  ~ 1),
         base_hearing_impairment = case_when(EPCLSX0B == -1 ~ 0,
                                        EPCLSX0B == 0 ~ 0,
                                        EPCLSX0B ==1 ~ 1)) |> 
  mutate(base_visual_impairment = case_when(EPCLSX0A == -1 ~ 0,
                                        EPCLSX0A == 0 ~ 0,
                                        EPCLSX0A ==1 ~ 1)) |>
  mutate(epilepsy = case_when(EPCLSX1L == -1 ~ 0,
                   EPCLSX1L == 0 ~ 0,
                   EPCLSX1L ==1 ~ 1))  



mcs_wave_4_selected <- mcs_wave_4 |> 
  rename(
         sex = FHCSEX00,
         age = FCMCS6AG,
         sdq_emot_p = FEMOTION,
         sdq_cond_p = FCONDUCT, 
         sdq_hyp_p = FHYPER,
         sdq_peer_p = FPEER,
         sdq_pro_p = FPROSOC,
         sdq_tot_p = FEBDTOT
        ) |> 
  mutate( q_autism = FPAUTS00, ID = paste(MCSID, FCNUM00, sep = "_")) |> 
  mutate(autism = case_when(FPAUTS00 == 1 | FPRASN0D == 1 | FPRSEN0D == 1   ~ 1)) 

mcs_wave_5_selected <- mcs_wave_5 |> 
  rename(
         age = GCMCS7AG,
         sdq_emot_p = GEMOTION,
         sdq_cond_p = GCONDUCT, 
         sdq_hyp_p = GHYPER,
         sdq_peer_p = GPEER,
         sdq_pro_p = GPROSOC,
         sdq_tot_p = GEBDTOT,
         learning_disability = GCWHYHLP0F)  |> 
  mutate(ID = paste(MCSID, GCNUM00, sep = "_")) 

#stacking datasets
mcs_all <- bind_rows(mcs_wave_1_selected, mcs_wave_2_selected, mcs_wave_3_selected, mcs_wave_4_selected, mcs_wave_5_selected) |> 
  filter(!is.na(wave)) |> 
  mutate(age = case_when(age <0 ~ NA,
                         TRUE ~ age)) 



ages_data <- mcs_all |> 
  select(ID, age, wave) 

ages_data |> group_by(wave) |> sum_detail("age")

ages_data_wide <- ages_data |> 
  pivot_wider(names_from = wave, values_from = age, names_prefix = "age") |> 
  mutate(out_wave = 4,
         base_wave =3)


autism_data <- mcs_all |> select(ID, autism, wave) |> 
  pivot_wider(names_from = wave, values_from = autism, names_prefix = "autism") |> 
  left_join(ages_data_wide) |> 
  mutate(autism = case_when(autism1 ==1 | autism2 ==1 | autism3 ==1~ "childhood, parent report",
                            autism4 ==1 ~ "post baseline", # note no report of autism age 5
                            TRUE ~ NA)) |> 
  filter(!is.na(autism))




ages_data_wide |> right_join(autism_data) |>  count(base_wave)
autism_data |> count(autism)

mcs_autistic <- mcs_all |> 
  rename(wave_autism =autism) |> 
  left_join(autism_data, by = "ID") |> 
  filter(!is.na(autism))




predictors <- autism_data |> 
  select(ID, autism) |> 
  left_join(mcs_wave_3_selected |> select(ID, base_hearing_impairment, base_visual_impairment)) 


sdq_data <- 
  mcs_autistic |> select(ID, wave, age, sdq_emot_p, sdq_cond_p, sdq_hyp_p, sdq_peer_p, sdq_pro_p) |> 
  left_join(ages_data_wide |> select(ID, base_wave, out_wave)) |> 
  pivot_longer(cols = starts_with("sdq"), names_to = "sdq_item", values_to = "score") |>
  mutate(score = case_when(score < 0 ~ NA,
                           TRUE ~ score)) |> 
  pivot_wider(names_from = sdq_item, values_from = score) 

sex_data <- sdq_data |> filter(wave == base_wave) |>  left_join(mcs_wave_1_selected |> select(ID, MCSID,  sex1 = base_sex), by = "ID") |> 
  left_join(mcs_wave_4_selected |> select(ID, sex4 = sex), by = "ID") |> 
    left_join(mcs_wave_3_selected |> select(ID, sex3 = sex), by = "ID") |> 
  mutate(base_sex = case_when(!is.na(sex1) ~ sex1, 
                         !is.na(sex3) ~ sex3, 
                         !is.na(sex4) ~ sex4)) |> 
  select(ID, MCSID, base_sex) |> 
  mutate(base_sex = base_sex -1)


part_acc <- get_age_range_data_sdq(
  wave1_data = sdq_data |> filter(wave == base_wave) |> right_join(autism_data, by = "ID"), 
  wave2_data = sdq_data |> filter(wave == out_wave) |> right_join(autism_data, by = "ID"))

part_acc |> count(include)

mcs_data <-  sdq_data |> 
  left_join(predictors, by = "ID") |> 
  left_join(sex_data, by = "ID") |> 
  left_join(ethnicity_data, by = "ID") |> 
  left_join(family_level_predictors, by = "MCSID") |>
  select(-MCSID, -mother_id) |> 
  left_join(part_acc |> select(ID, include)) |> 
  left_join(ld_data, by = "ID") |>
  filter(include == "include") |> 
  mutate(study = "MCS", 
         country = "UK") |> 
  mutate(ID = as.character(ID))


check_values(mcs_data)



saveRDS(mcs_data, file = file.path(derived_data, "MCS.Rds"))

saveRDS(part_acc, file = file.path(derived_data, "MCS_acc.Rds"))