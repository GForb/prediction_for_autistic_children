

#importing data; wave 1 corresponds to sweep at age 5 ; we have sdq from age 3
mcs_wave_1_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_cm_derived.dta"))
mcs_wave_2_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_cm_derived.dta"))
mcs_wave_3_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_cm_derived.dta"))
mcs_wave_4_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_derived.dta"))
mcs_wave_5_sdq <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_derived.dta"))

mcs_wave_1_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_cm_cognitive_assessment.dta"))
mcs_wave_2_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_cm_cognitive_assessment.dta"))
mcs_wave_3_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_cm_cognitive_assessment.dta"))
mcs_wave_4_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_cognitive_assessment.dta"))
mcs_wave_5_age <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_cognitive_assessment.dta"))

mcs_wave_1_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_parent_cm_interview.dta"))
mcs_wave_2_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_parent_cm_interview.dta"))
mcs_wave_3_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_parent_cm_interview.dta"))
mcs_wave_4_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_parent_cm_interview.dta"))
mcs_wave_5_asd <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_parent_cm_interview.dta"))

mcs_wave_1_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_cm_interview.dta"))
mcs_wave_2_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_cm_interview.dta"))
mcs_wave_3_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_cm_interview.dta"))
mcs_wave_4_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_interview.dta"))
mcs_wave_5_sex <- haven::read_stata(file.path(raw_data, "MCS/Sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_interview.dta"))

#adding wave 
mcs_wave_1_age$wave <- 1
mcs_wave_1_asd$wave <- 1

mcs_wave_2_age$wave <- 2
mcs_wave_2_asd$wave <- 2

mcs_wave_3_age$wave <- 3
mcs_wave_3_asd$wave <- 3

mcs_wave_4_age$wave <- 4
mcs_wave_4_asd$wave <- 4

mcs_wave_5_age$wave <- 5
mcs_wave_5_asd$wave <- 5


#renaming variables
mcs_wave_1 <- dplyr::left_join(mcs_wave_1_age, mcs_wave_1_sdq)
mcs_wave_2 <- dplyr::left_join(mcs_wave_2_age, mcs_wave_2_sdq)
mcs_wave_3 <- dplyr::left_join(mcs_wave_3_age, mcs_wave_3_sdq)
mcs_wave_4 <- dplyr::left_join(mcs_wave_4_age, mcs_wave_4_sdq)
mcs_wave_5 <- dplyr::left_join(mcs_wave_5_age, mcs_wave_5_sdq)

#selecting primary caregiver as informant for asd status
mcs_wave_1_asd_status <- mcs_wave_1_asd |> 
  filter(CPNUM00 == 1) 
mcs_wave_2_asd_status <- mcs_wave_2_asd |> 
  filter(DPNUM00 == 1)
mcs_wave_3_asd_status <- mcs_wave_3_asd |> 
  filter(EPNUM00 == 1)
mcs_wave_4_asd_status <- mcs_wave_4_asd |> 
  filter(FPNUM00 == 1)
mcs_wave_5_asd_status <- mcs_wave_5_asd |> 
  filter(GPNUM00 == 1)

#joining asd data 
mcs_wave_1 <- dplyr::left_join(mcs_wave_1, mcs_wave_1_asd_status)
mcs_wave_2 <- dplyr::left_join(mcs_wave_2, mcs_wave_2_asd_status)
mcs_wave_3 <- dplyr::left_join(mcs_wave_3, mcs_wave_3_asd_status)
mcs_wave_4 <- dplyr::left_join(mcs_wave_4, mcs_wave_4_asd_status)
mcs_wave_5 <- dplyr::left_join(mcs_wave_5, mcs_wave_5_asd_status)

#joining sex data
mcs_wave_1 <- dplyr::left_join(mcs_wave_1, mcs_wave_1_sex)
mcs_wave_2 <- dplyr::left_join(mcs_wave_2, mcs_wave_2_sex)
mcs_wave_3 <- dplyr::left_join(mcs_wave_3, mcs_wave_3_sex)
mcs_wave_4 <- dplyr::left_join(mcs_wave_4, mcs_wave_4_sex)
mcs_wave_5 <- dplyr::left_join(mcs_wave_5, mcs_wave_5_sex)

# renaming variables 
#i have no clue what impact is but i thought it was better to have it 
mcs_wave_1_selected <- mcs_wave_1 |> 
  rename(ID = MCSID,
         sdq_emot_p = CEMOTION,
         sdq_cond_p = CCONDUCT, 
         sdq_hyp_p = CHYPER,
         sdq_peer_p = CPEER,
         sdq_pro_p = CPROSOC,
         sdq_tot_p = CEBDTOT, 
         sdq_imp_p = CIMPACT, 
         age = CCYAGE00,
         autism = CPAUTS00,
         sex = CHCSEX00)
mcs_wave_2_selected <- mcs_wave_2 |> 
  rename(ID = MCSID,
         sdq_emot_p = DDEMOTION,
         sdq_cond_p = DDCONDUCT, 
         sdq_hyp_p = DDHYPER,
         sdq_peer_p = DDPEER,
         sdq_pro_p = DDPROSOC,
         sdq_tot_p = DDDEBDTOT, 
         sdq_imp_p = DDIMPACT, 
         age = DCYAGE00,
         autism = DPAUTS00,
         sex = DCCSEX00)
mcs_wave_3_selected <- mcs_wave_3 |> 
  rename(ID = MCSID,
         sdq_emot_p = EEMOTION,
         sdq_cond_p = ECONDUCT, 
         sdq_hyp_p = EHYPER,
         sdq_peer_p = EPEER,
         sdq_pro_p = EPROSOC,
         sdq_tot_p = EEBDTOT, 
         #no age for this wave?!
         autism = EPAUTS00,
         #NO SEX EITHER?
         )
#after this wave they just stopped with the age and sex data ... 
mcs_wave_4_selected <- mcs_wave_4 |> 
  rename(ID = MCSID,
         sdq_emot_p = FEMOTION,
         sdq_cond_p = FCONDUCT, 
         sdq_hyp_p = FHYPER,
         sdq_peer_p = FPEER,
         sdq_pro_p = FPROSOC,
         sdq_tot_p = FEBDTOT, 
         autism = FPAUTS00)
mcs_wave_5_selected <- mcs_wave_5 |> 
  rename(ID = MCSID,
         sdq_emot_p = GEMOTION,
         sdq_cond_p = GCONDUCT, 
         sdq_hyp_p = GHYPER,
         sdq_peer_p = GPEER,
         sdq_pro_p = GPROSOC,
         sdq_tot_p = GEBDTOT) #they didnt collect asd data for wave 5 (sweep 7)

#stacking datasets
mcs_all <- bind_rows(mcs_wave_1_selected, mcs_wave_2_selected, mcs_wave_3_selected, mcs_wave_4_selected, mcs_wave_5_selected)

#selecting variables 
mcs_selected <- mcs_all |> 
  select(ID, sdq_emot_p, sdq_cond_p, sdq_hyp_p, sdq_peer_p, sdq_pro_p, sdq_tot_p, sdq_imp_p, age, autism, sex, wave)
# arranging variables
mcs_selected <- mcs_selected |> 
  arrange(ID, wave)



mcs_asd <- mcs_selected |> 
  filter(autism == 1)

ids_of_asd_pcpts <- unique(mcs_asd$ID)

mcs_asd <- mcs_selected |> 
  filter(ID %in% ids_of_asd_pcpts) |> 
  arrange(ID, wave)

check_values(mcs_asd, var_metadata)

save(mcs_asd, file = file.path(derived_data, "mcs.Rdata"))