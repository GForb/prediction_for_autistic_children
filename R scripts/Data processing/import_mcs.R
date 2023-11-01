

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
#i have no clue what impact is but i thought it was better to have it 
mcs_wave_1_selected <- mcs_wave_1 |> 
  rename(
         sdq_emot_p = CEMOTION,
         sdq_cond_p = CCONDUCT, 
         sdq_hyp_p = CHYPER,
         sdq_peer_p = CPEER,
         sdq_pro_p = CPROSOC,
         sdq_tot_p = CEBDTOT, 
         sdq_imp_p = CIMPACT, 
         age = CHCAGE00,
         sex = CHCSEX00
         ) |>
  mutate(q_autism = CPAUTS00, 
         ID = paste(MCSID, CCNUM00, sep = "_")) |> 
  mutate(age  = na_if(age, -1),
         age = age/365.25) 

mcs_wave_1_selected <- mcs_wave_1_selected |> 
  mutate(autism = case_when(CPAUTS00 == 1 ~ 1)) |> 
  select_analysis_variables()

wave1_sex <- mcs_wave_1_selected |> select(ID, sex)

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
         age = age/365.25) |> 
  left_join(wave1_sex)
  

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
                                DPRSNX1D == 4 ~ 1)) |> 
  select_analysis_variables()

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
  mutate(autism = case_when(EPAUTS00 == 1 | EPRASN0D == 1 | EPRASX0D == 1 | EPRSEN0D == 1 | EPRSEX0D == 1  ~ 1)) |> 
  select_analysis_variables()


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
  mutate(autism = case_when(FPAUTS00 == 1 | FPRASN0D == 1 | FPRSEN0D == 1   ~ 1)) |> 
  select_analysis_variables()

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
  mutate(ID = paste(MCSID, GCNUM00, sep = "_")) |> 
  left_join(wave1_sex) |> 
  select_analysis_variables()  #they didnt collect asd data for wave 5 (sweep 7)

#stacking datasets
mcs_all <- bind_rows(mcs_wave_1_selected, mcs_wave_2_selected, mcs_wave_3_selected, mcs_wave_4_selected, mcs_wave_5_selected) |> 
  filter(!is.na(wave))


mcs_data <- mcs_all |> 
  mutate(autism = case_when(autism == 1 ~ 1,
         autism == 2 ~ 0,
         TRUE ~ NA)) |> 
  mutate(across(
    c(age, sex, sdq_cond_p, sdq_peer_p, sdq_pro_p,  sdq_hyp_p,  sdq_emot_p),  
    ~na_if(., -1))) |> 
  add_autistic_any_wave() |> 
  arrange(ID, wave) |> 
  mutate(ID = as.character(ID)) |> 
  mutate(study = "mcs",
         country = "UK")

# Checking whether autism is monotonic
mcs_data |> select(ID, wave, autism) |> 
  pivot_wider(names_from = wave, values_from = autism)


add_autistic_q_any_wave <- function(data) {
  autistic_id <- data |> filter(q_autism ==1) |> select(ID) |>  unique() |> tibble() |> mutate(autistic_q_any_wave = 1)
  data |> left_join(autistic_id, by = "ID")
}
mcs_data <- mcs_data |> add_autistic_q_any_wave()

mcs_data %>%
  check_single_obs_per_wave()

# counting autistic people by wave
counts <- mcs_data |>  group_by(wave) |> summarise(n = n(), 
                                                   age = mean(age, na.rm = TRUE), 
                                                   n_autistic = sum(autism, na.rm = TRUE), 
                                                   q_autism = sum(q_autism ==1, na.rm = TRUE), 
                                                   n_autistic_any_wave = sum(autistic_any_wave, na.rm = TRUE),
                                                   n_autistic_q_any_wave = sum(autistic_any_wave, na.rm = TRUE))
counts

wave1_sex <- mcs_data |> filter(wave ==1) |> select(ID, sex)

cw_check <- counts$n_autistic == c(133,225,351,390,0) 
if(!all(cw_check)){
  stop("numbers of autistic people do not match Catline Williams's work")
}
   

check_values(mcs_data, var_metadata)

save(mcs_data, file = file.path(derived_data, "mcs.Rdata"))


table(mcs_data$wave , mcs_data$sex, useNA = "always")