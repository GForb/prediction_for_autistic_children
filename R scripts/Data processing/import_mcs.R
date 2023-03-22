

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
mcs_wave_1 <- dplyr::bind_cols(mcs_wave_1_age, mcs_wave_1_sdq)
mcs_wave_2 <- dplyr::bind_cols(mcs_wave_2_age, mcs_wave_2_sdq)
mcs_wave_3 <- dplyr::bind_cols(mcs_wave_3_age, mcs_wave_3_sdq)
mcs_wave_4 <- dplyr::bind_cols(mcs_wave_4_age, mcs_wave_4_sdq)
mcs_wave_5 <- dplyr::bind_cols(mcs_wave_5_age, mcs_wave_5_sdq)

#selecting variables