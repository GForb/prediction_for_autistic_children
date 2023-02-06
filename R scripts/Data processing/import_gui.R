library(tidyverse)
library(here)

gui_wave_1 <- haven::read_sas(file.path(raw_data, "GUI/0020-01 GUI Child Cohort Wave 1/0020-01 GUI Child Cohort Wave 1_Data/9 Year Cohort Data/SAS/GUI Data_9YearCohort.sas7bdat"))
gui_wave_2 <- haven::read_sas(file.path(raw_data, "GUI/0020-02 GUI Child Cohort Wave 2/0020-02 GUI Child Cohort Wave 2_Data/13 year cohort data/SAS/GUI Data_ChildCohortWave2.sas7bdat"))
gui_wave_3 <- haven::read_sas(file.path(raw_data, "GUI/0020-03 GUI Child Cohort Wave 3 revised/0020-03 GUI Child Cohort Wave 3_Data revised/SAS/0020-03_GUI_data_childcohortwave3_v1.3.sas7bdat"))
gui_wave_4 <- haven::read_sas(file.path(raw_data, "GUI/0020-04 GUI Child Cohort Wave 4/0020-04 GUI Child Cohort Wave 4_Data/SAS/0020-04_GUI_Data_ChildCohortWave4.sas7bdat"))

gui_wave_2 <- gui_wave_2 |> 
  rename(ID = id)

# Defining wave variable
gui_wave_1$wave <- 1
gui_wave_2$wave <- 2
gui_wave_3$wave <- 3
gui_wave_4$wave <- 4 # no sdq

# Processing autism variables ----

# autism_status = 1: Autism reported & not diagnosed by a medical professional
# Autism_status = 2: Autism reported diagnosed by a medical professional

gui_wave_1 <- gui_wave_1 |> 
  mutate(autism_status = case_when(MMJ22c ==1 & MMJ23 ==1 ~ 2,
                                   MMJ22c == 1 ~ 1))


gui_wave_2 <-  gui_wave_2 |> 
  mutate(autism_status = case_when(pc2e16d ==1 ~ 2,
                                   pc2e15d == 1 ~ 1))

gui_wave_3 <- gui_wave_3 |> 
  mutate(autism_status = case_when(pc3d3d ==1 ~ 1))


# Renaming Variables ----

## SDQs ----

gui_wave_1 <- gui_wave_1 |> 
  rename(gender = mma5ap2, # 1 = male, 2 = female 
         age = MMagep2, 
         sdq_emot_p = MMH2_SDQemot, 
         sdq_cond_p = MMH2_SDQcond, 
         sdq_hyp_p = MMH2_SDQhyper, 
         sdq_peer_p = MMH2_SDQpeer, 
         sdq_pro_p = MMH2_SDQpro,
         sdq_tot_p = MMH2_SDQtot,
         sdq_emot_t = TCSDQemot, 
         sdq_cond_t = TCSDQcon, 
         sdq_hyp_t = TCSDQhyp, 
         sdq_peer_t = TCSDQpeer,
         sdq_pro_t = TCSDQpro, 
         sdq_tot_t = TCSDQtot)

gui_wave_2 <- gui_wave_2 |> 
  rename(gender = p2sexw2,
         age = p2agew2,
         sdq_emot_p = w2pcd2_sdqemot, 
         sdq_cond_p = w2pcd2_sdqcond, 
         sdq_hyp_p = w2pcd2_sdqhyper, 
         sdq_peer_p = w2pcd2_sdqpeer, 
         sdq_pro_p = w2pcd2_sdqpro, 
         sdq_tot_p = w2pcd2_sdqtot)

gui_wave_3 <- gui_wave_3 |> 
  rename(gender = p2sexW3, 
         age = p2ageW3, #1 = 16/17, 2 = 18
         sdq_emot_p = w3pcg_SDQemotional, 
         sdq_cond_p = w3pcg_SDQconduct, 
         sdq_hyp_p = w3pcg_SDQhyper,
         sdq_peer_p = w3pcg_SDQpeerprobs, 
         sdq_pro_p = w3pcg_SDQprosocial, 
         sdq_tot_p = w3pcg_SDQtotaldiffs,
         sdq_emot_p2 = w3scg_SDQemotional, 
         sdq_cond_p2 = w3scg_SDQconduct, 
         sdq_hyp_p2 = w3scg_SDQhyper,
         sdq_peer_p2 = w3scg_SDQpeerprobs, 
         sdq_pro_p2 = w3scg_SDQprosocial, 
         sdq_tot_p2 = w3scg_SDQtotaldiffs)



gui_all <- bind_rows(gui_wave_1, gui_wave_2, gui_wave_3)


# Selecting variables ----

gui_selected <- gui_all |> select(ID, wave, gender, age, autism_status, sdq_emot_p, sdq_cond_p, sdq_hyp_p, sdq_peer_p, sdq_pro_p, sdq_tot_p, sdq_emot_t, sdq_cond_t, sdq_hyp_t, sdq_peer_p, sdq_pro_t, sdq_tot_t, sdq_emot_p2, sdq_cond_p2, sdq_hyp_p2, sdq_peer_p2, sdq_pro_p2, sdq_tot_p2) # add sdqs

sorted_data <- gui_selected[order(gui_selected$ID, gui_selected$wave),]

# Create list of autism IDs ----
gui_autistic <- gui_all |> filter(autism_status ==1 |autism_status ==2) 
id_of_asd_pcpts <- gui_autistic[!duplicated(gui_autistic$ID), ]

id_of_asd_pcpts <- id_of_asd_pcpts[['ID']]

# Selecting autistic participants ----
clean_data <- sorted_data |> 
  filter(ID %in% id_of_asd_pcpts)

# Adding study level variables ----
clean_data <- clean_data |> mutate(study = "gui",
                                   country = "Ireland")


clean_data |> select(wave, autism_status) |>  table(useNA = "always")

print(clean_data)
length(unique(clean_data$ID))

clean_data |> select(wave) |> table()

write.csv(clean_data, file = file.path(derived_data, "GUI/gui_clean.csv"))
