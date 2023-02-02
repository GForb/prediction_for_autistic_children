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
  rename(sdq_emot_p = MMH2_SDQemot, 
         sdq_p_c = MMH2_SDQcond, 
         sdq_p_h = MMH2_SDQhyper, 
         sdq_peer_p = MMH2_SDQpeer, 
         sdq_peer_pro = MMH2_SDQpro,
         sdq_p = MMH2_SDQtot,
         sdq_emot_t = TCSDQemot, 
         sdq_t_c = TCSDQcon, sdq_t_h = TCSDQhyp, 
         sdq_t_p = TCSDQpeer,
         sdq_pro_t = TCSDQpro, 
         sdq_t = TCSDQtot)

gui_wave_2 <- gui_wave_2 |> 
  rename(sdq_emot_p = w2pcd2_sdqemot, 
         sdq_p_c = w2pcd2_sdqcond, 
         sdq_p_h = w2pcd2_sdqhyper, 
         sdq_peer_p = w2pcd2_sdqpeer, 
         sdq_peer_pro = w2pcd2_sdqpro, 
         sdq_p = w2pcd2_sdqtot)

gui_wave_3 <- gui_wave_3 |> 
  rename(sdq_emot_p = w3pcg_SDQemotional, 
         sdq_p_c = w3pcg_SDQconduct, 
         sdq_p_h = w3pcg_SDQhyper,
         sdq_peer_p = w3pcg_SDQpeerprobs, 
         sdq_peer_pro = w3pcg_SDQprosocial, 
         sdq_p = w3pcg_SDQtotaldiffs,
         sdq_p2_e = w3scg_SDQemotional, 
         sdq_p2_c = w3scg_SDQconduct, 
         sdq_p2_h = w3scg_SDQhyper,
         sdq_p2_p = w3scg_SDQpeerprobs, 
         sdq_p2_pro = w3scg_SDQprosocial, 
         sdq_p2 = w3scg_SDQtotaldiffs)



gui_all <- bind_rows(gui_wave_1, gui_wave_2, gui_wave_3)


# Selecting variables ----

gui_selected <- gui_all |> select(ID, wave, autism_status, ) # add sdqs

gui_selected <- gui_all

# Selecting SDQs ---- can delete this

sdqs_wave_1 <- gui_wave_1 |> 
  select(ID, wave, MMH2_SDQemot, MMH2_SDQcond, MMH2_SDQhyper, MMH2_SDQpeer, MMH2_SDQpro, MMH2_SDQtot, TCSDQemot, TCSDQcon, TCSDQhyp, TCSDQpeer, TCSDQpro, TCSDQtot)

sdqs_wave_2 <- gui_wave_2 |> 
  select(ID, wave, w2pcd2_sdqemot, w2pcd2_sdqcond, w2pcd2_sdqhyper, w2pcd2_sdqpeer, w2pcd2_sdqpro, w2pcd2_sdqtot)

sdqs_wave_3 <- gui_wave_3 |> 
  select(ID, wave, w3pcg_SDQemotional, w3pcg_SDQconduct, w3pcg_SDQhyper, w3pcg_SDQpeerprobs, w3pcg_SDQprosocial, w3pcg_SDQtotaldiffs, w3scg_SDQemotional, w3scg_SDQconduct, w3scg_SDQhyper, w3scg_SDQpeerprobs, w3scg_SDQprosocial, w3scg_SDQtotaldiffs)





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

