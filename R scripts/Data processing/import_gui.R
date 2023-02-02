library(tidyverse)
library(here)

gui_wave_1 <- haven::read_sas(file.path(raw_data, "GUI/0020-01 GUI Child Cohort Wave 1/0020-01 GUI Child Cohort Wave 1_Data/9 Year Cohort Data/SAS/GUI Data_9YearCohort.sas7bdat"))
gui_wave_2 <- haven::read_sas(file.path(raw_data, "GUI/0020-02 GUI Child Cohort Wave 2/0020-02 GUI Child Cohort Wave 2_Data/13 year cohort data/SAS/GUI Data_ChildCohortWave2.sas7bdat"))
gui_wave_3 <- haven::read_sas(file.path(raw_data, "GUI/0020-03 GUI Child Cohort Wave 3 revised/0020-03 GUI Child Cohort Wave 3_Data revised/SAS/0020-03_GUI_data_childcohortwave3_v1.3.sas7bdat"))
gui_wave_4 <- haven::read_sas(file.path(raw_data, "GUI/0020-04 GUI Child Cohort Wave 4/0020-04 GUI Child Cohort Wave 4_Data/SAS/0020-04_GUI_Data_ChildCohortWave4.sas7bdat"))

autism_status1 <- gui_wave_1 |> 
  select(ID, MMJ22c)

autism_status2 <- gui_wave_2 |> 
  rename(ID = id) |> 
  select(ID, pc2e16d) |> 
  rename(MMJ22c = pc2e16d)

autism_status3 <- gui_wave_3 |> 
  select(ID, pc3d3d) |> 
  rename(MMJ22c = pc3d3d)

autism_status <- bind_rows(autism_status1, autism_status2, autism_status3)

autism_status <- autism_status[order(autism_status$ID),]

autism_status <- autism_status |> 
  filter(MMJ22c == 1) |> 
  select(ID)

id_of_asd_pcpts <- autism_status[!duplicated(autism_status$ID), ]

id_of_asd_pcpts <- id_of_asd_pcpts[['ID']]

gui_wave_2 <- gui_wave_2 |> 
  rename(ID = id)

sdqs_wave_1 <- gui_wave_1 |> 
  select(ID, MMH2_SDQemot, MMH2_SDQcond, MMH2_SDQhyper, MMH2_SDQpeer, MMH2_SDQpro, MMH2_SDQtot, TCSDQemot, TCSDQcon, TCSDQhyp, TCSDQpeer, TCSDQpro, TCSDQtot)

sdqs_wave_2 <- gui_wave_2 |> 
  select(ID, w2pcd2_sdqemot, w2pcd2_sdqcond, w2pcd2_sdqhyper, w2pcd2_sdqpeer, w2pcd2_sdqpro, w2pcd2_sdqtot)

sdqs_wave_3 <- gui_wave_3 |> 
  select(ID, w3pcg_SDQemotional, w3pcg_SDQconduct, w3pcg_SDQhyper, w3pcg_SDQpeerprobs, w3pcg_SDQprosocial, w3pcg_SDQtotaldiffs, w3scg_SDQemotional, w3scg_SDQconduct, w3scg_SDQhyper, w3scg_SDQpeerprobs, w3scg_SDQprosocial, w3scg_SDQtotaldiffs)

sdqs_wave_1 <- sdqs_wave_1 |> 
  rename(sdq_p_e = MMH2_SDQemot, sdq_p_c = MMH2_SDQcond, sdq_p_h = MMH2_SDQhyper, 
         sdq_p_p = MMH2_SDQpeer, sdq_p_pro = MMH2_SDQpro, sdq_p = MMH2_SDQtot,
         sdq_t_e = TCSDQemot, sdq_t_c = TCSDQcon, sdq_t_h = TCSDQhyp, sdq_t_p = TCSDQpeer,
         sdq_t_pro = TCSDQpro, sdq_t = TCSDQtot)

sdqs_wave_2 <- sdqs_wave_2 |> 
  rename(sdq_p_e = w2pcd2_sdqemot, sdq_p_c = w2pcd2_sdqcond, sdq_p_h = w2pcd2_sdqhyper, sdq_p_p
         = w2pcd2_sdqpeer, sdq_p_pro = w2pcd2_sdqpro, sdq_p = w2pcd2_sdqtot)

sdqs_wave_3 <- sdqs_wave_3 |> 
  rename(sdq_p_e = w3pcg_SDQemotional, sdq_p_c = w3pcg_SDQconduct, sdq_p_h = w3pcg_SDQhyper,
         sdq_p_p = w3pcg_SDQpeerprobs, sdq_p_pro = w3pcg_SDQprosocial, sdq_p = w3pcg_SDQtotaldiffs,
         sdq_p2_e = w3scg_SDQemotional, sdq_p2_c = w3scg_SDQconduct, sdq_p2_h = w3scg_SDQhyper,
         sdq_p2_p = w3scg_SDQpeerprobs, sdq_p2_pro = w3scg_SDQprosocial, sdq_p2 = w3scg_SDQtotaldiffs)

sdqs_wave_1$wave <- 1

sdqs_wave_2$wave <- 2

sdqs_wave_3$wave <- 3

sdqs_all <- bind_rows(sdqs_wave_1, sdqs_wave_2, sdqs_wave_3)

sorted_data <- sdqs_all[order(sdqs_all$ID, sdqs_all$wave),]

clean_data <- sorted_data |> 
  filter(ID %in% id_of_asd_pcpts)

clean_data$study <- "gui"

print(clean_data)
length(unique(clean_data$ID))
