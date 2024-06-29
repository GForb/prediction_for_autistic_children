
gui_wave_1_raw <- haven::read_sas(file.path(raw_data, "GUI/0020-01 GUI Child Cohort Wave 1/0020-01 GUI Child Cohort Wave 1_Data/9 Year Cohort Data/SAS/GUI Data_9YearCohort.sas7bdat"))
gui_wave_2_raw <- haven::read_sas(file.path(raw_data, "GUI/0020-02 GUI Child Cohort Wave 2/0020-02 GUI Child Cohort Wave 2_Data/13 year cohort data/SAS/GUI Data_ChildCohortWave2.sas7bdat"))
gui_wave_3 <- haven::read_sas(file.path(raw_data, "GUI/0020-03 GUI Child Cohort Wave 3 revised/0020-03 GUI Child Cohort Wave 3_Data revised/SAS/0020-03_GUI_data_childcohortwave3_v1.3.sas7bdat"))
gui_wave_4 <- haven::read_sas(file.path(raw_data, "GUI/0020-04 GUI Child Cohort Wave 4/0020-04 GUI Child Cohort Wave 4_Data/SAS/0020-04_GUI_Data_ChildCohortWave4.sas7bdat"))

gui_wave_2_raw <- gui_wave_2_raw |> 
  rename(ID = id)

# -------------------------------------------------------------------------


# Defining wave variable
gui_wave_1_raw$wave <- 1
gui_wave_2_raw$wave <- 2
gui_wave_3$wave <- 3
gui_wave_4$wave <- 4 # no sdq



# Processing autism variables ----

# autism_status = 1: Autism reported & not diagnosed by a medical professional
# Autism_status = 2: Autism reported diagnosed by a medical professional
# Require both for someone to be autistic

gui_wave_1_raw <- gui_wave_1_raw |> 
  mutate(autism = case_when(MMJ22c ==1 & MMJ23 ==1 ~ 1,
                                   TRUE ~ 0))


gui_wave_2_raw <-  gui_wave_2_raw |> 
  mutate(autism = case_when(pc2e15d ==1 & pc2e16d ==1 ~ 1,
                                   TRUE ~ 0)
         )

gui_wave_3 <- gui_wave_3 |> 
  mutate(autism = case_when(pc3d3d ==1 ~ 1))


# Renaming Variables ----

## SDQs ----

gui_wave_1 <- gui_wave_1_raw |> 
  rename(sex = mma5ap2, # 1 = male, 2 = female 
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
         sdq_tot_t = TCSDQtot,
         household_income = Equivinc,
         subjective_poverty = MML2,
         ) 

gui_wave_2 <- gui_wave_2_raw |> 
  rename(sex = p2sexw2,
         age = p2agew2,
         sdq_emot_p = w2pcd2_sdqemot, 
         sdq_cond_p = w2pcd2_sdqcond, 
         sdq_hyp_p = w2pcd2_sdqhyper, 
         sdq_peer_p = w2pcd2_sdqpeer, 
         sdq_pro_p = w2pcd2_sdqpro, 
         sdq_tot_p = w2pcd2_sdqtot) 

gui_wave_3 <- gui_wave_3 |> 
  rename(sex = p2sexW3, 
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
         sdq_tot_p2 = w3scg_SDQtotaldiffs) |> 
  mutate(age = case_when(age == 1 ~ 17, age == 2 ~ 18)) 

gui_all <- bind_rows(gui_wave_1, gui_wave_2, gui_wave_3)


# Selecting variables ----


ages_data <- gui_all |> 
  select(ID, age, wave) 

ages_data |> group_by(wave) |> sum_detail("age")

ages_data_wide <- ages_data |> 
  pivot_wider(names_from = wave, values_from = age, names_prefix = "age") |> 
  mutate(out_wave = 3,
         base_wave =2)


autism_data <- gui_all |> select(ID, autism, wave) |> 
  pivot_wider(names_from = wave, values_from = autism, names_prefix = "autism") |> 
  left_join(ages_data_wide) |> 
  mutate(autism = case_when(autism1 ==1 | autism2 ==1 ~ "childhood, parent report",
                            autism3 ==1 & !(autism1 ==1 | autism2 ==1 ) ~ "post baseline",
                            TRUE ~ NA)) |> 
  filter(!is.na(autism))

autism_data |> count(autism)

ages_data_wide |> right_join(autism_data) |>  count(base_wave)
autism_data |> count(autism)

gui_autistic <- gui_all |> 
  rename(wave_autism =autism) |> 
  left_join(autism_data, by = "ID") |> 
  filter(!is.na(autism))

wave2_predictors <- gui_wave_2 |> 
  mutate(base_ld = case_when(pc2e15c ==1 ~ 1,
                             TRUE ~ 0),
         base_maternal_education = case_when( pc2h1 == 5 |  pc2h1 ==6 ~ 1,
                                              TRUE ~ 0),
         base_subjective_poverty = pc2g36) |> 
  select(ID, starts_with("base"))
  


predictors <- autism_data |> 
  select(ID, autism) |> 
  left_join(gui_wave_1 |> select(ID, 
                                 base_sex = sex)) |> 
  mutate(base_sex = base_sex - 1) |> 
  left_join(wave2_predictors)
       


sdq_data <- 
  gui_autistic |> select(ID, wave, age, sdq_emot_p, sdq_cond_p, sdq_hyp_p, sdq_peer_p, sdq_pro_p) |> 
  left_join(ages_data_wide |> select(ID, base_wave, out_wave)) |> 
  pivot_longer(cols = starts_with("sdq"), names_to = "sdq_item", values_to = "score") |>
  mutate(score = case_when(score < 0 ~ NA,
                           TRUE ~ score)) |> 
  pivot_wider(names_from = sdq_item, values_from = score) 

part_acc <- get_age_range_data_sdq(
  wave1_data = sdq_data |> filter(wave == base_wave) |> right_join(autism_data, by = "ID"), 
  wave2_data = sdq_data |> filter(wave == out_wave) |> right_join(autism_data, by = "ID"))

part_acc |> count(include)

gui_data <-  sdq_data |> 
  left_join(predictors, by = "ID") |> 
  left_join(part_acc |> select(ID, include)) |> 
  filter(include == "include") |> 
  mutate(study = "GUI", 
         country = "UK") |> 
  mutate(ID = as.character(ID))


check_values(gui_data)



saveRDS(gui_data, file = file.path(derived_data, "GUI.Rds"))

saveRDS(part_acc, file = file.path(derived_data, "GUI_acc.Rds"))
