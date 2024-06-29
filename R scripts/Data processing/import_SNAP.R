data_folder <- here::here(raw_data, "SNAP")

age16_data <- readxl::read_xlsx(here::here(data_folder, "age_SNAP16.xlsx")) |> 
  mutate(ID = as.character(idnum)) |> 
  select(ID, age = age_at_16)

iq_data <- haven::read_dta(here::here(data_folder, "pa_snap.dta")) |> 
  mutate(ID = as.character(idnum),
         base_iq_standard = case_when(rfsiq == wisc12_fsiq ~ 1,
                                      is.na(rfsiq) ~ NA,
                                      TRUE ~ 0)) |>
  select(ID, base_iq_full_scale = rfsiq, base_iq_standard, asdpopsample) 

raw_predictor_data <- haven::read_dta(here::here(data_folder, "predictors_12yo.dta"))

predictors1 <- haven::read_dta(here::here(data_folder, "predictors_12yo.dta")) |> 
  mutate(ID = as.character(idnum),
         base_sex = gender-1,
         base_imd_decile = imd_score/10,
         base_vabs_abc_ss = vineland_abc_ss) |> 
  left_join(iq_data) 

initial_interview <- haven::read_dta(here::here(data_folder, "initial_interview_18apr06.dta")) |> 
  mutate(ID = as.character(idnum)) |>  
  select(ID, initint_vision, initint_hearing) 

scq <- haven::read_dta(here::here(data_folder, "scq_18apr06.dta")) |> 
  mutate(ID = as.character(idnum)) |> 
  select(ID, scq_vision, scq_hearing) 

scq |> count(scq_vision)
scq |> count(scq_hearing)

initial_interview |> count(initint_vision)
initial_interview |> count(initint_hearing)


data <- haven::read_dta(here::here(data_folder, "SNAP_12_16_23_forGordon.dta")) |> 
  mutate(ID = as.character(idnum)) |> 
  left_join(iq_data) |> 
  filter(asdpopsample == 1) 
  

hearing_vision <- data |> 
  select(ID) |> 
  left_join(scq) |> 
  left_join(initial_interview) |> 
  mutate(base_hearing_impairment = case_when(initint_hearing == 2 ~ 1, 
                                             initint_hearing %in% c(0, 1)~ 0),
         base_visual_impairment = case_when(initint_vision == 2 ~ 1, 
                                            initint_vision %in% c(0, 1)~ 0)) 

hearing_vision |> count(initint_vision, scq_vision)

hearing_vision |> count(initint_hearing, scq_hearing)


hearing_vision |> count(initint_hearing)
hearing_vision |> count(scq_hearing)
hearing_vision |> count(initint_vision)
hearing_vision |> count(scq_vision)

predictors2 <- data |>
  select(
    ID,
    ados_social_12,
    ados_rep_12,
    ados_module = ados_mod_12,
    base_ethnicity = ethnicity_12,
    base_maternal_education = edn_best_12)  |> 
  mutate(
    base_ethnicity = case_when(base_ethnicity == 1 ~1, 
                               base_ethnicity > 1 ~ 0),
    base_maternal_education = case_when(base_maternal_education %in% c(2, 4) ~ 1, 
                                          base_maternal_education %in% c(0,1,3,5) ~ 0),
    base_ados_css_sa = calculate_css_sa(sa_raw_score = ados_social_12, module = ados_module),
    base_ados_css_rrb = calculate_css_rrb(rrb_raw_score = ados_rep_12, module = ados_module)
    ) |> 
    select(-starts_with("ados")) |> 
  left_join(hearing_vision |> select(ID, starts_with("base")))


predictors <- predictors1 |> left_join(predictors2)  |> 
  select(ID, starts_with("base"))

w0_sdq <- data |>  
  select(
    ID,
    sdq_emot_p = SDQ_pemotdis_12,
    sdq_cond_p = SDQ_pcondis_12, 
    sdq_hyp_p = SDQ_phyper_12  ,
    sdq_peer_p = SDQ_ppeerrel_12 ,
    sdq_pro_p = SDQ_pprosoc_12
  ) |> 
  mutate(wave = 0) |> 
  mutate(across(starts_with("sdq"), ~case_when(. == 999 ~ NA, TRUE ~ .))) |> 
  right_join(predictors1 |> select(ID, age = age12)) 


w1_sdq <- data |>  
  select(
    ID,
    sdq_emot_p = SDQ_pemotdis_16,
    sdq_cond_p = SDQ_pcondis_16, 
    sdq_hyp_p = SDQ_phyper_16,
    sdq_peer_p = SDQ_ppeerrel_16 ,
    sdq_pro_p = SDQ_pprosoc_16 
  ) |> 
  mutate(wave = 1) |> 
  mutate(across(starts_with("sdq"), ~case_when(. == 999 ~ NA, TRUE ~ .))) |> 
  filter(!is.na(sdq_emot_p) | !is.na(sdq_cond_p) | !is.na(sdq_hyp_p) |!is.na(sdq_peer_p) |!is.na(sdq_pro_p)) |> 
  right_join(age16_data)




particpant_accounting <- get_age_range_data_sdq(wave1_data = w0_sdq, wave2_data = w1_sdq) |> 
  mutate(autism = "childhood, researcher") 
particpant_accounting |> count(include)

snap_data <- 
  bind_rows(w0_sdq, w1_sdq) |> 
  right_join(predictors) |> 
  left_join(particpant_accounting |> 
              select(ID, include)) |> 
  filter(include == "include") |> 
  mutate(study = "SNAP",
         country = "UK",
         base_wave = 0, 
         out_wave = 1,
         autism = "childhood, researcher") 



check_values(snap_data)

saveRDS(snap_data, file = file.path(derived_data, "SNAP.Rds"))

saveRDS(particpant_accounting, file = file.path(derived_data, "SNAP_acc.Rds"))
