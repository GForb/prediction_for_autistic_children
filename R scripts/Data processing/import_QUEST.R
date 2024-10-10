data_folder <- here::here(raw_data, "Quest")

quest_w1_data <- haven::read_dta(here::here(data_folder, "QUEST1_masterfile.dta")) |> 
  mutate(
    ID = as.character(idnum)
  ) 

w1_predictors <- quest_w1_data |> 
  select(ID, 
         base_maternal_education = t1_edn_best_Alevels,
         base_imd_decile = t1_depriv_index,
         base_ld_w1 = t1_IQ_gp,
         base_iq_full_scale_w1 = t1_rIQ) # find out what index

quest_w2_data <- haven::read_dta(here::here(data_folder, "QUEST2_masterfile.dta")) |> 
  mutate(
    ID = as.character(idnum)
  ) |> filter(!is.na(subsample))

quest_w3_data <- haven::read_dta(here::here(data_folder, "QUEST3_masterfile.dta")) |> 
  mutate(
    ID = as.character(idnum)
  ) |> filter(!is.na(subsample))

t2_intensive_raw <- haven::read_dta(here::here(data_folder, "t2_intensive_raw_questionnaires.dta")) |> 
  mutate(ID = as.character(idnum))|> 
  select(ID, starts_with("k10"), FIMH_1, FIMH_2)
  
t2_extensive_raw <- haven::read_dta(here::here(data_folder, "t2_extensive_questionnaires_raw 05-03-17.dta")) |> 
  mutate(ID = as.character(idnum)) |> 
  filter(DEM1 != "" | DEM1_other != "") |> 
  select(ID, starts_with("k10"), , FIMH_1, FIMH_2)

t2_raw <- bind_rows(t2_intensive_raw, t2_extensive_raw) |> 
  mutate(base_maternal_mh = K101_b + K101_d + K101_e + K101_h + K101_i + K101_j, # calculating k6 usings items 2, 4, 5, 8, 9 and 10 only - see https://bmcpublichealth.biomedcentral.com/articles/10.1186/1471-2458-13-128

         base_visual_impairment = case_when(FIMH_2 == "Y" ~ 1,
                                            FIMH_2 == "N" ~ 0),
         base_hearing_impairment= case_when(FIMH_1 == "Y" ~ 1,
                                            FIMH_1 == "N" ~ 0)) 


t2_ados <- haven::read_dta(here::here(data_folder, "t2_ADOS_SSA_RRB.dta")) |> 
  mutate(ID = idnum |> as.character()) |> 
  select(ID, base_ados_css_rrb = t2_ados_RRB_CSS, base_ados_css_sa = t2_ados_SSA_CSS)

cn <- colnames(quest_w2_data)
predictors <- quest_w2_data |> 
  select(
    ID,
    base_sex = sex,
    base_iq_full_scale = t2_IQ,
    base_iq_standard = t2_IQ_test,
    base_ethnicity = t1_ethn_white,
) |> 
  left_join(w1_predictors) |> 
  mutate(base_sex = base_sex - 1,
         base_imd_decile = base_imd_decile/10,
         base_iq_standard = case_when(base_iq_standard == 1 ~ 1,
                                      base_iq_standard >1 ~ 0),
         base_maternal_education = base_maternal_education-1,
         base_ld = case_when(is.na(base_iq_full_scale) ~  1 - base_ld_w1,
                             !is.na(base_iq_full_scale) ~ if_else(base_iq_full_scale <70, 1,0))) |> 
  left_join(t2_ados, by = "ID") |> 
  left_join(t2_raw |> select(ID, starts_with("base")), by = "ID")


w0_sdq <- quest_w2_data |>  
  select(
    ID,
    age = t2_age_y,
    sdq_emot_p = t2_SDQ_pemot,
    sdq_cond_p = t2_SDQ_pconduct, 
    sdq_hyp_p = t2_SDQ_phyper,
    sdq_peer_p = t2_SDQ_ppeer,
    sdq_pro_p = t2_SDQ_pprosoc
  ) |> mutate(wave = 0)

w1_sdq <- quest_w3_data |>  
  select(
    ID,
    age = t3_age_yrs,
    sdq_emot_p = t3_SDQ_pemot,
    sdq_cond_p = t3_SDQ_pconduct, 
    sdq_hyp_p = t3_SDQ_phyper,
    sdq_peer_p = t3_SDQ_ppeer,
    sdq_pro_p = t3_SDQ_pprosoc
  ) |> mutate(wave = 1)

particpant_accounting <- get_age_range_data_sdq(wave1_data = w0_sdq, wave2_data = w1_sdq) |> 
  mutate(autism = "childhood, researcher") 

quest_data <- 
  bind_rows(w0_sdq, w1_sdq) |> 
  left_join(predictors) |> 
  left_join(particpant_accounting |> 
              select(ID, include)) |> 
  filter(include == "include") |> 
  mutate(study = "Quest",
        country = "UK",
        base_wave = 0, 
        out_wave = 1,
        autism = "childhood, researcher") 


particpant_accounting |> count(include)
check_values(quest_data)

saveRDS(quest_data, file = file.path(derived_data, "quest.Rds"))

saveRDS(particpant_accounting, file = file.path(derived_data, "quest_acc.Rds"))
