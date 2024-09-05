# Aim: Create file in long format with one row per participant and wave

# Predictors:
# visual/hearing = 0 for everyone
# Sex
# FSIQ
# ADOS rrb/sa got
# VAVS ABC
# ADI 65 missing.
# Ethnicity
# Parental Education

study_name <- "TRAILS"
country_name <- "Netherlands"



data_folder <- here::here(raw_data, "TRAILS")
data_raw <- haven::read_sav(here::here(data_folder, "Hartman C 170624 - IPD-MA.sav")) 


cn <- colnames(data_raw)

data_lables <- get_var_labels(data_raw)

predictors <- data_raw |> 
  select(base_sex = g1sex)

cbcl_base <- data_raw |> select(
  age = g2ageyrc
)






# IRP - perceptive reasoning
# IRF fluid reasoning

# Determining base wave

data <- data_raw |> 
  mutate(age_vabs1 = as.numeric(agevinelandV0),
         age_cbcl1 = as.numeric(age_cbclV0),
         age_vabs2 = as.numeric(agevinelandV2),
         age_cbcl2 = as.numeric(age_cbclV2),
         age_vabs3 = as.numeric(agevinelandV4),
         age_cbcl3 = as.numeric(age_cbclV4),
         fu_vabs1 = age_vabs2 - age_vabs1,
         fu_vabs2 = age_vabs3 - age_vabs2,
         base_wave_vabs = case_when(age_vabs3 < vabs_check_out_age("max") & age_vabs2 < vabs_check_base_age("max")   ~ 2,  # restricts max age at outcome and baseline to those used in selecting participants.
                                    TRUE ~ 1,
                                    ),
         base_wave_cbcl = case_when(age_cbcl3 < cbcl_check_out_age("max") & age_cbcl3 < cbcl_check_base_age("max")   ~ 2,  # restricts max age at outcome and baseline to those used in selecting participants.
         TRUE ~ 1),
         method_bestQD = 1,
         bestQDV0 = as.numeric(bestQDV0),
         bestQDV2 = as.numeric(bestQDV2),
         base_sex = ifelse(data_raw$sex == "Girl", 1, 0),
         base_maternal_education = case_when(niveau_etude_mere == "3_Graduated school" ~ 1,
                                       niveau_etude_mere == "2_High school" ~ 0,
                                       niveau_etude_mere == "1_Middle school" ~ 0,
                                       is.na(niveau_etude_mere) ~ NA),
         base_iq_standard1 = case_when(calcul_bestQDV0 == "2_wiscirpv0" ~ 1,
                                      TRUE ~ 0),
         base_iq_standard2 = case_when(calcul_bestQDV2 == "2_wiscirpv2" ~ 1,
                                      TRUE ~ 0),
  )
data |> count(base_wave_vabs)
data |> count(base_wave_cbcl)

cn <- colnames(data)





# Predictors

# Hearing and visual impariment
# Review of the below, with help from chatGPT for translate shows no uncorrected visual or hearing impairment.
# data_raw |> count(cecite)
# data_raw |> count(trouble_audition)
# data_raw |> filter(trouble_audition == "Yes") |> pull(preci_conclusion_consultationV2)
# data_raw |> filter(trouble_audition == "Yes") |> pull(preci_conclusion_consultationV2)

predictors_all<- data |> select(ID,
                                base_wave_vabs,
                                base_wave_cbcl,
                             base_sex, 
                             base_maternal_education) |> 
  mutate(base_hearing_impairment = 0,
         base_visual_impairment = 0)

wave1_predictors <- data |> 
  select(ID, 
         base_ados_css_rrb = ADOS_scoreCOMPRRBV0,
         base_ados_css_sa = ADOS_scoreCOMPSAV0,
         base_vabs_abc_ss = TOTALCOMPOSITEV0
  ) |> 
  left_join(wave1_iq)

wave2_predictors <- data |> select(
  ID,
  base_ados_css_rrb = ADOS_scoreCOMPRRBV2,
  base_ados_css_sa = ADOS_scoreCOMPSAV2,
  base_vabs_abc_ss = TOTALCOMPOSITEV2
) |> 
  left_join(wave2_iq)

predictors_vabs1 <- predictors_all |> filter(base_wave_vabs ==1) |> left_join(wave1_predictors, by = "ID")
predictors_vabs2 <- predictors_all |> filter(base_wave_vabs ==2) |> left_join(wave1_predictors, by = "ID")
predictors_vabs <- bind_rows(predictors_vabs1, predictors_vabs2)

predictors_cbcl1 <- predictors_all |> filter(base_wave_cbcl ==1) |> left_join(wave1_predictors, by = "ID")
predictors_cbcl2 <- predictors_all |> filter(base_wave_cbcl ==2) |> left_join(wave1_predictors, by = "ID")
predictors_cbcl <- bind_rows(predictors_cbcl1, predictors_cbcl2)

# VABS
vabs_ages <- data |> 
  select(ID, starts_with("agevineland")) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "age") |> 
  mutate(wave = substr(name, nchar(name), nchar(name)) |> as.numeric()/2 + 1) |> 
  select(-name) |> 
  select(ID, wave, age)
  
# Calculating age equivalent scores for each domain as mean of subdomains
# Note scores are stored in two variables _mois and _ans. The former contains the months and the latter the years. These must be added together to give the propper score.
vabs_data <-  data |>  select(ID, starts_with("AE")) |> 
  select(-aerateur_transtympanique) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "score") |> 
  mutate(wave = substr(name, nchar(name), nchar(name)) |> as.numeric()/2 + 1,
         what = substr(name, nchar(name)-5, nchar(name)-2) |> str_replace("_", "") |> tolower(),
         domain = substr(name, 1,nchar(name)-6) |> str_replace("_", "") |> tolower(),
         ) |> 
  select(-name) |> 
  pivot_wider(names_from = what, values_from = score) |> 
  mutate(score = ans + mois/12) |> 
  select(-ans, -mois) |> 
  pivot_wider(names_from = domain, values_from = score) |> 
  rowwise() |> 
  mutate(vabs_dls_ae = mean(c(aesoin, aedomicile, aecommunaute), na.rm = TRUE),
         vabs_com_ae = mean(c(aeecouter, aeparler, aelire), na.rm = TRUE),
          vabs_soc_ae = mean(c(aecontact, aejouer,aeadapt), na.rm = TRUE)) |> 
  ungroup() |> 
  right_join(vabs_ages, by = c("ID", "wave")) |> 
  select(ID, wave, age, starts_with("vabs")) |> 
  left_join(predictors_vabs, by = "ID") |>
  mutate(wave = wave - base_wave_vabs)  |> 
  select(-starts_with("base_wave"))


ages_vabs <- get_age_range_data_vabs(wave1_data = vabs_data |> filter(wave == 0), wave2_data = vabs_data |> filter(wave == 1))

vabs_data_restricted <- vabs_data |> 
  left_join(ages_vabs |> select(ID, include), by = "ID") |> 
  filter(include == "include") |> 
  filter(wave != 2)

elena_data_vabs <- vabs_data_restricted |> 
  mutate(country = country_name,
         study = study_name,
         base_wave = 0,
         out_wave = 1) 

vabs_acc <- ages_vabs

# CBCL
edit_cbcl_item_names <- Vectorize(function(old_name) {
  parts <- unlist(strsplit(old_name, "_"))
  last_part <- tail(parts, 1)
  last_part_cleaned <- gsub("V[0-9]*$", "", last_part)
  new_name <- paste0("cbcl_item_", last_part_cleaned)
  
  return(new_name)
})


cbcl_item_data <- data |> select(ID, starts_with("item_invent")) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "score_words") |> 
  mutate(score_factor = factor(score_words, levels = c("Not true", "Somewhat or sometimes true", "Very true or often true")),
         score_number = as.numeric(score_factor) -1,
         wave = substr(name, nchar(name), nchar(name)) |> as.numeric()/2 + 1,
         item = substr(name, 1,nchar(name)-2),
         new_item = edit_cbcl_item_names(item)) |> 
  filter(!is.na(score_number))

cbcl_item_data

cbcl_item_data_wide <- 
  cbcl_item_data |> select(-score_factor, -score_words, -item, -name) |> 
  pivot_wider(names_from = new_item, values_from = score_number)

cbcl_item_data_wide |> select(-ID, -wave) |>  check_cbcl_items()

cbcl_data_orig_waves <- data |> select(ID, starts_with("age_cbclV"), starts_with("R_DS")) |> 
  select(-ends_with("cl")) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "score") |> 
  mutate(wave = substr(name, nchar(name), nchar(name)) |> as.numeric()/2 + 1,
         domain = substr(name, 1,nchar(name)-2) ) |> 
  select(-name) |> 
  pivot_wider(names_from = domain, values_from = score) |> 
  rename(age = age_cbcl, 
         cbcl_aff = R_DS1_AFFECT_PB, 
         cbcl_anx = R_DS2_ANXIETY_PB, 
         cbcl_som = R_DS3_SOMATIC_PB,
         cbcl_adhd = R_DS4_ATTENTION_DEF,
         cbcl_odd = R_DS5_OPP_PB,
         cbcl_con = R_DS6_CONDUCT_PB)

cbcl_data <- cbcl_data_orig_waves |>
  left_join(predictors_cbcl, by = "ID") |>
  left_join(cbcl_item_data_wide, by = c("ID", "wave")) |>
  mutate(wave = wave - base_wave_cbcl)   |> 
  select(-starts_with("base_wave"))

cbcl_checks <- cbcl_item_data_wide |> calc_cbcl_dsmIV_domains() |> check_cbcl_domain_scores(cbcl_data_orig_waves)
cbcl_checks |> filter(diff_any != TRUE)


ages_cbcl <- get_age_range_data_vabs(wave1_data = cbcl_data |> filter(wave == 0), wave2_data = cbcl_data |> filter(wave == 1))

cbcl_data_restricted <- cbcl_data |> 
  left_join(ages_cbcl |> select(ID, include), by = "ID") |> 
  filter(include == "include") |> 
  filter(wave != 2)

elena_data_cbcl<- cbcl_data_restricted |> 
  mutate(country = country_name,
         study = study_name,
         base_wave = 0,
         out_wave = 1) 

check_values(elena_data_cbcl)
check_values(elena_data_vabs)
saveRDS(elena_data_cbcl, file = here::here(derived_data, "elena_cbcl.RDS"))
saveRDS(elena_data_vabs, file = here::here(derived_data, "elena_vabs.Rds"))

saveRDS(vabs_acc  |>  mutate(study = study_name), file = here::here(derived_data, "elena_vabs_acc.Rds"))
saveRDS(ages_cbcl  |>  mutate(study = study_name), file = here::here(derived_data, "elena_cbcl_acc.Rds"))


# -------------------------------------------------------------------------

