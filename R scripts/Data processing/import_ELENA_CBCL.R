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

study_name <- "ELENA"
country_name <- "France"



data_folder <- here::here(raw_data, "ELENA")
data_raw <- readxl::read_xlsx(here::here(data_folder, "ELENA_DATA_20240613_protege.xlsx")) 


iq_method_mapping <- tibble(
  test_label = unique(c(data_raw$calcul_bestQDV0, data_raw$calcul_bestQDV2)), 
  test = c("Non-verbal", "Perceptual", "Fluid reasoning", "FSIQ", "Non-standard", 
           "Non-standard", "Fluid reasoning", "Fluid reasoning", "Vineland DLS SS", 
           "Perceptual", "Non-standard", "Non-standard", "Non-standard", "Perceptual")) |> 
    mutate(standard = case_when(test == "Non-standard" ~ 0,
                                TRUE ~ 1),
           treat_as = case_when(test == "FSIQ" ~ "base_iq_full_scale",
                                test == "Perceptual" ~ "base_iq_perceptual",
                                test == "Fluid reasoning" ~ "base_iq_perceptual",
                                test == "Vineland DLS SS" ~ "base_vabs_abc_ss",
                                TRUE ~ "base_iq_full_scale"))
  



data_raw |> count(calcul_bestQDV0)

data_raw |> 
  select(test_label = calcul_bestQDV0, bestQDV0) |> 
  left_join(iq_method_mapping) |> 
  group_by(test) |>
  summarise(
    n = sum(!is.na(bestQDV0)),
    mean = mean(bestQDV0, na.rm = TRUE),
    min = min(bestQDV0, na.rm = TRUE),  
    max = max(bestQDV0, na.rm = TRUE)
  ) 

data_raw |> 
  select(ID,test_label = calcul_bestQDV2, bestQDV2) |> 
  left_join(iq_method_mapping) |> 
  group_by(test) |>
  summarise(
    n = sum(!is.na(bestQDV2)),
    mean = mean(bestQDV2, na.rm = TRUE),
    min = min(bestQDV2, na.rm = TRUE),  
    max = max(bestQDV2, na.rm = TRUE)
  ) 

# Wave 1 mostly perceptual reasoning, wave 2 mostly fluid.

wave1_iq <- data_raw |> 
  select(ID, test_label = calcul_bestQDV0, bestQDV0) |> 
  left_join(iq_method_mapping) |> 
  pivot_wider(names_from = treat_as, values_from = bestQDV0) |> 
  select(ID, base_iq_full_scale, base_iq_perceptual, base_iq_standard = standard) 

wave2_iq <- data_raw |> 
  select(ID, test_label = calcul_bestQDV2, bestQDV2) |> 
  left_join(iq_method_mapping) |> 
  pivot_wider(names_from = treat_as, values_from = bestQDV2) |> 
  select(ID, base_iq_full_scale, base_iq_perceptual, base_iq_standard = standard) 
  



# IRP - perceptive reasoning
# IRF fluid reasoning

# Determining base wave

data <- data_raw |> 
  mutate(
         age_cbcl1 = as.numeric(age_cbclV0),
         age_cbcl2 = as.numeric(age_cbclV2),
         age_cbcl3 = as.numeric(age_cbclV4),
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
data |> count(base_wave_cbcl)

data |> 
  select( age_cbcl1, age_cbcl2, age_cbcl3) |> 
  pivot_longer(cols = everything(), names_to = "what", values_to = "age") |> 
  group_by(what) |> 
  summarise(n = sum(!is.na(age)), min = min(age, na.rm = TRUE), max = max(age, na.rm = TRUE))


cn <- colnames(data)





# Predictors

# Hearing and visual impariment
# Review of the below, with help from chatGPT for translate shows no uncorrected visual or hearing impairment.
# data_raw |> count(cecite)
# data_raw |> count(trouble_audition)
# data_raw |> filter(trouble_audition == "Yes") |> pull(preci_conclusion_consultationV2)
# data_raw |> filter(trouble_audition == "Yes") |> pull(preci_conclusion_consultationV2)

predictors_all<- data |> select(ID,
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



predictors_cbcl1 <- predictors_all |> filter(base_wave_cbcl ==1) |> left_join(wave1_predictors, by = "ID")
predictors_cbcl2 <- predictors_all |> filter(base_wave_cbcl ==2) |> left_join(wave1_predictors, by = "ID")
predictors_cbcl <- bind_rows(predictors_cbcl1, predictors_cbcl2)



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
saveRDS(elena_data_cbcl, file = here::here(derived_data, "elena_cbcl.RDS"))

saveRDS(ages_cbcl  |>  mutate(study = study_name), file = here::here(derived_data, "elena_cbcl_acc.Rds"))


# -------------------------------------------------------------------------

