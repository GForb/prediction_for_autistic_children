# Aim: Create file in long format with one row per participant and wave

# Predictors:
# visual/hearing = 0 for everyone
# Sex
# FSIQ
# ADOS rrb/sa
# VAVS ABC
# ADI 65
# Ethnicity
# Parental Education

add_observation_numbers <- function(data){
  data |> 
    group_by(ID) |> 
    arrange(age) |> 
    mutate(wave = row_number(), n_obs = max(wave)) |> 
    ungroup() |>
    select(any_of(c("ID", "age", "wave", "n_obs")), everything()) 
}

study_name <- "EDX"

data_folder <- here::here(raw_data, "EDX")

max_age = max(cbcl_check_out_age("max"),
              vabs_check_out_age("max"))

data_raw_wide <-readxl::read_xlsx(here::here(data_folder, "EDX_data_wide.xlsx")) |>
  select(ID = INIT,everything()) |> 
  mutate(age = AGE/12) |> 
  filter(EXAMTYPE %in% c("Vineland I", "Vineland II", "CBCL(4-18)", "CBCL (Age 6 - 18)"),
         is.na(INITSCR),
         age < max_age) |> 
  mutate(measure = case_when(EXAMTYPE == "Vineland I" ~ "VABS",
                             EXAMTYPE == "Vineland II" ~ "VABS",
                             EXAMTYPE == "CBCL(4-18)" ~ "CBCL",
                             EXAMTYPE == "CBCL (Age 6 - 18)" ~ "CBCL"),
         visit_exam = paste0(VISITNAME, "_", measure),
         maternal_education = as.numeric(`Binary educbm`)) |> 
  add_observation_numbers()


ids_data <- data_raw_wide |> select(ID) |> unique()

data_raw_long <- readxl::read_xlsx(here::here(data_folder, "EDX_data.xlsx")) |> 
  rename(ID = INIT) |> 
  mutate(age = AGE/12) |> 
  filter(!(EXAMTYPE %in% c("CBCL (Adult )", "TRF(5-18)")),
         is.na(INITSCR)) |> 
  add_observation_numbers()

ids_data_orig <- data_raw_long |> select(ID) |> unique()


data_raw_1obs <- data_raw_long |> 
  filter(wave ==1)  |> 
  select(ID, age, n_obs, Sex) |> 
  arrange(ID)

data_wide_1obs <- data_raw_wide |> 
  filter(wave ==1)  |> 
  select(ID, age, n_obs, Sex) |> 
  arrange(ID)

visit_ages <- data_raw_wide |> 
  select(ID, visit_exam, age) |> 
  group_by(ID, visit_exam) |> 
  mutate(occasion = row_number()) |> 
  ungroup() |> 
  pivot_wider(names_from = visit_exam, values_from = age) |> 
  rowwise() |> 
  mutate(age_C9 = mean(c(C9_CBCL, C9_VABS), na.rm = TRUE),
         age_C5 = mean(c(C5_VABS), na.rm = TRUE),
         diff = max(abs(age_C9 - C9_CBCL), abs(age_C9 - C9_VABS))) |> 
  ungroup()


summary(visit_ages$diff)
summary(visit_ages$C9_age)
summary(visit_ages$C5_age)

visit_ages |> filter(age_C5 > 6) |> select(ID, age_C5, age_C9) # all people with data at age 5, and older than 6 at age 5 have data recorded at age 9.

predictors <- data_raw_wide |> 
  group_by(ID) |> 
  mutate(occasion = row_number()) |>
  ungroup() |> 
  filter(occasion ==1) |>
  select(ID, 
         base_ethnicity = Race, 
         base_sex = Sex, 
         base_maternal_education = 'Binary educbm', 
         base_vabs_abc_ss = VABCST_C9,
         base_adi_65 = ADI65_C9,
         base_ados_css_rrb = css_rrb_C9,
         base_ados_css_sa = CSS_sa_C9,
         base_iq_full_scale = COMB_BESTFSIQ_C9,
         base_nviq = COMB_BESTNVIQ_C9,
         base_viq = COMB_BESTVIQ_C9
         ) |> 
  mutate(mean_iq = (base_nviq+base_viq)/2,
         base_iq_standard = ifelse(is.na(base_iq_full_scale), 
                                   ifelse(is.na(mean_iq), NA, 0)
                                   ,  1),
         base_iq_full_scale = ifelse(is.na(base_iq_full_scale), mean_iq,  base_iq_full_scale),
         base_hearing_impairment = 0,
         base_visual_impairment = 0,
         base_adi_65 = process_adi65(base_adi_65),
         base_sex = base_sex -1,
         base_maternal_education = as.numeric(base_maternal_education),
         base_ethnicity = ifelse(base_ethnicity==1, 1, 0)) 

predictors |> count(base_iq_standard)
predictors |> count(base_maternal_education)
predictors |> count(base_ethnicity)

predictors |> filter(base_iq_standard == 0) |> pull(base_iq_full_scale) |> summary()

predictors |> filter(base_iq_standard == 1) |> pull(base_iq_full_scale) |> summary()





#VABS
vabs_data_wide_long <- data_raw_wide |> 
  select(ID, age, EXAMTYPE, VDLAE,	VCAE,	VSAE) |> 
  arrange(ID, age) |> 
  filter(EXAMTYPE %in% c("Vineland I", "Vineland II"))|> 
  add_observation_numbers() |> 
  left_join(data_raw_long |> select(ID, age, VDLAE,  VCAE,  VSAE), by = c("ID", "age"), suffix = c(".wide", ".long")) |> 
  mutate(VDLAE = case_when(is.na(VDLAE.wide) ~ VDLAE.long,
                           TRUE ~ VDLAE.wide),
         VCAE = case_when(is.na(VCAE.wide) ~ VCAE.long,
                          TRUE ~ VCAE.wide),
         VSAE = case_when(is.na(VSAE.wide) ~ VSAE.long,
                          TRUE ~ VSAE.wide),
         VDLAE = na_if(VDLAE, 999),
         VCAE = na_if(VCAE, 999),
         VSAE = na_if(VSAE, 999))


vabs_data <- vabs_data_wide_long |> select(ID, age, wave, n_obs, EXAMTYPE, VDLAE, VCAE, VSAE)

vabs_data |> filter(wave ==1) |> count(n_obs) 

vabs_last_obs <- vabs_data |> 
  filter(wave == n_obs) |> 
  mutate(final_age = age) 
  

vabs_data <- vabs_data |> 
  left_join(vabs_last_obs |> select(ID, final_age), by = "ID") 

vabs_check_base_age("min")
vabs_check_base_age("max")

vabs_check_out_age("min")
vabs_check_out_age("max")

vabs_data <- vabs_data |> 
  arrange(ID, desc(wave)) |> 
  group_by(ID) |> 
    mutate(out_wave = case_when(n_obs ==1 ~ NA,
                                n_obs > 1 ~wave[1]),
           base1_wave = case_when(n_obs ==1 ~ 1,
                                  n_obs >= 2 ~ wave[2]),
           base2_wave = case_when(n_obs >= 3 ~ wave[3],
                               TRUE ~ NA)
           ) %>%
    ungroup()

vabs_last_obs$age |> summary()


vabs_out_data <- vabs_data |> filter(wave == out_wave)  |> select(ID, age_out = age)
vabs_base_data1 <- vabs_data |> filter(wave == base1_wave) |> select(ID, age_base1 = age, n_obs)
vabs_base_data2 <- vabs_data |> filter(wave == base2_wave) |> select(ID, age_base2 = age)

vabs_ages <- ids_data_orig |> 
  left_join(vabs_out_data) |> 
  left_join(vabs_base_data1) |>
  left_join(vabs_base_data2) |> 
  mutate(fu1 = age_out - age_base1,
         fu2 = age_out - age_base2,
         eligible_age_base1 = vabs_check_base_age(age_base1),
         eligbiel_age_base2 = vabs_check_base_age(age_base2),
         base_wave = case_when(fu1 >=2 & eligible_age_base1 ==1 ~ 1,
                               fu1 < 2 & is.na(fu2) ~ 1,
                               !is.na(fu2) ~ 2,
                               is.na(fu1) ~ 1,
                               TRUE ~ NA_real_),
         base_age = case_when(base_wave ==1 ~ age_base1,
                              base_wave ==2 ~ age_base2)
  )

vabs_ages |> count(base_wave)
vabs_ages |> filter(is.na(base_wave)) |> print(n = 30)

vabs_accounting <- get_age_range_data_vabs(
    wave2_data = vabs_ages |> select(ID, age = age_out), 
    wave1_data = vabs_ages |> select(ID, age = base_age)
    ) |> 
  mutate(study = study_name)

vabs_accounting |> count(include)

vabs_data <- vabs_data |> 
  left_join(vabs_accounting |> select(ID, include))

vabs_data |> count(include)
vabs_data |> filter(include == "eligible baseline, no followup")
vabs_data |> filter(include == "eligible baseline, no followup in range")

edx_data_vabs <- vabs_data  |> 
  filter(include == "include") |> 
  left_join(vabs_ages |> select(ID, base_wave)) |> 
  mutate(base_wave = case_when(base_wave == 1 ~ base1_wave,
                               base_wave == 2 ~ base2_wave),
         wave = case_when(wave != out_wave ~ wave - base_wave,
                          wave == out_wave ~ 2),
         out_wave = 2,
         base_wave = 0,
         vabs_dls_ae = VDLAE/12,
         vabs_com_ae = VCAE/12,
         vabs_soc_ae = VSAE/12)|> 
  select(ID, 
         age, 
         wave,
         base_wave,
         out_wave,
         vabs_dls_ae,
         vabs_com_ae,
         vabs_soc_ae, 
         EXAMTYPE) |> 
  left_join(predictors, by = "ID") |> 
  mutate(study = study_name)

# CBCL - I'm still waiting for item data on the CBCL - CBCL data not currently avaialble for EDX...
cbcl_data_all <- data_raw_long |> 
  select(ID, age, EXAMTYPE, INITSCR, starts_with("CBCL")) |> 
  filter(EXAMTYPE == "CBCL (Age 6 - 18)" | EXAMTYPE == "CBCL(4-18)", is.na(INITSCR))  |> # Teacher report is filtered out by examtype, sibling data by INITSCR
  add_observation_numbers()


cbcl_data_1618 <- data_raw_long |> 
  select(ID, age, EXAMTYPE, INITSCR, starts_with("CBCL")) |> 
  filter(EXAMTYPE == "CBCL (Age 6 - 18)", is.na(INITSCR))  |> 
  add_observation_numbers()


cbcl_data_all  |> filter(wave ==1)  |> count(n_obs)
cbcl_data_1618 |> filter(wave ==1) |>  count(n_obs)

# cbcl_data_1618 |> filter(wave ==1, n_obs > 1)
# 
# 
# cbcl_data_all |> filter(n_obs >= 4) |> print(n = 50)


filtered_data <- cbcl_data_all %>%
  group_by(age, ID) %>%
  filter(n() > 1) |> select(ID, age, EXAMTYPE, INITSCR, CBCL_TOTAL_RAW, starts_with("CBCL")) 


 
cbcl_ages <- cbcl_data_all |> 
  select(ID, age, n_obs, wave) |> 
  mutate(
    eligible_base = cbcl_check_base_age(age),
    base_wave = case_when(eligible_base == 1 ~ wave,
                          TRUE ~ NA),
    eligible_out = cbcl_check_out_age(age),
    out_wave = case_when(eligible_out == 1 ~ wave,
                          TRUE ~ NA)) |> 
  group_by(ID) |> 
  mutate(
    id_eligible_out = max(eligible_out),
    id_eligible_base = max(eligible_base),
    base_wave = max(base_wave, na.rm = TRUE),
    out_wave = max(out_wave, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(base_wave = case_when(base_wave == out_wave ~ base_wave - 1,
         TRUE ~ base_wave))

diplicate_waves <- cbcl_ages |> filter(id_eligible_out == 1, base_wave == out_wave)
if(nrow(diplicate_waves) > 0){
  diplicate_waves |> print(n = 10)
  stop("Some observations have base waves equal outcome waves")
}

eligible <- cbcl_ages |> 
  filter(wave ==1) |> 
  select(ID, n_obs, id_eligible_out, id_eligible_base, base_wave, out_wave) |> 
  mutate(eligible = case_when(n_obs >= 2 & id_eligible_out == 1 & id_eligible_base == 1 ~ 1,
                              TRUE ~ 0)) 

eligible |> count(eligible)
       
cbcl_data_analysis <- cbcl_data_all |> 
  left_join(eligible, by = "ID") |>
  select(
  ID,
  cbcl_aff = CBCL_AFFECTIVE_RAW,
  cbcl_anx = CBCL_ANXIETY_RAW, 
  cbcl_som = CBCL_SOMATIC_P_RAW,
  cbcl_adhd = CBCL_ADHD_RAW,
  cbcl_odd = CBCL_OPPOSITIONAL_RAW,
  cbcl_con = CBCL_CONDUCT_RAW,
  age,
  wave,
  base_wave,
  n_obs = n_obs.x,
  eligible) |> 
  filter(n_obs >= 2) |> 
  mutate(wave = wave - base_wave,
         base_wave = 0,
         out_wave = 1) |> 
  mutate(across(starts_with("CBCL"), ~ ifelse(.x == 999, NA, .x))) 

cbcl_data_analysis |> arrange(ID) |> filter(eligible == 1) |> print( n = 50)

cbcl_acc <- get_age_range_data_cbcl(wave1_data = cbcl_data_analysis |> filter(wave == base_wave), wave2_data = cbcl_data_analysis |> filter(wave == out_wave)) |> 
  mutate(study = study_name)

cbcl_acc |> count(include)

edx_data_cbcl <- cbcl_data_analysis |> 
  left_join(predictors, by = "ID") |> 
  left_join(cbcl_acc |> select(ID, include), by = "ID") |>
  add_follow_up_length() |> 
  mutate(study = study_name) |> 
  filter(include == "include")

 

check_values(edx_data_cbcl)
check_values(edx_data_vabs)

saveRDS(edx_data_cbcl, file = here::here(derived_data, "edx_cbcl.Rds"))
saveRDS(edx_data_vabs, file = here::here(derived_data, "edx_vabs.Rds"))

saveRDS(vabs_accounting, file = here::here(derived_data, "edx_vabs_acc.Rds"))


# -------------------------------------------------------------------------

