# Aim: Create file in long format with one row per participant and wave

data_folder <- here::here(raw_data, "TOGO")

data_raw <- haven::read_sav(here::here(data_folder, "ASS_W1_W2_W3_cohort1.sav"))

data <- data_raw |> 
  rename(ID = idnr) |> 
  mutate(
    base_sex = Sex_child - 1,
    base_maternal_education = case_when(W1educlevel_mother != 12 & !is.na(W1educlevel_mother) ~ 0,
                                        W1educlevel_mother ==12 ~ 1),
    ID = case_when(
      is.na(ID) ~ "999",
      TRUE ~ as.character(ID))
  ) 


data |> select(dplyr::starts_with("W1")) |> colnames()
data |> select(dplyr::starts_with("W2")) |> colnames()
data |> select(dplyr::starts_with("W3")) |> colnames()

tibble(data)


wave1_cbcl <- data |> select(
  ID,
  cbcl_aff = W1CBCL_DSM_af,
  cbcl_anx = W1CBCL_DSM_an, 
  cbcl_som = W1CBCL_DSM_so,
  cbcl_adhd = W1CBCL_DSM_ad,
  cbcl_odd = W1CBCL_DSM_od,
  cbcl_con = W1CBCL_DSM_cd,
  age = W1age_child) |> 
  filter(!is.na(age)) |> 
  mutate(wave = 1)



wave2_cbcl <- data |> select(
  ID,
  cbcl_aff = W2CBCL_DSM_af,
  cbcl_anx = W2CBCL_DSM_an, 
  cbcl_som = W2CBCL_DSM_so,
  cbcl_adhd = W2CBCL_DSM_ad,
  cbcl_odd = W2CBCL_DSM_od,
  cbcl_con = W2CBCL_DSM_cd
) |> 
  mutate(wave = 2)


wave3_data <- data |> select(
  ID,
  cbcl_aff = W3_Aps_DSM_af,
  cbcl_anx = W3_Aps_DSM_an, 
  cbcl_som = W3_Aps_DSM_so,
  cbcl_adhd = W3_Aps_DSM_ad,
  cbcl_odd = W3_Aps_DSM_od,
  cbcl_con = W3_Aps_DSM_cd,
  age = W3age_child
) |> 
  mutate(wave = 3,
         in_range_out = cbcl_check_out_age(age))


wave1_cbcl <- data |> select(
  ID,
  cbcl_aff = W1CBCL_DSM_af,
  cbcl_anx = W1CBCL_DSM_an, 
  cbcl_som = W1CBCL_DSM_so,
  cbcl_adhd = W1CBCL_DSM_ad,
  cbcl_odd = W1CBCL_DSM_od,
  cbcl_con = W1CBCL_DSM_cd,
  age = W1age_child) |> 
  filter(!is.na(age)) |> 
  mutate(wave = 1)



wave2_cbcl <- data |> select(
  ID,
  age = W2age_child,
  cbcl_aff = W2CBCL_DSM_af,
  cbcl_anx = W2CBCL_DSM_an, 
  cbcl_som = W2CBCL_DSM_so,
  cbcl_adhd = W2CBCL_DSM_ad,
  cbcl_odd = W2CBCL_DSM_od,
  cbcl_con = W2CBCL_DSM_cd
) |> 
  mutate(wave = 2) |> 
  filter(!is.na(age))


wave3_data <- data |> select(
  ID,
  cbcl_aff = W3_Aps_DSM_af,
  cbcl_anx = W3_Aps_DSM_an, 
  cbcl_som = W3_Aps_DSM_so,
  cbcl_adhd = W3_Aps_DSM_ad,
  cbcl_odd = W3_Aps_DSM_od,
  cbcl_con = W3_Aps_DSM_cd,
  age = W3age_child
) |> 
  mutate(wave = 3)|> 
  filter(!is.na(age))

# Selecting base wave. Should be wave 2 if wave 2 baseline complete, in range and wave 3 outcome in range. Otherwise wave 1

cbcl_wide_ages <- bind_rows(wave1_cbcl, wave2_cbcl, wave3_data) |> 
  select(ID, wave, age) |> 
  pivot_wider(names_from = wave, values_from = age, names_prefix = "age") |> 
  mutate(w2_in_range_base = cbcl_check_base_age(age2),
         w3_in_range_out = cbcl_check_out_age(age3),
         base_wave = case_when(w2_in_range_base == 1 & w3_in_range_out == 1 ~ 2,
                               TRUE ~ 1))



data |> dplyr::count(W1educlevel_mother)
data |> dplyr::count(W3comOther)

wave1_predictors <- data |> 
  select( ID,
          base_sex,
          base_learning_disability = W1comIntDisab,
          base_maternal_education,
  ) |> 
  mutate(wave = 1)


wave2_predictors <- data |> select(
  ID,
  age = W2age_child,
  base_sex,
  base_maternal_education,
  base_iq_full_scale = W2IQ, # there is very little IQ data (is there any at wave 1????)
  base_learning_disability = W2comLearnDisord
) |> 
  mutate(wave = 2)


# Creating predictors dataset

IDs_base_wave <- data |> 
  select(ID) |> 
  left_join(cbcl_wide_ages |> select(ID, base_wave)) |> 
  mutate(
    base_wave = case_when(
      is.na(base_wave) ~ 1,
      TRUE ~ base_wave
    )
  )

bw1_predictors <- IDs_base_wave |> filter(base_wave ==1) |> left_join(wave1_predictors, by = "ID")
bw2_predictors <-  IDs_base_wave |> filter(base_wave ==2) |> left_join(wave1_predictors, by = "ID")

predictors <- bind_rows(bw1_predictors, bw2_predictors) |> 
  select(-wave)

cbcl_data <- bind_rows(wave1_cbcl |> right_join(predictors), # right join used to ensure all individuals are included in base_wave
                       wave2_cbcl |> left_join(predictors), 
                       wave3_data |> left_join(predictors)) |> 
  mutate(out_wave= base_wave +1,
         wave = case_when(!is.na(wave) ~ wave - base_wave,
                          is.na(wave) ~ 0))


base_data <-  cbcl_data |> filter(wave ==0)
sum(is.na(base_data$age))

age_range_data <- get_age_range_data_cbcl(
  wave1_data = cbcl_data |> filter(wave == 0), 
  wave2_data = cbcl_data |> filter(wave == 1))

age_range_data |> count(include)


togo1_data <- cbcl_data |> 
  left_join(age_range_data |> select(ID, include), by = "ID") |>
  filter(include == "include", wave <2) |> 
  mutate(country = "Belgium",
         study = "TOGO1",
         base_wave = 0,
         out_wave = 1) 


particpant_accounting <- age_range_data |> 
  mutate(study = "TOGO1")

check_values(togo1_data)

saveRDS(togo1_data, file = here::here(derived_data, "togo1.Rds"))
saveRDS(particpant_accounting, file = here::here(derived_data, "togo1_acc.Rds"))

# -------------------------------------------------------------------------

