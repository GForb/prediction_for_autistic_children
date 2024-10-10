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
data_raw <- haven::read_sav(here::here(data_folder, "Forbes G 300924.sav")) |> 
  rename(ID = idno) 



data_raw |> count(cohort)
cn <- colnames(data_raw)

data_raw |> count(DxPDD)
data_raw |> count(autism_dx_accare)
data_raw |> count(selection_catharina)
data_raw |> count(select_pdd)

data_lables <- get_var_labels(data_raw)


predictors <- data_raw |>
  mutate(
    base_ethnicity = 1 - p1ethni2,
    base_maternal_education = case_when(p1educmo %in%  c(4,5) ~ 1, is.na(p1educmo) ~ NA, TRUE ~ 0),
    base_iq_standard = case_when(!is.na(g1wiscdq) ~ 1,
                                 TRUE ~ NA),
    base_sex = 1- g1sex,
  ) |>
  select(
    ID,
    cohort,
    base_sex,
    base_iq_full_scale = g1wiscdq,
    base_iq_standard,
    c4ados4socaff,
    c4ados4rrb,
    base_ethnicity,
    base_maternal_education,
  )

# Missing predictors:
# base_vabs_abc_ss
# ados_css_rrb
# ados_css_sa
# ADI 65


cbcl1 <- data_raw |> select(
  ID , cohort,
  age = g1ageyrc,
  cbcl_ext_total = p1cbext,
  cbcl_int_total = p1cbint,
  cbcl_aff = p1cbdaff,
  cbcl_anx = p1cbdanx,
  cbcl_som = p1cbdsom,
  cbcl_adhd = p1cbdadh,
  cbcl_odd = p1cbdod,
  cbcl_con = p1cbdcd
) |>
  mutate(wave = -1)|> 
  drop_na()

cbcl2 <- data_raw |> select(
  ID, cohort,
  age = g2ageyrc,
  cbcl_ext_total = p2cbext,
  cbcl_int_total = p2cbint,
  cbcl_aff = p2cbdaff,
  cbcl_anx = p2cbdanx,
  cbcl_som = p2cbdsom,
  cbcl_adhd = p2cbdadh,
  cbcl_odd = p2cbdod,
  cbcl_con = p2cbdcd
) |> mutate(wave = 0)|> 
  right_join(predictors |> select(ID), by = "ID")

cbcl3 <- data_raw |> select(
  ID, cohort,
  age = g3ageyrc,
  cbcl_ext_total = p3cbext,
  cbcl_int_total = p3cbint,
  cbcl_aff = p3cbdaff,
  cbcl_anx = p3cbdanx,
  cbcl_som = p3cbdsom,
  cbcl_adhd = p3cbdadh,
  cbcl_odd = p3cbdod,
  cbcl_con = p3cbdcd
) |> mutate(wave = 1) |> 
  drop_na()

# The CBCL scores in TRAILS are the raw scores divided by the number of items in the scales
# In other cohorts, only raw scores are provided.
# The following 3 code blocks multiplies by the number of items and checks that round score result.

cbcl_item_numbers <- calc_cbcl_dsmIV_domains(NULL) |> 
  rowwise() |> 
  mutate(n_items = length(items)) |> 
  ungroup() |> 
  select(domain, n_items)




cbcl_data_long <- bind_rows(cbcl1, cbcl2, cbcl3) |> 
  select(-cohort, -cbcl_ext_total, -cbcl_int_total) |> 
  pivot_longer(cols = starts_with("cbcl"), names_to = "domain", values_to = "score" ) |> 
  left_join(cbcl_item_numbers) |> 
  mutate(score = score * n_items) |> 
  mutate(round_score = round(score),
         round_diff = abs(score - round_score))

cbcl_data_long |> filter(round_diff != 0) # checking

cbcl_data <- cbcl_data_long |> select(ID, age, wave, domain, score) |> 
  pivot_wider(names_from = domain, values_from = score) |> 
  left_join(predictors, by = "ID")

cbcl_data |> count(wave)

ages_data <-  cbcl_data |> select(ID, wave, age) |> 
  pivot_wider(names_from = wave, values_from = age, names_prefix = "age") |> 
  mutate(gap0 = age0 - `age-1`,
         gap1 = age1 - age0,
         gap_long = age1 - `age-1`)

acc_data <- get_age_range_data_cbcl(cbcl_data |> filter(wave == 0), cbcl_data |> filter(wave == 1)) |> 
  mutate(base_wave = case_when(include != "include" ~ -1,
                               TRUE ~ 0)) |> 
  left_join(predictors |> select(ID, cohort))

acc_data |> count(include, cohort)
acc_data |> count(include)

cbcl_data_restricted <- cbcl_data |>
  left_join(acc_data |> select(ID, include), by = "ID") |>
  filter(include == "include") |> 
  mutate(study = "TRAILS", 
         country = "Netherlands",
         base_wave = 0,
         out_wave = 1)


cbcl_data_restricted |> check_values()

saveRDS(cbcl_data_restricted, file = here::here(derived_data, "TRAILS.RDS"))



saveRDS(
  acc_data  |>  mutate(study = study_name),
  file = here::here(derived_data, "TRAILS_acc.Rds")
)
