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
data_raw <- haven::read_sav(here::here(data_folder, "Hartman C 160924 - IPD-MA - update 170624.sav")) 

data_raw |> select(research_number, cohort) |> filter(cohort ==0) |>  unique() |> nrow()

data_raw |> select(research_number, cohort) |> filter(cohort ==1) |>  unique() |> nrow()

data_raw |> select(idno, cohort) |> filter(cohort ==0) |>  unique() |> nrow()
data_raw |> select(idno, cohort) |> filter(cohort ==1) |>  unique() |> nrow()


data_raw |> count(cohort)
cn <- colnames(data_raw)

data_lables <- get_var_labels(data_raw)

data_lables |> print(n = 400)

predictors <- data_raw |> 
  select(
    cohort, 
    base_sex = g1sex,
     base_full_scale_iq = g1wiscdq, 
     base_maternal_education = p1educmo, 
     c4ados4socaff, 
     c4ados4rrb,
    age_ados = c4adosageyrc,
      ) |> 
  mutate(base_ethnicity = 1- p1ethni2,
         base_maternal_education = case_when(p1educmo ==  ~ 5,
                                             is.na(p1educmo) ~ NA,
                                             TRUE ~ 0))

cbcl1 <- data_raw |> select(
  age = g1ageyrc,
  cbcl_ext_total = p1cbext,
  cbcl_int_total = p1cbint,
  cbcl_aff = p1cbdaff,
  cbcl_anx = p1cbdanx,
  cbcl_som = p1cbdsom,
  cbcl_adhd = p1cbdadh,
  cbcl_odd = p1cbdod,
  cbcl_con = p1cbdcd
) |> mutate(wave = 1)

cbcl2 <- data_raw |> select(
  age = g2ageyrc,
  cbcl_ext_total = p2cbext,
  cbcl_int_total = p2cbint,
  cbcl_aff = p2cbdaff,
  cbcl_anx = p2cbdanx,
  cbcl_som = p2cbdsom,
  cbcl_adhd = p2cbdadh,
  cbcl_odd = p2cbdod,
  cbcl_con = p2cbdcd
) |> mutate(wave = 2)

cbcl3 <- data_raw |> select(
  age = g3ageyrc,
  cbcl_ext_total = p3cbext,
  cbcl_int_total = p3cbint,
  cbcl_aff = p3cbdaff,
  cbcl_anx = p3cbdanx,
  cbcl_som = p3cbdsom,
  cbcl_adhd = p3cbdadh,
  cbcl_odd = p3cbdod,
  cbcl_con = p3cbdcd
) |> mutate(wave = 3)





# IRP - perceptive reasoning
# IRF fluid reasoning

# Determining base wave


cbcl_data_restricted <- cbcl_data |> 
  left_join(ages_cbcl |> select(ID, include), by = "ID") |> 
  filter(include == "include") |> 
  filter(wave != 2)



check_values(elena_data_cbcl)
check_values(elena_data_vabs)
saveRDS(elena_data_cbcl, file = here::here(derived_data, "elena_cbcl.RDS"))
saveRDS(elena_data_vabs, file = here::here(derived_data, "elena_vabs.Rds"))

saveRDS(vabs_acc  |>  mutate(study = study_name), file = here::here(derived_data, "elena_vabs_acc.Rds"))
saveRDS(ages_cbcl  |>  mutate(study = study_name), file = here::here(derived_data, "elena_cbcl_acc.Rds"))


# -------------------------------------------------------------------------

