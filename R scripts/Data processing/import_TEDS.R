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
    mutate(wave = row_number(), n_obs = max(wave)) |> 
    ungroup() |>
    select(ID, age, wave, n_obs, everything()) 
}


data_folder <- here::here(raw_data, "TEDS")

srs_data <- readRDS(here::here(data_folder, "srs_data.RDS"))
srs_data |> count(group_splitHTandDx)

data  <- haven::read_sav(here::here(data_folder, "TEDS_data_raw.sav")) |> 
  rename(ID = randomtwinid) |> 
  mutate(autism = case_when(
    group_splitHTandDx ==1 ~ "childhood, researcher",
    hdauti1 ==1 | ipldaut1 ==1 | gillaut1 ==1 | gldaut1 ~ "childhood, parent report",
    u2cexcl05asd1 ==1 | zmhmhddx2h1 ==1 | autism1  >0  ~ "post baseline"
  )) |> 
  filter(!is.na(autism))



ids_autism_status <- data |> select(
  ID, 
  family = randomfamid,
  pick_twin = random,
  autism
) 



ids_autism_status |> count(autism)




cn <- colnames(data)

get_var_labels <- function(data){
  labels = data %>% map(attr_getter("label")) |> unlist()
  label_lookup_map <- tibble(
    col_name = labels %>% names(),
    labels = labels
  )
  return(label_lookup_map)
}

data |> select(starts_with("dep")) |> get_var_labels()

# Other cognative functioning variables:
# G-composite: 

predictors <- data |> 
  mutate(base_maternal_education = case_when(
    gmohqual < 6 ~ 0,
    gmohqual >= 6 ~ 1
         ),
         base_hearing_impairment = case_when(
           gheardif1 %in% c(2, 4) ~ 1,
           is.na(gheardif1) ~ NA,
           TRUE ~ 0
         )) |> 
   select(ID,
          base_sex = sex1, 
          base_maternal_education,
          base_ados_css_rrb = ados_css_rrb,
          base_ados_css_sa = ados_css_sa,
          base_iq_full_scale = FSIQ,
          base_iq_standard = standard_IQ,
          g_composite = lcg1, # Other cognative functioning variables, highest correlation with IQ is g_composite
          ravens_total = lrvtota1, # Adjusted total score for ravens web test =  Other cognative functioning variables, highest correlation with IQ is g_composite
          base_hearing_impairment,
          base_imd_decile = dep15pc2012imddec)  |> 
  mutate(base_sex = 1-base_sex)




sdq_srs <- data |> 
  select(ID, Age_at_assessment_years_SRS1_SDQ, starts_with("SDQ"), -SDQ_total) |> 
  rename(age = Age_at_assessment_years_SRS1_SDQ,
         sdq_cond_p = SDQ_conduct,
         sdq_peer_p = SDQ_peer,
         sdq_pro_p = SDQ_prosocial,
         sdq_hyp_p = SDQ_hyperactivity,
         sdq_emot_p = SDQ_emotiol) |> 
  filter(!is.na(age)) |> 
  mutate(wave_age = 13, wave_letter = "srs")



sdq_age_data <- data |> 
  select(ID, d = dtwbage1, gp = gpbage, ip = icpage, lp = lpqage, ppbh = ppbhage) |> 
  pivot_longer(cols = -ID, names_to = "wave_letter", values_to = "age") 



sdq_data_raw <- data |> 
  select(ID, matches(".*sdq.*"), -starts_with("Age_"), -starts_with("SDQ_")) |> 
  pivot_longer(cols = -ID, names_to = "sdq_item", values_to = "score") |> 
  filter(!is.na(score))  |> 
  rowwise() |> 
  mutate(wave_letter = strsplit(sdq_item, "sdq")[[1]][1],
         item = strsplit(sdq_item, "sdq")[[1]][2],
         domain = case_when(item == "cont1" ~ "sdq_cond_p",
                            item == "pert1" ~ "sdq_peer_p",
                            item == "prot1" ~ "sdq_pro_p",
                            item == "hypt1" ~ "sdq_hyp_p",
                            item == "emot1" ~ "sdq_emot_p",
                            item == "beht1" ~ "sdq_total_p"),
         wave_age = case_when(wave_letter == "d" ~ 4,
                          wave_letter == "gp" ~ 7,
                          wave_letter == "ip" ~ 9,
                          wave_letter == "lp" ~ 12,
                          wave_letter == "ppbh" ~ 16,
                          wave_letter == "u1p" ~ 21
                          )
         )  |> 
    ungroup() 

sdq_data <- sdq_data_raw |> 
  left_join(sdq_age_data, by = c("ID", "wave_letter")) |>
  select(-item, -sdq_item) |> 
  pivot_wider(names_from = domain, values_from = score) |> 
  select(-sdq_total_p) |> 
  bind_rows(sdq_srs) |> 
  arrange(ID, wave_age) |> 
  filter(wave_age != 21) |> 
  mutate(wave = case_when(wave_letter == "d" ~ -3,
                          wave_letter == "gp" ~ -2,
                          wave_letter == "ip" ~ -1,
                          wave_letter == "lp" ~ 0,
                          wave_letter == "ppbh" ~ 2,
                          wave_letter == "srs" ~ 1))

sdq_ages <- sdq_data |> 
  select(ID, wave_age, age) |> 
  pivot_wider(names_from = wave_age, values_from = age, names_prefix = "age") |> 
  mutate(fu_12_13 = age13 - age12,
         fu_12_16 = age16 - age12,
         fu_13_16 = age16 - age13,
         fu_9_13 = age13 - age9,
         out_wave = 1,
         base_wave =  0
         )
 


sdq_ages |> count(base_wave)
sdq_ages |> count(out_wave)

sdq_data <- sdq_data |> 
  left_join(sdq_ages |> select(ID, base_wave, out_wave))

sdq_acc <- get_age_range_data_sdq(sdq_data |> filter(wave ==base_wave) |> right_join(ids_autism_status), wave2_data = sdq_data |> filter(wave ==out_wave)|> right_join(ids_autism_status))


sdq_acc |> count(include)

TEDS_sdq <- sdq_data |> 
  left_join(sdq_acc |> select(ID, include), by = "ID") |> 
  left_join(ids_autism_status, by = "ID") |>
  filter(include == "include") |> 
  left_join(predictors, by = "ID") |> 
  mutate(country = "UK",
         study = "TEDS")

TEDS_sdq |> pull(family) |> unique() |> length()
TEDS_sdq |> pull(ID) |> unique() |> length()

TEDS_sdq |> filter(autism == "childhood, researcher") |> pull(ID) |> unique() |> length()
TEDS_sdq |> filter(autism == "childhood, researcher") |> pull(family) |> unique() |> length()

TEDS_sdq |> count(base_sex)

check_values(TEDS_sdq)


saveRDS(TEDS_sdq, file = file.path(derived_data, "TEDS.Rds"))

saveRDS(sdq_acc, file = file.path(derived_data, "TEDS_acc.Rds"))
