
data_folder <- here::here(raw_data, "Pathways")
study_name <- "Pathways"
country_name = "Canada"

data_raw  <- haven::read_sav(here::here(data_folder, "Pathways T1 to T12 data Jul 7 2022.sav")) 

data <- data_raw |> 
  mutate(ID = row_number() |> as.character())

ids <- data |> select(ID)

ethnicity <- data |>  
  select(ID,matches("^FBI._7p.*$")) |> 
  pivot_longer(cols = -ID) |> 
  mutate(wave = sub("^[^_]*?([0-9]+)_.*", "\\1", name),
         who = sub(".*7(.*)$", "\\1", name)) |> 
  select(-name) |> 
  pivot_wider(values_from = value, names_from = who) |> 
  mutate(base_ethnicity = case_when(pmk ==1 & part ==1 ~ 1,
                              part ==1 & is.na(pmk) ~ 1,
                              pmk ==1 & is.na(part) ~ 1,
                              (pmk !=1 & !is.na(pmk)) | (part !=1 & !is.na(part)) ~ 0)) |> 
  filter(!is.na(base_ethnicity)) |> 
  group_by(ID) |> 
  summarise(base_ethnicity = max(base_ethnicity),
            part = max(part),
            pmk = max(pmk)) |> 
  ungroup()

ethnicity_rest <- ethnicity |> select(ID, base_ethnicity)

# Predicotrs
predictors <- data |>   mutate(
     base_sex = Gender -1,
     base_adi_65 = process_adi65(ADI8_65c),
     base_maternal_education = case_when(FBI8_3pmk %in% c(6, 7, 8,9,10, 11, 12) & FBI8_10 != 1 ~ 1,
                                    FBI8_3pmk <6  & FBI8_10 != 1 ~ 0, # note other classified as higher education due to free text
                                    FBI8_3part <6 & FBI8_10 == 1 ~ 0,
                                    TRUE ~ NA),
     dev_q = MP6dae/MP6dae*100,
     base_iq_full_scale = case_when(!is.na(WISCfsiq) ~ WISCfsiq,
                              !is.na(MP6dae) ~ dev_q,
                              TRUE ~ NA),
     base_iq_standard = case_when(!is.na(WISCfsiq) ~ 1,
                             is.na(WISCfsiq) & !is.na(MP6dae) ~ 0,
                             TRUE ~ NA),
     base_iq_perceptual = WISCpriq,
     base_visual_impairment = 0,
     base_hearing_impairment =0
     
  )|> 
  select(ID,
         base_sex,
         base_ados_css_rrb = AD8_RRBCSS,
         base_ados_css_sa = AD8_SACSS,
         base_adi_65,
         base_maternal_education,
         base_iq_full_scale,
         base_iq_standard,
         base_iq_perceptual,
         base_visual_impairment,
         base_hearing_impairment,
         base_vabs_abc_ss = VABS8cp) |> 
  left_join(ethnicity_rest)

# VABS

vabs_ages_long <-  data |> 
  select(ID, starts_with("VABS")) |> select(ID, ends_with("age")) |> 
  pivot_longer(cols = !any_of("ID"), names_to = "wave", values_to = "age") |> 
  mutate(wave = as.numeric(str_extract(wave, "\\d+"))) |> 
  mutate(age = age/12) 



wave_info <- vabs_ages_long |> filter(!is.na(age)) |>
  mutate(wave2 = case_when(wave ==9 ~ NA,
                           TRUE ~ wave)) |>
  group_by(ID) |> 
  mutate(last_wave = max(wave),
         last_base_wave = max(wave2, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(wave == last_wave) |> 
  select(ID, last_wave, last_base_wave)

vabs_ages_wide <- vabs_ages_long |> 
  pivot_wider(names_from = wave, values_from = age, names_prefix = "wave_") |> 
  left_join(wave_info, by = "ID") |> 
  mutate(fu_w8_w9 = wave_9 - wave_8,
         base_wave = case_when(
           is.na(wave_9) ~ last_base_wave, # no follow up data, use latest wave
           fu_w8_w9 < 2 & !is.na(wave_6) ~ 6,
           fu_w8_w9 < 2 & is.na(wave_6) ~ 8,
           fu_w8_w9 >= 2 ~ 8,
           is.na(wave_8)  ~ last_base_wave),
         out_wave = case_when(is.na(wave_9) ~ NA,
                              !is.na(wave_9) ~ 9),
         base_age = case_when(base_wave == 6 ~ wave_6,
                              base_wave == 8 ~ wave_8,
                              base_wave == 4 ~ wave_4,
                              base_wave ==3 ~ wave_3,
                              base_wave == 2 ~ wave_2,
                              base_wave ==1 ~ wave_1),
         fu_up_adj = wave_9 - base_age) 


age_range_data_vabs <- get_age_range_data_vabs(wave1_data = vabs_ages_wide |> select(ID, age = base_age), wave2_data = vabs_ages_wide |> select(ID, age = wave_9)) 
age_range_data_vabs |> count(include)

vabs_data_all <- data |> 
  select(ID, starts_with("VABS")) |> 
  select(ID, ends_with("ay")) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "score") |> 
  rowwise() |>
  mutate(years = str_split(score, "-")[[1]][1]|> as.numeric(),
         months = str_split(score, "-")[[1]][2] |> as.numeric(),
         age_eqiv = case_when(score == "<0-1" ~ 1/24,
                         score == ">22-0" ~ 22 + 1/12,
                         TRUE ~ years + months/12),
        domain = substr(name, 6,7),
        wave = substr(name, 5,5) |> as.numeric()) |> 
  ungroup() |> 
  filter(domain != "mg" & domain != "mf") 

vabs_data_all |> filter(is.na(age_eqiv)) |> count(score)
vabs_data_all |> count(domain)
vabs_data_all |> count(wave)

vabs_data <- vabs_data_all |> 
  select(-score,- years, -months, -name) |> 
  filter(!is.na(age_eqiv), domain %in% c("cr", "ce", "cw","si", "sp", "sc", "dp", "dd", "dc")) |> 
  pivot_wider(names_from = domain, values_from = age_eqiv) |> 
  rowwise() |> 
  mutate(vabs_com_ae = mean(c(cr, ce, cw)),
         vabs_soc_ae = mean(c(si, sp, sc)),
         vabs_dls_ae = mean(c(dp, dd, dc))) |> 
        left_join(vabs_ages_long, by = c("ID", "wave")) |> 
        select(ID, wave, age, starts_with("vabs"))  |> 
  ungroup() |> 
  left_join(vabs_ages_wide |> select(ID, base_wave, out_wave), by = "ID") 


vabs_out_data <- vabs_data |> filter(wave == 9) 

# This sets age at wave 9 to be missing if no vabs data is available at age 9
vabs_ages_wide <- vabs_ages_wide |> left_join(vabs_out_data, by = "ID") |> 
  mutate(wave_9 = case_when(is.na(vabs_com_ae) & is.na(vabs_soc_ae) & is.na(vabs_dls_ae) ~ NA,
                            TRUE~ wave_9)) 
  
age_range_data_vabs <- get_age_range_data_vabs(wave1_data = vabs_ages_wide |> select(ID, age = base_age), wave2_data = vabs_ages_wide |> select(ID, age = wave_9)) 
age_range_data_vabs |> count(include)


vabs_data_analysis <- 
  vabs_data |>
  left_join(age_range_data_vabs |> select(ID, include), by = "ID") |> 
  filter(include == "include", !is.na(age)) |> 
  mutate(wave = case_when(wave != 9 & base_wave == 8 ~ wave - base_wave,
                          wave < 8 & base_wave == 6 ~ wave - base_wave,
                          wave == 8 & base_wave == 6 ~1,
                          wave == 9 ~ 2),
         out_wave = 2,
         base_wave = 0,
         study = study_name,
         country = country_name) |> 
  left_join(predictors, by = "ID") 


# CBCL
# CBCL
cbcl_item_data <- data |> select(ID, starts_with("CBP")) |> 
  select(
    ID, 
    matches("[0-9]$"), 
    ends_with(paste0("56", letters[1:8])),
    ends_with(paste0("113", letters[1:3]))
   ) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "score") |> 
  mutate(wave = substr(name, 4, 4) |> as.numeric(),
         item_number = substr(name, 6,nchar(name)),
         new_item = paste0("cbcl_item_", item_number)) |> 
  filter(!is.na(score), wave !=1) 



cbcl_item_data_wide <- 
  cbcl_item_data |> select(-item_number, -name) |> 
  pivot_wider(names_from = new_item, values_from = score)

cbcl_item_data_wide |> select(-ID, -wave) |>  check_cbcl_items()

cbcl_data <- data |> select(ID, starts_with("age_cbclV"), starts_with("CBP68")) |> 
  select(ID, ends_with("age"), ends_with(paste0("_", 1:6, "tot"))) |> 
  pivot_longer(cols = !ID, names_to = "name", values_to = "score") |> 
  mutate(wave = substr(name, 6, 6) |> as.numeric(),
         domain = substr(name, 7,nchar(name)) ) |> 
  select(-name) |> 
  pivot_wider(names_from = domain, values_from = score) |> 
  select(ID,
         wave,
         age = age, 
         cbcl_aff = `_1tot`, 
         cbcl_anx = `_2tot`, 
         cbcl_som = `_3tot`,
         cbcl_adhd = `_4tot`,
         cbcl_odd = `_5tot`,
         cbcl_con = `_6tot`) |> 
  mutate(age = age/12) |> 
  left_join(predictors, by = "ID") |>
  left_join(cbcl_item_data_wide, by = c("ID", "wave")) 

# Selecting base waves for CBCL
cbcl_ages_long <-  cbcl_data |> select(ID, wave, age) 

wave_info_cbcl <- cbcl_ages_long |> 
  filter(!is.na(age)) |>
  mutate(wave2 = case_when(wave ==9 ~ NA,
                           TRUE ~ wave)) |>
  group_by(ID) |> 
  mutate(n_obs = sum(!is.na(age)),
         n_obs_base = sum(!is.na(wave2)),
        last_wave = max(wave),
        last_base_wave = case_when(n_obs_base ==0 ~ NA,
                                  TRUE ~ max(wave2, na.rm = TRUE))) |> 
  ungroup() |> 
  filter(wave == last_wave) |> 
  select(ID, last_wave, last_base_wave)

cbcl_ages_wide <- cbcl_ages_long |> 
  pivot_wider(names_from = wave, values_from = age, names_prefix = "wave_") |> 
  left_join(wave_info_cbcl, by = "ID") |> 
  mutate(fu_w8_w9 = wave_9 - wave_8,
         base_wave = case_when(
           is.na(wave_9) ~ last_base_wave, # no follow up data, use latest wave
           fu_w8_w9 < 2 & !is.na(wave_7) ~ 7,
           fu_w8_w9 < 2 & is.na(wave_6) ~ 8,
           fu_w8_w9 >= 2 ~ 8,
           is.na(wave_8)  ~ last_base_wave),
         out_wave = case_when(is.na(wave_9) ~ NA,
                              !is.na(wave_9) ~ 9),
         base_age = case_when(base_wave == 8 ~ wave_8,
                              base_wave == 7 ~ wave_7,
                              base_wave == 6 ~ wave_6,
                              base_wave ==5 ~ wave_5,
                              base_wave == NA ~NA),
         fu_up_adj = wave_9 - base_age) 

cbcl_data_with_waves <- cbcl_data |> 
  left_join(cbcl_ages_wide |> 
  select(ID, base_wave, out_wave))

ages_cbcl <- get_age_range_data_cbcl(
  wave1_data = cbcl_ages_wide |> select(ID, age = base_age), 
  wave2_data = cbcl_ages_wide |> select(ID, age = wave_9)
)

ages_cbcl |> count(include)


cbcl_data_restricted <- cbcl_data |> 
  filter(!is.na(age)) |> 
  left_join(ages_cbcl |> select(ID, include), by = "ID") |> 
  filter(include == "include") |> 
  left_join(cbcl_ages_wide |> select(ID, base_wave)) |> 
  select(ID, wave, base_wave, age, everything(), -starts_with("cbcl_item")) |> 
  mutate(wave = case_when(wave < base_wave ~ wave - base_wave,
                          wave != 9 & wave != base_wave ~ wave - base_wave, 
                          wave == 9 ~ 2,
                          wave == base_wave ~ 0))


pathways_data_cbcl<- cbcl_data_restricted |> 
  mutate(country = country_name,
         study = study_name,
         base_wave = 0,
         out_wave = 2) 


particpant_accounting_vabs <- 
  age_range_data_vabs |> 
  mutate(study = study_name)


particpant_accounting_cbcl <- 
  ages_cbcl |> 
  mutate(study = study_name)

vabs_data_analysis |> check_values()

pathways_data_cbcl |> check_values()
pathways_data_cbcl |> saveRDS(here::here(derived_data, "pathways_cbcl.Rds"))
vabs_data_analysis |> saveRDS(here::here(derived_data, "pathways_vabs.Rds"))

saveRDS(particpant_accounting_cbcl, file = here::here(derived_data, "pathways_cbcl_acc.Rds"))
saveRDS(particpant_accounting_vabs, file = here::here(derived_data, "pathways_vabs_acc.Rds"))


