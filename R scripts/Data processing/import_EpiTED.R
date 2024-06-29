
data_folder <- here::here(raw_data, "EpiTED")
data_raw <- readxl::read_excel(here::here(data_folder, "EPITEDT2T3vf3_protege.xls"))



data <- data_raw |> 
  rename(ID = numeroanonyme,
         base_sex = sexes3)

cn <- colnames(data)
cn

data_raw |> select(
  totcomisr_ados1s3,
  jeutotal_ados1s3,
  csirtotal_ados1s3,
  totcomisr_ados2s3,
  csirtotal_ados2s3,
  totcomisr_ados3,
  csirtotal_ados3,
  totcomisr_ados4s3,
  csirtotal_ados4s3
)

# ADOS not collected at time 2 (baseline) but collected at time 3. CARS is collected at time 2 so may be useful for imputation. 

ados_sa_data <- data |> select(ID, m1 = totcomisr_ados1s3, m1b = jeutotal_ados1s3, m2 = totcomisr_ados2s3, m3 = totcomisr_ados3, m4 = totcomisr_ados4s3) |> 
  rowwise() |> 
  mutate(score = mean(c(m1, m2, m3, m4), na.rm = TRUE),
         n_modules = sum(!is.na(c(m1, m2, m3, m4))),
         module = case_when(!is.na(m1) ~ 1,
                            !is.na(m2) ~ 2,
                            !is.na(m3) ~ 3,
                            !is.na(m4) ~ 4)) |> 
  ungroup()

ados_rrb_data <- data |> select(ID, m1 = csirtotal_ados1s3, m2 = csirtotal_ados2s3, m3 = csirtotal_ados3, m4 = csirtotal_ados4s3) |> 
  rowwise() |> 
  mutate(score = mean(c(m1, m2, m3, m4), na.rm = TRUE),
         n_modules = sum(!is.na(c(m1, m2, m3, m4))),
         module = case_when(!is.na(m1) ~ 1,
                            !is.na(m2) ~ 2,
                            !is.na(m3) ~ 3,
                            !is.na(m4) ~ 4)) |> 
  ungroup()



ados_sa_data$score |> summary()
ados_rrb_data$score |> summary()


sum(!is.na(ados_rrb_data$score))
sum(!is.na(data$carstotals3))
sum(!is.na(data$CARSppS))


predictors <- data |> 
  mutate(base_maternal_education = case_when(NIVETUDEPERES4 == 3 ~ 1,
                                         NIVETUDEPERES4 <3 ~ 0,
                                         is.na(NIVETUDEPERES4) ~ NA),
         base_hearing_impairment = case_when(tbauditions4 == NA ~ NA,
                                        tbauditionprecs4 == "SURDITE PROFONDE" ~ 1,
                                        TRUE ~ 0),
         base_visual_impairment = 0,
         base_sex = base_sex -1) |> 
  select(ID,
         base_sex,
         base_maternal_education,
         base_iq_full_scale = BestQIT2ch,
         base_hearing_impairment,
         base_visual_impairment
  ) |> 
  mutate(base_iq_full_scale = as.numeric(base_iq_full_scale),
         base_iq_standard = 0) 

# Vineland
wave0 <- data |> 
  select(ID, age = nage, vabs_dls_ae = VineVieQ, vabs_com_ae = vineland, vabs_soc_ae =vinesoci) |> 
  mutate(wave = -1, 
         age = age |> as.numeric(),
         vabs_dls_ae = vabs_dls_ae/12,
         vabs_com_ae = vabs_com_ae/12,
         vabs_soc_ae = vabs_soc_ae/12)

wave1 <- data |> 
  mutate(age = nages |> as.numeric(), vabs_dls_ae = VVQs/12, vabs_com_ae = Vcoms/12, vabs_soc_ae = Vsocs/12, wave = 0) |> 
  select(ID, wave, age, starts_with("vabs")) |> 
  mutate(age = case_when(is.na(vabs_dls_ae) ~ NA,
                               TRUE ~ age)) # setting age to missing where no assessment made

# In the raw data the wave 2 age data is coded yy.mm eg. 16.01 for 16 years and 1 month. This function converts to decimal age.
convert_age_to_decimal <- Vectorize(function(age) {
  # Split age data into years and months
  age_years <- floor(age)
  age_months = (age - age_years)*100
  
  # Calculate age in decimal format
  age_decimal <- age_years + age_months / 12
  
  return(age_decimal)
})



wave2 <- data |> 
  mutate(wave =1, 
         age  = convert_age_to_decimal(ageenfants3vf |> as.numeric()),
         vabs_dls_ae = autonomieageequmoiss3/12,
         vabs_com_ae = communicationageeqmoiss3/12,
         vabs_soc_ae = socialisationageeqmoiss3/12
  ) |> 
    select(ID, wave, age, starts_with("vabs"))

vabs_data <- bind_rows(wave1, wave2, wave0) |> 
  left_join(predictors, by = "ID") 

particpant_accounting <- get_age_range_data_vabs(wave1, wave2) |> 
  mutate(study = "EpiTED")


epited_data <- vabs_data |> 
  mutate(country = "France",
         base_wave = 0,
         out_wave = 1) |> 
  left_join(particpant_accounting, by = "ID") |>
  filter(include == "include")

check_values(epited_data)

saveRDS(epited_data, here::here(derived_data, "epited.rds"))
saveRDS(particpant_accounting, here::here(derived_data, "epited_acc.rds"))