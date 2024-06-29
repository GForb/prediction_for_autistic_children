data_folder <- here::here(raw_data, "ALSPAC")

data_raw <- haven::read_dta(here::here(data_folder, "B4313_Pickles_31MAY2024.dta")) |> 
  mutate(
    ID = paste0(cidB4313, "_", qlet),
    autism = case_when(
      autism ==1 ~ "childhood, researcher",
      ku360 ==2  ~ "childhood, parent report",
  )) |> 
  filter(!is.na(autism))

data_raw |> count(autism)

ids_autism_status <- data_raw |> select(
  ID, 
  autism
)

data_raw |> pull(ID, qlet) |> unique() |> length()

predictors <- data_raw |> 
  mutate(base_ethnicity = case_when(c804 == 1~ 1,
                                    c804 ==2 ~ 0,
                                    TRUE ~ NA),
         base_imd_decile = case_when(ccbimd2004q5<0 ~ NA,
                                     ccbimd2004q5 > 0 ~ ccbimd2004q5*2-0.5), # Data is for quitiles, converting to mid point of deciles
         base_sex = case_when(kz021 < 0 ~ NA,
                             kz021 == 1 ~ 0,
                             kz021 == 2 ~ 1),
         base_hearing_impairment = case_when(
           kr565 < 0 ~ NA,
           kz021 == 1 ~ 1,
           kz021 == 2 ~ 0),
         base_visual_impairment = case_when(
           kr566 < 0 ~ NA,
           kr566 == 1 ~ 1,
           kr566 == 2 ~ 0),
         base_iq_full_scale = case_when(f8ws111 < 0 ~ NA,
                                        TRUE ~ f8ws111),
         base_subjective_poverty = case_when( # k6200 to k6208, define properly.
           k6200 < 0 ~ NA,
           k6200 == 1 ~ 1,
           TRUE ~ 0),
         base_iq_standard = 1) |> 
  select(ID,
         base_ethnicity, 
         base_imd_decile,
         base_sex,
         base_iq_full_scale,
         base_iq_standard,
         base_hearing_impairment, 
         base_visual_impairment,
         base_subjective_poverty) |> 
  right_join(ids_autism_status, by = "ID")

age_data <- data_raw |> 
  select(ID,
         matches("*.999*.")) |> 
  pivot_longer(cols = -ID, names_to = "variable_name", values_to = "age") |> 
  rowwise() |> 
  mutate(wave_letters = strsplit(variable_name, "99")[[1]][1]) |> 
    ungroup() |> 
  mutate(
    age = case_when(age < 0 ~ NA,
                    TRUE ~ age),
    age = case_when(wave_letters  =="YPB" ~ age/12,
                    TRUE ~ age/52))

wave_ages <- age_data |> 
  group_by(wave_letters) |> 
  sum_detail("age") |> 
  ungroup() |> 
  mutate(wave_age = round(median)) |> 
  arrange(wave_age) |> 
  mutate(wave = -6:2)

sdq_data_raw <- data_raw |> 
  select(ID,
         matches("kq348*."),
         matches("ku70*.a"),
         matches("kw660*.a"),
         matches("ta7025*."),
         matches("tc4025*.")) 

sdq_labels <- sdq_data_raw |> 
  get_var_labels() |> 
  rowwise() |> 
  mutate(wave_letters = strsplit(col_name, "[0-9]")[[1]][1],
         item_part1 = strsplit(labels, "SDQ")[[1]][2],
         item_part2 = strsplit(item_part1, "score")[[1]][1],
         item_part2 = gsub(" ", "", item_part2),
         item_part2 = gsub("-", "", item_part2),
         item_part2 = tolower(item_part2)) |> 
    ungroup() |> 
  mutate(varname = case_when(
    item_part2 == "conductproblems" ~ "sdq_cond_p",
    item_part2 == "emotionalsymptoms" ~ "sdq_emot_p",
    item_part2 == "hyperactivity" ~ "sdq_hyp_p",
    item_part2 == "peerproblems" ~ "sdq_peer_p",
    item_part2 == "prosocial" ~ "sdq_pro_p",
    item_part2 == "totaldifficulties" ~ "sdq_tot_p",
  ))

sdq_labels |> count(varname)

sdq_data <- sdq_data_raw |>
  pivot_longer(cols = -ID, names_to = "col_name", values_to = "score") |> 
  left_join(sdq_labels, by = "col_name") |> 
  select(ID, wave_letters, varname, score) |> 
  filter(!is.na(score), score >=0) |> 
  pivot_wider(names_from = varname, values_from = score) |> 
  select(-sdq_tot_p) |> 
  left_join(wave_ages |> select(wave_letters, wave_age, wave), by = "wave_letters") |> 
  left_join(age_data |> select(ID, wave_letters, age), by = c("ID", "wave_letters")) 


  
acc <- get_age_range_data_sdq(
  wave1_data = sdq_data |> filter(wave == 0) |> right_join(ids_autism_status, by = "ID"), 
  wave2_data = sdq_data |> filter(wave == 1) |> right_join(ids_autism_status, by = "ID"))

acc |> count(include)

ALSPAC_sdq <- sdq_data |> left_join(acc |> select(ID, include)) |> 
  left_join(predictors, by = "ID") |> 
  filter(include == "include") |> 
  mutate(base_wave = 0,
         out_wave = 1,
         study = "ALSPAC",
         country = "UK")



check_values(ALSPAC_sdq)


saveRDS(ALSPAC_sdq, file = file.path(derived_data, "ALSPAC.Rds"))

saveRDS(acc, file = file.path(derived_data, "ALSPAC_acc.Rds"))