# Aim: Create file in long format with one row per participant and wave

data_folder <- here::here(raw_data, "TOGO")

data  <- haven::read_sav(here::here(data_folder, "ASS_W1_W2_cohort2-2.zsav")) |> 
  rename(ID = IDcode) |> 
  mutate(ID = as.character(ID))

comorbid_data <- haven::read_sav(here::here(data_folder, "ASS_W1_W2_cohort2_W1ComorOther.zsav"))


data |> select(dplyr::starts_with("W1CBCL")) |> colnames() 
print("CBCL Columns")
af_items <- 13 # i get 11
an_items <- 6 # I get 9, extra 31, 32 and 33, 78 - difference may be due to difference between DSM-IV and DSM-V
so_items <- 7 # I get 7
ad_items <- 7 # I get 6, missing item 100
od <- 5 # I get 5
cd <- 17 # I get 16

#Number of Items reported
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("af")) |> ncol() 
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("an")) |> ncol() 
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("so")) |> ncol() 
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("ad")) |> ncol() 
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("od")) |> ncol() 
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("cd")) |> ncol() 

# Colnames for items
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("af")) |> colnames() 
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("an")) |> colnames()
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("so")) |> colnames()
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("ad")) |> colnames()
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("od")) |> colnames()
data |> select(dplyr::starts_with("W1CBCL")) |> select(ends_with("cd")) |> colnames()



data |> select(dplyr::starts_with("W2")) |> colnames()

tibble(data)

data |> dplyr::count(w1educlevel_mother)
data |> dplyr::count(W1Diagn_comor_none)

sum(!is.na(data$W1IQ_points))
sum(!is.na(data$W1IQ_cat))

edit_cbcl_item_namesTOGO2 <- Vectorize(function(old_name) {
  parts <- unlist(strsplit(old_name, "_"))
  item_number <- parts[2] |> as.numeric()
  if(item_number %in% 56:62){
    item_number <- paste0(56, letters[item_number  -55])
  } else if (item_number > 62){
    item_number <- item_number - 7
  }

  new_name <- paste0("cbcl_item_", item_number)
  
  return(new_name)
})

edit_cbcl_item_namesTOGO2_2 <- Vectorize(function(old_name) {
  parts <- unlist(strsplit(old_name, "_"))
  item_number <- parts[3] |> as.numeric()
  if(item_number %in% 56:62){
    item_number <- paste0(56, letters[item_number  -55])
  } else if (item_number > 62){
    item_number <- item_number - 7
  }
  
  new_name <- paste0("cbcl_item_", item_number)
  
  return(new_name)
})


get_dsm_name <- Vectorize(function(old_name) {
  parts <- unlist(strsplit(old_name, "_"))
  domain_tag <- parts[3]
  gsub("[^a-z]", "", domain_tag)
})

get_emp_name <- Vectorize(function(old_name) {
  parts <- unlist(strsplit(old_name, "_"))
  domain_tag <- parts[3]
  gsub("[^A-Z]", "", domain_tag)
})

allocate_items <- Vectorize(function(item_no){
  if(item_no %in% c(5, 14, 18, 24, 35, 52, 54, 76, 77, 91, 100, 102, 103)){
    return("af")
  } else if(item_no %in% c(11, 29, 30, 45, 50, 112)){
    return("an")
  } else if(item_no == 56){
    return("so")
  } else if(item_no %in% c(4, 8, 10, 41, 78, 93, 104)){
    return("ad")
  } else if(item_no %in%  c(3, 22, 23, 86, 95)){
    return("od")
  } else if(item_no %in% c(15, 16, 21, 26, 28, 37, 39, 43, 57, 67, 72, 81, 82, 90, 97, 101, 106)){
    return("cd")
  } else {
    return("")
  }
})


annotated_items1 <- data |> 
  select(ID, dplyr::starts_with("W1CBCL")) |> 
  pivot_longer(cols = -ID, names_to = "cbcl_item", values_to = "score") |> 
  mutate(my_item = edit_cbcl_item_namesTOGO2(cbcl_item),
         dsm_domain = get_dsm_name(cbcl_item),
         emp_domain = get_emp_name(cbcl_item),
         item_no = as.numeric(gsub("[^0-9]", "", my_item)),
         my_domain = allocate_items(item_no),
         domain_check = my_domain == dsm_domain) 
  
annotated_items1 |> print(n = 200)

annotated_items2 <- data |> 
  select(-starts_with("W2CBCL__")) |> 
  select(ID, dplyr::starts_with("W2_CBCL")) |> 
  pivot_longer(cols = -ID, names_to = "cbcl_item", values_to = "score") |> 
  mutate(my_item = edit_cbcl_item_namesTOGO2_2(cbcl_item),
         dsm_domain = get_dsm_name(cbcl_item),
         emp_domain = get_emp_name(cbcl_item),
         item_no = as.numeric(gsub("[^0-9]", "", my_item)),
         my_domain = allocate_items(item_no),
         domain_check = my_domain == dsm_domain)  

annotated_items2 |> print(n = 200)

annotated_items2 |> filter(!domain_check)
annotated_items1 |> filter(!domain_check)


wave1_cbcl_items <- data  |> rename_with(edit_cbcl_item_namesTOGO2, .cols = starts_with("W1CBCL_")) |> 
  select(ID, age = W1age_child, starts_with("cbcl_item")) |> 
  calc_cbcl_dsmIV_domains() |> 
  mutate(wave = 0)

wave2_cbcl_items <- data |> select(-starts_with("W2CBCL_")) |> rename_with(edit_cbcl_item_namesTOGO2_2, .cols = starts_with("W2_CBCL")) |> 
  select(ID, age = W2age_child, starts_with("cbcl_item") )|> 
  calc_cbcl_dsmIV_domains() |> 
  filter(!is.na(age)) |>  
  mutate(wave =1)



predictors <- data |> 
  mutate(
    base_maternal_education = case_when(w1educlevel_mother != 6 & w1educlevel_mother != 999 & !is.na(w1educlevel_mother) ~ 0,
                                   w1educlevel_mother ==6 ~ 1,
                                   w1educlevel_mother == 999 ~ NA,),
    sex = W1sex_child -1 # missing item 15
  ) |> 
  select(
      ID,
      base_sex = sex,
      base_maternal_education,
      base_iq_full_scale = W1IQ_points,
      iq_cat = W1IQ_cat,
) |> 
  mutate(0,
     base_hearing_impairment = 0,
     base_visual_impairment = 0
  )

wave1_data <- wave1_cbcl_items |> 
  right_join(predictors, by = "ID") 

wave2_data <- wave2_cbcl_items |> 
  left_join(predictors, by = "ID") 



age_range_data <- get_age_range_data_cbcl(wave1_data, wave2_data)

age_range_data |> count(include)

data_all <- bind_rows(wave1_data, wave2_data) |> 
  select(-starts_with("cbcl_item")) |> 
  left_join(age_range_data |> select(ID, include), by = "ID") |>
  filter(include == "include") |> 
  select(-include)
  



# Renaming Variables ----


togo2_data <- data_all |> mutate(study = "TOGO2",
                                      country = "Belgium",
                                      base_wave = 0, 
                                      out_wave = 1) 

check_values(togo2_data)

saveRDS(togo2_data, file = here::here(derived_data, "togo2.Rds"))


particpant_accounting <- age_range_data |> 
  mutate(study = "TOGO2")
saveRDS(particpant_accounting, file = here::here(derived_data, "togo2_acc.Rds"))

# -------------------------------------------------------------------------

