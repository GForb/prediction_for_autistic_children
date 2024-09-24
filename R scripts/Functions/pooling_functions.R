pool_datasets <- function(dataset_strings, data_folder = derived_data, include_acc = TRUE){
  dataset_list <- map(
    dataset_strings, 
    ~readRDS(here::here(
      data_folder, 
      paste0(.,".Rds")
      )) |> mutate(ID = as.character(ID))
    )
    
  pooled_data <- bind_rows(dataset_list) |> 
    mutate(ID = paste0(study, "_", ID)) |> 
    one_hot_encode_study()
  

  pooled_data_baseline <- pooled_data |> 
    filter(wave == base_wave) |> 
    select(ID,age,  any_of(c("vabs_dls_ae", "vabs_com_ae", "vabs_soc_ae")), 
           starts_with("cbcl"), starts_with("sdq"), -starts_with("cbcl_item_"),
           base_sex) 
    
    pooled_data_baseline$base_all_complete <-  complete.cases(pooled_data_baseline)
    
    print(colnames(pooled_data_baseline))
    
    
                                
    pooled_data_out_vars <- pooled_data |> 
    select(ID, age, wave, out_wave,  any_of(c("vabs_dls_ae", "vabs_com_ae", "vabs_soc_ae")), 
           starts_with("cbcl"), starts_with("sdq"), -starts_with("cbcl_item_") )
    
    pooled_data_out_vars_only <- pooled_data_out_vars |> select( any_of(c("vabs_dls_ae", "vabs_com_ae", "vabs_soc_ae")), 
                                                                 starts_with("cbcl"), starts_with("sdq"), -starts_with("cbcl_item_"))
    
    pooled_data_out_vars$all_complete <-  complete.cases(pooled_data_out_vars)
    pooled_data_out_vars$partially_complete <- rowSums(pooled_data_out_vars_only, na.rm = TRUE) > 0
    
    pooled_data_outcomes <- pooled_data_out_vars |> 
      filter(wave == out_wave) |> 
      mutate(out_all_complete = all_complete)
    

  pooled_data_wide <- pooled_data |> 
    filter(wave == base_wave) |>
    left_join(pooled_data_outcomes |> select(-wave, -out_wave), by = "ID", suffix = c("_base", "_out")) |> 
    left_join(pooled_data_baseline |> select(ID, base_all_complete, - base_sex), by = "ID") |>
    rename_with( .cols = c(ends_with("_base"), ends_with("_out")), .fn = move_suffix_to_preffix) |> 
    mutate(fu_length = out_age - base_age)  |> 
    select(ID, starts_with("out"), study, starts_with("base"), everything())
    
  pooled_data_long <- pooled_data |> 
    left_join(pooled_data_baseline |> select(-base_sex), by = "ID", suffix = c("", "_base")) |> 
    rename_with( .cols = c(ends_with("_base")), .fn = move_suffix_to_preffix) |> 
    group_by(ID) |> 
    mutate(n_obs = n()) |>
    ungroup()
  
  n_obs <- pooled_data_long |> 
    filter(wave == base_wave) |> 
    select(ID, n_obs)
  
  pooled_data_wide <- pooled_data_wide |> 
    left_join(n_obs, by = "ID") 

  
  pooled_data_wide |> count(study) |> print()
  
  age_summaries <- pooled_data_wide |> 
    summarise_pooled_age() |> 
    arrange(name) 
  
  print_age_summaries(age_summaries)
  
  mean_age = mean(pooled_data_long$age, na.rm = TRUE)
  

  pooled_data_long <- pooled_data_long |> 
    mutate(age_c = age - mean_age,
           wave = case_when(
             wave >= out_wave ~ wave - out_wave + 2,
             wave < out_wave ~ wave - base_wave,
           ),
           out_wave = 2,
           base_wave = 0) |>
    select(-starts_with("cbcl_item"), -any_of("fu_length")) |> 
    left_join(pooled_data_wide |> select(ID, out_all_complete, fu_length)) |> 
    left_join(pooled_data_out_vars |> select(ID, age, partially_complete, all_complete), by = c("ID", "age")) |> 
    filter(all_complete | partially_complete)
  
  
  pooled_data_wide <- pooled_data_wide |> select(-starts_with("cbcl_item"))
  
  
data_list <- list(pooled_data_long = pooled_data_long, pooled_data_wide = pooled_data_wide, age_summareis = age_summaries)

if(include_acc){
  
  print("running include acc")
  pooled_data_acc <- map(
    dataset_strings, 
    ~readRDS(here::here(
      data_folder, 
      paste0(.,"_acc.Rds")
    )) |> mutate(ID = as.character(ID),
                 study = .)
  ) |> 
    bind_rows()
  colnames(pooled_data_acc) |> print()
  pooled_data_acc <-  pooled_data_acc |> 
    mutate(old_ID = ID,
           ID = paste0(study, "_", ID)) 
  
  pooled_data_acc |> count(include) |> print()
  data_list <- c(data_list, list(pooled_data_acc = pooled_data_acc))
}
  
print(" returning pooled data")

return(data_list)
}

summarise_pooled_age <- function(pooled_data) {
  pooled_data |>  select(ID, study, base_age, out_age, fu_length) |> 
    pivot_longer(cols = c(base_age, out_age, fu_length), values_to = "age") |>
    group_by(study, name) |> 
    sum_detail("age") |> 
    ungroup()
  
} 


print_age_summaries <- function(age_summaries) {
  age_summaries |> filter(name == "base_age") |> print()
  age_summaries |> filter(name == "out_age") |> print() # think more about eligibility descision
  age_summaries |> filter(name == "fu_length") |> print()
}
  
move_suffix_to_preffix <- Vectorize(function(string) {
  parts <- str_split_1(string, "_")
  suffix <- utils::tail(parts,1)
  first_part <- sub("_[^_]+$", "", string)
  new_string <-  paste0(suffix, "_", first_part)
  return(new_string)
})


one_hot_encode_study <- function(data) {
 unique_values <- unique(data$study)
  
  # Create the one-hot encoded columns
  for(value in unique_values) {
    new_col_name <- paste0("study_", value)
    data[[new_col_name]] <- ifelse(data$study == value, 1, 0)
  }
 return(data)
}  
