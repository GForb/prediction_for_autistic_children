back_transform_rsq <- function(transformed_rsq) {
  rsq <- 1-exp(-transformed_rsq)
  return(rsq)
}

delta_method_se <- function(transformed_rsq, se_transformed_rsq) {
  se <- se_transformed_rsq  * exp(-transformed_rsq)
  return(se)
}

back_transform_rsq_cols <- function(data) {
  original_col_order <- colnames(data)
  
  # Columns to be transformed by back_transform_rsq
  back_transform_cols <- c("est", "est_cv", "ci.lb", "ci.ub", "pi.lb", "pi.ub")
  
  # Columns to be transformed by delta_method_se
  delta_transform_cols <- c("tau", "se")
  
  # Apply back_transform_rsq to the relevant columns
  data <- data |> 
    mutate(across(
      any_of(back_transform_cols),
      ~ if_else(metric == "r_squared_transformed", back_transform_rsq(.), .),
      .names = "transformed_{col}"
    ))
  
  # Apply delta_method_se to the relevant columns
  data <- data |> 
    mutate(across(
      any_of(delta_transform_cols),
      ~ if_else(metric == "r_squared_transformed", delta_method_se(est, .), .),
      .names = "transformed_{col}"
    ))
  
  # Rename the transformed columns back to their original names
  data <- data |> 
    select(-any_of(c(back_transform_cols, delta_transform_cols))) |>
    rename_with(~ sub("transformed_", "", .), starts_with("transformed_")) |> 
    select(all_of(original_col_order))
  

  
  
  return(data)
    
}
  