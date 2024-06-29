get_counts_table_by_study <- function(data) {
  table_data <- data |> 
    pivot_longer(where(is.numeric), names_to = "variable_name", values_to = "value") |> 
    group_by(variable_name, study) |> 
    summarise(n = sum(!is.na(value)), 
              N = n()) |>
    ungroup() |> 
    mutate(n = paste0(n, " (", round((n/N*100)), "%)"),
           variable_name = get_label(variable_name)) |> 
    select(-N) |> 
    pivot_wider(names_from = study, values_from = n)
  
  n_studies <- length(unique(data$study))
  if(n_studies > 1){
    totals <- data |> 
      pivot_longer(where(is.numeric), names_to = "variable_name", values_to = "value") |> 
      group_by(variable_name) |> 
      summarise(n = sum(!is.na(value)), 
                N = n()) |>
      ungroup() |> 
      mutate(Overall = paste0(n, " (", round((n/N*100)), "%)")) |> 
      select(-N,-n, -variable_name) 
    
    table_data <- bind_cols(table_data, totals)
  }
  return(table_data)
}