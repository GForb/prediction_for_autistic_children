
minmax <- function(value) {
  paste(min(value, na.rm = TRUE) |> round(1), 
        max(value, na.rm  = TRUE)|> round(1), 
        sep = ", ")
}

make_summary_table <- function(data) {
  data_long <- data |> 
    select(where(~!is.character(.)), -ID, -wave) |> 
    pivot_longer(everything())
  summary_statistics <- data_long |> 
    group_by(name) |> 
    summarise(n = sum(!is.na(value)),
              mean  = qwraps2::mean_sd(value, na_rm = TRUE, denote_sd = "paren", show_n = 'never'),
              median = qwraps2::median_iqr(value, na_rm = TRUE, show_n = 'never'),
              minmax = minmax(value))

              #paste("mean (sd)") = mean_sd(value, na.rm = TRUE, denote_sd = "paren"), 
              #sd = sd(value, na.rm = TRUE),
              #median = median(value, na.rm =TRUE),
              #p25 = quantile (value, probs = 0.25, na.rm = TRUE),
              #p75 = quantile (value, probs = 0.75, na.rm = TRUE),
              #min = min(value, na.rm = TRUE),
              #max = max(value, na.rm = TRUE))
  return(summary_statistics)
}

make_summary_by_wave <- function(data) {
  data_long <- data |> 
    select(where(~!is.character(.)), -ID, -wave) |> 
    pivot_longer(everything())
  wave_data <- data$wave
  no_vars <- length(data |> select(where(~!is.character(.)), -ID, -wave))
  wave_data <- rep(wave_data, each = no_vars)
  data_long$wave <- wave_data
  summary_statistics <- data_long |> 
    group_by(name, wave) |> 
    summarise(n = sum(!is.na(value)),
              mean  = mean_sd(value, na_rm = TRUE, denote_sd = "paren"),
              #sd = sd(value, na.rm = TRUE),
              median = median_iqr(value, na_rm = TRUE),
              #median = median(value, na.rm =TRUE),
              #p25 = quantile (value, probs = 0.25, na.rm = TRUE),
              #p75 = quantile (value, probs = 0.75, na.rm = TRUE),
              minmax = minmax(value))
              #min = min(value, na.rm = TRUE),
              #max = max(value, na.rm = TRUE))
  return(summary_statistics)
}

make_summary_table_by_wave <- function(data) {
  data_wave <- divide_by_wave(data)
  summary_tables <- lapply(data_wave, make_summary_table)
  return(summary_tables)
}

sum_detail_by_study <- function(data, column) {
  data |> 
    group_by(studyid) |> 
    sum_detail(column)
  
}

sum_detail <- function(data, column) {
  data |> 
    summarise(n = sum(!is.na(.data[[column]])),
              n_distinct = n_distinct(.data[[column]], na.rm = TRUE),
              mean = mean(.data[[column]], na.rm = TRUE),
              var = var(.data[[column]], na.rm = TRUE),
              median = median(.data[[column]], na.rm = TRUE),
              p25 = quantile(.data[[column]], na.rm = TRUE, probs = 0.25),
              p75 = quantile(.data[[column]], na.rm = TRUE, probs = 0.75),
              min = min(.data[[column]], na.rm = TRUE),
              max = max(.data[[column]], na.rm = TRUE)
    )
}

summarise_variables <- function(data) {
  data |> pivot_longer(-any_of("studyid"), values_to = "value", names_to = "variable") |> 
    group_by(variable) |> 
    sum_detail("value")
  
}

summarise_age <- function(data) {
  data |>  summarise(mean_age = mean(age, na.rm = TRUE), 
                     min_age = min(age, na.rm = TRUE),
                     max_age = max(age, na.rm = TRUE),
                     n = sum(!is.na(age))) |> 
    print(n = 30)
  
} 



