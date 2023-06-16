
make_summary_table <- function(data) {
  data_long <- data |> 
    select(where(~!is.character(.)), -ID, -wave) |> 
    pivot_longer(everything())
  summary_statistics <- data_long |> 
    group_by(name) |> 
    summarise(n = sum(!is.na(value)),
              mean  = qwraps2::mean_sd(value, na_rm = TRUE, denote_sd = "paren", show_n = 'never'),
              median = qwraps2::median_iqr(value, na_rm = TRUE, show_n = 'never'),
              minmax = paste(min(value, na.rm = TRUE), max(value, na.rm  = TRUE), sep = ","))

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
              minmax = paste(min(value, na.rm = TRUE), max(value, na.rm  = TRUE), sep = ","))
              #min = min(value, na.rm = TRUE),
              #max = max(value, na.rm = TRUE))
  return(summary_statistics)
}

make_summary_table_by_wave <- function(data) {
  data_wave <- divide_by_wave(data)
  summary_tables <- lapply(data_wave, make_summary_table)
  return(summary_tables)
}

