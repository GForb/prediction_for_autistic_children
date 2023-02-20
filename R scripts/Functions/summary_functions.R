library(tidyverse)
library(here)
#this is a really good helper function so I'll put it here as well 
divide_by_wave <- function(data, n_o_waves){
  data_wave <- list()
  for (i in 1:n_o_waves) {
    subseted <- data |> 
      filter(wave == i)
    data_wave[i] <- list(subseted)
  }
  return(data_wave)
}

make_summary_table <- function(data) {
  data_long <- data |> 
    select(where(~!is.character(.)), -ID, -wave) |> 
    pivot_longer(everything())
  summary_statistics <- data_long |> 
    group_by(name) |> 
    summarise(n = sum(!is.na(value)),
              mean = mean(value,na.rm = TRUE), 
              sd = sd(value, na.rm = TRUE),
              median = median(value, na.rm =TRUE),
              p25 = quantile (value, probs = 0.25, na.rm = TRUE),
              p75 = quantile (value, probs = 0.75, na.rm = TRUE),
              min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE))
  return(summary_statistics)
}

make_summary_by_wave <- function(data, n_o_waves) {
  data_wave <- divide_by_wave(data, n_o_waves)
  summary_tables <- lapply(data_wave, make_summary_table)
  return(summary_tables)
}

make_summary_by_wave(gui_data, 3)
