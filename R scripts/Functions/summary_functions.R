library(tidyverse)
library(here)

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



