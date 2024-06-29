# Aim: Create file in long format with one row per participant and wave

library(tidyverse)

data_folder <- here::here(raw_data, "TEDS")

data  <- haven::read_sav(here::here(data_folder, "Data For Gordon - ADOS, SDQ, IQ, CAST.sav"))
colnames(data) 
data |>   count(group_splitHTandDx)
autistic_data <- data  |> filter(group_splitHTandDx==1)

ids <- autistic_data[, 1:3]

write.csv(ids, here::here(data_folder, "autistc_ids.csv"))