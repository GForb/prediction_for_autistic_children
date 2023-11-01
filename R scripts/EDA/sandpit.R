
source(here::here("R scripts/config.R"))
library(lme4)
library(MASS)
library(RStata)
library(glue)
select <-  dplyr::select

options(digits=3)

data_folder <-  here::here(derived_data, "SDQ_gen_pop")

train_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic1000.rds"))
test_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic_test.rds"))


train_data_cc <- train_data |> select(ID, studyid, wave, out_wave, age, starts_with("base"), starts_with("sdq")) |> na.omit() |> data.frame()

test_data_cc <- test_data |> 
  select(ID, studyid, wave, out_wave, age, starts_with("base"), starts_with("sdq")) |> na.omit() |> data.frame()

train_data_stp  <-  train_data |> filter(wave == out_wave)
test_data_cc_stp <- test_data_cc |> filter(wave == out_wave)







