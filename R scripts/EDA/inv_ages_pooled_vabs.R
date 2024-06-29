pooled_data_vabs <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))
vabs_acc <- readRDS(here(derived_data, "pooled_vabs_acc.Rds"))

# Proportion of outcome data below ages:
pooled_data_vabs |> filter(age_out <13) |> nrow()/nrow(pooled_data_vabs)
pooled_data_vabs|> filter(age_out <14) |> nrow()/nrow(pooled_data_vabs)
pooled_data_vabs |> filter(age_out >17) |> nrow()/nrow(pooled_data_vabs)

pooled_data_vabs |> filter(age_base >13) |> nrow()/nrow(pooled_data_vabs)

# If I restrict to baseline prior to age 13, outcome post age 13, I'd keep 89% of the data - 408 people.
strict_13_data <- pooled_data_vabs |> filter(age_base <13 & age_out >13 & age_out < 17) 
nrow(strict_13_data)
nrow(strict_13_data)/nrow(pooled_data_vabs)

strict_13_data |> summarise_pooled_age() |>   print_age_summaries()

vabs_acc |> count(study, include)
vabs_acc|> filter(include %in% c("eligible baseline, no followup in range", "ineligible follow up length")) # 6 entries - have checked all manually that data is shoriing correctly
