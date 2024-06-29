studies = c("mcs", "ssc")
pooled_data = tibble()
for(dataset in studies){
  load(here(derived_data, paste0(dataset, ".Rdata")))
  data <- get(paste0(dataset, "_data"))
  pooled_data <- bind_rows(pooled_data, data)
}


pooled_data_wide <- pooled_data |> 
  filter(wave == base_wave | 
           wave == out_wave, 
         autism ==1) |> 
  mutate(wave = wave - base_wave) |> 
  make_wide_dataset() |> 
  filter(!is.na(age_0), !is.na(age_1)) |> 
  mutate(study_factor = factor(study))

pooled_data_wide |> janitor::tabyl(study, hearing_impairment_0) |> 
  janitor::adorn_totals("row") |> 
  janitor::adorn_percentages("row")

pooled_data_wide |> janitor::tabyl(study, visual_impairment_0) |> 
  janitor::adorn_totals("row") |> 
  janitor::adorn_percentages("row")

pooled_data_wide |> janitor::tabyl(study, epilepsy_0) |> 
  janitor::adorn_totals("row") |> 
  janitor::adorn_percentages("row")

